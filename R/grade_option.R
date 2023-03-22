#' @title Tutorial grade button
#'
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#'
#' Shiny ui and server logic for the grade computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `grade_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `grade_ui()`, must *not* be included in an R chunk with a `context`.
#'
#' @param id ID matching ui with server
#' @param label Label to appear on the submit grade button
#'
#' @import dplyr
#' @import tidyr
#' @export
grade_button_ui <- function(id, label = "grade tutorial") {
  ns <- NS(id)
  tagList(
    actionButton( ns("button"), label = label)
  )
}

#' @title Tutorial print grade
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#' @param id ID matching ui with server
#' @param label Label to appear on the button
#' @export
grade_print_ui <- function(id, label = "Download Grade") {
  ns <- NS(id)

  tagList(
    #actionButton( ns("printGrade"), label = "Download Grade"),
    downloadButton(ns("downloadHTML"), label)
  )
}

#' @title Tutorial grade output
#'
#' @description
#' Obtain grade on all question and exercise submissions for a user.
#' @param id ID matching ui with server
#' @export
grade_output_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tableOutput(ns("grade"))
  )

}

# Define the server logic for a module to compute grade
#' @title Tutorial grade server
#' @param id ID matching ui with server
#' @param label Either NULL or a vector containing the names of each question/exercise.
#' @param pts_possible Either NULL or a vector containing the number of points corresponding to each question/exercise in label.
#' @param num_try Number of tries allowed before grade deduction on that question. Default is 3.
#' @param deduction The percent (as a decimal) to be deducted for each additional incorrect attempt after num_try. Default is 0.1.
#' @param exclude Either NULL or a vector of names of questions/exercises to exclude.
#'
#' @export
grade_server <- function(id, label = NULL, pts_possible = NULL, num_try = 3, deduction = 0.1, exclude = NULL ) {
  moduleServer(
    id,
    function(input, output, session) {
    # View grade
    observeEvent(input$button, {

      ns <- getDefaultReactiveDomain()$ns
      
      tutorial_info <- isolate(get_tutorial_info())
      
      #check if exclude list is valid
      if(!is.null(exclude)){
         purrr::map(exclude, function(x){
           if(!(x %in% tutorial_info$items$label)){
             stop(paste0(x, " is not a name of a question or exercise."))
          }
        })
      }
      #check if label and pts_possible are valid
      #names(isolate(learnr:::get_tutorial_cache())
      if(!is.null(label) || !is.null(pts_possible)){
        if (length(label) != length(pts_possible)) {
          stop("Length of label must equal length of pts_possible.")
        }
        map(label, function(x){
          if( !(x %in% tutorial_info$items$label)){
            stop(paste0(x, " is not a name of a question or exercise."))
          }
        })
        rubric <- tidyr::tibble(label = label, 
                         pts_possible = as.numeric(pts_possible))
      }else{
        rubric <- tidyr::tibble(label = tutorial_info$items$label,
                         pts_possible = rep(1, length(label)) )
      }
      
      
      #this will get number of attempts and if correct
      get_grades <- isolate(learnr::get_tutorial_state())
      # create a list of each question/exercise
      table_list <- map(names(get_grades), function(x){
        get_grades[[x]]$answer <- toString(get_grades[[x]]$answer)
        
        store <- get_grades[[x]] %>% 
          tidyr::as_tibble()
        
        store$label = x
        
        if(store$type == "exercise"){
          #if this column exists proceed...
          if("answer_last" %in% colnames(store)){
            store <- store %>% 
              dplyr::mutate(answer = answer_last,
                            correct = correct_last,
                            timestamp = time_last) %>% 
              dplyr::select(-c(answer_last, correct_last, time_last))
          }
        }
        # fix possible data typing errors
        store %>% 
          dplyr::mutate(label = as.character(label),
                        answer = as.character(answer),
                        attempt = as.numeric(attempt))
      })
      # turn table_list into tibble
      table <- dplyr::bind_rows(table_list) 
      # catch error - if empty do not continue
      if(rlang::is_empty(table)){
        return()
      }
      
      grades <- dplyr::left_join(rubric, table, by = "label")
      
      calc <- grades %>% 
        dplyr::mutate(deduction = ifelse(attempt > num_try, deduction*(attempt - num_try), 0),
               deduction = ifelse(deduction > 1, 1, deduction),
               pts_earned = pts_possible *as.numeric(correct)*(1-deduction),
               pts_earned = ifelse(is.na(pts_earned), 0, pts_earned))
      
      #if there is a code chunk question labeled "Name" get the name
      user_name <- ifelse("Name" %in% calc$label, calc %>% dplyr::filter(label == "Name") %>% dplyr::pull(answer),
                     tutorial_info$user_id)
      
      if(!is.null(exclude)){
        calc <- calc %>% 
          dplyr::filter(!(label %in% exclude))
      }
      
      scaled <- round(10*sum(calc$pts_earned)/sum(calc$pts_possible), 2)
      
      output$grade <- renderTable({
        calc %>% 
          dplyr::select(label, pts_possible, correct, attempt, pts_earned)
      }, caption = paste0('<span style=\"font-size:30px; font-weight:normal; color:red\">',
                          scaled, "/10"))
      
      output$downloadHTML <- downloadHandler(
        filename = function() {
          paste0(tutorial_info$tutorial_id,
                 "-",
                 user_name,
                ".html")
        },
        content = function(file) {
          ns <- getDefaultReactiveDomain()$ns
          
          tab_html <- calc %>%
            as.data.frame() %>%
            dplyr::select(-c(type, answer, timestamp, deduction)) %>% 
            tableHTML::tableHTML(footer = paste0(format(as.POSIXct(Sys.time()),
                                                 tz = "America/Chicago",
                                                 usetz = TRUE), " - ",
                                                 tutorial_info$user_id),
                                 rownames = FALSE,
                                 caption = paste0("Scaled score: ",
                                                  scaled, "/10"),
                                 second_headers = list(c(5),
                                                       c(paste0(tutorial_info$tutorial_id,
                                                                 " - ",
                                                                 user_name)) 
                                                       ) ) %>%
            tableHTML::add_theme('rshiny-blue')
          
          tableHTML::write_tableHTML(tab_html,
                                     file)
        },
        contentType = "text/html"
      )
      
      
    }) #close observe event
    }) #close module server
  } #close main grade server



################################################################################
################################################################################
# GRADE AN EXAM