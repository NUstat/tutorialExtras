#' @title Tutorial grade button
#'
#' @description
#' View current grade and submission attempt on all question and exercises for a user.
#' This is used so users can check their progress when working on a tutorial 
#' that allows for multiple attempts, immediate feedback, and deductions after x attempts. 
#' The user can download their grade to an html by including the `grade_print_ui()` function.
#' 
#' Shiny ui and server logic for the grade computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `grade_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `grade_button_ui()`, must *not* be included in an R chunk with a `context`.
#'
#' @param id ID matching ui with server
#' @param label Label to appear on the submit grade button
#'
#' @import dplyr
#' @import tidyr
#' @export
grade_button_ui <- function(id, label = "View grade") {
  ns <- NS(id)
  tagList(
    actionButton( ns("button"), label = label),
    
    tableOutput(ns("grade"))
  )
}

#' @title Tutorial print grade
#' @description
#' Download an html grade of the user's tutorial.
#'
#' Shiny ui and server logic for the grade computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `grade_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `grade_print_ui()`, must *not* be included in an R chunk with a `context`.
#' 
#' @param id ID matching ui with server
#' @param label Label to appear on the button
#' @export
grade_print_ui <- function(id, label = "Download Grade") {
  ns <- NS(id)

  tagList(
    downloadButton(ns("downloadHTML"), label)
  )
}

# #' @title Tutorial grade output
# #'
# #' @description
# #' Obtain grade on all question and exercise submissions for a user.
# #' @param id ID matching ui with server
# #' @export
# grade_output_ui <- function(id) {
#   ns <- NS(id)
# 
#   tagList(
#     tableOutput(ns("grade"))
#   )
# 
# }

# Define the server logic for a module to compute grade
#' @title Tutorial grade server
#' @param id ID matching ui with server
#' @param label Either NULL or a vector containing the names of each question/exercise.
#' @param pts_possible Either NULL or a vector containing the number of points corresponding to each question/exercise in label.
#' @param num_try Number of tries allowed before grade deduction on that question. Default is 3.
#' @param deduction The percent (as a decimal) to be deducted for each additional incorrect attempt after num_try. Default is 0.1.
#' @param exclude Either NULL or a vector of names of questions/exercises to exclude.
#' @param tz Time zone to display start time on report.
#'
#' @export
grade_server <- function(id, label = NULL, pts_possible = NULL, num_try = 3, deduction = 0.1, exclude = NULL, tz = Sys.timezone() ) {
  moduleServer(
    id,
    function(input, output, session) {
    # View grade
    observeEvent(input$button, {
      grade <- grade_calc(session = session, id = id, label = label, pts_possible = pts_possible, num_try = num_try, deduction = deduction, exclude = exclude)
      
      output$grade <- renderTable({
        grade$calc %>% 
          dplyr::select(label, pts_possible, attempt, pts_earned)
      }, caption = paste0('<span style=\"font-size:30px; font-weight:normal; color:red\">',
                          grade$scaled, "/10"))
      
    }) #close observe event button View grade
    
    # move download handler outside of observe event
    output$downloadHTML <- downloadHandler(
        filename = function() {
          paste0("STAT_RC_", Sys.time(),
                 ".html")
          # paste0(tutorial_info$tutorial_id,
          #        "-",
          #        user_name,
          #        ".html")
        },
        content = function(file) {
          ns <- getDefaultReactiveDomain()$ns
          
          grade <- grade_calc(session = session, id = id, label = label, pts_possible = pts_possible, num_try = num_try, deduction = deduction, exclude = exclude)
          
          if(is.null(grade)){
            return()
          }
          
          tab_html <- grade$calc %>%
            as.data.frame() %>%
            dplyr::select(label, pts_possible, attempt, pts_earned) %>% 
            tableHTML::tableHTML(footer = paste0(format(as.POSIXct(Sys.time()),
                                                        #tz = "America/Chicago",
                                                        tz = tz,
                                                        usetz = TRUE), " - ",
                                                 #tutorial_info$user_id,
                                                 grade$user_name),
                                 rownames = FALSE,
                                 caption = paste0("Scaled score: ",
                                                  grade$scaled, "/10"),
                                 second_headers = list(c(5),
                                                       c(paste0(grade$tutorial_id,
                                                                " - ",
                                                                grade$user_name)) 
                                 ) ) %>%
            tableHTML::add_theme('rshiny-blue')
          
          tableHTML::write_tableHTML(tab_html,
                                     file)
        },
        contentType = "text/html"
      )
    }) #close module server
  } #close main grade server


#####################################################################################
#####################################################################################
# need to calculate outside of observe event so that it can apply to download handler
grade_calc <- function(session = session, id, label = NULL, pts_possible = NULL, 
                       num_try = 3, deduction = 0.1, exclude = NULL){
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
  
  ##################################################################
  #this will get number of attempts and if correct
  get_grades <- isolate(learnr::get_tutorial_state())
  
  #---------------------------------------------
  # for some reason sometimes doesn't always grab exercises
  # manually grab exercises just in case
  ex_names <- tutorial_info$items %>% 
    filter(type == "exercise") %>% 
    pull(label)
  
  for(ex in ex_names){
    if(is.na(names(get_grades[ex]))){
      # if ex submission is not in get_grades
      # manually grab submission and add to get_grades
      ns <- NS(ex)
      add_ex <- list(type = "exercise",
                     answer = 0,
                     correct = 0,
                     attempt = isolate(ifelse(is.null(learnr:::get_object(session, ns("count"))$data$numtry),
                                              0, learnr:::get_object(session, ns("count"))$data$numtry)),
                     answer_last = isolate(ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$code),
                                                  "Not Submitted", learnr:::get_object(session, ns("ex_submit"))$data$code)),
                     correct_last = isolate(ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$correct),
                                                   FALSE, learnr:::get_object(session, ns("ex_submit"))$data$correct)),
                     time_last = isolate(ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$time),
                                                learnr:::timestamp_utc(), learnr:::get_object(session, ns("ex_submit"))$data$time))
      )
      get_grades[[ex]] <- add_ex
      
    }
  }
  # End grab exercise fix
  #---------------------------------------------
  
  table <- ISDSfunctions:::submissions(get_grades = get_grades)
  
  # catch error - if empty do not continue
  if(rlang::is_empty(table)){
    return()
  }
  
  # merge rubric of all questions with table of submitted questions
  grades <- dplyr::left_join(rubric, table, by = "label")
  
  calc <- grades %>% 
    dplyr::mutate(deduction = ifelse(attempt > num_try, deduction*(attempt - num_try), 0),
                  deduction = ifelse(deduction > 1, 1, deduction),
                  pts_earned = pts_possible *as.numeric(correct)*(1-deduction),
                  pts_earned = ifelse(is.na(pts_earned), 0, pts_earned))
  
  # if there is a code chunk question labeled "Name" get the name
  user_name <- ifelse("Name" %in% calc$label, calc %>% dplyr::filter(label == "Name") %>% dplyr::pull(answer),
                      tutorial_info$user_id)
  
  # exclude specified questions
  if(!is.null(exclude)){
    calc <- calc %>% 
      dplyr::filter(!(label %in% exclude))
  }
  
  # grade out of 10
  scaled <- round(10*sum(calc$pts_earned)/sum(calc$pts_possible), 2)
  
  return(list(calc = calc, scaled = scaled, 
              user_name = user_name, tutorial_id = tutorial_info$tutorial_id))
}

