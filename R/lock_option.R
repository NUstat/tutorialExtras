# load isds_setup on load to prevent errors
# user can define the options in setup
# don't think we need this?
.onLoad <- function(libname, pkgname) {
  isds_setup()
}

#' @title ISDS setup variables
#'
#' @description
#' Define if the tutorial is an exam and if there is a max_attempt limit on quiz questions
#' 
#' @param isds_exam defaults to FALSE. If the tutorial is an exam with a lock button set to TRUE.
#' @param max_attempt stop allowing submits if max_attempt is reached.
#'
#' @export
isds_setup <- function(isds_exam = FALSE, max_attempt = NULL){
  #hacky less than optimal solution
  #setting global variables accessible to override_exercise and override_quiz
  isds_exam <<- isds_exam
  max_attempt <<- max_attempt
  
  # need to set.seed that changes every "reattempt"
  init.seed <<- Sys.info()["user"]
  print(Sys.info())
  print(init.seed)
  
  tmp_dir <- tempdir()
  end_dir <- ifelse(!is.na(stringi::stri_locate_last_fixed(tmp_dir, "/")[,1]),
                    stringi::stri_locate_last_fixed(tmp_dir, "/")[,1],
                    stringi::stri_locate_last_fixed(tmp_dir, "\\")[,1])
  mod_dir <- stringr::str_sub(tmp_dir, end = end_dir)
  
  # get attempt if it exists
  if(file.exists(paste0(mod_dir,"attempt.RData"))){
    load(file = paste0(mod_dir,"attempt.RData"))
  }
  
  attempt <<- ifelse(exists("attempt"), attempt, 1)
  
  TeachingDemos::char2seed(paste0(init.seed, attempt))
  
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#' @title Tutorial lock button
#'
#' @description
#' Lock all "submit" buttons.
#'
#' Shiny ui and server logic for the lock computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `lock_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `lock_button_ui()`, must *not* be included in an R chunk with a `context`.
#'
#' @param id ID matching ui with server
#' @param label Label to appear on the submit grade button
#'
#'
#' @export
lock_button_ui <- function(id, label = "lock exam") {
  ns <- NS(id)
  tagList(
    actionButton( ns("lock"), label = label),
    uiOutput(ns("dwnld"))
  )
}


#' @title View exam submission output
#'
#' @description
#' Obtain output for exam.
#' @param id ID matching ui with server
#' @param label Label for view button
#' @export
exam_output_ui <- function(id, label = "Check submissions") {
  ns <- NS(id)
  
  tagList(
    actionButton( ns("viewExam"), label = label), 
    tableOutput(ns("tblExam"))
  )
  
}

# Define the server logic for a module to lock and grade exam
#' @title Exam lock and grade server
#' @param id ID matching ui with server
#' @param num_blanks TRUE/FALSE: Set the number of points for a question equal to the number of blanks. Default TRUE.
#' @param show_correct TRUE/FALSE: Whether or not to show points for each question in grade output. Default TRUE.
#' @param graded Either NULL or a vector containing the names of each question/exercise.
#' @param graded_pts Either NULL or a vector containing the number of points corresponding to each question/exercise in graded.
#' @param ex Either NULL or a vector containing the names of each question/exercise.
#' @param ex_pts Either NULL or a vector containing the number of points corresponding to each question/exercise in ex.
#' @param manual Either NULL or a vector containing the names of each question/exercise to be manually graded.
#' @param manual_pts Either NULL or a vector containing the number of points corresponding to each question/exercise in manual.
#' @param exclude Either NULL or a vector containing the names of each question/exercise to exclude from grading.
#' @param tz Time zone to display start time on report.
#' @export
lock_server <- function(id, num_blanks = TRUE, show_correct = FALSE,
                        graded = NULL, graded_pts = NULL, 
                        ex = NULL, ex_pts = NULL, 
                        manual = NULL, manual_pts = NULL,
                        exclude = NULL, tz = Sys.timezone()) {
  moduleServer(
    id,
    function(input, output, session) {
     
      # Set time and determine if download should be shown
      observeEvent(
        req(session$userData$learnr_state() == "restored"),{
        ns <- getDefaultReactiveDomain()$ns
        
        # if session restarted get original start time or set start time to now
        start_time <<- ifelse(is.null(learnr:::get_object(session, NS("time", id = "start"))$data$time),
                              learnr:::timestamp_utc(), learnr:::get_object(session, NS("time", id = "start"))$data$time)
        # save original start time as object for restart
        learnr:::save_object(session, object_id = NS("time", id = "start"),
                             learnr:::tutorial_object("time",
                                                      list(time = start_time) ) )
        
        #show download button if lock is pressed
        output$dwnld <- renderUI({
          if(is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock)){
            return(NULL)
          }else{
            downloadButton(ns("downloadExam"), label = "Download Exam")
          }
        })
      })
      
########################################################################
########################################################################
########################################################################
########################################################################
      # View grade
      observeEvent(input$viewExam, {
        ns <- getDefaultReactiveDomain()$ns
        
        tutorial_info <- isolate(get_tutorial_info())
        #--------------------------------------------------------------------
        
        #define rubric points
        if(!is.null(graded)){
          rubric_1 <- tidyr::tibble(label = graded,
                                    pts_possible = graded_pts,
                                    eval = rep("question", length(label)))
        }else{rubric_1 <- NULL}
        if(!is.null(ex)){
          rubric_2 <- tidyr::tibble(label = ex,
                                    pts_possible = ex_pts,
                                    eval = rep("exercise", length(label)))
        }else{rubric_2 <- NULL}
        if(!is.null(manual)){
          rubric_3 <- tidyr::tibble(label = manual,
                                    pts_possible = manual_pts,
                                    eval = rep("manual", length(label)) )
        }else{rubric_3 <- NULL}
        
        set_pts <- rbind(rubric_1, rubric_2, rubric_3)
        
        rubric_tmp <- tidyr::tibble(label = tutorial_info$items$label)
        
        if(!is.null(set_pts)){
          rubric <- dplyr::left_join(rubric_tmp, set_pts, by = "label") #%>%
          #mutate(pts_possible = ifelse(is.na(pts_possible), 1, pts_possible))
        }else{
          rubric <- rubric_tmp %>%
            mutate(pts_possible = rep(NA, length(label)),
                   eval = rep(NA, length(label)))
        }
        #Set up rubric points complete
        #--------------------------------------------------------------------
        
        get_grades <- isolate(learnr::get_tutorial_state())
        
        #---------------------------------------------
        # for some reason sometimes doesn't always grab exercises
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
        
        if(rlang::is_empty(table)){
          return()
        }
        
        # merge rubric of all questions with table of submitted questions
        grades <- dplyr::left_join(rubric, table, by = "label") %>%
          # set question type for display
          dplyr::mutate(eval = ifelse(!is.na(eval), eval,
                                      ifelse(!is.na(type), type, eval)),
                        partial_cred = ifelse(is.na(partial_cred), as.numeric(correct), partial_cred),
                        #calculate time since exam start
                        time = round(as.numeric(difftime(timestamp, start_time, units="mins")), 2)) %>%
          dplyr::select(label, answer)
        
        # only print unsubmitted
        unsubmitted <- grades %>% 
          filter(is.na(answer) | answer == "Not Submitted")
        
        caption <- ifelse(nrow(unsubmitted) == 0,
                      "All questions and exercises have been submitted",
                      "Unsubmitted questions/exercises will receive a 0.")
        
        output$tblExam <- renderTable({
          unsubmitted
        }, caption = caption)
        
      })
      
########################################################################
########################################################################
      # lock exam and show download button
      # register that lock was pressed and reload exam to lock all questions
      observeEvent(input$lock, {
        ns <- getDefaultReactiveDomain()$ns
        
        # Change global lock identifier to "pressed"
        learnr:::save_object(session, NS("lock", id = "pressed"), 
                             learnr:::tutorial_object("lock",
                                                      list(lock = TRUE) ) )
        
        # trigger a reload to resubmit questions forcing the lock
        session$reload()
        
      }) #close observe event
      
########################################################################
########################################################################
      
      # Download grade handler
      output$downloadExam <- downloadHandler(
        filename = function() {
          paste0("STAT_Exam_", Sys.time(),
                 ".html")
        },
        content = function(file) {
          ns <- getDefaultReactiveDomain()$ns
          
          tutorial_info <- isolate(get_tutorial_info())
          
          #error checking for label names provided
          #check if exclude list is valid
          #remove error checking because added random question selection
          # all_labels <- c(graded, ex, manual, exclude)
          # if(!is.null(all_labels)){
          #   purrr::map(all_labels, function(x){
          #     if(!(x %in% tutorial_info$items$label)){
          #       stop(paste0(x, " is not a name of a question or exercise."))
          #     }
          #   })
          # }
          # add error checking that length names match length points
          
          # Error checking complete
          #--------------------------------------------------------------------
          
          #define rubric points
          if(!is.null(graded)){
            rubric_1 <- tidyr::tibble(label = graded,
                                      pts_possible = graded_pts,
                                      eval = rep("question", length(label)))
          }else{rubric_1 <- NULL}
          if(!is.null(ex)){
            rubric_2 <- tidyr::tibble(label = ex,
                                      pts_possible = ex_pts,
                                      eval = rep("exercise", length(label)))
          }else{rubric_2 <- NULL}
          if(!is.null(manual)){
            rubric_3 <- tidyr::tibble(label = manual,
                                      pts_possible = manual_pts,
                                      eval = rep("manual", length(label)) )
          }else{rubric_3 <- NULL}
          
          set_pts <- rbind(rubric_1, rubric_2, rubric_3)
          
          rubric_tmp <- tidyr::tibble(label = tutorial_info$items$label)
          
          if(!is.null(set_pts)){
            rubric <- dplyr::left_join(rubric_tmp, set_pts, by = "label") #%>%
              #mutate(pts_possible = ifelse(is.na(pts_possible), 1, pts_possible))
          }else{
            rubric <- rubric_tmp %>%
              mutate(pts_possible = rep(NA, length(label)),
                     eval = rep(NA, length(label)))
          }
          #Set up rubric points complete
          #--------------------------------------------------------------------
          
          # get and organize all user submission questions and exercises
          get_grades <- isolate(learnr::get_tutorial_state())
     
          #---------------------------------------------
          # for some reason sometimes doesn't always grab exercises
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
          
          # # catch error - if empty do not continue
          if(rlang::is_empty(table)){
            return()
          }
          
          # merge rubric of all questions with table of submitted questions
          grades <- dplyr::left_join(rubric, table, by = "label") %>%
            # not needed for exams
            dplyr::select(-attempt) %>%
            # set question type for display
            dplyr::mutate(eval = ifelse(!is.na(eval), eval,
                                        ifelse(!is.na(type), type, eval)),
                          partial_cred = ifelse(is.na(partial_cred), as.numeric(correct), partial_cred),
                          #calculate time since exam start
                          time = round(as.numeric(difftime(timestamp, start_time, units="mins")), 2))
          
          # handle pts_possible 1) priority goes to manual setting 
          # 2) then to num_blanks setting 3) then default to 1
          if(isTRUE(num_blanks)){
            grades <- grades %>% 
              dplyr::mutate(
                # set all question to number of blanks
                pts_possible = ifelse(is.na(pts_possible), blanks, pts_possible),
                pts_earned = pts_possible * as.numeric(partial_cred),
                pts_earned = ifelse(is.na(pts_earned), 0, pts_earned),
              )
          }
          if(isFALSE(num_blanks)){
            grades <- grades %>% 
              dplyr::mutate(
                # set all questions to 1 point
                pts_possible = ifelse(is.na(pts_possible), 1, pts_possible),
                pts_earned = pts_possible * as.numeric(partial_cred),
                pts_earned = ifelse(is.na(pts_earned), 0, pts_earned),
              )
          }
          
          # need to get name before removing "excluded" questions
          # if there is a code chunk question labeled "Name" get the name
          tmp_name <- ifelse("Name" %in% grades$label, 
                             grades %>% dplyr::filter(label == "Name") %>% dplyr::pull(answer),
                             NA)
          
          user_name <- ifelse(is.na(tmp_name), 
                              tutorial_info$user_id,
                              tmp_name
                              )
          
          # exclude questions if listed
          if(!is.null(exclude)){
            grades <- grades %>%
              dplyr::filter(!(label %in% exclude))
          }
          
          # submission organization complete
          #--------------------------------------------------------------------
          
          # Divide into subsections for rendered report
          graded <- grades %>%
            dplyr::filter(eval == "question") %>% 
            dplyr::select(label, answer, time, pts_earned)
            
          
          score <- ifelse(nrow(graded) == 0, 0, sum(graded$pts_earned))
          
          if(show_correct == FALSE){
            graded <- graded %>% select(-pts_earned) #remove pts earned
          }
          
          exercises <- grades %>%
            dplyr::filter(eval == "exercise") %>%
            transpose()
          
          manual <- grades %>%
            dplyr::filter(eval == "manual") %>%
            dplyr::select(label, answer, time) 
            #dplyr::select(label, answer, time, pts_earned) 
          
          incomplete <- grades %>%
            dplyr::filter(is.na(eval)) %>%
            dplyr::select(label, answer, time) 
          #dplyr::select(label, answer, time, pts_earned)
          
          #--------------------------------------------------------------------
          # Create string of each section header
          yaml_string <- c("---", 
                           toString(paste0("title: ", tutorial_info$tutorial_id)), 
                           toString(paste0("author: ", user_name)),
                           toString(paste0("date: ", format(as.POSIXct(start_time, tz = "UTC"), tz = tz, usetz=TRUE)) ), "---")
         
          graded_string <- c(toString(paste0("# Concept  ", score)), "```{r concept, echo=FALSE}", "graded %>% knitr::kable()", "```")
          
          
          exercise_substring <- map(exercises, function(x){
            c(toString(paste0("### ", x$label, " - ", ifelse(isTRUE(x$correct), "Correct", "Needs Grading")) ), 
              toString(paste0("Time: ", x$time)),
              toString(paste0("```{r ", x$label, ", echo = TRUE}")), 
              as.character(x$answer), "```", " ")
          })
          
          exercise_substring <- unlist(exercise_substring)
          
          exercise_string <- c("# Exercises", exercise_substring)
          
          manual_string <- c("# Manually Graded", "```{r manual, echo=FALSE}", "manual %>% knitr::kable()", "```")
          
          if(nrow(manual) == 0){
            manual_string <- c(" ", "# Manually Graded", "No manually graded questions.", " ")
          }
          
          dnf_string <- c("# Incomplete Problems", "```{r incomplete, echo=FALSE}", "incomplete %>% knitr::kable()", "```")
          
          if(nrow(incomplete) == 0){
            dnf_string <- c("# Incomplete Problems", "No incomplete problems.")
          }
          
          #--------------------------------------------------------------------
          # create a temporary empty file to write rmd and data folder to
          tmp_dir <- tempdir()
          
          #--------------------------------------------------------------------
          # write data folder to a temporary directory
          file.copy("data", tmp_dir, recursive = TRUE)
          # Need to load required datasets and packages
          file_names <- list.files("data", full.names=FALSE)
          dataset_substring <- purrr::map(file_names, function(x){
            if(grepl(".csv", x, fixed = TRUE)){
              paste0(gsub(".csv", "", x), " <- read.csv('", gsub("\\\\", "/", tmp_dir), "/data/", x, "')")
            }else if(grepl(".rds", x, fixed = TRUE)){
              paste0(gsub(".rds", "", x), " <- read_rds('", gsub("\\\\", "/", tmp_dir), "/data/", x, "')")
            }else if(grepl(".rda", x, fixed = TRUE)){
              paste0("load('", gsub("\\\\", "/", tmp_dir), "/data/", x, "')")
            }
          })
          
          setup_string <- c("```{r setup, include = FALSE, cache = FALSE}",
                            "knitr::opts_chunk$set(error = TRUE)",
                            unlist(dataset_substring), "```")
          
          #--------------------------------------------------------------------
          tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".Rmd")
          # write to Rmd
          writeLines(c(yaml_string, setup_string, graded_string, 
                       exercise_string, manual_string, dnf_string),
                     con = tmp_file)
          # render to html
          output <- rmarkdown::render(tmp_file)
          # download html
          file.copy(output, file)
        },
        contentType = "text/html"
      )
      
    }) #close module server
} #close main function


#################################################################
#################################################################
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#' @title Tutorial reset button
#'
#' @description
#' Reset the entire exam to allow another attempt.
#'
#' Shiny ui and server logic for the lock computation.
#'
#' Note that when including these functions in a learnr Rmd document it is necessary that
#' the server function, `reset_server()`, be included in an R chunk where `context="server"` as
#' they interact with the underlying Shiny functionality. Conversely, the ui function,
#' `reset_button_ui()`, must *not* be included in an R chunk with a `context`.
#'
#' @param id ID matching ui with server
#' @param label Label to appear on the submit grade button
#'
#'
#' @export
reset_button_ui <- function(id, label = "retry exam") {
  ns <- NS(id)
  tagList(
    actionButton( ns("reset"), label = label)
  )
}

# Define the server logic for a module to lock exam
#' @title Tutorial reset server
#' @param id ID matching ui with server
#' @export
reset_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset, {
        ns <- getDefaultReactiveDomain()$ns
        
        # clear all question and exercise cache?
        #learnr:::clear_tutorial_cache()
        
        # YES this resets all questions and exercises
        # this does NOT reset global variables
        learnr:::remove_all_objects(session)
        
        attempt <<- attempt + 1
        tmp_dir <- tempdir()
        end_dir <- ifelse(!is.na(stringi::stri_locate_last_fixed(tmp_dir, "/")[,1]),
                          stringi::stri_locate_last_fixed(tmp_dir, "/")[,1],
                          stringi::stri_locate_last_fixed(tmp_dir, "\\")[,1])
        mod_dir <- stringr::str_sub(tmp_dir, end = end_dir)
        
        save(attempt, file = paste0(mod_dir,"attempt.RData"))
        # trigger a reload to resubmit questions and unlock
        
        #set new seed
        init.seed <- Sys.info()["user"]
        TeachingDemos::char2seed(paste0(init.seed, attempt))
        
        print(Sys.info())
        
        print(rappdirs::user_data_dir())
        
        script <- list.files(path = "rappdirs::user_data_dir()", full.names = TRUE)
        print(script)
        
        #print(file.path(rappdirs::user_data_dir(), "R", "learnr", "tutorial", "storage", Sys.info()["user"], learnr:::get_tutorial_info()$tutorial_id))
        
        # temp_directory <- tempdir()
        # tutorial_storage_directory <- file.path(temp_directory, ".learnr")
        # 
        # script <- list.files(tutorial_storage_directory, full.names = TRUE)
        # print(script)
        # 
        # 
        # script2 <- list.files(file.path(rappdirs::user_data_dir(), "R", "learnr", "tutorial", "storage"), full.names = TRUE)
        # print(script2)
        # 
        # 
        # print(list.files(withr::local_tempdir()))
        # NEED TO CLEAR PRERENDERED OUTPUT AND RERUN
        
        #learnr:::run_clean_tutorial_prerendered(tmp)
        session$reload()
        
      }) #close observe event
      
      
    }) #close module server
} #close main function
#################################################################
#################################################################


#################################################################
#################################################################
# need to calculate outside of observe event so that it can apply to download handler
submissions <- function(get_grades = list()){
  # get and organize all user submission questions and exercises
  # organize submissions in a list
  table_list <- map(names(get_grades), function(x){
    # handle multiple answer issues
    get_grades[[x]]$blanks <- length(get_grades[[x]]$answer)
    
    get_grades[[x]]$answer <- toString(get_grades[[x]]$answer)
    
    #if empty set to FALSE
    get_grades[[x]]$correct <- ifelse(rlang::is_empty(get_grades[[x]]$correct), 
                                      FALSE,
                                      get_grades[[x]]$correct)
    
    # handle numeric 0 issues
    if(get_grades[[x]]$type == "question"){
      get_grades[[x]]$partial_cred <- ifelse(length(get_grades[[x]]$partial_cred) == 0, 
                                             NA, get_grades[[x]]$partial_cred)
    }
    
    store <- get_grades[[x]] %>%
      tidyr::as_tibble()
    
    store$label = x
    
    if(store$type == "exercise"){
      #if this column exists proceed...
      if("answer_last" %in% colnames(store)){
        store <- store %>%
          dplyr::mutate(answer = answer_last,
                        correct = correct_last,
                        partial_cred = NA) %>%
          #timestamp = time_last
          dplyr::select(-c(answer_last, correct_last))
      }
    }
    # fix possible data typing errors
    store %>%
      dplyr::mutate(label = as.character(label),
                    answer = as.character(answer),
                    attempt = as.numeric(attempt),
                    timestamp = time_last) %>% 
      dplyr::select(-time_last)
  })
  
  table <- dplyr::bind_rows(table_list)
  
  return(table = table)
}


