#' @title Tutorial lock button
#'
#' @description
#' Lock all "submit" buttons. This is used when the user can re-try questions and will
#' not receive feedback until they submit the entire tutorial. This will not technically
#' "lock" the exercise submit buttons, however, it does prevent the saving of any further 
#' exercise attempts. Once the tutorial is locked a download option will appear.
#'
#' It is recommended that 'lock_check_ui()' is included before the lock button so users
#' can check if they forgot to submit any questions or exercises.
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
#' @export
lock_button_ui <- function(id, label = "Lock Exam") {
  ns <- NS(id)
  tagList(
    actionButton( ns("lock"), label = label),
    uiOutput(ns("dwnld"))
  )
}

#' @title Check Exam Submission
#'
#' @description
#' View if any questions or exercises have not been submitted prior to locking the exam.
#' @param id ID matching ui with "lock_server()"
#' @param label Label for button
#' @export
lock_check_ui <- function(id, label = "Check Submissions") {
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
#' @param graded Either NULL or a vector containing the names of each question with custom points.
#' @param graded_pts Either NULL or a vector containing the number of points corresponding to each question in graded.
#' @param ex Either NULL or a vector containing the names of exercises with custom points.
#' @param ex_pts Either NULL or a vector containing the number of points corresponding to each exercise in ex.
#' @param manual Either NULL or a vector containing the names of each question to be manually graded.
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
        # if user clicks "start over" we do not want time to restart
        tutorial_id <- isolate(get_tutorial_info())$tutorial_id
        exam_dir <- paste0(mod_dir, tutorial_id)
        
        # remove so not lingering from other exams
        if(exists("start_time")){
          rm(start_time)
        }
        
        if(file.exists(paste0(exam_dir,"time.RData"))){
          # if file exists time has already started
          load(file = paste0(exam_dir,"time.RData"))
        }else{
          # if file does not exist we need to start the time
          start_time <- setup_time
        }
        
        start_time <<- start_time
        
        save(start_time, file = paste0(exam_dir, "time.RData"))
        
        ###################################################################################
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
# VIEW UNSUBMITTED QUESTIONS: lock_check_ui  ------------------------------
      observeEvent(input$viewExam, {
        ns <- getDefaultReactiveDomain()$ns
        
        tutorial_info <- isolate(get_tutorial_info())
        
        # get start time ----------------------------------------------------------
        tutorial_id <- tutorial_info$tutorial_id
        exam_dir <- paste0(mod_dir, tutorial_id)
        
        # remove so not lingering from other exams
        if(exists("start_time")){
          rm(start_time)
        }
        
        if(file.exists(paste0(exam_dir,"time.RData"))){
          # if file exists time has already started
          load(file = paste0(exam_dir,"time.RData"))
        }else{
          # if file does not exist we need to start the time
          start_time <- setup_time
        }
        
        start_time <<- start_time
          
        # ERROR CHECKING START-----------------------------------------------------
        
        #error checking for label names provided
        all_labels <- c(graded, ex, manual, exclude)
        if(!is.null(all_labels)){
          purrr::map(all_labels, function(x){
            if(!(x %in% tutorial_info$items$label)){
              stop(paste0(x, " is not a name of a question or exercise."))
            }
          })
        }
        # add error checking that length names match length points
        if(!is.null(graded) || !is.null(graded_pts)){
          if (length(graded) != length(graded_pts)) {
            stop("Length of graded must equal length of graded_pts.")
          }
        }
        if(!is.null(ex) || !is.null(ex_pts)){
          if (length(ex) != length(ex_pts)) {
            stop("Length of ex must equal length of ex_pts.")
          }
        }
        if(!is.null(manual) || !is.null(manual_pts)){
          if (length(manual) != length(manual_pts)) {
            stop("Length of manual must equal length of manual_pts.")
          }
        }
        
        # ERROR CHECKING END ------------------------------------------------------
        
        # DEFINE RUBRIC POINTS ----------------------------------------------------

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
          rubric <- dplyr::left_join(rubric_tmp, set_pts, by = "label") 
        }else{
          rubric <- rubric_tmp %>%
            mutate(pts_possible = rep(NA, length(label)),
                   eval = rep(NA, length(label)))
        }
        
        # RUBRIC POINTS COMPLETE ---------------------------------------------
        
        ################################################
        #this will get submissions and if correct
        get_grades <- isolate(learnr::get_tutorial_state())
        
        
        # Grab exercise fix start ----------------------------------------
        # for some reason doesn't always grab exercises
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
        # Grab exercise fix end -----------------------------------------------
        
        table <- tutorialExtras:::submissions(get_grades = get_grades)
        
        # handle no submissions
        if(rlang::is_empty(table)){
          table <- rubric %>%
            select(label) %>%
            mutate(answer = NA,
                   attempt = 0,
                   type = NA,
                   blanks = 1,
                   partial_cred = 0,
                   correct = 0,
                   time_stamp = learnr:::timestamp_utc(),
            )
        }
        
        # merge rubric of all questions with table of submitted questions
        grades <- dplyr::left_join(rubric, table, by = "label") %>%
          # set question type for display
          dplyr::mutate(eval = ifelse(!is.na(eval), eval,
                                      ifelse(!is.na(type), type, eval)),
                        partial_cred = ifelse(is.na(partial_cred), as.numeric(correct), partial_cred),
                        #calculate time since exam start
                        time = round(as.numeric(difftime(time_stamp, start_time, units="mins")), 2)) %>%
          dplyr::select(label, answer)
        
        # only print unsubmitted questions/exercises
        unsubmitted <- grades %>% 
          filter(is.na(answer) | answer == "Not Submitted")
        
        caption <- ifelse(nrow(unsubmitted) == 0,
                      "All questions and exercises have been submitted",
                      "Unsubmitted questions/exercises will receive a 0.")
        
        output$tblExam <- renderTable({
          unsubmitted
        }, caption = caption)
        
      })
      
      
# LOCK EXAM ---------------------------------------------------------------
      
      # register that lock was pressed and reload exam to lock all questions
      observeEvent(input$lock, {
        ns <- getDefaultReactiveDomain()$ns
        
        # Change global lock identifier to "pressed"
        learnr:::save_object(session, NS("lock", id = "pressed"), 
                             learnr:::tutorial_object("lock",
                                                      list(lock = TRUE,
                                                           time = learnr:::timestamp_utc()) ) )
        
        # trigger a reload to resubmit questions forcing the lock
        session$reload()
        
      }) #close observe event
      
# DOWNLOAD GRADE HANDLER --------------------------------------------------

      output$downloadExam <- downloadHandler(
        filename = function() {
          paste0("STAT_Exam_", Sys.time(),
                 ".html")
        },
        content = function(file) {
          ns <- getDefaultReactiveDomain()$ns
          
          tutorial_info <- isolate(get_tutorial_info())
          
          # get start time ----------------------------------------------------------
          tutorial_id <- tutorial_info$tutorial_id
          exam_dir <- paste0(mod_dir, tutorial_id)
          
          # remove so not lingering from other exams
          if(exists("start_time")){
            rm(start_time)
          }
          
          if(file.exists(paste0(exam_dir,"time.RData"))){
            # if file exists time has already started
            load(file = paste0(exam_dir,"time.RData"))
          }else{
            # if file does not exist we need to start the time
            start_time <- setup_time
          }
          
          start_time <<- start_time
          
          # ERROR CHECKING START-----------------------------------------------------
          
          #error checking for label names provided
          all_labels <- c(graded, ex, manual, exclude)
          if(!is.null(all_labels)){
            purrr::map(all_labels, function(x){
              if(!(x %in% tutorial_info$items$label)){
                stop(paste0(x, " is not a name of a question or exercise."))
              }
            })
          }
          # add error checking that length names match length points
          if(!is.null(graded) || !is.null(graded_pts)){
              if (length(graded) != length(graded_pts)) {
                stop("Length of graded must equal length of graded_pts.")
              }
          }
          if(!is.null(ex) || !is.null(ex_pts)){
            if (length(ex) != length(ex_pts)) {
              stop("Length of ex must equal length of ex_pts.")
            }
          }
          if(!is.null(manual) || !is.null(manual_pts)){
            if (length(manual) != length(manual_pts)) {
              stop("Length of manual must equal length of manual_pts.")
            }
          }
          
          # ERROR CHECKING END ------------------------------------------------------
          
          # DEFINE RUBRIC START -----------------------------------------------------

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
            rubric <- dplyr::left_join(rubric_tmp, set_pts, by = "label")
          }else{
            rubric <- rubric_tmp %>%
              mutate(pts_possible = rep(NA, length(label)),
                     eval = rep(NA, length(label)))
          }
          
          # DEFINE RUBRIC END -----------------------------------------------------
          
          # SUBMISSION ORGANIZATION START ----------------------------------------
          # get all user submission questions and exercises
          get_grades <- isolate(learnr::get_tutorial_state())
     
          # Grab exercise fix start ----------------------------------------
          # for some reason doesn't always grab exercises
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
          # Grab exercise fix end -----------------------------------------------
          
          table <- tutorialExtras:::submissions(get_grades = get_grades)
          
          # handle no submissions
          if(rlang::is_empty(table)){
            table <- rubric %>%
              select(label) %>%
              mutate(answer = NA,
                     attempt = 0,
                     type = NA,
                     blanks = 1,
                     partial_cred = 0,
                     correct = 0,
                     time_stamp = learnr:::timestamp_utc(),
              )
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
                          time = round(as.numeric(difftime(time_stamp, start_time, units="mins")), 2))
          
          # handle pts_possible 1) priority goes to manual setting 
          # 2) then to num_blanks setting 3) then default to 1
          if(isTRUE(num_blanks)){
            grades <- grades %>% 
              dplyr::mutate(
                # set all question to number of blanks
                pts_possible = ifelse(is.na(pts_possible), blanks, pts_possible),
                pts_earned = pts_possible * as.numeric(partial_cred),
                pts_earned = ifelse(is.na(pts_earned), 0, pts_earned)
              )
          }
          if(isFALSE(num_blanks)){
            grades <- grades %>% 
              dplyr::mutate(
                # set all questions to 1 point
                pts_possible = ifelse(is.na(pts_possible), 1, pts_possible),
                pts_earned = pts_possible * as.numeric(partial_cred),
                pts_earned = ifelse(is.na(pts_earned), 0, pts_earned)
              )
          }
          
          # need to get name before removing "excluded" questions
          # if there is a code chunk question labeled "Name" get the name
          tmp_name <- ifelse("Name" %in% grades$label,
                             grades %>% dplyr::filter(label == "Name") %>% dplyr::pull(answer),
                             NA)
          
          # handle unsubmitted names
          user_name <- ifelse(is.na(tmp_name),
                              tutorial_info$user_id,
                              tmp_name
                              )
          
          # exclude questions if listed
          if(!is.null(exclude)){
            grades <- grades %>%
              dplyr::filter(!(label %in% exclude))
          }

          # SUBMISSION ORGANIZATION COMPLETE ----------------------------------------

          # Divide into subsections for rendered report
          graded <- grades %>%
            dplyr::filter(eval == "question") %>% 
            dplyr::select(label, answer, time, pts_earned)
            
          score <- ifelse(nrow(graded) == 0, 0, sum(graded$pts_earned))
          
          if(show_correct == FALSE){
            graded <- graded %>% select(-pts_earned) #remove pts earned
          }
          
          exercise_tmp <- grades %>%
            dplyr::filter(eval == "exercise")
          
          manual <- grades %>%
            dplyr::filter(eval == "manual") %>%
            dplyr::select(label, answer, time) 
          
          incomplete <- grades %>%
            dplyr::filter(is.na(eval)) %>%
            dplyr::select(label, answer, time) 
          
          #--------------------------------------------------------------------
          # Get any text descriptions stored with text_ui/text_server
          state_objects <- learnr:::get_all_state_objects(session, exercise_output = FALSE)
          store_objects <- learnr:::filter_state_objects(state_objects, "store")
          
          if(length(store_objects) != 0){
            descr <- list()
            for(x in store_objects){
              id <- gsub( pattern = "store-", replacement = "", x = x$id)
              descr[[id]] <- as.character(x$data$store)
            }
            content_tmp <- unnest(enframe(descr, name = "label", value = "content"), content)
            
            # put text with corresponding exercises
            ex_text <- full_join(exercise_tmp, content_tmp, by = "label")
            
          }else{
            # otherwise define content variable so we don't get an error
            ex_text <- exercise_tmp
            ex_text$content <- rep(NA, length(ex_text$label))
          }
          
          exercises <- ex_text %>%
            dplyr::filter(eval == "exercise") %>%
            dplyr::mutate(content = ifelse(is.na(content), "", content)) %>%
            transpose()
          
          content <- ex_text %>%
            dplyr::filter(is.na(eval)) %>%
            select(label, content)
          

          # Create document content to render ---------------------------------------
          #--------------------------------------------------------------------------
          yaml_string <- c("---", 
                           toString(paste0("title: ", tutorial_info$tutorial_id)), 
                           toString(paste0("author: ", user_name)),
                           toString(paste0("date: ", format(as.POSIXct(start_time, tz = "UTC"), tz = tz, usetz=TRUE)) ), "---")
          
          # if we are counting attempts let's print it in subtitle of yaml
          if(max_retry != Inf && exists("attempt")){
            
            yaml_string <- c("---", 
                             toString(paste0("title: ", tutorial_info$tutorial_id)), 
                             toString(paste0("subtitle: Attempt ", attempt)), 
                             toString(paste0("author: ", user_name)),
                             toString(paste0("date: ", format(as.POSIXct(start_time, tz = "UTC"), tz = tz, usetz=TRUE)) ), "---")
            
          }
         
          graded_string <- c(toString(paste0("# Concept  ", score)), "```{r concept, echo=FALSE}", "graded %>% knitr::kable()", "```")
          
          if(nrow(graded) == 0){
            graded_string <- c(" ", paste0("# Concept  ", score), "No concept questions graded.", " ")
          }
          
          exercise_substring <- map(exercises, function(x){
            c(toString(paste0("### ", x$label, " - ", ifelse(isTRUE(x$correct), "Correct", "Needs Grading")) ), 
              toString(paste0("Time: ", x$time, " <br> <br> ")),
              toString(paste0(x$content)),
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
            dnf_string <- c("# Incomplete Problems", "No incomplete problems.", " ")
          }
          
          content_string <- c("# Description Content", "```{r content, echo=FALSE}", "content %>% knitr::kable()", "```")
          if(nrow(content) == 0){
            content_string <- c(" ")
          }
          
          #--------------------------------------------------------------------
          # create a temporary empty file to write rmd and data folder to
          tmp_dir <- tempdir()
          
          #--------------------------------------------------------------------
          # write images folder to a temporary directory
          file.copy("images", tmp_dir, recursive = TRUE)
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
                       exercise_string, manual_string, dnf_string,
                       content_string),
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

# -------------------------------------------------------------------------
# SUBMISSION FUNCTION -----------------------------------------------------
# used for button_grade_tutorial AND button_lock_exam
# need to calculate outside of observe event so that it can apply to download handler
submissions <- function(get_grades = list()){
  # get and organize all user submission questions and exercises
  table_list <- map(names(get_grades), function(x){
    # handle multiple answer issues
    get_grades[[x]]$blanks <- length(get_grades[[x]]$answer)
    get_grades[[x]]$answer <- toString(get_grades[[x]]$answer)
    
    # if correct is empty then set to FALSE
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
          dplyr::select(-c(answer_last, correct_last))
      }
    }
    # fix possible data typing errors
    store %>%
      dplyr::mutate(label = as.character(label),
                    answer = as.character(answer),
                    attempt = as.numeric(attempt),
                    time_stamp = time_last) %>% 
      dplyr::select(-time_last)
  })
  
  table <- dplyr::bind_rows(table_list)
  
  return(table = table)
}


