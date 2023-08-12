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
#' @export
reset_button_ui <- function(id, label = "Retry Exam") {
  ns <- NS(id)
  tagList(
    tags$button(
      id = ns('reset'),
      type = "button",
      class = "btn action-button",
      onclick = "setTimeout(function(){window.close();},5000);",  # close browser
      label
    ),
    #text output not working?!
    textOutput(ns("response"))
  )
  
}

# Define the server logic for a module to lock exam
#' @title Tutorial reset server
#' @param id ID matching ui with server
#' @param file_name Name of the .Rmd file (not the tutorial id)
#' @param package_name Name of the package with tutorial.
#' @param tz Time zone to display retry time.
#' @export
reset_server <- function(id, file_name = NULL, package_name = NULL, tz = Sys.timezone()) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$reset, {
        
        ns <- getDefaultReactiveDomain()$ns
        
        #if lock was pressed get time since lock was pressed
        if(!is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock)){
          
          lock_time <- learnr:::get_object(session, NS("lock", id = "pressed"))$data$time
          
          wait_time <- round(as.numeric(difftime(lock_time, learnr:::timestamp_utc(), units="hours")), 2)
          
          retry_time <- as.POSIXct(lock_time, tz = "UTC") + lubridate::hours(retry_cooldown)
          
          
          #do not allow retry until cooldown is met.
          if(wait_time <  retry_cooldown){
            print(paste0("Retry option available at ", format(retry_time, tz = tz, usetz=TRUE)))
            
            #WHY is this not working???
            output$response <- renderText({
              paste0("Retry option available at ", format(retry_time, tz = tz, usetz=TRUE))
              })
            
            return()
          }
        }
        
        # check if max_attempt is reached
        if(attempt > max_retry){
          return()
        }
        
        
        ##########################################################
        # YES this resets all questions and exercises
        # this does NOT reset global variables
        # must have storage set to LOCAL or won't work on Posit Cloud/Shiny.io
        learnr:::remove_all_objects(session)
        
        # reset the time to the current time
        start_time <<- learnr:::timestamp_utc()
        save(start_time, file = paste0(mod_dir, "time.RData"))
        
        # update attempt to set new seed
        attempt <<- attempt + 1
        
        save(attempt, file = paste0(mod_dir, "attempt.RData"))
        # trigger a reload to resubmit questions and unlock
        
        #set new seed
        init.seed <- Sys.info()["user"]
        TeachingDemos::char2seed(paste0(init.seed, attempt))
        
        # NEED TO CLEAR PRERENDERED OUTPUT AND RERUN
        # get tutorial path
        rmd_path <- learnr:::get_tutorial_path(file_name, package_name)
        files <- list.files(rmd_path, pattern = "\\.Rmd$", full.names = TRUE)
        
        # we need this to clear the pre-rendered chunks.
        rmarkdown::shiny_prerendered_clean(files)
        
        # can't run app within an app
        # workaround is to write the run_tutorial function in a .R script
        # and call the script to run on session end
        #tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".R")
        tmp_file <- tempfile(tmpdir = mod_dir, fileext = ".R")
        # write to R file
        writeLines(paste0("learnr::run_tutorial(name = '",file_name, "', package = '",package_name,"')"),
                   con = tmp_file)
        #writeLines(paste0("job::job(learnr::run_tutorial(name = '",file_name, "', package = '",package_name,"'))"),
        #           con = tmp_file)
        ##############################################################
        # close the session
        session$close()
        
        # open new tutorial with everything cleared and pre-rendered!
        onSessionEnded(function() {
          rstudioapi::jobRunScript(path = tmp_file)
          # stop the old session after new one is open
          Sys.sleep(10)
          stopApp()
        })
        # FINALLY!
        
      }) #close observe event
      
    }) #close module server
} #close main function
