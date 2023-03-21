# load isds_setup on load to prevent errors
# user can define the options in setup
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
}


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
    actionButton( ns("lock"), label = label)
  )
}

# Define the server logic for a module to lock exam
#' @title Tutorial lock server
#' @param id ID matching ui with server#'
#' @export
lock_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$lock, {
        ns <- getDefaultReactiveDomain()$ns
        
        # Change global lock identifier to "pressed"
        learnr:::save_object(session, NS("lock", id = "pressed"), 
                             learnr:::tutorial_object("lock",
                                                      list(lock = TRUE) ) )
        
        # trigger a reload to resubmit questions and lock
        session$reload()
      }) #close observe event
      
      # eventReactive(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock, 
      #               ignoreNULL = F,{
      #                 isolate(mastery <<- ifelse(is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock),
      #                                            FALSE, learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock))
      #                 print(mastery)
      #               })
      # observeEvent(
      #   req(session$userData$learnr_state() == "restored"),
      #   once = TRUE,
      #   
      # )
      
    }) #close module server
} #close main function



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
        
        # Change lock identifier to "not pressed"
        learnr:::save_object(session, NS("lock", id = "pressed"), 
                             learnr:::tutorial_object("lock",
                                                      list(lock = FALSE) ) )
        
        # clear all question and exercise cache?
        #learnr:::clear_tutorial_cache()
        #YES this resets everything
        learnr:::remove_all_objects(session)
        
        # trigger a reload to resubmit questions and unlock
        session$reload()
      }) #close observe event
      
      
    }) #close module server
} #close main function

















# DELETE - FAILED ATTEMPT AT OVERRIDING exercise options
# .onLoad <- function(libname, pkgname) {
#   mastery <<- FALSE
#   
#   knitr::opts_hooks$set(mastery = function(options) {
#     if (isTRUE(options$mastery)) {
#       options$context = "render";
#       options$echo= TRUE;
#       options$exercise = FALSE;
#     }else{
#       options$context = NULL;
#       options$echo= FALSE;
#       options$exercise = TRUE;
#     }
#     options
#   })
  #register_ISDS_handlers()
  # callModule(
  #   masteryExams:::register_ISDS_handlers("run"),
  #   "test"
  # )
  #NEED TO FIND A WAY TO TRIGGER SESSION OBJECT ON LOAD
#}

# register_ISDS_handlers <-  function() {
#   rmarkdown::shiny_prerendered_chunk('server', 
#       mastery <<- ifelse(is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock),
#                          FALSE, learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock)
#       )
#   print(mastery)
# }
