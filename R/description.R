#' @title Text and description storage
#'
#' @description
#' Add text descriptions to exercises. 
#' Including a ui/server for the question text will allow
#' the text to be printed in the downloaded exam html. 
#' 
#' In order for the text to appear with the exercise when downloading,
#' the id for the text_ui and text_server must match the
#' name of the exercise code chunk. Otherwise, it will print at the
#' end of the html document.
#' 
#' @param id ID matching ui with server
#' 
#' @export
text_ui <- function(id = NULL) {
  
  ns <- NS(id)
  tagList(
    uiOutput(ns("text"))
  )
  
}
  
#' @title Text and description server
#' @param id ID matching ui with server
#' @param text output to save and print
#' @export
text_server <- function(id, text = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(
             req(session$userData$learnr_state() == "restored"),
             once = TRUE,
             {
               ns <- getDefaultReactiveDomain()$ns
               
               learnr:::save_object(session, 
                                    NS("store", id = id), 
                                    learnr:::tutorial_object("store",
                                                             list(store = text) ) )
               
               output$text <- renderUI({
                     text
                 })
               
             }
      )
    }
  )
  }
