#' Tutorial exam
#'
#' @description
#' Add interactive exam questions to a tutorial.
#' 
#' @param ... One or more questions or answers
#' @param caption Optional quiz caption (defaults to "Quiz")
#' @param shuffle Randomly shuffle the questions within quiz
#' @param randomize Randomly select 1 question from each group
#' @rdname quiz
#' @export
exam <- function(..., caption = rlang::missing_arg(), 
                 shuffle = FALSE, randomize = FALSE) {
  
  q_list <- list(...)
  
  if(shuffle == TRUE){
    q_list <- sample(q_list, length(q_list))
  }
  
  # create table rows from questions
  index <- 1
  questions <- lapply(q_list, function(question) {
    if (!is.null(question$label)) {
      label <- paste(question$label, index, sep="-")
      question$label <- label
      question$ids$answer <- NS(label)("answer")
      question$ids$question <- label
      index <<- index + 1
    }
    question
  })
  
  caption <-
    if (rlang::is_missing(caption)) {
      learnr:::i18n_span("text.quiz", "Quiz")
    } else if (!is.null(caption)) {
      quiz_text(caption)
    }
  
  ret <- list(caption = caption, questions = questions)
  class(ret) <- "tutorial_quiz"
  ret
}

# Does not work
# empty ui for server to match and set the seed
# exam_seed_ui <- function(id){
#   
# }
# 
# # server for initializing seed
# exam_seed_server <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       observeEvent(
#         req(session$userData$learnr_state() == "restored"),{
#           ns <- getDefaultReactiveDomain()$ns
#           
#           tutorial_info <- isolate(get_tutorial_info())
#           attempt <- learnr:::get_object(session,  NS("seed", id = "seed"))$data$seed
#           print(attempt)
#           
#           #print(paste0(tutorial_info$user_id, attempt))
#           
#           TeachingDemos::char2seed(paste0(tutorial_info$user_id, attempt))
#           newseed <<- paste0(tutorial_info$user_id, attempt)
#           #return(paste0(tutorial_info$user_id, attempt))
#         })
#     })
# }