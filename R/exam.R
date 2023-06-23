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
