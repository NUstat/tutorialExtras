#' Tutorial exam
#'
#' @description
#' Add interactive exam questions to a tutorial.
#' 
#' @param ... One or more questions or answers
#' @param caption Optional quiz caption (defaults to "Quiz")
#' @param shuffle Randomly shuffle the questions within quiz
#' @rdname quiz
#' @export
exam <- function(..., caption = rlang::missing_arg(), 
                 shuffle = FALSE) {
  
  q_list <- list(...)
  
  # label before shuffling so that you can set gradebook labels
  # create table rows from questions
  index <- 1
  q_list <- lapply(q_list, function(question) {
    if (!is.null(question$label)) {
      label <- paste(question$label, index, sep="-")
      question$label <- label
      question$ids$answer <- NS(label)("answer")
      question$ids$question <- label
      index <<- index + 1
    }
    question
  })
  
  ###############################################################
  # organize data to find which have multi_part and group options
  ###############################################################
  j <- 0
  #figure out which questions need to be kept together.
    multi_part <- lapply(q_list, function(x){
      j <<- j+1
      ifelse(!is.null(x[["options"]]$multi_part), x[["options"]]$multi_part, paste0("unique",j))
    }
    )
    group <- lapply(q_list, function(x){
      j <<- j+1
      ifelse(!is.null(x[["options"]]$group), x[["options"]]$group, paste0("unique",j))
    }
    )
    
    df <- data.frame(q_list = I(q_list),
                     multi_part = unlist(multi_part),
                     group = unlist(group))
    
    
    ############################
    # sample 1 from each "group"
    ############################
    df <- df %>% 
      group_by(group) %>% 
      sample_n(1) %>% 
      ungroup()
    
    ###########################################################
    # shuffle the display order of questions
    # keep multi-part questions next to each other
    if(shuffle == TRUE){
      #split data based on multi_part groups
      grouped_df <- split(df, df$multi_part)
      # Shuffle the groups
      shuffled_groups <- sample(grouped_df)
      # Combine the shuffled groups back into a single dataset
      df <- do.call(rbind, shuffled_groups) 
      #overwrite df so that we can set q_list regardless of shuffle
    }
    
    # set the question list to the new order
    questions <- df$q_list
  
  # create table rows from questions
  # index <- 1
  # questions <- lapply(q_list, function(question) {
  #   if (!is.null(question$label)) {
  #     label <- paste(question$label, index, sep="-")
  #     question$label <- label
  #     question$ids$answer <- NS(label)("answer")
  #     question$ids$question <- label
  #     index <<- index + 1
  #   }
  #   question
  # })
  
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
