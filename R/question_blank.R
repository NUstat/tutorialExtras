#' Fill in the blank question for learnr tutorials
#'
#' Add interactive fill in the blank tasks to your `learnr` tutorials.
#' You must include at least one single blank line ___ in the question text.
#' A blank is created by typing 3 underscores.
#'
#' The text utilizes HTML for further display customization, which can be used
#' to insert an image or line break.
#'
#' @param placeholder Sample text to appear in blank.
#' @param rows,cols Defines the size of the text input area in terms of the
#'   number of rows or character columns visible to the user. If either `rows`
#'   or `cols` are provided, the quiz input will use [shiny::textAreaInput()]
#'   for the text input, otherwise the default input element is a single-line
#'   [shiny::textInput()].
#' @param trim Logical to determine if whitespace before and after the answer
#'   should be removed.  Defaults to `TRUE`.
#' @param style can change display of question to "notes" or "exam"
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = blank`.
#'
#' @examples
#' question_blank(
#'   "3 + ___ = 5 <br/>
#'    ___ - 4 = 4",
#'   learnr::answer("2", correct = TRUE),
#'   learnr::answer("8", correct = TRUE),
#'   allow_retry = TRUE
#' )
#'
#' @export
question_blank <- function(
  text,
  ...,
  type = "blank",
  #correct = paste0(icons::ionicons("checkmark-outline") ),
  #incorrect = paste0(icons::ionicons("close-outline") ),
  correct = "Correct!",
  incorrect = "Incorrect",
  try_again = incorrect,
  allow_retry = FALSE,
  random_answer_order = FALSE,
  placeholder = "Enter answer here...",
  trim = TRUE,
  rows = NULL,
  cols = NULL,
  style = "tutorial_question",
  options = list()
) {
  checkmate::assert_character(placeholder, len = 1, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_logical(trim, len = 1, null.ok = FALSE, any.missing = FALSE)

  question <- masteryExams:::blank_question(
    text = text,
    ...,
    type = "blank",
    correct = correct,
    incorrect = incorrect,
    allow_retry = allow_retry,
    random_answer_order = FALSE,
    options = utils::modifyList(
      options,
      list(
        placeholder = placeholder,
        trim = trim,
        rows = rows,
        cols = cols
      )
    )
  )
    question

}


blank_question <- function(
    text,
    ...,
    type = c("blank"),
    correct = "Correct!",
    incorrect = "Incorrect",
    try_again = incorrect,
    message = NULL,
    post_message = NULL,
    loading = c("**Loading:** ", format(text), "<br/><br/><br/>"),
    submit_button = rlang::missing_arg(),
    try_again_button = rlang::missing_arg(),
    allow_retry = FALSE,
    random_answer_order = FALSE,
    style = "tutorial_question",
    options = list()
) {

  # one time tutor initialization
  learnr::initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  lapply(answers, function(answer) {
    checkmate::assert_class(answer, "tutorial_question_answer")
  })

  # ensure answer choices = num blank
  split <- unlist(stringr::str_split(text, pattern = "___") )
  num_blank = length(split) - 1
  if (num_blank != length(answers)) {
    stop("Number of blanks must equal number of answer() inputs.")
  }
  # ensure style is correct
  if (! style %in% c("tutorial_question", "notes", "exam", "notes_question")) {
    stop("style must be either 'tutorial_question', 'notes', or 'exam'")
  }

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- label %||% learnr:::random_question_id()

  # i18nize button labels if default values are used
  submit_button <-
    if (rlang::is_missing(submit_button)) {
      learnr:::i18n_span("button.questionsubmit", "Submit Answer")
    } else {
      learnr:::quiz_text(submit_button)
    }

  try_again_button <-
    if (rlang::is_missing(try_again_button)) {
      learnr:::i18n_span("button.questiontryagain", "Try Again")
    } else {
      learnr:::quiz_text(try_again_button)
    }

  ret <- list(
    type = type,
    label = label,
    question = learnr:::quiz_text(text),
    answers = answers,
    style = style,
    button_labels = list(
      submit = submit_button,
      try_again = try_again_button
    ),
    messages = list(
      correct = learnr:::quiz_text(correct),
      try_again = learnr:::quiz_text(try_again),
      incorrect = learnr:::quiz_text(incorrect),
      message = learnr:::quiz_text(message),
      post_message = learnr:::quiz_text(post_message)
    ),
    ids = list(
      answer = NS(q_id)("answer"),
      question = q_id
    ),
    loading = learnr:::quiz_text(loading),
    random_answer_order = random_answer_order,
    allow_retry = allow_retry,
    # Set a seed for local testing, even though it is overwritten for each shiny session
    seed = learnr:::random_seed(),
    options = options
  )

  if(style == "notes" || style == "notes_question"){
    class(ret) <- c(type, "notes_question")
  #}else if(style == "exam"){
    #class(ret) <- c(type, "exam")
  }else{
    class(ret) <- c(type, "tutorial_question")
  }

  ret
}


#' @export
#' @seealso question_blank
question_ui_initialize.blank <- function(question, value, ...) {

  #split question by blanks
  split <- unlist(stringr::str_split(question$question, pattern = "___") )

  pos <- NULL
  for(i in 1:length(split)){
    if(i < length(split)){
      pos <- c(pos, split[i], "___")
    }
    else{
      pos <- c(pos, split[i])
    }
  }
  pos <- as.list(pos)

  #now separate out the line breaks
  for(i in 1:length(pos)){
    if(stringr::str_detect(pos[i], "<br/>")){
      tmp <- unlist(stringr::str_split(pos[i], pattern = "<br/>") )
      new <- NULL
      for(j in 1:length(tmp)){
        if(j < length(tmp)){
          new <- c(new, tmp[j], "<br/>")
        }else{
          new <- c(new, tmp[j])
        }
      }
      pos[[i]] <- new
    }
  }

  pos <- unlist(pos)

  where_blank <- vector()
  j = 1
  for(i in 1:length(pos)){
    if(pos[i] == "___"){
      where_blank[i] = j
      j = j+1
    }else{
      where_blank[i] = 0
    }
  }

  num_blank = length(split)-1
  num_pos = length(pos)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- as.character( unlist(value) )
  } else {
    ans <- rep("", num_blank)
  }

  # set unique ids
  # pretty close to unique id
  rand = paste0(sample.int(100,1), sample.int(100,1) )
  # set input and bucket ids
  input_ids = lapply(seq(1,num_blank), function(x) paste0("blank", rand, x) )
  css_ids = lapply(seq(1,num_blank), function(x) paste0("text", rand, x) )
  other_ids = lapply(seq(1,num_pos), function(x) paste0("other", rand, x) )
  group_ids = paste0("group", rand)
  class_id = paste0("class",rand)

  bootstrapPage(
    tags$style(
      ".container {
      display: flex;
      flex-wrap: wrap;
      flex-direction: row;
      width: 100%;
      padding: 3px;
    }") ,
    tags$style(
      ".break {
      flex-basis: 100%;
      height: 0;
      }"),
    tags$style(
      ".item {
      display: flex;
      flex-wrap: wrap;
      flex-direction: row;
      padding: 3px;
    }"),

    div(id = question$ids$answer,
        class = "container",

        lapply(seq(1,num_pos), function(x)
          if(pos[x] == "___"){
            tags$div(
              class = "item",
              style="display:inline-block",
              id = css_ids[where_blank[x]],
              tags$input(type = "text",
                         id = paste0(input_ids[where_blank[x]]),
                         class = class_id,
                         value = ans[where_blank[x]],
                         onkeyup = htmlwidgets::JS(
                           paste0("Shiny.setInputValue('",question$ids$answer,"',
                     [", toString(lapply(input_ids, function(z)
                       paste0("document.getElementById('",z,"').value")
                     ) ),
                     "] )")
                         ) #end JS
              )
            )
          }else if(pos[x] == "<br/>"){
            div(
              class = "break",
              id = other_ids[x]
            )
          }else{
            div(
              class = "item",
              id = other_ids[x],
              HTML(pos[x] )
            )
          }
        ) #end lapply
    ) #ends bucket div group

  ) #end bootstrap page

}


#' @export
#' @seealso question_blank
question_is_valid.blank <- function(question, value, ...) {
   split <- unlist(stringr::str_split(question$question, pattern = "___") )
   num_blank = length(split) - 1

   if (is.null(value)) {
     return(FALSE)
   }else if(length(value) < num_blank ){
     return(FALSE)
   }

   if (isTRUE(question$options$trim)) {
     return(min( nchar(stringr::str_trim(value)) ) >= 1)
   } else{
     return(min( nchar(value) ) >= 1)
   }

}

#' @export
#' @seealso question_blank
question_is_correct.blank <- function(question, value, ...) {

  if(question$style == "exam"){
    return(mark_as(FALSE, NULL))
  }

  append_message <- function(x, ans) {
    message <- ans$message
    if (is.null(message)) {
      return(x)
    }
    if (length(x) == 0) {
      message
    } else {
      tagList(x, message)
    }
  }

  #how is value being taken in
  if (min(nchar(value)) == 0) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      showNotification("Please enter some text before submitting", type = "error")
    }
    #shiny::validate("Please enter some text")
    value
  }

  if (isTRUE(question$options$trim)) {
    value <- stringr::str_trim(value)
  }

  record_correct <- c()
  for(i in 1:length(value)){
    q_ans <- question$answers[[i]]
    v_ans <- value[i]
    #check if answer is a value or function
    if(q_ans$type == "function"){
      answer_checker <- eval(parse(text = q_ans$value), envir = rlang::caller_env())
      ret <- answer_checker(v_ans)
      record_correct[i] <- ret$correct
    } else{ # literal check
      if (v_ans == q_ans$value){ # correct
        record_correct[i] <- TRUE
      } else if(v_ans != q_ans$value){ # wrong
        record_correct[i] <- FALSE
      }
    }
  }

  #if everything is correct mark TRUE
  if(all(record_correct)){
    value_is_correct <- TRUE
  }else{
    value_is_correct <- FALSE
  }

  ret_messages <- c()
  q_answers <- learnr:::answers_split_type(question$answers)
  if (value_is_correct) {
    # selected all correct answers. get all good messages as all correct answers were selected
    for (q_answer in q_answers[["literal"]]) {
      if (q_answer$correct) {
        ret_messages <- append_message(ret_messages, q_answer)
      }
    }
  }

  learnr::mark_as(value_is_correct, ret_messages)

}

#' @export
#' @seealso question_blank
question_ui_completed.blank <- function(question, value, ...) {
disable_all_tags(
  question_ui_initialize(question, value, ...)
)
}
