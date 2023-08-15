#' Word bank question for learnr tutorials
#'
#' The following function was modified from the sortable package, available at
#' https://github.com/rstudio/sortable and a solution thread by "stefan's on Jun 01, 2022".
#' Many thanks to the sortable author who developed tools to drag and drop objects in rank order.
#' This extends the "question_rank" option so that you can drag any option from the
#' word bank into the answer blanks.
#'
#' Add interactive word bank tasks to your `learnr` tutorials.  The student can
#' drag-and-drop the answer options from the word bank to match the corresponding
#' options.
#'
#' The choices should be a set of options that the word bank choices will
#' match to. The choices should be of equal length to the answers.
#'
#' The answer should contain the set of solutions that match the choices. The first
#' answer much match the first choice and so forth.
#'
#' The word bank should be a set of options for the user to choose from to
#' drag and drop into the blanks. If can be shorter or longer than then number
#' of blanks but must include all unique answer options. If no word bank is
#' provided it will be set equal to the answer options.
#'
#' @param choices a vector of choices that will remain stationary in the left column.
#' @param wordbank a vector of choices to be placed into the blanks when providing more options than answers.
#' If NULL then the wordbank will be set equal to the answer choices.
#' @param arrange either  'random', 'ordered' or 'none'; default is random. Set equal to ordered if
#' you want the wordbank list to appear alphabetically. Set equal to none for the wordbank to appear exactly
#' as provided.
#' @param box a number between 1 and 11, inclusive, indicating the width of the 'choices' box.
#' The default is 6 which corresponds to 50% of the page width.
#' @param ... parameters passed onto learnr answer.
#' @inheritParams learnr::question
#'
#' @return A custom `learnr` question, with `type = wordbank`.
#'
#' @importFrom learnr question_ui_initialize
#' @importFrom learnr question_ui_completed
#' @importFrom learnr question_ui_try_again
#' @importFrom learnr question_is_valid
#' @importFrom learnr question_is_correct
#' @import learnr
#' @import shiny
#' @importFrom rlang "%||%"
#'
#' @examples
#' question_wordbank(
#'   "Drag the numbers to match the corresponding letter's position in the alphabet.",
#'   choices = c("C", "B", "A", "D"),
#'   learnr::answer(c("3", "2", "1", "4"), correct = TRUE),
#'   allow_retry = TRUE
#' )
#'
#' @export
question_wordbank <- function(
    text,
    ...,
    choices,
    wordbank = NULL,
    box = 6,
    arrange = "random",
    type = "wordbank",
    correct = "Correct!",
    incorrect = "Incorrect",
    loading = c("**Loading:** ", text, "<br/><br/><br/>"),
    submit_button = "Submit Answer",
    try_again_button = "Try Again",
    allow_retry = FALSE,
    random_answer_order = TRUE,
    options = sortable::sortable_options()
) {
  ISDSfunctions:::wordbank_question(
    text = text,
    ...,
    choices = choices,
    wordbank = wordbank,
    arrange = arrange,
    box = box,
    type = "wordbank",
    correct = correct,
    incorrect = incorrect,
    loading  = loading,
    submit_button = submit_button,
    try_again_button = try_again_button,
    allow_retry = allow_retry,
    random_answer_order = random_answer_order,
    options = options
  )
}


wordbank_question <- function(
    text,
    ...,
    choices = choices,
    wordbank = wordbank,
    arrange = arrange,
    box = box,
    type = c("wordbank"),
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
    options = list()
) {

  # one time tutor initialization
  initialize_tutorial()

  # capture/validate answers
  ellipsis::check_dots_unnamed() # validate all answers are not named and not a misspelling
  answers <- list(...)
  lapply(answers, function(answer) {
    checkmate::assert_class(answer, "tutorial_question_answer")
  })

  #set wordbank equal to answers if NULL
  if(is.null(wordbank)){
    wordbank <- unique(answers[[1]]$option)
  }

  # ensure box is in 1:11
  if (! box %in% c(1:11)) {
    stop("Box must be a number between 1 to 11, inclusive.")
  }
  # ensure arrange is correct
  if (! arrange %in% c("random", "ordered", "none")) {
    stop("arrange must be either 'random', 'ordered', or 'none' ")
  }
  # all correct answers must be an option in wordbank
  if (!all( answers[[1]]$option %in% c(wordbank,"answer","dummy","placeholder") ) ) {
    stop("All answers must be an option in the wordbank.")
  }
  # number of choices must equal number of answers
  if (length(choices) != length(answers[[1]]$option)) {
    stop("Length of choices must equal to length in answer().")
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
    choices = choices,
    wordbank = wordbank,
    box = box,
    arrange = arrange,
    answers = answers,
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

  class(ret) <- c(type, "tutorial_question")

  ret
}


#' @export
#' @seealso question_wordbank
question_ui_initialize.wordbank <- function(question, value, ...) {

  # get number of parts
  num <- length(question$choices)

  # set output to previous answers
  if (!is.null(value)) {
    ans <- as.character( unlist(value) )
  } else {
    ans <- rep(NULL, num)
  }
  # need to eventually add randomization to question init
  # or it will randomize everytime
  # shuffle wordbank options either ordered or random
  options <- question$wordbank
  if(question$arrange == "ordered"){
    labels <- sort(options)
  } else if(question$arrange == "random"){
    labels <- sample(options, length(options))
  } else{
    labels <- options
  }

  num_bank = length(labels)

  # pretty close to unique id
  rand = paste0(sample.int(100,1), sample.int(100,1) )
  # set input and bucket ids
  input_ids = lapply(seq(1,num), function(x) paste0("select", rand, x) )
  css_ids = lapply(seq(1,num), function(x) paste0("drag_to", rand, x) )
  other_ids = lapply(seq(1,num_bank), function(x) paste0("drag_from", rand, x) )
  group_ids = paste0("group", rand)

  set_bucket <- sortable::sortable_js_capture_bucket_input(input_id = question$ids$answer,
                                                           input_ids = input_ids,
                                                           css_ids = css_ids)



  icons <- function(x) {lapply(x,function(x){tags$div(tags$strong(x))})}

  fluidPage(
    withMathJax(),
    div(class = "panel-heading",
        strong(question$question)  ),

    fixedRow(
      column(
        width = 12,
        div(
          div(
            class = "panel panel-default",
            lapply(seq(1,num_bank), function(x)
              div(
                class = "panel-body",
                style="display:inline-block",
                id = other_ids[x],
                icons(c(labels[x]))
              )),
            div(
              class = "panel-body",
              style="display:inline-block",
              id = paste0("bin", rand),
              icon("trash")
            )
          )
        )
      )
    ),
    div( id = paste0("bucket", rand),
         lapply(seq(1,num), function(x)
           fixedRow(
             column(
               width = question$box,
               HTML(question$choices[x])
             ),
             column(
               width = 12-question$box,
               div(
                 class = "panel panel-default",
                 div(
                   class = "panel-body",
                   id = css_ids[x], #paste0("drag_to",x)
                   icons(ans[x])
                 )
               )
             )
           ) #ends fixed row
         ) #ends lapply
    ), #ends bucket div group

    # separate columns for each drag
    lapply(seq(1,num_bank), function(x)
      sortable::sortable_js(
        other_ids[x],
        options = sortable::sortable_options(
          group = list(
            pull = "clone",
            name = group_ids,
            put = FALSE
          )
        )
      ) ),
    lapply(seq(1,num), function(x)
      sortable::sortable_js(
        css_ids[x],
        options = sortable::sortable_options(
          group = list(
            group = group_ids,
            put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
            pull = TRUE
          ),
          onSort = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )), #paste0("select",x)
          onLoad = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )) # << solution by stefan on Jun 01, 2022
        ) )
    ),
    sortable::sortable_js(
      paste0("bin", rand),
      options = sortable::sortable_options(
        group = list(
          group = paste0("sort", rand),
          put = TRUE,
          pull = FALSE
        ),
        onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
      )
    )
  )

}


#' @export
#' @seealso question_wordbank
question_is_correct.wordbank <- function(question, value, ...) {

  for (ans in question$answers) {

    if (identical(as.character(ans$option), as.character(value) ) ) {
      return(mark_as(
        ans$correct,
        ans$message
      ))
    }
    # how many are correct out of total blanks?
    partial <- sum(ans$option == value)/length(value)

  }
  mark_as(FALSE, partial)

}



#' @export
#' @seealso question_wordbank
question_ui_completed.wordbank <- function(question, value, ...) {
  # TODO display correct values with X or √ compared to best match
  # TODO DON'T display correct values (listen to an option?)

  # get number of parts
  num <- length(question$choices)

  # shuffle wordbank options either ordered or random
  options <- question$wordbank
  if(question$arrange == "ordered"){
    labels <- sort(options)
  } else if(question$arrange == "random"){
    labels <- sample(options, length(options))
  } else{
    labels <- options
  }

  num_bank = length(labels)

  # set output to previous answers
  ans <- unlist(value)

  rand = paste0(sample.int(100,1), sample.int(100,1) )
  input_ids = lapply(seq(1,num), function(x) paste0("select", rand, x) )
  css_ids = lapply(seq(1,num), function(x) paste0("drag_to", rand, x) )
  other_ids = lapply(seq(1,num), function(x) paste0("drag_from", rand, x) )
  group_ids = paste0("group", rand)

  set_bucket <- sortable::sortable_js_capture_bucket_input(input_id = question$ids$answer,
                                                           input_ids = input_ids,
                                                           css_ids = css_ids)

  icons <- function(x) {lapply(x,function(x){tags$div(tags$strong(x))})}

  learnr::disable_all_tags(

    fluidPage(
      withMathJax(),
      div(class = "panel-heading",
          strong(question$question)  ),

      fixedRow(
        column(
          width = 12,
          div(
            div(
              class = "panel panel-default",
              lapply(seq(1,num_bank), function(x)
                div(
                  class = "panel-body",
                  style="display:inline-block",
                  id = other_ids[x],
                  icons(c(labels[x]))
                )),
              div(
                class = "panel-body",
                style="display:inline-block",
                id = paste0("bin", rand),
                icon("trash")
              )
            )
          )
        )
      ),
      div( id = paste0("bucket", rand),
           lapply(seq(1,num), function(x)
             fixedRow(
               column(
                 width = question$box,
                 HTML(question$choices[x])
               ),
               column(
                 width = 12-question$box,
                 div(
                   class = "panel panel-default",
                   div(
                     class = "panel-body",
                     id = css_ids[x], #paste0("drag_to",x)
                     icons(ans[x])
                   )
                 )
               )
             ) #ends fixed row
           ) #ends lapply
      ), #ends bucket div group

      # separate columns for each drag
      lapply(seq(1,num_bank), function(x)
        sortable::sortable_js(
          other_ids[x],
          options = sortable::sortable_options(
            group = list(
              pull = "clone",
              name = group_ids,
              put = FALSE
            )
          )
        ) ),
      lapply(seq(1,num), function(x)
        sortable::sortable_js(
          css_ids[x],
          options = sortable::sortable_options(
            group = list(
              group = group_ids,
              put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
              pull = FALSE
            ),
            onSort = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )), #paste0("select",x)
            onLoad = sortable::chain_js_events(set_bucket, sortable::sortable_js_capture_input(input_id = input_ids[x] )) # << solution by stefan on Jun 01, 2022
          ) )
      ),
      sortable::sortable_js(
        paste0("bin", rand),
        options = sortable::sortable_options(
          group = list(
            group = paste0("sort", rand),
            put = TRUE,
            pull = FALSE
          ),
          onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
        )
      )
    )
  )

}


#' @export
#' @seealso question_wordbank
question_is_valid.wordbank <- function(question, value, ...) {
  if (length(as.character( unlist(value) ) ) != length(question$choices)) {
    return(FALSE)
  }
  return(TRUE)
}
