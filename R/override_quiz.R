# OVERRIDE LEARNR TO COUNT ATTEMPTS
#' Knitr quiz print methods
#'
#' \code{knitr::\link[knitr]{knit_print}} methods for \code{\link{question}} and
#' \code{\link{quiz}}
#'
#' @inheritParams knitr::knit_print
#' 
#' @import shiny
#' @import learnr
#' @importFrom knitr knit_print
#' @method knit_print tutorial_question
#' @keywords internal
#' @export
#' @rdname knit_print
knit_print.tutorial_question <- function(x, ...) {
  question <- x
  ui <- tutorialExtras:::question_module_ui(question$ids$question)
  
  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk(
    'server',
    sprintf(
      #OVERRIDE
      'tutorialExtras:::question_prerendered_chunk(%s, session = session)',
      learnr:::dput_to_string(question)
    )
  )
  
  # regular knit print the UI
  knitr::knit_print(ui)
}


question_prerendered_chunk <- function(question, ..., session = getDefaultReactiveDomain()) {
  learnr:::store_question_cache(question)
  
  question_state <-
    callModule(
      tutorialExtras:::question_module_server,
      question$ids$question,
      question = question,
      session = session
    )
  
  observe({
    req(question_state())
    learnr:::set_tutorial_state(question$label, question_state(), session = session)
  })
  
  question_state
}

# WHEN LOCK IS PRESSED AUTOSUBMIT
question_module_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "panel panel-default tutorial-question-container",
    div(
      "data-label" = as.character(id),
      class = "tutorial-question panel-body",
      uiOutput(ns("answer_container")),
      uiOutput(ns("message_container")),
      uiOutput(ns("action_button_container")),
      learnr:::withLearnrMathJax()
    )
  )
}

question_module_server <- function(
    input, output, session,
    question
) {
  
  output$answer_container <- renderUI({
    if (is.null(question$loading)) {
      learnr:::question_ui_loading(question)
    } else {
      div(
        class="loading",
        question$loading
      )
    }
  })
  
  # Setup reactive here that will be updated by the question modules
  question_state <- reactiveVal()
  
  observeEvent(
    req(session$userData$learnr_state() == "restored"),
    once = TRUE,
    tutorialExtras:::question_module_server_impl(input, output, session, question, question_state)
  )
  
  question_state
}

# Internal function
# OVERRIDE QUESTION MODULE SERVER
# ADDED COUNT ATTEMPTS FEATURE
# ADDED LOCK FEATURE
question_module_server_impl <- function(
    input, output, session,
    question,
    question_state = NULL
) {
  
  ns <- getDefaultReactiveDomain()$ns
  # set a seed for each user session for question methods to use
  question$seed <- learnr:::random_seed()
  
  # new ============================
  # initialize counter
  val <- reactiveValues(
      numtry = 0,
      lock = FALSE
  )
  # restore counter
  isolate(val$numtry <- ifelse(is.null(learnr:::get_object(session, ns("count"))$data$numtry),
          0, learnr:::get_object(session, ns("count"))$data$numtry- 1))
  # subtracting 1 because when you close out and restore it runs once
  # ================================
  # if it is pressed override allow_retry to FALSE
  isolate(val$lock <- ifelse(is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock),
                               FALSE, learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock))
  
  # once lock is pressed; do not allow retry
  if(isolate(val$lock)){
    question$allow_retry = FALSE
  }
  
  # initialize partial_cred as NA
  partial_cred <- NA
  # ================================
  
  # only set when a submit button has been pressed
  # (or reset when try again is hit)
  # (or set when restoring)
  submitted_answer <- reactiveVal(NULL, label = "submitted_answer")
  
  is_correct_info <- reactive(label = "is_correct_info", {
    # question has not been submitted
    if (is.null(submitted_answer())) return(NULL)
    # find out if answer is right
    ret <- question_is_correct(question, submitted_answer())
    
    # new: get partial credit for wordbank and blank
    if(question$type == "wordbank"){
      partial_cred <- as.numeric(ret$messages)
      # then remove partial credit message output
      ret$messages <- NULL
    }
    
    question$partial_cred <<- partial_cred
    
    # new : mark wrong until locked =======
    # if exam == TRUE and lock is NOT pressed; mark_as(FALSE)
    if(isds_exam & !isolate(val$lock)){
      ret <- mark_as(FALSE, NULL)
    }
    
    # new : Increment counter =======
    isolate(val$numtry <- val$numtry+1)
    learnr:::save_object(session, ns("count"), 
                         learnr:::tutorial_object("count",
                                                  list(numtry = isolate(val$numtry)) ) )
    # ===============================
   
    if (!inherits(ret, "learnr_mark_as")) {
      stop("`question_is_correct(question, input$answer)` must return a result from `correct`, `incorrect`, or `mark_as`")
    }
    ret
  })
  
  # should present all messages?
  is_done <- reactive(label = "is_done", {
    if (is.null(is_correct_info())) return(NULL)
    
    # new if number of try is greater then max attempt do not allow more submissions.
    if (isTRUE(isolate(val$numtry) >= max_attempt)) return(TRUE)
    
    (!isTRUE(question$allow_retry)) || is_correct_info()$correct
  })
  
  
  button_type <- reactive(label = "button type", {
    if (is.null(submitted_answer())) {
      "submit"
    } else {
      # is_correct_info() should be valid
      if (is.null(is_correct_info())) {
        stop("`is_correct_info()` is `NULL` in a place it shouldn't be")
      }
      
      # update the submit button label
      if (is_correct_info()$correct) {
        "correct"
      } else {
        # not correct
        # new add check for numtry < max_attempt
        if (isTRUE(isolate(val$numtry)<max_attempt)&isTRUE(question$allow_retry)) {
          # not correct, but may try again
          "try_again"
        } else {
          # not correct and can not try again
          "incorrect"
        }
      }
    }
  })
  
  # disable / enable for every input$answer change
  answer_is_valid <- reactive(label = "answer_is_valid", {
    if (is.null(submitted_answer())) {
      question_is_valid(question, input$answer)
    } else {
      question_is_valid(question, submitted_answer())
    }
  })
  
  init_question <- function(restoreValue = NULL) {
    if (question$random_answer_order) {
      # see if we can shuffle the wordbank choices
      # if(question$type == "wordbank"){
      #   new_order <- learnr:::shuffle(1:length(question$choices))
      #   question$choices <<- question$choices[new_order]
      #   question$answers[[1]]$option <<- question$answers[[1]]$option[new_order]
      # }
      # Shuffle visible answer options (i.e. static, non-function answers)
      is_visible_option <- !learnr:::answer_type_is_function(question$answers)
      question$answers[is_visible_option] <<- learnr:::shuffle(question$answers[is_visible_option])
    }
    submitted_answer(restoreValue)
  }
  
  # restore past submission
  #  If no prior submission, it returns NULL
  past_submission_answer <- learnr:::retrieve_question_submission_answer(session, question$label)
  # initialize like normal... nothing has been submitted
  #   or
  # initialize with the past answer
  #  this should cascade throughout the app to display correct answers and final outputs
  init_question(past_submission_answer)
  
  output$action_button_container <- renderUI({
    learnr:::question_button_label(
      question,
      button_type(),
      answer_is_valid()
    )
  })
  
  output$message_container <- renderUI({
    req(!is.null(is_correct_info()), !is.null(is_done()))
    
    learnr:::withLearnrMathJax(
      learnr:::question_messages(
        question,
        messages = is_correct_info()$messages,
        is_correct = is_correct_info()$correct,
        is_done = is_done()
      )
    )
  })
  
  output$answer_container <- renderUI({
    if (is.null(submitted_answer())) {
      # has not submitted, show regular answers
      return(
        # if there is an existing input$answer, display it.
        # if there is no answer... init with NULL
        # Do not re-render the UI for every input$answer change
        learnr:::withLearnrMathJax(
          question_ui_initialize(question, isolate(input$answer))
        )
      )
    }
    
    # has submitted
    
    if (is.null(is_done())) {
      # has not initialized
      return(NULL)
    }
    
    if (is_done()) {
      # if the question is 'done', display the final input ui and disable everything
      
      return(
        learnr:::withLearnrMathJax(
          question_ui_completed(question, submitted_answer())
        )
      )
    }
    
    # if the question is NOT 'done', disable the current UI
    #   until it is reset with the try again button
    
    return(
      learnr:::withLearnrMathJax(
        question_ui_try_again(question, submitted_answer())
      )
    )
  })
  
  
  observeEvent(input$action_button, {
    
    if (button_type() == "try_again") {
      # maintain current submission / do not randomize answer order
      # only reset the submitted answers
      # does NOT reset input$answer
      submitted_answer(NULL)
      
      # submit "reset" to server
      learnr:::event_trigger(
        session,
        "reset_question_submission",
        data = list(
          label    = as.character(question$label),
          question = as.character(question$question)
        )
      )
      return()
    }
    
    submitted_answer(input$answer)
    
    # new ------
    # when submit button is clicked save the time
    learnr:::save_object(session, ns("submit"),
                         learnr:::tutorial_object("submit",
                                                  list(time = learnr:::timestamp_utc())))
    #-------
    
    # submit question to server
    learnr:::event_trigger(
      session = session,
      event   = "question_submission",
      data    = list(
        label    = as.character(question$label),
        question = as.character(question$question),
        answer   = as.character(input$answer),
        correct  = is_correct_info()$correct,
        #NEW ADD TRACKER
        attempt = isolate(val$numtry)
      )
    )
    
  })
  
  observe({
    # Update the `question_state()` reactive to report state back to the Shiny session
    req(submitted_answer(), is.reactive(question_state))
    
    # new ------
    # get time from last submission
    time_last <- ifelse(is.null(learnr:::get_object(session, ns("submit"))$data$time),
                               learnr:::timestamp_utc(), learnr:::get_object(session, ns("submit"))$data$time)
    # ----------
    
    current_answer_state <- list(
      type = "question",
      answer = submitted_answer(),
      correct = is_correct_info()$correct,
      #NEW ADDED TRACKER
      attempt = isolate(val$numtry),
      #Only record time when submitting - do not want to update every restart
      time_last = time_last,
      partial_cred = question$partial_cred
    )
    question_state(current_answer_state)
  })
}


