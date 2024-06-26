# Need to override exercise to accomplish the following tasks:
# 1. count number of submit attempts
# 2. store last "submitted" for grading purposes
# 3. stop storing submits/runs when lock is pressed for exam


# Copy setup_exercise_handler from exercise.R
# This is directly tied to exercise_server_chunk from knitr_hooks.R
# In order to override need to copy tutorial_knitr_options from knitr_hooks.R
# Need to install_knitr_hooks from knitr_hooks.R which is loaded in .onattach

# FUTURE: Disable submit answer button and run code button
# Can we do this with a disable_all_tags in the rendering?
# Or add new knitr option but this will most likely be in the js
# Or in rendering can you change knitr option while maintaining answer?
# Looks like this might be able to be done within get_exercise_cache/cache_complete_exercise


# taken from knitr_hooks.R
exercise_server_chunk <- function(label) {
  # reactive for exercise execution
  rmarkdown::shiny_prerendered_chunk('server', sprintf(
    '`tutorial-exercise-%s-result` <- tutorialExtras:::setup_exercise_handler(reactive(req(input$`tutorial-exercise-%s-code-editor`)), session)
output$`tutorial-exercise-%s-output` <- renderUI({
  `tutorial-exercise-%s-result`()
})', label, label, label, label))
}

#current_exercise_version <- "4"

# run an exercise and return HTML UI
# This is what we need to override for counter
# Might be able to override knitr options as well??
setup_exercise_handler <- function(exercise_rx, session) {
  # get the environment where shared setup and data is located. one environment up
  # includes all of the shiny housekeeping (e.g. inputs, output, etc.); two
  # environments up will be an empty environment
  # (https://github.com/rstudio/rmarkdown/blob/54bf8fc70122c6a435bba2ffcac8944d04498541/R/shiny_prerendered.R#L10)
  # that is parented by the shiny_prerendered server_envir (which has all of
  # the shared setup, data chunks executed).
  server_envir <- parent.env(parent.env(parent.frame()))
  
  # setup reactive values for return
  rv <- reactiveValues(triggered = 0, result = NULL)
  
  # observe input
  observeEvent(exercise_rx(), {
    
    # get exercise from app
    exercise <- exercise_rx()
    # Add tutorial information
    exercise$tutorial <- learnr::get_tutorial_info()
    # Remove tutorial items from exercise object
    exercise$tutorial$items <- NULL
    
    #-----------------------------------
    ## NEW 
    val <- reactiveValues(
      numtry = 0,
      lock = FALSE,
      code = NULL,
      correct = NULL,
      time = NULL
    )
    # restore counter
    ns <- NS(exercise$label)
    isolate(val$numtry <- ifelse(is.null(learnr:::get_object(session, ns("count"))$data$numtry),
                                 0, learnr:::get_object(session, ns("count"))$data$numtry))
    # if it is pressed do NOT SAVE any more submits/run code
    isolate(val$lock <- ifelse(is.null(learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock),
                               FALSE, learnr:::get_object(session, NS("lock", id = "pressed"))$data$lock))
    # get last submitted answer and info
    isolate(val$code <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$code),
                               "Not Submitted", learnr:::get_object(session, ns("ex_submit"))$data$code))
    isolate(val$correct <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$correct),
                                  FALSE, learnr:::get_object(session, ns("ex_submit"))$data$correct))
    isolate(val$time <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$time),
                               learnr:::timestamp_utc(), learnr:::get_object(session, ns("ex_submit"))$data$time))
    #--------------------------------------------------
    #-----------------------------------
    
    # short circuit for restore (we restore some outputs like errors so that
    # they are not re-executed when bringing the tutorial back up)
    if (exercise$restore) {
      if (
        getOption(
          "tutorial.quick_restore",
          identical(Sys.getenv("TUTORIAL_QUICK_RESTORE", "0"), "1")
        )
      ) {
        # don't evaluate at all if quick_restore is enabled
        rv$result <- list()
        return()
      }
      
      object <- learnr:::get_exercise_submission(session = session, label = exercise$label)
      if (!is.null(object) && !is.null(object$data$output)) {
        # restore user state, but don't report correct
        # since the user's code wasn't re-evaluated
        restored_state <- list(
          type = "exercise",
          answer = object$data$code,
          correct = NA, 
          answer_last = isolate(val$code),
          correct_last = isolate(val$correct),
          time_last = isolate(val$time),
          attempt = isolate(val$numtry)
        )
        learnr:::set_tutorial_state(exercise$label, restored_state, session = session)
        # get the output
        output <- object$data$output
        
        # ensure that html dependencies only reference package files
        dependencies <- htmltools::htmlDependencies(output)
        if (!is.null(dependencies))
          htmltools::htmlDependencies(output) <- learnr:::filter_dependencies(dependencies)
        
        # assign to rv and return
        rv$result <- output
        return()
      }
    }
    
    # get exercise evaluator factory function (allow replacement via global option)
    evaluator_factory <- getOption("tutorial.exercise.evaluator", default = NULL)
    if (is.null(evaluator_factory)) {
      remote_host <- getOption("tutorial.external.host", Sys.getenv("TUTORIAL_EXTERNAL_EVALUATOR_HOST", NA))
      if (!is.na(remote_host)){
        evaluator_factory <- learnr::external_evaluator(remote_host)
      } else if (!learnr:::is_windows() && !learnr:::is_mac())
        evaluator_factory <- learnr:::forked_evaluator_factory
      else
        evaluator_factory <- learnr:::inline_evaluator
    }
    # retrieve exercise cache information:
    # - chunks (setup + exercise) for the exercise to be processed in `evaluate_exercise`
    # - checker code (check, code-check, error-check)
    # - solution
    # - engine
    exercise <- learnr:::cache_complete_exercise(exercise)
    
    check_was_requested <- exercise$should_check
    # remove "should_check" item from exercise for legacy reasons, it's inferred downstream
    exercise$should_check <- NULL
    
    if (!isTRUE(check_was_requested)) {
      exercise$check <- NULL
      exercise$code_check <- NULL
      exercise$error_check <- NULL
    }
    # get timelimit option (either from chunk option or from global option)
    timelimit <- exercise$options$exercise.timelimit
    if (is.null(timelimit))
      timelimit <- getOption("tutorial.exercise.timelimit", default = 30)
    
    # placeholder for current learnr version to deal with exercise structure differences
    # with other learnr versions
    exercise$version <- learnr:::current_exercise_version
    
    # create a new environment parented by the global environment
    # transfer all of the objects in the server_envir (i.e. setup and data chunks)
    envir <- duplicate_env(server_envir, parent = globalenv())
    if (exists(".server_context", envir = envir)) {
      rm(".server_context", envir = envir)
    }
    
    # create exercise evaluator
    evaluator <- evaluator_factory(learnr:::evaluate_exercise(exercise, envir),
                                   timelimit, exercise, session)
    
    # Create exercise ID to map the associated events.
    ex_id <- learnr:::random_id("lnr_ex")
    
    # fire event before computing
    learnr:::event_trigger(
      session,
      "exercise_submitted",
      data = list(
        label   = exercise$label,
        id      = ex_id,
        code    = exercise$code,
        restore = exercise$restore,
        #Store last submitted
        answer_last = isolate(val$code),
        correct_last = isolate(val$correct),
        time_last = isolate(val$time),
        #NEW ADD TRACKER
        attempt = isolate(val$numtry)
      )
    )
    
    start <- Sys.time()
    
    # start it
    evaluator$start()
    
    # poll for completion
    o <- observe({
      
      if (evaluator$completed()) {
        
        # get the result
        result <- evaluator$result()
        
        #------------------------------------------------------------------
        #------------------------------------------------------------------
        # NEW
        # store the last SUBMITTED answer
        # if submit was pressed AND exam is NOT locked
        if(check_was_requested & isFALSE(isolate(val$lock))){
          learnr:::save_object(session, ns("ex_submit"), 
                               learnr:::tutorial_object("ex_submit",
                                                        list(code = exercise$code,
                                                             correct = result$feedback$correct,
                                                             time = learnr:::timestamp_utc()) ) )
        }
        
        # get last submitted answer and info
        isolate(val$code <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$code),
                                   "Not Submitted", learnr:::get_object(session, ns("ex_submit"))$data$code))
        isolate(val$correct <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$correct),
                                   FALSE, learnr:::get_object(session, ns("ex_submit"))$data$correct))
        isolate(val$time <- ifelse(is.null(learnr:::get_object(session, ns("ex_submit"))$data$time),
                                   learnr:::timestamp_utc(), learnr:::get_object(session, ns("ex_submit"))$data$time))
        #--------------------------------------------------
        #--------------------------------------------------
        
        
        # fire event with evaluation result
        learnr:::event_trigger(
          session,
          "exercise_result",
          data = list(
            label            = exercise$label,
            id               = ex_id,
            code             = exercise$code,
            output           = result$html_output,
            timeout_exceeded = result$timeout_exceeded,
            time_elapsed     = as.numeric(difftime(Sys.time(), start, units="secs")),
            error_message    = result$error_message,
            checked          = check_was_requested,
            feedback         = result$feedback,
            #Store last submitted
            answer_last = isolate(val$code),
            correct_last = isolate(val$correct),
            time_last = isolate(val$time),
            #NEW ADD TRACKER
            attempt = isolate(val$numtry)
          )
        )
        # assign reactive result to be sent to the UI
        rv$triggered <- isolate({ rv$triggered + 1})
        rv$result <- learnr:::exercise_result_as_html(result)
        
        isolate({
          # update the user_state with this submission, matching the behavior of
          # questions: always update exercises until correct answer is submitted
          current_state <- learnr::get_tutorial_state(exercise$label, session = session)
          
          # if restore = FALSE AND lock = TRUE do NOT update state
          # OR if is_exam = FALSE and correct = TRUE do NOT update state
          if(!(isFALSE(exercise$restore) & isTRUE(isolate(val$lock))) & 
             !(isFALSE(is_exam) & isTRUE(current_state$correct) ) ){
          #if (!isTRUE(current_state$correct)) {
            # new ============================
            # initialize counter
            # only count if "submit" was clicked; do not count restore
            isolate(val$numtry <- val$numtry + as.numeric(check_was_requested) - as.numeric(exercise$restore))
            learnr:::save_object(session, ns("count"), 
                                 learnr:::tutorial_object("count",
                                                          list(numtry = isolate(val$numtry)) ) )
            # end new: =======================
            
            new_state <- list(
              type = "exercise",
              answer = exercise$code,
              correct = result$feedback$correct %||% NA,
              #Store last submitted
              answer_last = isolate(val$code),
              correct_last = isolate(val$correct),
              time_last = isolate(val$time),
              #NEW ADDED TRACKER
              attempt = isolate(val$numtry)
            )
            learnr:::set_tutorial_state(exercise$label, new_state, session = session)
          }
        })
        
        
        # destroy the observer
        o$destroy()
        
      } else {
        invalidateLater(100, session)
      }
    })
  })
  
  
  # return reactive
  reactive({
    rv$triggered
    req(rv$result)
  })
  
}
################################################################################
################################################################################


################################################################################
################################################################################
# Taken from knitr_hooks.R
# contains the function exercise_server_chunk() which we need to override
# ONLY change is forcing tutorialExtras:::exercise_server_chunk
tutorial_knitr_options <- function() {
  
  
  
  # helper to check for runtime: shiny_prerendered being active
  is_shiny_prerendered_active <- function() {
    identical(knitr::opts_knit$get("rmarkdown.runtime"),"shiny_prerendered")
  }
  
  # helper to check for an exercise chunk
  is_exercise_chunk <- function(options) {
    isTRUE(options[["exercise"]])
  }
  
  is_chunk_empty_or_mismatched_exercise <- function(options) {
    label <- options$label
    
    if (is.null(get_knitr_chunk(label))) {
      return(TRUE)
    }
    
    chunk_opt_exercise <- attr(get_knitr_chunk(label), "chunk_opts")[["exercise"]]
    if (is.symbol(chunk_opt_exercise)) {
      # original chunk options might not be evaluated yet, see #757
      chunk_opt_exercise <- eval(chunk_opt_exercise, knitr::knit_global())
    }
    
    if (!identical(options$exercise, chunk_opt_exercise)) {
      # this looks like an exercise chunk, but knitr knows about a different
      # chunk that isn't an exercise here. so there must be a problem (i.e. this
      # is an empty chunk that didn't trigger knitr's duplicate chunk error).
      # Note that we can't rely on knit_code$get() or options$code since they
      # both report the code for the non-exercise chunk.
      msg <- sprintf("Cannot create exercise '%s': duplicate chunk label", label)
      rlang::abort(msg)
    }
    
    FALSE
  }
  
  # helper to find chunks that name a chunk as their setup chunk
  exercise_chunks_for_setup_chunk <- function(label) {
    label_query <- paste0("knitr::all_labels(exercise.setup == '", label, "')")
    eval(parse(text = label_query))
  }
  
  ensure_knit_code_exists <- function(
    current = knitr::opts_current$get(),
    all = knitr::opts_chunk$get()
  ) {
    label <- current$label
    
    # Recreate chunk options: unique to chunk or different from default.
    # Typically, we'd use `knit_code$get()` to find the chunk options defined
    # directly on the chunk, but that returns `NULL` for empty chunks and
    # doesn't include the chunk options. If we call this function in the options
    # hooks, we have an opportunity to infer the chunk options.
    chunk_opts <- current[setdiff(names(current), names(all))]
    for (opt in names(all)) {
      if (!identical(all[[opt]], current[[opt]])) {
        chunk_opts[[opt]] <- current[[opt]]
      }
    }
    
    n_lines <- current[["exercise.lines"]] %||% all[["exercise.lines"]] %||% 3L
    code <- rep_len("", n_lines)
    
    # https://github.com/yihui/knitr/blob/0f0c9c26/R/parser.R#L118
    chunk <- list(structure(code, chunk_opts = chunk_opts))
    names(chunk) <- label
    knitr::knit_code$set(chunk)
  }
  
  # helper to check for an exercise support chunk
  is_exercise_support_chunk <- function(
    options,
    type = c(
      "setup",
      "hint",
      "hint-\\d+",
      "solution",
      "error-check",
      "code-check",
      "check",
      "tests"
    )
  ) {
    # is this a support chunk using chunk labels to match with an exercise?
    support_regex <- paste0("-(", paste(type, collapse = "|"), ")$")
    if (grepl(support_regex, options$label)) {
      exercise_label <- sub(support_regex, "", options$label)
      label_query <- "knitr::all_labels(exercise == TRUE)"
      all_exercise_labels <- eval(parse(text = label_query))
      return(exercise_label %in% all_exercise_labels)
    }
    
    if ("setup" %in% type) {
      if (identical(options$label, "setup-global-exercise")) {
        return(TRUE)
      }
      
      # look for another chunk which names this as its setup chunk or if it has `exercise.setup`
      # this second condition is for support chunks that isn't referenced by an exercise yet
      # but is part of a chain and should be stored as a setup chunk
      is_referenced <- length(exercise_chunks_for_setup_chunk(options$label)) > 0
      if (is_referenced) {
        find_parent_setup_chunks(options) # only used to check for cycles; the return value is not useful here
        return(TRUE)
      }
      
      # if this looks like a setup chunk, but no one references it, error
      if (is.null(options[["exercise"]]) && !is.null(options$exercise.setup)) {
        stop(
          "Chunk '", options$label, "' is not being used by any exercise or exercise setup chunk.\n",
          "Please remove chunk '", options$label, "' or reference '", options$label, "' with `exercise.setup = '", options$label, "'`",
          call. = FALSE
        )
      }
    }
    
    FALSE
  }
  
  is_exercise_setup_chunk <- function(label) {
    grepl("-setup$", label) || (length(exercise_chunks_for_setup_chunk(label)) > 0)
  }
  
  # helper function to grab the raw knitr chunk associated with a chunk label
  get_knitr_chunk <- function(label) {
    # Note: we directly call the knitr function in this case because we do not
    # need to pass expressions which required delayed evaluation.
    knitr::knit_code$get(label)
  }
  
  get_setup_global_exercise <- function() {
    # setup-global-exercise is a special chunk name that will over-ride the
    # global setup chunk, but only for external evaluators. This lets tutorials
    # have separate setup code for the local shiny app and the remote evaluator.
    knitr::knit_code$get("setup-global-exercise") %||%
      knitr::knit_code$get("setup")
  }
  
  # helper function to find all the setup chunks associated with an exercise chunk
  # it goes up the chain of setup dependencies and returns a list of raw knitr chunks (if any)
  find_parent_setup_chunks <- function(options, visited = NULL) {
    # base case: when options are null, there are no more setup references
    if (is.null(options))
      return(NULL)
    has_visited <- options$label %in% visited
    # update visited set
    visited <- append(visited, options$label)
    # error out if there is a cycle
    if (has_visited) {
      stop("Chained setup chunks form a cycle!\nCycle: ", paste0(visited, collapse = " => "), call. = FALSE)
    }
    # check if the chunk with label has another setup chunk associated with it
    setup_label <- options$exercise.setup
    setup_chunk <- get_knitr_chunk(setup_label)
    # if the setup_label is mispelled, throw an error to user instead of silently ignoring
    # which would cause other issues when data dependencies can't be found
    if (!is.null(setup_label) && is.null(setup_chunk))
      stop(paste0("exercise.setup label '", setup_label, "' not found for exercise '", options$label, "'"))
    
    setup_options <- attr(setup_chunk, "chunk_opts")
    # serialize the options here so that the values are not evaluated when retrieved from learnr cache
    current_setup_chunks <- if (is.null(setup_options)) {
      list()
    } else {
      list(
        list(
          label = setup_label,
          code = paste0(setup_chunk, collapse = "\n"),
          opts = lapply(setup_options, learnr:::dput_to_string),
          engine = learnr:::knitr_engine(setup_options$engine)
        )
      )
    }
    # recurse
    append(find_parent_setup_chunks(setup_options, visited), current_setup_chunks)
  }
  
  # helper function to return a list of exercise chunk and its setup chunks
  get_all_chunks <- function(options) {
    # get the exercise chunk
    exercise_chunk <- get_knitr_chunk(options$label)
    exercise_options <- attr(exercise_chunk, "chunk_opts")
    # serialize the exercise options here so that the values are not evaluated when retrieved from learnr cache
    chunk_opts <- lapply(exercise_options, learnr:::dput_to_string)
    exercise_chunk <- paste0(exercise_chunk, collapse = "\n")
    # append the setup chunks at the front
    # retrieve the setup chunks associated with the exercise
    # if there is no `exercise.setup` find one with "label-setup"
    setup_chunks <-
      if (!is.null(options$exercise.setup)) {
        find_parent_setup_chunks(options)
      } else if (!is.null(get_knitr_chunk(paste0(options$label, '-setup')))) {
        options$exercise.setup <- paste0(options$label, '-setup')
        find_parent_setup_chunks(options)
      } else {
        NULL
      }
    append(setup_chunks, list(list(label = options$label, code = exercise_chunk, opts = chunk_opts, engine = learnr:::knitr_engine(options$engine))))
  }
  
  get_reveal_solution_option <- function(solution_opts) {
    exercise_chunk <- get_knitr_chunk(sub("-solution$", "", solution_opts$label))
    if (is.null(exercise_chunk)) {
      stop("Can not find exercise chunk for solution: `", solution_opts$label, "`")
    }
    
    # these are unevaluated options at this point
    exercise_opts <- attr(exercise_chunk, "chunk_opts")
    # get explicit opts on solution chunk since solution_opts was merged
    # with the global knitr chunk options
    sol_opts_user <- attr(get_knitr_chunk(solution_opts$label), "chunk_opts")
    
    # Determine if we should reveal the solution using...
    reveal_solution <-
      # 1. the option explicitly set on the solution chunk
      eval(sol_opts_user$exercise.reveal_solution, envir = knitr::knit_global()) %||%
      # 2. the option explicitly set on the exercise chunk
      eval(exercise_opts$exercise.reveal_solution, envir = knitr::knit_global()) %||%
      # 3. the global knitr chunk option
      solution_opts$exercise.reveal_solution %||%
      # 4. the global R option
      getOption("tutorial.exercise.reveal_solution", TRUE)
    
    isTRUE(reveal_solution)
  }
  
  # hook to turn off evaluation/highlighting for exercise related chunks
  tutorial_opts_hook <-  function(options) {
    
    # check for chunk type
    exercise_chunk <- is_exercise_chunk(options)
    exercise_support_chunk <- is_exercise_support_chunk(options)
    exercise_setup_chunk <- is_exercise_support_chunk(options, type = "setup")
    
    # validate that we have runtime: shiny_prerendered
    if ((exercise_chunk || exercise_support_chunk) && !is_shiny_prerendered_active()) {
      stop("Tutorial exercises require the use of 'runtime: shiny_prerendered'",
           call. = FALSE)
    }
    
    # validate or ensure that the exercise chunk is 'defined'
    if (exercise_chunk && is_chunk_empty_or_mismatched_exercise(options)) {
      ensure_knit_code_exists(options)
    }
    
    # if this is an exercise chunk then set various options
    if (exercise_chunk) {
      
      # one time tutor initialization
      learnr::initialize_tutorial()
      
      options$echo <- TRUE
      options$include <- TRUE
      options$highlight <- FALSE
      options$comment <- NA
      
      if (!is.null(options$exercise.eval))
        options$eval <- options$exercise.eval
      else
        options$eval <- FALSE
      # exercises can be support chunks, but if it's an exercise it should be treated that way
      return(options)
    }
    
    # if this is an exercise support chunk then force echo, but don't
    # eval or highlight it
    if (exercise_support_chunk) {
      options$echo <- TRUE
      options$include <- TRUE
      options$eval <- FALSE
      options$highlight <- FALSE
    }
    
    if (is_exercise_support_chunk(options, type = c("code-check", "error-check", "check", "tests"))) {
      # completely suppress behind-the-scenes support chunks
      options$include <- FALSE
    }
    
    if (is_exercise_support_chunk(options, type = "check")) {
      if (is.null(knitr::opts_chunk$get("exercise.checker"))) {
        stop(
          "An exercise check chunk exists ('", options$label, "') but an ",
          "exercise checker function is not configured for this tutorial. ",
          "Please use `tutorial_options()` to define an `exercise.checker`."
        )
      }
    }
    
    if (is_exercise_support_chunk(options, type = "solution")) {
      # only print solution if exercise.reveal_solution is TRUE
      options$echo <- get_reveal_solution_option(options)
    }
    
    # if this is an exercise setup chunk then eval it if the corresponding
    # exercise chunk is going to be executed
    if (exercise_setup_chunk) {
      # figure out the default behavior
      exercise_eval <- knitr::opts_chunk$get('exercise.eval')
      if (is.null(exercise_eval))
        exercise_eval <- FALSE
      
      # look for chunks that name this as their setup chunk
      labels <- exercise_chunks_for_setup_chunk(options$label)
      if (grepl("-setup$", options$label))
        labels <- c(labels, sub("-setup$", "", options$label))
      labels <- paste0('"', labels, '"')
      labels <- paste0('c(', paste(labels, collapse = ', ') ,')')
      label_query <- paste0("knitr::all_labels(label %in% ", labels, ", ",
                            "identical(exercise.eval, ", !exercise_eval, "))")
      
      default_reversed <- length(eval(parse(text = label_query))) > 0
      if (default_reversed)
        exercise_eval <- !exercise_eval
      
      # set the eval property as appropriate
      options$eval <- exercise_eval
      options$echo <- FALSE
    }
    
    # If this is an -error-check function, then make sure that -check chunk is provided
    if (
      is_exercise_support_chunk(options, type = "error-check") &&
      is.null(get_knitr_chunk(sub("-error", "", options$label)))
    ) {
      stop(
        "Exercise '", sub("-error-check", "", options$label), "': ",
        "a *-check chunk is required when using an *-error-check chunk, but",
        " '", sub("-error", "", options$label), "' was not found in the tutorial.",
        call. = FALSE
      )
    }
    
    # return modified options
    options
  }
  
  # hook to amend output for exercise related chunks
  tutorial_knit_hook <- function(before, options, envir) {
    if (!before) {
      # Signal any messages added during the chunk evaluation. This exists so
      # that we can direct messages to the console even if created inside a chunk
      learnr:::.learnr_messages$flush()
    }
    
    # helper to produce an exercise wrapper div w/ the specified class
    exercise_wrapper_div <- function(suffix = NULL, extra_html = NULL) {
      # before exercise
      if (before) {
        if (!is.null(suffix))
          suffix <- paste0("-", suffix)
        class <- paste0("exercise", suffix)
        lines <- ifelse(is.numeric(options$exercise.lines),
                        options$exercise.lines, 0)
        completion  <- as.numeric(options$exercise.completion %||% 1 > 0)
        diagnostics <- as.numeric(options$exercise.diagnostics %||% 1 > 0)
        startover <- as.numeric(options$exercise.startover %||% 1 > 0)
        paste0('<div class="tutorial-', class,
               '" data-label="', options$label,
               '" data-completion="', completion,
               '" data-diagnostics="', diagnostics,
               '" data-startover="', startover,
               '" data-lines="', lines, '">')
      }
      # after exercise
      else {
        c(extra_html, '</div>')
      }
    }
    
    # handle exercise chunks
    if (is_exercise_chunk(options)) {
      
      # one-time dependencies/server code
      extra_html <- NULL
      if (before) {
        
        # verify the chunk has a label if required
        learnr:::verify_tutorial_chunk_label()
        
        # inject ace and clipboardjs dependencies
        knitr::knit_meta_add(list(
          list(learnr:::ace_html_dependency()),
          list(learnr:::clipboardjs_html_dependency())
        ))
        
        # write server code
        tutorialExtras:::exercise_server_chunk(options$label)
      }
      else {
        # forward a subset of standard knitr chunk options
        options$engine <- learnr:::knitr_engine(options$engine)
        options$exercise.df_print <- options$exercise.df_print %||% knitr::opts_knit$get('rmarkdown.df_print') %||% "default"
        options$exercise.checker <- learnr:::dput_to_string(options$exercise.checker)
        all_chunks <- get_all_chunks(options)
        
        code_check_chunk <- get_knitr_chunk(paste0(options$label, "-code-check"))
        error_check_chunk <- get_knitr_chunk(paste0(options$label, "-error-check"))
        check_chunk <- get_knitr_chunk(paste0(options$label, "-check"))
        solution <- get_knitr_chunk(paste0(options$label, "-solution"))
        tests <- get_knitr_chunk(paste0(options$label, "-tests"))
        
        # remove class of "knitr_strict_list" so (de)serializing works properly for external evaluators
        class(options) <- NULL
        # we collect all the setup code to make exercise compatible with old learnr
        # note: this means that chained setup chunks will not work for non-R exercises
        # Remove this after v0.13 is released
        all_setup_code <- NULL
        if (length(all_chunks) > 1) {
          all_setup_code <- paste0(
            vapply(all_chunks[-length(all_chunks)], function(x) x$code, character(1)),
            collapse = "\n"
          )
        }
        
        this_exercise <- structure(
          list(
            label = options[["label"]],
            global_setup = get_setup_global_exercise(),
            setup = all_setup_code,
            chunks = all_chunks,
            code_check = code_check_chunk,
            error_check = error_check_chunk,
            check = check_chunk,
            solution  = solution,
            tests = tests,
            options = options[setdiff(names(options), "tutorial")],
            engine = options$engine,
            version = learnr:::current_exercise_version
          ),
          class = c(options$engine, "tutorial_exercise")
        )
        
        # serialize the list of chunks to server
        rmarkdown::shiny_prerendered_chunk(
          'server',
          sprintf(
            'learnr:::store_exercise_cache(%s)',
            learnr:::dput_to_string(this_exercise)
          )
        )
        
        # script tag with knit options for this chunk
        caption <-
          if (!is.null(options$exercise.cap)) {
            as.character(options$exercise.cap)
          } else {
            cap_engine <- learnr:::knitr_engine(options$engine)
            
            # use logo shipped within learnr pkg (currently none)
            cap_engine_file <- system.file(file.path("internals", "icons", paste0(cap_engine, ".svg")), package = "learnr")
            if (file.exists(cap_engine_file)) {
              as.character(htmltools::div(
                class = "tutorial_engine_icon",
                htmltools::HTML(readLines(cap_engine_file))
              ))
            } else {
              cap_engine_val <- learnr:::knitr_engine_caption(options[["engine"]])
              learnr:::i18n_span(
                "text.enginecap",
                paste(cap_engine_val, "Code"),
                opts = list(engine = cap_engine_val)
              )
            }
          }
        ui_options <- list(
          engine = options$engine,
          has_checker = (!is.null(check_chunk) || !is.null(code_check_chunk)),
          caption = as.character(caption)
        )
        extra_html <- c('<script type="application/json" data-ui-opts="1">',
                        jsonlite::toJSON(ui_options, auto_unbox = TRUE),
                        '</script>')
      }
      
      # wrapper div (called for before and after)
      exercise_wrapper_div(extra_html = extra_html)
    }
    
    # handle exercise support chunks (hints, solution)
    else if (is_exercise_support_chunk(options)) {
      
      # setup and checking code (-setup, -code-check, and -check) are included in exercise cache
      # do not send the setup and checking code to the browser
      
      # send hint and solution to the browser
      # these are visibly displayed in the UI
      if (is_exercise_support_chunk(options, type = c("hint", "hint-\\d+"))) {
        exercise_wrapper_div(suffix = "support")
      } else if (is_exercise_support_chunk(options, type = "solution")) {
        if (get_reveal_solution_option(options)) {
          exercise_wrapper_div(suffix = "support")
        }
      }
      
    }
  }
  list(
    # learnr uses `tutorial` for options and hooks, and we also globally set the
    # chunk option `tutorial = TRUE`. This allows the learnr tutorial hooks to
    # visit every chunk without colliding with hooks or options set by other
    # packages or Rmd formats.
    opts_chunk = list(tutorial = TRUE),
    opts_hooks = list(tutorial = tutorial_opts_hook),
    knit_hooks = list(tutorial = tutorial_knit_hook)
  )
}
################################################################################
################################################################################
#making sure we grab our tutorial_knitr_options above
install_knitr_hooks <- function() {
  knit_opts <- tutorialExtras:::tutorial_knitr_options()
  knitr::opts_chunk$set(tutorial = knit_opts$opts_chunk$tutorial)
  knitr::opts_hooks$set(tutorial = knit_opts$opts_hooks$tutorial)
  knitr::knit_hooks$set(tutorial = knit_opts$knit_hooks$tutorial)
}
################################################################################
################################################################################
# override exercise initial start
.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  #initialize_seed()
}

    
