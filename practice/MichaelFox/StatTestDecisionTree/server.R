`%||%` <- function(a, b) if (!is.null(a)) a else b

server <- function(input, output, session) {

  # ── State ──────────────────────────────────────────────────────────────────
  rv <- reactiveValues(
    answers    = list(),
    step_index = 1,
    pending    = NULL   # "step_id|value" string of the currently highlighted choice
  )

  # ── Derived reactives ──────────────────────────────────────────────────────
  active_steps <- reactive({
    get_active_steps(rv$answers)
  })

  current_step_id <- reactive({
    steps <- active_steps()
    if (rv$step_index <= length(steps)) steps[[rv$step_index]] else NULL
  })

  total_steps <- reactive({
    length(active_steps())
  })

  # Display total: show max (5) until goal is answered so the bar never jumps
  # backward. Once goal is known the real path length is used.
  display_total <- reactive({
    if (is.null(rv$answers[["goal"]])) 5L else total_steps()
  })

  is_last_step <- reactive({
    rv$step_index == total_steps()
  })

  # ── Capture choice selection from JS ──────────────────────────────────────
  observeEvent(input$wizard_selection, {
    rv$pending <- input$wizard_selection
  })

  # ── Render wizard UI ───────────────────────────────────────────────────────
  output$wizard_ui <- renderUI({
    step_id <- current_step_id()

    if (is.null(step_id)) {
      return(div(
        class = "no-results-box",
        h4("Wizard complete"),
        p("Switch to the Results tab to see your recommendations."),
        actionButton("reset_from_empty", "Start Over",
                     class = "btn btn-outline-secondary mt-2")
      ))
    }

    config      <- STEP_CONFIGS[[step_id]]
    existing    <- rv$answers[[step_id]]
    step_idx    <- rv$step_index
    total       <- display_total()
    pct         <- round((step_idx - 1) / total * 100)
    last_step   <- is_last_step()

    # Choice buttons
    choice_btns <- lapply(config$choices, function(ch) {
      is_active <- !is.null(existing) && existing == ch$value
      tags$button(
        class   = paste("wizard-choice", if (is_active) "active" else ""),
        onclick = sprintf("selectWizardChoice('%s','%s',this)", step_id, ch$value),
        type    = "button",
        tags$span(class = "choice-label", ch$label),
        tags$span(class = "choice-desc",  ch$desc)
      )
    })

    # Navigation buttons
    back_btn <- if (step_idx > 1) {
      actionButton("back_btn", label = tagList(icon("arrow-left"), " Back"),
                   class = "btn btn-outline-secondary")
    } else {
      tags$span()
    }

    next_label <- if (last_step) {
      tagList("See Results ", icon("arrow-right"))
    } else {
      tagList("Next ", icon("arrow-right"))
    }

    tagList(
      div(
        class = "wizard-card",

        # Progress
        div(
          class = "wizard-step-label",
          sprintf("Step %d of %d", step_idx, total)
        ),
        div(
          class = "progress",
          div(
            class = "progress-bar",
            role  = "progressbar",
            style = sprintf("width: %d%%", pct),
            `aria-valuenow` = pct,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100"
          )
        ),

        # Question
        div(class = "wizard-question", config$question),

        # Choice cards
        tagList(choice_btns),

        # Error message (shown by JS / server)
        div(id = "wizard-error", class = "wizard-error",
            icon("exclamation-circle"), " Please select an option before continuing."),

        # Help section
        div(
          class = "mt-3",
          tags$button(
            type            = "button",
            class           = "help-toggle-btn",
            `data-bs-toggle`= "collapse",
            `data-bs-target`= "#help-panel",
            `aria-expanded` = "false",
            icon("info-circle"), " What does this mean?"
          ),
          div(
            id    = "help-panel",
            class = "collapse",
            div(class = "help-content mt-2", HTML(config$help_text))
          )
        ),

        # Navigation
        div(
          class = "wizard-nav",
          back_btn,
          actionButton("start_over", label = tagList(icon("undo"), " Start Over"),
                       class = "btn btn-link text-muted"),
          actionButton("next_btn",
                       label = next_label,
                       class = "btn btn-primary btn-next")
        )
      )
    )
  })

  # ── Next button ────────────────────────────────────────────────────────────
  observeEvent(input$next_btn, {
    step_id  <- current_step_id()
    if (is.null(step_id)) return()

    # Determine answer: pending input or already-stored answer
    pending  <- rv$pending
    stored   <- rv$answers[[step_id]]

    chosen_value <- NULL
    if (!is.null(pending)) {
      parts <- strsplit(pending, "\\|")[[1]]
      if (length(parts) == 2 && parts[1] == step_id) {
        chosen_value <- parts[2]
      }
    }
    if (is.null(chosen_value) && !is.null(stored)) {
      chosen_value <- stored
    }

    if (is.null(chosen_value)) {
      # Show error via JS
      runjs("var err = document.getElementById('wizard-error');
             if (err) err.classList.add('visible');")
      return()
    }

    rv$answers[[step_id]] <- chosen_value
    rv$pending <- NULL

    if (is_last_step()) {
      updateNavbarPage(session, "main_nav", selected = "results")
    } else {
      rv$step_index <- rv$step_index + 1
    }
  })

  # ── Back button ────────────────────────────────────────────────────────────
  observeEvent(input$back_btn, {
    if (rv$step_index > 1) {
      steps <- active_steps()
      for (i in rv$step_index:length(steps)) {
        rv$answers[[steps[[i]]]] <- NULL
      }
      rv$step_index <- rv$step_index - 1
      rv$pending    <- NULL
    }
  })

  # ── Start Over (from wizard and results) ───────────────────────────────────
  reset_wizard <- function() {
    rv$answers    <- list()
    rv$step_index <- 1
    rv$pending    <- NULL
    updateNavbarPage(session, "main_nav", selected = "wizard")
  }

  observeEvent(input$start_over,         { reset_wizard() })
  observeEvent(input$start_over_results, { reset_wizard() })
  observeEvent(input$reset_from_empty,   { reset_wizard() })

  # ── Render Results UI ──────────────────────────────────────────────────────
  output$results_ui <- renderUI({
    steps <- active_steps()

    # Check all steps are answered
    all_answered <- all(vapply(steps, function(s) !is.null(rv$answers[[s]]), logical(1)))

    if (!all_answered || length(steps) < 3) {
      return(div(
        class = "no-results-box",
        h4("No results yet"),
        p("Complete the wizard in the \"Find My Test\" tab to see your test recommendations."),
        actionButton("go_to_wizard", "Go to Find My Test",
                     class = "btn btn-primary mt-2",
                     onclick = "Shiny.setInputValue('switch_to_wizard', 1, {priority:'event'})")
      ))
    }

    recs <- get_recommendations(rv$answers)

    # ── Answer summary (collapsible) ─────────────────────────────────────────
    answer_chips <- lapply(steps, function(s) {
      val   <- rv$answers[[s]]
      lbl   <- step_label(s)
      anlbl <- answer_label(s, val)
      tags$span(
        class = "answer-chip",
        tags$span(class = "chip-label", lbl, ": "),
        anlbl
      )
    })

    summary_section <- div(
      class = "answer-summary mb-3",
      div(
        style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:0.5rem;",
        tags$button(
          type            = "button",
          class           = "help-toggle-btn fw-bold",
          `data-bs-toggle`= "collapse",
          `data-bs-target`= "#summary-collapse",
          `aria-expanded` = "true",
          icon("chevron-down"), " Your Answers"
        )
      ),
      div(
        id    = "summary-collapse",
        class = "collapse show",
        div(class = "mt-2", tagList(answer_chips))
      )
    )

    # ── Optional note ────────────────────────────────────────────────────────
    note_box <- if (!is.null(recs$note) && nchar(recs$note) > 0) {
      div(
        class = "recommendation-note",
        icon("lightbulb"), " ", HTML(recs$note)
      )
    } else NULL

    # ── Build test cards ─────────────────────────────────────────────────────
    best_ids <- recs$best
    alt_ids  <- recs$alternatives

    if (length(best_ids) == 0 && length(alt_ids) == 0) {
      return(tagList(
        summary_section,
        note_box,
        div(
          class = "no-results-box",
          h4("No specific test found"),
          p(if (!is.null(recs$note)) recs$note else
            "The combination you selected does not map to a test in this guide."),
          br(),
          actionButton("start_over_results", "Start Over",
                       class = "btn btn-primary")
        )
      ))
    }

    make_test_card <- function(test_id, rank_label) {
      test <- TESTS[[test_id]]
      if (is.null(test)) return(NULL)

      is_best   <- rank_label == "Best Fit"
      card_cls  <- paste("test-card", if (is_best) "best-fit" else "")
      badge_cls <- paste("badge-fit", if (is_best) "badge-best" else "badge-alt")

      # When-to-use / when-not-to-use / assumptions as bullet lists
      make_list <- function(items) {
        tags$ul(lapply(items, tags$li))
      }

      div(
        class = card_cls,

        # Header
        div(
          class = "test-card-header",
          h5(class = "test-card-name", test$name),
          span(class = badge_cls, rank_label)
        ),

        # Body
        div(
          class = "test-card-body",

          span(class = "test-section-label", "What it does"),
          p(test$description),

          span(class = "test-section-label", "When to use"),
          make_list(test$when_to_use),

          span(class = "test-section-label", "When NOT to use"),
          make_list(test$when_not_to_use),

          span(class = "test-section-label", "Assumptions"),
          make_list(test$assumptions),

          span(class = "test-section-label mt-3", "Example R Code"),
          tags$pre(tags$code(test$r_code))
        )
      )
    }

    best_cards <- lapply(best_ids, make_test_card, rank_label = "Best Fit")
    alt_cards  <- lapply(alt_ids,  make_test_card, rank_label = "Alternative")

    tagList(
      h2("Test Recommendations", class = "text-uf-blue mb-1 mt-2"),
      p("Based on your answers, here are the most appropriate statistical tests.",
        class = "text-muted mb-3"),
      summary_section,
      note_box,
      tagList(best_cards),
      if (length(alt_cards) > 0) {
        tagList(
          h5("Alternative Options", class = "text-muted mt-4 mb-2"),
          tagList(alt_cards)
        )
      },
      div(
        class = "text-center mt-4 mb-4",
        actionButton("start_over_results",
                     label = tagList(icon("undo"), " Start Over"),
                     class = "btn btn-outline-secondary")
      )
    )
  })

  # Switch to wizard tab from results page
  observeEvent(input$switch_to_wizard, {
    updateNavbarPage(session, "main_nav", selected = "wizard")
  })

  # ── Render Glossary UI ─────────────────────────────────────────────────────
  output$glossary_ui <- renderUI({
    query <- tolower(trimws(input$glossary_search %||% ""))

    # Filter terms
    df <- glossary_terms
    if (nchar(query) > 0) {
      df <- df[grepl(query, tolower(df$term)) |
               grepl(query, tolower(df$definition)), ]
    }

    if (nrow(df) == 0) {
      return(div(
        class = "text-center text-muted py-4",
        p("No terms found matching \"", input$glossary_search, "\".")
      ))
    }

    # Sort A-Z
    df <- df[order(df$term), ]

    # Group by first letter
    letters_present <- unique(toupper(substr(df$term, 1, 1)))

    letter_sections <- lapply(letters_present, function(ltr) {
      subset_df <- df[toupper(substr(df$term, 1, 1)) == ltr, ]
      term_tags <- do.call(tagList, lapply(seq_len(nrow(subset_df)), function(i) {
        div(
          id    = paste0("glossary-term-", subset_df$id[i]),
          class = "glossary-term-card",
          h6(class = "glossary-term-heading", subset_df$term[i]),
          p(class  = "glossary-term-def",     subset_df$definition[i])
        )
      }))
      tagList(
        div(class = "glossary-section-letter", ltr),
        term_tags
      )
    })

    div(tagList(letter_sections))
  })

  # Glossary navigation from wizard links
  observeEvent(input$glossary_navigate, {
    term_id <- input$glossary_navigate
    # Clear search so the term is visible
    updateTextInput(session, "glossary_search", value = "")
    # Switch to glossary tab
    updateNavbarPage(session, "main_nav", selected = "glossary")
    # Scroll to and highlight the term
    session$sendCustomMessage("scrollToGlossaryTerm", term_id)
  })
}
