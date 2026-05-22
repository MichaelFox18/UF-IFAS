uf_theme <- bs_theme(
  version    = 5,
  primary    = "#FA4616",
  secondary  = "#003087",
  success    = "#28a745",
  info       = "#17a2b8",
  bg         = "#f5f6fa",
  fg         = "#212529",
  base_font  = font_google("Inter"),
  code_font  = font_google("Fira Code")
)

wizard_js <- "
// Select a wizard choice card
function selectWizardChoice(stepId, value, el) {
  document.querySelectorAll('.wizard-choice').forEach(function(btn) {
    btn.classList.remove('active');
  });
  el.classList.add('active');
  var err = document.getElementById('wizard-error');
  if (err) err.classList.remove('visible');
  Shiny.setInputValue('wizard_selection', stepId + '|' + value, {priority: 'event'});
}

// Navigate to Glossary tab and highlight/search a term
$(document).on('click', '.glossary-link', function(e) {
  e.preventDefault();
  var term = $(this).data('term');
  Shiny.setInputValue('glossary_navigate', term, {priority: 'event'});
});

// Scroll to highlighted glossary term after tab switch
Shiny.addCustomMessageHandler('scrollToGlossaryTerm', function(termId) {
  setTimeout(function() {
    var el = document.getElementById('glossary-term-' + termId);
    if (el) {
      el.scrollIntoView({behavior: 'smooth', block: 'center'});
      el.classList.add('highlighted');
      setTimeout(function() { el.classList.remove('highlighted'); }, 2500);
    }
  }, 300);
});
"

ui <- page_navbar(
  id    = "main_nav",
  title = tags$span(
    style = "font-weight:800; letter-spacing:-0.01em;",
    tags$span(style = "color:#FA4616;", "Stat"),
    tags$span(style = "color:white;",   "Guide")
  ),
  theme     = uf_theme,
  navbar_options = navbar_options(bg = "#003087"),
  fillable  = FALSE,
  header    = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$script(HTML(wizard_js))
    )
  ),

  # ── Tab 1: Find My Test ────────────────────────────────────────────────────
  nav_panel(
    title = "Find My Test",
    value = "wizard",
    div(
      class = "wizard-container",
      uiOutput("wizard_ui")
    )
  ),

  # ── Tab 2: Results ─────────────────────────────────────────────────────────
  nav_panel(
    title = "Results",
    value = "results",
    div(
      class = "results-container",
      uiOutput("results_ui")
    )
  ),

  # ── Tab 3: Glossary ────────────────────────────────────────────────────────
  nav_panel(
    title = "Glossary",
    value = "glossary",
    div(
      style = "max-width:800px; margin:2rem auto; padding:0 1rem;",
      h2("Statistical Glossary", class = "text-uf-blue mb-1"),
      p("Plain-English definitions of common statistical terms, A to Z.",
        class = "text-muted mb-3"),
      div(
        class = "glossary-search-box",
        textInput(
          inputId     = "glossary_search",
          label       = NULL,
          placeholder = "Search terms...",
          width       = "100%"
        )
      ),
      uiOutput("glossary_ui")
    )
  ),

  # ── Tab 4: About ───────────────────────────────────────────────────────────
  nav_panel(
    title = "About",
    value = "about",
    div(
      class = "about-container",

      h2("About StatGuide", class = "text-uf-blue mb-1"),
      p("A step-by-step decision tool for choosing the right statistical test.",
        class = "text-muted mb-4"),

      h4("How to Use StatGuide", class = "fw-bold mb-3"),
      div(
        class = "about-step",
        div(class = "about-step-num", "1"),
        div(
          tags$strong("Open the Find My Test tab"),
          tags$br(),
          "Answer five short questions about your data, one at a time.
           Use the \"What does this mean?\" sections if any term is unfamiliar."
        )
      ),
      div(
        class = "about-step",
        div(class = "about-step-num", "2"),
        div(
          tags$strong("Review your recommendations"),
          tags$br(),
          "The Results tab shows ranked test cards — the Best Fit test plus alternatives.
           Each card explains when to use the test, its assumptions, and includes
           copy-ready R code."
        )
      ),
      div(
        class = "about-step",
        div(class = "about-step-num", "3"),
        div(
          tags$strong("Look up unfamiliar terms"),
          tags$br(),
          "Use the Glossary tab for plain-English definitions. Terms highlighted in
           blue inside the wizard are clickable and jump directly to their definition."
        )
      ),
      div(
        class = "about-step",
        div(class = "about-step-num", "4"),
        div(
          tags$strong("Start Over any time"),
          tags$br(),
          "Use the Start Over button to reset the wizard and try a different path."
        )
      ),

      div(class = "divider"),

      h4("Who is this for?", class = "fw-bold mb-2"),
      p("StatGuide is designed for undergraduate and graduate students in the biological,
         social, and health sciences who need guidance selecting an appropriate statistical
         test. It covers the most commonly used tests in applied research."),

      div(class = "divider"),

      h4("Important Notes", class = "fw-bold mb-2"),
      tags$ul(
        tags$li("This guide covers the most commonly used classical tests.
                 Specialized designs (e.g., mixed models, survival analysis, SEM)
                 are beyond its scope."),
        tags$li("Statistical significance (p < 0.05) is not the only criterion for a
                 good analysis — always report effect sizes and confidence intervals."),
        tags$li("When in doubt about your data, consult a statistician.")
      ),

      div(class = "divider"),

      h4("References", class = "fw-bold mb-2"),
      tags$ul(
        style = "font-size:0.9rem; line-height:1.8;",
        tags$li("Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test:
                 A Decision Tree Approach. ", tags$em("Statology.")),
        tags$li("McCrum-Gardner, E. (2008). Which is the correct statistical test to use?
                 ", tags$em("British Journal of Oral and Maxillofacial Surgery, 46"), ", 38–41."),
        tags$li("Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical
                 differences. ", tags$em("Biochemia Medica, 20"), "(1), 15–32.")
      ),

      div(class = "divider"),
      p(
        class = "text-muted",
        style = "font-size:0.8rem;",
        "StatGuide was built with R Shiny for the University of Florida Institute of
         Food and Agricultural Sciences (UF/IFAS)."
      )
    )
  )
)
