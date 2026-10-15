# Earlybird: an early-warning view of a student's final grade.
# Uses only shiny and bslib, so it also runs in the browser through Shinylive.

library(shiny)
library(bslib)

source("predict.R")
models <- readRDS("model.rds")
heldout <- read.csv("heldout.csv", stringsAsFactors = FALSE)
heldout_sat <- heldout[!heldout$no_final, ]

PASS <- 10  # Portuguese grades run 0-20; 10 is a pass.
`%||%` <- function(a, b) if (is.null(a)) b else a

MOMENT_CHOICES <- c(
  "Start of year" = "start",
  "After term 1" = "term1",
  "After term 2" = "term2"
)

yes_no <- c("Yes" = "yes", "No" = "no")
scale5 <- c("1 (very low)" = 1, "2" = 2, "3" = 3, "4" = 4, "5 (very high)" = 5)
edu <- c("None" = 0, "Primary (to 4th grade)" = 1, "5th-9th grade" = 2, "Secondary" = 3, "Higher education" = 4)
jobs <- c("Teacher" = "teacher", "Health care" = "health", "Civil services" = "services",
          "At home" = "at_home", "Other" = "other")

# Every input: id = dataset column. Defaults are a typical held-out student
# (median or most common value), not a real person.
typical <- function(v) {
  x <- heldout[[v]]
  if (is.numeric(x)) round(median(x)) else names(sort(table(x), decreasing = TRUE))[1]
}

field <- function(id, label, type, choices = NULL, min = NULL, max = NULL) {
  value <- typical(id)
  switch(type,
    select = selectInput(id, label, choices, selected = value),
    radio = radioButtons(id, label, choices, selected = value, inline = TRUE),
    number = numericInput(id, label, value, min = min, max = max, step = 1),
    slider = sliderInput(id, label, min = min, max = max, value = value, step = 1, ticks = FALSE)
  )
}

inputs_ui <- accordion(
  id = "groups", open = c("Grades so far", "School"),
  accordion_panel("Grades so far",
    conditionalPanel("input.moment == 'start'",
      p(class = "hint", "No grades yet at the start of the year. Pick a later point in the year at the top to add them.")),
    conditionalPanel("input.moment != 'start'",
      field("G1", "Term 1 grade (G1, 0 to 20)", "slider", min = 0, max = 20)),
    conditionalPanel("input.moment == 'term2'",
      field("G2", "Term 2 grade (G2, 0 to 20)", "slider", min = 0, max = 20))
  ),
  accordion_panel("School",
    field("subject", "Subject", "radio", c("Math" = "math", "Portuguese" = "portuguese")),
    field("school", "School", "radio", c("Gabriel Pereira" = "GP", "Mousinho da Silveira" = "MS")),
    field("failures", "Classes failed before", "select", c("0" = 0, "1" = 1, "2" = 2, "3 or more" = 3)),
    field("absences", "Absences this year", "number", min = 0, max = 93),
    field("studytime", "Weekly study time", "select",
          c("Under 2 hours" = 1, "2 to 5 hours" = 2, "5 to 10 hours" = 3, "Over 10 hours" = 4)),
    field("schoolsup", "Extra school support", "radio", yes_no),
    field("paid", "Paid extra classes", "radio", yes_no),
    field("higher", "Wants higher education", "radio", yes_no),
    field("reason", "Why this school", "select",
          c("Close to home" = "home", "Reputation" = "reputation", "Course" = "course", "Other" = "other"))
  ),
  accordion_panel("Family",
    field("Medu", "Mother's education", "select", edu),
    field("Fedu", "Father's education", "select", edu),
    field("Mjob", "Mother's job", "select", jobs),
    field("Fjob", "Father's job", "select", jobs),
    field("guardian", "Guardian", "select", c("Mother" = "mother", "Father" = "father", "Other" = "other")),
    field("famsize", "Family size", "radio", c("3 or fewer" = "LE3", "More than 3" = "GT3")),
    field("famsup", "Family help with schoolwork", "radio", yes_no),
    field("famrel", "Family relationships", "select", scale5),
    field("internet", "Internet at home", "radio", yes_no),
    field("nursery", "Attended nursery school", "radio", yes_no)
  ),
  accordion_panel("Life outside class",
    field("activities", "Extracurricular activities", "radio", yes_no),
    field("freetime", "Free time after school", "select", scale5),
    field("goout", "Going out with friends", "select", scale5),
    field("Dalc", "Weekday drinking", "select", scale5),
    field("Walc", "Weekend drinking", "select", scale5),
    field("health", "Current health", "select", scale5),
    field("traveltime", "Travel to school", "select",
          c("Under 15 min" = 1, "15 to 30 min" = 2, "30 min to 1 hour" = 3, "Over 1 hour" = 4))
  )
)

LABELS <- c(
  G1 = "Term 1 grade", G2 = "Term 2 grade", failures = "Classes failed before",
  absences = "Absences", studytime = "Study time", higher = "Wants higher education",
  schoolsup = "Extra school support", paid = "Paid extra classes", famsup = "Family help",
  Medu = "Mother's education", Fedu = "Father's education", Mjob = "Mother's job",
  Fjob = "Father's job", guardian = "Guardian", famsize = "Family size", Pstatus = "Parents together",
  famrel = "Family relationships", activities = "Activities", freetime = "Free time",
  goout = "Going out", Dalc = "Weekday drinking", Walc = "Weekend drinking", health = "Health",
  romantic = "Relationship", internet = "Internet at home", sex = "Sex", age = "Age",
  address = "Lives in", traveltime = "Travel time", nursery = "Nursery school",
  school = "School", subject = "Subject", reason = "Why this school"
)

ui <- page_fillable(
  theme = bs_theme(
    version = 5, bg = "#fbf8f1", fg = "#1d2433", primary = "#1f4e8c", danger = "#c0392b",
    success = "#2e7d4f", base_font = font_collection("Iowan Old Style", "Georgia", "serif"),
    heading_font = font_collection("Avenir Next", "Segoe UI", "Helvetica Neue", "sans-serif"),
    "border-radius" = "6px"
  ),
  tags$head(tags$style(HTML(readLines("style.css", warn = FALSE)))),
  title = "Earlybird",
  padding = 0, gap = 0,
  div(class = "masthead",
    div(
      h1("Earlybird"),
      p("How a student's final grade is shaping up, and how early we can tell.")
    ),
    radioButtons("moment", NULL, MOMENT_CHOICES, selected = "start", inline = TRUE)
  ),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(4, 8)), gap = "0", class = "body",
    div(class = "inputs",
      div(class = "inputs-head",
        h2("The student"),
        actionButton("sample", "Load a real held-out student", class = "btn-sm btn-outline-primary")
      ),
      uiOutput("sample_note"),
      inputs_ui
    ),
    div(class = "results",
      uiOutput("grade_card"),
      div(class = "panel",
        h2("What moves this prediction"),
        p(class = "hint", "Compared with an average student in the training data. These are associations the model found, not causes: changing one of them won't necessarily change the grade."),
        uiOutput("drivers")
      ),
      div(class = "panel",
        h2("How well it works"),
        uiOutput("track_record")
      ),
      div(class = "panel notes",
        h2("Read before using"),
        tags$ul(
          tags$li(HTML("Trained on two Portuguese secondary schools in 2005–2006 (UCI Student Performance data). It is a teaching project, not a tool to make decisions about real students.")),
          tags$li(uiOutput("absent_note", inline = TRUE)),
          tags$li("The data also records sex, age, home address, whether parents live together and whether the student is in a relationship. This model leaves them out: in cross-validation the version without them was just as accurate.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  student <- reactive({
    ids <- unique(unlist(lapply(models[MOMENT_CHOICES], function(m) m$features)))
    vals <- lapply(setNames(ids, ids), function(id) {
      v <- input[[id]]
      if (is.null(v)) return(NA)
      num <- suppressWarnings(as.numeric(v))
      if (!is.na(num) && !id %in% c("subject", "school")) num else v
    })
    as.data.frame(vals, stringsAsFactors = FALSE)
  })

  model <- reactive(models[[input$moment]])
  pred <- reactive(predict_exported(model(), student()))

  sampled <- reactiveVal(NULL)
  observeEvent(input$sample, {
    row <- heldout_sat[sample(nrow(heldout_sat), 1), ]
    sampled(row)
    for (id in intersect(names(row), names(LABELS))) {
      if (is.null(input[[id]])) next  # not an input in this app (a left-out attribute)
      v <- row[[id]]
      if (id %in% c("G1", "G2", "age")) updateSliderInput(session, id, value = v)
      else if (id == "absences") updateNumericInput(session, id, value = v)
      else if (id %in% c("subject", "school", "sex", "address", "famsize", "Pstatus",
                         "schoolsup", "paid", "higher", "famsup", "activities", "romantic",
                         "internet", "nursery")) updateRadioButtons(session, id, selected = v)
      else updateSelectInput(session, id, selected = as.character(v))
    }
  })
  output$sample_note <- renderUI({
    row <- sampled()
    if (is.null(row)) return(p(class = "hint", "Or set the fields yourself. Defaults are a typical student."))
    div(class = "sample-note",
      HTML(sprintf("A real student from the held-out set, never seen in training. Their actual final grade was <b>%d</b>.", row$G3)))
  })

  output$grade_card <- renderUI({
    p <- pred(); m <- model()
    shown <- max(0, min(20, p))
    lo <- max(0, p - 1.28 * m$cv_rmse); hi <- min(20, p + 1.28 * m$cv_rmse)
    risk <- pnorm(PASS - 0.5, mean = p, sd = m$cv_rmse)  # chance the grade lands below 10
    band <- if (risk >= 0.5) "danger" else if (risk >= 0.2) "warning" else "success"
    word <- c(danger = "At risk", warning = "Borderline", success = "On track")[[band]]
    div(class = paste("grade-card", band),
      div(class = "grade",
        span(class = "num", sprintf("%.1f", shown)), span(class = "out", "/ 20"),
        div(class = "range", sprintf("likely %.0f to %.0f", lo, hi))
      ),
      div(class = "verdict",
        span(class = paste0("badge text-bg-", band), word),
        p(sprintf("%s chance of finishing below a pass (%d).",
                  if (risk < 0.01) "Under a 1%" else if (risk > 0.99) "Over a 99%" else sprintf("About a %d%%", round(100 * risk)),
                  PASS)),
        p(class = "hint", sprintf("%s. Typical miss on unseen students: %.1f points.",
                                  names(MOMENT_CHOICES)[MOMENT_CHOICES == input$moment], m$cv_rmse))
      )
    )
  })

  output$drivers <- renderUI({
    m <- model()
    contrib <- contributions(m, student())[1, ]
    src <- vapply(names(contrib), function(t) term_source(m, t)$var, "")
    by_var <- tapply(contrib, src, sum)
    by_var <- by_var[abs(by_var) > 0.05]
    if (!length(by_var)) return(p(class = "hint", "Nothing stands out: this looks like an average student."))
    top <- by_var[order(-abs(by_var))][seq_len(min(8, length(by_var)))]
    scale <- max(abs(top), 1)
    tags$div(class = "bars", lapply(names(top), function(v) {
      w <- 50 * abs(top[[v]]) / scale
      div(class = "bar-row",
        span(class = "bar-label", LABELS[[v]] %||% v),
        div(class = "bar-track",
          div(class = paste("bar", if (top[[v]] > 0) "up" else "down"),
              style = sprintf("width:%.1f%%;%s", w, if (top[[v]] > 0) "left:50%" else sprintf("left:%.1f%%", 50 - w)))
        ),
        span(class = "bar-value", sprintf("%+.1f", top[[v]]))
      )
    }))
  })

  output$track_record <- renderUI({
    t <- models$meta$test
    rows <- lapply(MOMENT_CHOICES, function(mo) {
      base <- t[t$variant == mo & t$model == "baseline", ]
      net <- t[t$variant == paste0(mo, "_nosensitive") & t$model == "elastic_net", ]
      tags$tr(class = if (mo == input$moment) "current",
        tags$td(names(MOMENT_CHOICES)[MOMENT_CHOICES == mo]),
        tags$td(sprintf("%.2f", base$rmse)),
        tags$td(sprintf("%.2f", net$rmse)),
        tags$td(sprintf("%.0f%%", 100 * net$at_risk_recall))
      )
    })
    tagList(
      tags$table(class = "table table-sm record",
        tags$thead(tags$tr(tags$th("When"), tags$th("Baseline miss"), tags$th("This model's miss"),
                           tags$th("At-risk students caught"))),
        tags$tbody(rows)
      ),
      p(class = "hint", sprintf("Typical miss is RMSE in grade points on %d held-out rows. The baseline guesses the class average at the start of the year, then repeats the latest term grade.",
                                models$meta$test_rows))
    )
  })

  output$absent_note <- renderUI({
    a <- models$meta$absent
    HTML(sprintf("%d of the 1,044 rows (%.0f%%) have a final grade of 0 with no absences: students who left or missed the exam, not students who scored zero. The model predicts grades for students who sit the final, so it can't warn about dropping out.",
                 a$n, 100 * a$share))
  })
}

shinyApp(ui, server)
