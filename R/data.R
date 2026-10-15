# Loading and preparing the UCI Student Performance data.
#
# Two files, one per subject: math (395 students) and Portuguese (649). Many
# students appear in both. The dataset doesn't give them an id; its own merge script
# matches them on 13 background columns, and so do we, so one student never lands in
# both the training and the test set.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Columns the dataset authors use to recognise the same student across both files.
STUDENT_KEY <- c(
  "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu",
  "Mjob", "Fjob", "reason", "nursery", "internet"
)

# What a teacher knows at each point in the year. Grades come in three terms:
# G1 and G2 are the first two period grades, G3 is the final grade we predict.
MOMENTS <- c("start", "term1", "term2")
MOMENT_LABELS <- c(
  start = "Start of year (no grades yet)",
  term1 = "After term 1 (G1 known)",
  term2 = "After term 2 (G1 and G2 known)"
)

# Attributes a school might reasonably decide not to use for a prediction about a
# student. Evaluated with and without them in analysis/run.R.
SENSITIVE <- c("sex", "age", "address", "Pstatus", "romantic")

read_subject <- function(path, subject) {
  read_delim(path, delim = ";", show_col_types = FALSE) |>
    mutate(subject = subject, G1 = as.numeric(G1), G2 = as.numeric(G2), G3 = as.numeric(G3))
}

#' Both subjects in one table, with a student id shared across subjects and a flag
#' for students who did not sit the final exam.
load_students <- function(dir = "data") {
  d <- bind_rows(
    read_subject(file.path(dir, "student-mat.csv"), "math"),
    read_subject(file.path(dir, "student-por.csv"), "portuguese")
  )
  d$student_id <- as.integer(factor(do.call(paste, c(d[STUDENT_KEY], sep = "|"))))
  # Every final grade of 0 comes with 0 absences, and most have a non-zero G2: these
  # are students who left or missed the exam, not students who scored zero on it.
  d$no_final <- d$G3 == 0
  d
}

#' The features available at a given moment (everything except later grades).
features_for <- function(moment, drop_sensitive = FALSE) {
  background <- c(
    "subject", "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu",
    "Mjob", "Fjob", "reason", "guardian", "traveltime", "studytime", "failures",
    "schoolsup", "famsup", "paid", "activities", "nursery", "higher", "internet",
    "romantic", "famrel", "freetime", "goout", "Dalc", "Walc", "health", "absences"
  )
  grades <- switch(moment, start = character(), term1 = "G1", term2 = c("G1", "G2"),
                   stop("unknown moment: ", moment))
  f <- c(background, grades)
  if (drop_sensitive) f <- setdiff(f, SENSITIVE)
  f
}

#' Split students (not rows) into train and test, so a student taking both subjects
#' sits entirely on one side.
split_by_student <- function(d, test_share = 0.2, seed = 2026) {
  set.seed(seed)
  ids <- unique(d$student_id)
  test_ids <- sample(ids, round(length(ids) * test_share))
  list(train = d[!d$student_id %in% test_ids, ], test = d[d$student_id %in% test_ids, ])
}
