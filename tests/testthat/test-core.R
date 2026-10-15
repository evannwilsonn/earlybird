root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
withr::local_dir(root)
source("R/data.R")
source("R/ols.R")
source("app/predict.R")

test_that("both subjects load with the documented row counts", {
  d <- load_students()
  expect_equal(sum(d$subject == "math"), 395)
  expect_equal(sum(d$subject == "portuguese"), 649)
  expect_false(anyNA(d[c("G1", "G2", "G3")]))
})

test_that("every final grade of 0 is flagged as a missed final", {
  d <- load_students()
  expect_equal(sum(d$no_final), sum(d$G3 == 0))
  expect_true(all(d$absences[d$no_final] == 0))
})

test_that("a student never sits on both sides of the split", {
  d <- load_students()
  s <- split_by_student(d)
  expect_length(intersect(s$train$student_id, s$test$student_id), 0)
  expect_equal(nrow(s$train) + nrow(s$test), nrow(d))
})

test_that("features never include grades from later in the year", {
  expect_false(any(c("G1", "G2", "G3") %in% features_for("start")))
  expect_equal(intersect(c("G1", "G2", "G3"), features_for("term1")), "G1")
  expect_equal(intersect(c("G1", "G2", "G3"), features_for("term2")), c("G1", "G2"))
  expect_false(any(SENSITIVE %in% features_for("start", drop_sensitive = TRUE)))
})

test_that("hand-written OLS matches lm()", {
  set.seed(1)
  x <- matrix(rnorm(300), 100, 3, dimnames = list(NULL, c("a", "b", "c")))
  y <- drop(2 + x %*% c(1, -0.5, 3) + rnorm(100, sd = 0.1))
  mine <- ols_fit(x, y)
  theirs <- coef(lm(y ~ x))
  expect_equal(unname(mine$coef), unname(theirs), tolerance = 1e-10)
  expect_equal(unname(predict(mine, x)), unname(fitted(lm(y ~ x))), tolerance = 1e-10)
})

test_that("hand-written OLS drops a duplicated column instead of failing", {
  set.seed(2)
  x <- matrix(rnorm(100), 50, 2, dimnames = list(NULL, c("a", "b")))
  x <- cbind(x, a_again = x[, "a"])
  fit <- ols_fit(x, rnorm(50))
  expect_length(fit$dropped, 1)
  expect_length(predict(fit, x), 50)
})

test_that("exported model contributions add up to the prediction", {
  m <- list(
    levels = list(subject = c("math", "portuguese")),
    intercept = 11,
    coef = c(studytime = 0.5, subject_portuguese = 0.8),
    center = c(studytime = 2, subject_portuguese = 0.6),
    scale = c(studytime = 0.8, subject_portuguese = 0.5)
  )
  nd <- data.frame(studytime = c(1, 4), subject = c("math", "portuguese"))
  p <- predict_exported(m, nd)
  manual <- 11 + 0.5 * (c(1, 4) - 2) / 0.8 + 0.8 * (c(0, 1) - 0.6) / 0.5
  expect_equal(p, manual)
  expect_equal(unname(rowSums(contributions(m, nd))) + 11, p)
})

test_that("the shipped app models exist for every moment and predict on real rows", {
  skip_if_not(file.exists("app/model.rds"), "run analysis/run.R first")
  models <- readRDS("app/model.rds")
  d <- load_students()
  for (moment in MOMENTS) {
    p <- predict_exported(models[[moment]], d[1:20, ])
    expect_true(all(is.finite(p)))
    expect_true(all(p > -5 & p < 25))
  }
})
