# Feature engineering (the "improvement" step) and the export that turns a fitted
# tidymodels workflow into plain numbers the Shiny app can use without tidymodels.

source("app/predict.R")

#' Extra features built from existing columns, as expressions for step_mutate().
engineered_steps <- function(f) {
  e <- list()
  if (all(c("Dalc", "Walc") %in% f)) e$alcohol <- rlang::expr(Dalc + Walc)
  if (all(c("Medu", "Fedu") %in% f)) e$parent_edu <- rlang::expr(pmax(Medu, Fedu))
  if ("absences" %in% f) e$absences_log <- rlang::expr(log1p(absences))
  if ("failures" %in% f) e$any_failure <- rlang::expr(as.numeric(failures > 0))
  e
}

#' A fitted workflow (dummy -> zero-variance filter -> normalize -> glmnet) as a list:
#' the levels used for each categorical column, the mean and sd used to scale each
#' column, and one coefficient per scaled column.
export_linear <- function(wf, train, features, cv_rmse) {
  rec <- workflows::extract_recipe(wf)
  steps <- vapply(rec$steps, function(s) class(s)[1], "")
  norm <- recipes::tidy(rec, number = which(steps == "step_normalize"))
  stats <- tidyr::pivot_wider(norm[c("terms", "statistic", "value")],
                              names_from = statistic, values_from = value)
  coefs <- generics::tidy(workflows::extract_fit_parsnip(wf))
  coefs <- coefs[coefs$estimate != 0 | coefs$term == "(Intercept)", ]

  nominal <- features[vapply(train[features], function(x) is.character(x) || is.factor(x), TRUE)]
  levels <- lapply(setNames(nominal, nominal), function(v) sort(unique(as.character(train[[v]]))))
  numeric <- setdiff(features, nominal)
  ranges <- lapply(setNames(numeric, numeric), function(v) range(train[[v]]))

  terms <- setdiff(coefs$term, "(Intercept)")
  list(
    features = features,
    levels = levels,
    ranges = ranges,
    intercept = coefs$estimate[coefs$term == "(Intercept)"],
    coef = setNames(coefs$estimate[match(terms, coefs$term)], terms),
    center = setNames(stats$mean[match(terms, stats$terms)], terms),
    scale = setNames(stats$sd[match(terms, stats$terms)], terms),
    cv_rmse = cv_rmse,
    penalty = workflows::extract_spec_parsnip(wf)$args$penalty |> rlang::eval_tidy(),
    mixture = workflows::extract_spec_parsnip(wf)$args$mixture |> rlang::eval_tidy()
  )
}
