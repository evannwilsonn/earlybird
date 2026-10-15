# The whole loop: data -> baseline -> models -> evaluation -> improvement -> export.
# Run from the repo root:  Rscript analysis/run.R
# Writes results/metrics.json, results/cv.csv, results/test.csv and app/model.rds.

suppressPackageStartupMessages({
  library(tidymodels)
  library(jsonlite)
})
tidymodels_prefer()
source("R/data.R")
source("R/ols.R")
source("R/export.R")

dir.create("results", showWarnings = FALSE)
set.seed(2026)

students <- load_students()
split <- split_by_student(students)
# The regression is about students who sat the final. The ones who didn't are
# reported separately (they are 5% of rows, and a grade of 0 there means "absent").
train <- filter(split$train, !no_final)
test <- filter(split$test, !no_final)
folds <- group_vfold_cv(train, group = student_id, v = 10)

cat(sprintf("train %d rows (%d students), test %d rows (%d students)\n",
            nrow(train), n_distinct(train$student_id), nrow(test), n_distinct(test$student_id)))

# ---- Recipes -------------------------------------------------------------------

base_recipe <- function(moment, drop_sensitive = FALSE, engineered = FALSE) {
  f <- features_for(moment, drop_sensitive)
  rec <- recipe(train[c(f, "G3")]) |>
    update_role(all_of(f), new_role = "predictor") |>
    update_role(G3, new_role = "outcome")
  if (engineered) rec <- rec |> step_mutate(!!!engineered_steps(f))
  rec |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
}

# ---- Models --------------------------------------------------------------------

specs <- list(
  linear = linear_reg() |> set_engine("lm"),
  elastic_net = linear_reg(penalty = tune(), mixture = tune()) |> set_engine("glmnet"),
  random_forest = rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
    set_engine("ranger") |> set_mode("regression"),
  boosted_trees = boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune(),
                             min_n = tune(), sample_size = 0.8) |>
    set_engine("xgboost") |> set_mode("regression")
)
grids <- list(
  elastic_net = grid_regular(penalty(c(-3, 0)), mixture(), levels = c(12, 5)),
  random_forest = grid_regular(mtry(c(3, 25)), min_n(c(2, 20)), levels = 4),
  boosted_trees = grid_space_filling(trees(c(100, 800)), tree_depth(c(2, 6)),
                                     learn_rate(c(-2.5, -1)), min_n(c(2, 20)), size = 20)
)

metric_set_used <- metric_set(rmse, mae, rsq)

cv_one <- function(rec, name) {
  wf <- workflow(rec, specs[[name]])
  if (name %in% names(grids)) {
    res <- tune_grid(wf, folds, grid = grids[[name]], metrics = metric_set_used)
    best <- select_best(res, metric = "rmse")
    m <- collect_metrics(res) |> filter(.config == best$.config)
    list(wf = finalize_workflow(wf, best), metrics = m, params = best)
  } else {
    res <- fit_resamples(wf, folds, metrics = metric_set_used)
    list(wf = wf, metrics = collect_metrics(res), params = NULL)
  }
}

# Baselines the models have to beat. At the start of the year: the training mean.
# Once a period grade exists: carry the latest grade forward unchanged.
baseline_pred <- function(d, moment) {
  switch(moment, start = rep(mean(train$G3), nrow(d)), term1 = d$G1, term2 = d$G2)
}
baseline_cv <- function(moment) {
  r <- map_dbl(folds$splits, function(s) {
    a <- analysis(s); b <- assessment(s)
    p <- switch(moment, start = rep(mean(a$G3), nrow(b)), term1 = b$G1, term2 = b$G2)
    rmse_vec(b$G3, p)
  })
  tibble(.metric = "rmse", mean = mean(r), std_err = sd(r) / sqrt(length(r)))
}

# The hand-written OLS, cross-validated on exactly the same folds and features as
# the lm() workflow, so the two rows of the table should match.
scratch_ols_cv <- function(rec) {
  r <- map_dbl(folds$splits, function(s) {
    p <- prep(rec, analysis(s))
    a <- bake(p, NULL); b <- bake(p, assessment(s))
    x <- as.matrix(select(a, -G3)); fit <- ols_fit(x, a$G3)
    rmse_vec(b$G3, predict(fit, as.matrix(select(b, -G3))))
  })
  tibble(.metric = "rmse", mean = mean(r), std_err = sd(r) / sqrt(length(r)))
}

# ---- Cross-validated comparison -------------------------------------------------

rows <- list(); finals <- list()
for (moment in MOMENTS) {
  for (sens in c(FALSE, TRUE)) {
    for (eng in c(FALSE, TRUE)) {
      variant <- paste0(moment, if (sens) "_nosensitive" else "", if (eng) "_engineered" else "")
      cat("\n==", variant, "==\n")
      rec <- base_recipe(moment, sens, eng)
      add <- function(model, m) {
        r <- filter(m, .metric == "rmse")
        cat(sprintf("  %-14s RMSE %.3f (± %.3f)\n", model, r$mean, r$std_err))
        rows[[length(rows) + 1]] <<- tibble(moment, drop_sensitive = sens, engineered = eng,
                                            model, rmse = r$mean, rmse_se = r$std_err)
      }
      if (!eng && !sens) add("baseline", baseline_cv(moment))
      if (!eng && !sens) add("ols_scratch", scratch_ols_cv(rec))
      for (name in names(specs)) {
        if ((eng || sens) && name != "elastic_net") next  # improvements are tried on the linear model
        res <- cv_one(rec, name)
        add(name, res$metrics)
        finals[[paste(variant, name)]] <- res
      }
    }
  }
}
cv <- bind_rows(rows)
write_csv(cv, "results/cv.csv")

# ---- One look at the test set ----------------------------------------------------
# Every CV'd configuration is refit on the whole training set and scored once on the
# held-out students. Nothing below changes a model after seeing these numbers.

score <- function(truth, pred, g3) {
  at_risk <- g3 < 10; flagged <- pred < 10
  tibble(rmse = rmse_vec(truth, pred), mae = mae_vec(truth, pred), rsq = rsq_vec(truth, pred),
         at_risk_recall = sum(flagged & at_risk) / sum(at_risk),
         at_risk_precision = if (sum(flagged)) sum(flagged & at_risk) / sum(flagged) else NA)
}
test_rows <- list(); fitted <- list()
for (key in names(finals)) {
  parts <- strsplit(key, " ")[[1]]
  fit <- fit(finals[[key]]$wf, train)
  fitted[[key]] <- fit
  p <- predict(fit, test)$.pred
  test_rows[[key]] <- bind_cols(tibble(variant = parts[1], model = parts[2]), score(test$G3, p, test$G3))
}
for (moment in MOMENTS) {
  test_rows[[paste(moment, "baseline")]] <- bind_cols(
    tibble(variant = moment, model = "baseline"), score(test$G3, baseline_pred(test, moment), test$G3))
}
test_scores <- bind_rows(test_rows) |> arrange(variant, rmse)
write_csv(test_scores, "results/test.csv")
print(test_scores, n = 50)

# ---- Checks ---------------------------------------------------------------------

# The hand-written OLS reproduces lm() on the full training set.
rec <- prep(base_recipe("start"), train)
baked <- bake(rec, NULL)
mine <- ols_fit(as.matrix(select(baked, -G3)), baked$G3)
theirs <- coef(lm(G3 ~ ., data = baked))
names(theirs) <- gsub("`", "", names(theirs))
ols_check <- max(abs(mine$coef - theirs[names(mine$coef)]), na.rm = TRUE)
cat(sprintf("\nmax |coef difference| hand-written OLS vs lm(): %.2e\n", ols_check))

# Students who didn't sit the final: how many, and what their period grades were.
absent <- students |> filter(no_final) |>
  summarise(n = n(), share = n() / nrow(students), mean_G1 = mean(G1), mean_G2 = mean(G2))

# ---- Export the app's models -------------------------------------------------------
# The app ships the elastic net for each moment: a linear model whose prediction can
# be explained term by term. It uses the version without the sensitive attributes,
# because cross-validation shows dropping them costs nothing. Exported as plain
# numbers, so the app needs only shiny.

app_models <- list()
for (moment in MOMENTS) {
  key <- paste0(moment, "_nosensitive elastic_net")
  wf <- fitted[[key]]
  cv_rmse <- filter(cv, moment == !!moment, drop_sensitive, !engineered, model == "elastic_net")$rmse
  app_models[[moment]] <- export_linear(wf, train, features_for(moment, drop_sensitive = TRUE),
                                        cv_rmse = cv_rmse)
  # The plain-R version must give the same predictions as the tidymodels workflow.
  diff <- max(abs(predict_exported(app_models[[moment]], test) - predict(wf, test)$.pred))
  cat(sprintf("%s: exported model matches workflow to %.1e\n", moment, diff))
  stopifnot(diff < 1e-8)
}
app_models$meta <- list(trained_on = nrow(train), test_rows = nrow(test),
                        built = format(Sys.Date()), absent = absent,
                        test = filter(test_scores, variant %in% c(MOMENTS, paste0(MOMENTS, "_nosensitive"))))
dir.create("app", showWarnings = FALSE)
saveRDS(app_models, "app/model.rds")
# The held-out students ship with the app so a visitor can load a real student the
# model never saw and compare its prediction with the grade they actually got.
write_csv(select(split$test, all_of(features_for("term2")), G3, no_final), "app/heldout.csv")

write_json(list(cv = cv, test = test_scores, ols_vs_lm = ols_check, absent = absent,
                rows = list(train = nrow(train), test = nrow(test)),
                students = list(total = n_distinct(students$student_id))),
           "results/metrics.json", auto_unbox = TRUE, digits = 6, pretty = TRUE)
cat("\nwrote results/ and app/model.rds\n")
