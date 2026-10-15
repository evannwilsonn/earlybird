# Prediction with an exported linear model, in base R only (the app ships this file
# and model.rds, nothing else). Mirrors the training recipe: a 0/1 column per
# non-reference category, every column centred and scaled, then a weighted sum.

# Which input column, and which category of it, a model term refers to.
term_source <- function(model, term) {
  for (v in names(model$levels)) {
    for (lvl in model$levels[[v]]) {
      if (term %in% c(paste0(v, "_", lvl), paste0(v, "_", make.names(lvl)))) {
        return(list(var = v, level = lvl))
      }
    }
  }
  list(var = term, level = NULL)
}

# The raw (unscaled) value of each model term for every row of `newdata`.
term_values <- function(model, newdata) {
  vals <- lapply(names(model$coef), function(t) {
    s <- term_source(model, t)
    x <- newdata[[s$var]]
    if (is.null(s$level)) as.numeric(x) else as.numeric(as.character(x) == s$level)
  })
  matrix(unlist(vals), nrow = nrow(newdata), dimnames = list(NULL, names(model$coef)))
}

# Each term's share of the prediction, relative to an average student: positive
# pushes the predicted grade up. These sum, with the intercept, to the prediction.
contributions <- function(model, newdata) {
  z <- sweep(sweep(term_values(model, newdata), 2, model$center), 2, model$scale, "/")
  sweep(z, 2, model$coef, "*")
}

predict_exported <- function(model, newdata) {
  drop(model$intercept + rowSums(contributions(model, newdata)))
}
