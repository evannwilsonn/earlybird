# Ordinary least squares written out by hand, to check against lm() and glmnet.
#
# The model is y = X b + e. Minimising the squared error ||y - X b||^2 gives the
# normal equations X'X b = X'y. Solving them through the QR decomposition of X
# (X = QR, so R b = Q'y) avoids forming X'X, which squares the condition number and
# loses precision when features are correlated (G1 and G2 are, at r = 0.9).

#' Fit OLS. `x` is a numeric matrix without an intercept column; one is added.
ols_fit <- function(x, y) {
  stopifnot(is.matrix(x), is.numeric(y), nrow(x) == length(y))
  X <- cbind(`(Intercept)` = 1, x)
  qx <- qr(X)
  if (qx$rank < ncol(X)) {
    # Collinear columns (a dummy that is always 0 in this sample, say) have no unique
    # coefficient. Drop them, as lm() does, and report which.
    keep <- qx$pivot[seq_len(qx$rank)]
    dropped <- colnames(X)[-keep]
    X <- X[, sort(keep), drop = FALSE]
    qx <- qr(X)
  } else {
    dropped <- character()
  }
  b <- backsolve(qr.R(qx), crossprod(qr.Q(qx), y))[, 1]
  names(b) <- colnames(X)
  fitted <- drop(X %*% b)
  structure(list(coef = b, dropped = dropped, residuals = y - fitted), class = "ols")
}

predict.ols <- function(object, newdata, ...) {
  X <- cbind(`(Intercept)` = 1, newdata)
  drop(X[, names(object$coef), drop = FALSE] %*% object$coef)
}
