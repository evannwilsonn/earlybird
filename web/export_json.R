# Writes web/model.json: the three exported models, the held-out students and the
# test scores, for the single-page version in web/index.html. Run from the repo root
# after analysis/run.R:  Rscript web/export_json.R
library(jsonlite)
m <- readRDS("app/model.rds")
out <- lapply(c(start = "start", term1 = "term1", term2 = "term2"), function(k) {
  x <- m[[k]]
  list(features = x$features, levels = x$levels, intercept = x$intercept,
       coef = as.list(x$coef), center = as.list(x$center), scale = as.list(x$scale), cv_rmse = x$cv_rmse)
})
h <- read.csv("app/heldout.csv", stringsAsFactors = FALSE)
fields <- c("subject","school","famsize","Medu","Fedu","Mjob","Fjob","reason","guardian","traveltime",
            "studytime","failures","schoolsup","famsup","paid","activities","nursery","higher","internet",
            "famrel","freetime","goout","Dalc","Walc","health","absences","G1","G2","G3","no_final")
out$heldout <- h[fields]
out$meta <- list(test_rows = m$meta$test_rows, absent = m$meta$absent,
                 test = m$meta$test[c("variant", "model", "rmse", "at_risk_recall")])
write_json(out, "web/model.json", auto_unbox = TRUE, digits = 10, dataframe = "rows")
