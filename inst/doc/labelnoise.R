## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
has_tidymodels <- all(vapply(
  c("tune", "parsnip", "rsample", "workflows", "nnet"),
  requireNamespace, logical(1), quietly = TRUE
))

## -----------------------------------------------------------------------------
library(labelnoise)

## -----------------------------------------------------------------------------
sim <- simulate_label_noise(n = 1000, noise_rate = 0.1, signal = 3, seed = 1)
sim

## -----------------------------------------------------------------------------
issues <- find_label_issues(sim, truth = given)
issues

## -----------------------------------------------------------------------------
flagged <- issues[issues$is_label_issue, ]
head(flagged[order(flagged$label_quality), ])

## -----------------------------------------------------------------------------
table(flagged = issues$is_label_issue, mislabelled = sim$given != sim$true)

## ----eval = has_tidymodels, message = FALSE-----------------------------------
library(parsnip)
library(workflows)

iris_noisy <- iris
flipped <- c(5, 30, 60, 80, 110, 140)
iris_noisy$Species[flipped] <- c(
  "versicolor", "virginica", "setosa", "virginica", "setosa", "versicolor"
)

wf <- workflow(Species ~ ., multinom_reg() |> set_engine("nnet"))
set.seed(1)
folds <- rsample::vfold_cv(iris_noisy, v = 5)
res <- tune::fit_resamples(
  wf, folds,
  control = tune::control_resamples(save_pred = TRUE)
)

issues <- find_label_issues(res)
which(issues$is_label_issue)

## -----------------------------------------------------------------------------
health_summary(sim, truth = given)

## ----eval = requireNamespace("nnet", quietly = TRUE)--------------------------
set.seed(1)
fold <- sample(rep(1:5, length.out = nrow(iris)))
probs <- matrix(NA_real_, nrow(iris), 3, dimnames = list(NULL, levels(iris$Species)))
for (k in 1:5) {
  fit <- nnet::multinom(Species ~ ., data = iris[fold != k, ], trace = FALSE)
  probs[fold == k, ] <- predict(fit, iris[fold == k, ], type = "probs")
}
num_label_issues(iris$Species, pred_probs = probs)

## ----eval = FALSE-------------------------------------------------------------
# library(tidymodels)
# library(textrecipes)
# 
# rec <- recipe(code ~ description, data = survey) |>
#   step_tokenize(description) |>
#   step_tokenfilter(description, max_tokens = 5000) |>
#   step_tfidf(description)
# wf <- workflow(rec, multinom_reg(penalty = 0.01) |> set_engine("glmnet"))
# 
# folds <- vfold_cv(survey, v = 5, strata = code)
# res <- fit_resamples(wf, folds, control = control_resamples(save_pred = TRUE))
# 
# review <- survey |>
#   bind_cols(find_label_issues(res) |> select(-given)) |>
#   filter(is_label_issue) |>
#   arrange(label_quality)
# 
# health_summary(res)$overlapping_classes

