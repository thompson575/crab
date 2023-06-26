# --------------------------------------------------------------
# XGBoost for the clean data files for the kaggle playground problem on
# predicting crab age
#
library(tidyverse)
library(fs)
library(fileArchive)
library(xgboost)

cache   <- "C:/Projects/Kaggle/Playground/crab/data/cache"
archive <- "C:/Projects/Kaggle/Playground/crab/data/repos"

readRDS( path(archive, "clean.rds")) |>
  as_tibble() -> cleanDF

readRDS( path(archive, "clean_test.rds")) |>
  as_tibble() -> cleanTestDF

# --- convert the factors to numbers ----------------------
cleanDF |>
  mutate( sexI = as.numeric(sex=="I"),
          sexM = as.numeric(sex=="M"),
          sexF = as.numeric(sex=="F")) |>
  select(-sex) -> cleanDF

cleanTestDF |>
  mutate( sexI = as.numeric(sex=="I"),
          sexM = as.numeric(sex=="M"),
          sexF = as.numeric(sex=="F")) |>
  select(-sex) -> cleanTestDF

# --- set the estimation data ----------------------------
cleanDF |>
  select( -id, -age ) |>
  as.matrix() -> X

cleanDF |>
  pull(age) -> Y

dtrain <- xgb.DMatrix(data = X, label = Y)

# --- set the validation data ---------------------------
cleanTestDF |>
  select( -id ) |>
  as.matrix() -> XV


# --- fit the xgboost model -----------------------------
xgb.train(data = dtrain,
          objective = "reg:absoluteerror",
          eval_metric = "mae",
          nrounds = 1000, eta = 0.1, max_depth = 4,
          verbose = 2, print_every_n = 50) -> xgb

cleanTestDF |>
  mutate( Age = round(predict(xgb, newdata=XV),2)) |>
  select(id, Age) |>
  arrange(id) |>
  write.csv(file=path(cache, "submission.csv"), quote=FALSE,
                      row.names=FALSE)

copyToArchive(archive, path(cache, "submission.csv"),
              name="submission",
              tag="crab initial submission based on xgboost",
              replace=TRUE)
