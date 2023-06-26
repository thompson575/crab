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

# --- convert the factors to numbers ----------------------
cleanDF |>
  mutate( sexI = as.numeric(sex=="I"),
          sexM = as.numeric(sex=="M"),
          sexF = as.numeric(sex=="F")) |>
  select(-sex) -> cleanDF

# --- split the data --------------------------------------
set.seed(7818)
split <- sample(1:74051, size=10000, replace=FALSE)

estimateDF <- cleanDF[-split, ]
validateDF <- cleanDF[ split, ]

# --- save to archive --------------------------------------
saveRDS(estimateDF, path(cache, "estimate.rds"))
saveRDS(validateDF, path(cache, "validate.rds"))

copyToArchive(archive, path(cache, "estimate.rds"),
              name="estimate",
              tag="crab estimation data",
              replace=TRUE)

copyToArchive(archive, path(cache, "validate.rds"),
              name="validate",
              tag="crab validation data",
              replace=TRUE)

# --- set the estimation data ----------------------------
estimateDF |>
  select( -id, -age ) |>
  as.matrix() -> X

estimateDF |>
  pull(age) -> Y

dtrain <- xgb.DMatrix(data = X, label = Y)

# --- set the validation data ---------------------------
validateDF |>
  select( -id, -age ) |>
  as.matrix() -> XV

validateDF |>
  pull(age) -> YV

dtest <- xgb.DMatrix(data = XV, label=YV)

# --- fit the xgboost model -----------------------------
xgb.train(data = dtrain,
          watchlist = list(train=dtrain, test = dtest),
          objective = "reg:absoluteerror",
          eval_metric = "mae",
          nrounds = 2000, eta = 0.1, max_depth = 4,
          verbose = 2, print_every_n = 50) |>
  saveRDS( path(cache, "xgboost.rds"))

copyToArchive(archive, path(cache, "xgboost.rds"),
              name="xgboost",
              tag="crab xgboost model",
              replace=TRUE)


