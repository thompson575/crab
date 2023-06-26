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

# --- prepare 10 folds
set.seed(6782)
fold <- sample(c(rep(1:10, 7405), 0))

tibble(
  in_xgb = rep(0, 10),
  out_xgb = rep(0, 10),
  in_mrs = rep(0, 10),
  out_mrs = rep(0, 10)
) -> maeDF

for( i in 1:10 ) {
   estimateDF <- cleanDF[fold!=i, ]
   validateDF <- cleanDF[fold==i, ]

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
             nrounds = 1000, eta = 0.1, max_depth = 4,
             verbose = 2, print_every_n = 50) -> xgb

   estimateDF |>
     mutate(yhat = predict(xgb, newdata=X)) |>
     summarise( mae = mean(abs(age-yhat))) |>
     pull(mae) -> maeDF$in_xgb[i]

   validateDF |>
     mutate(yhat = predict(xgb, newdata=XV)) |>
     summarise( mae = mean(abs(age-yhat))) |>
     pull(mae) -> maeDF$out_xgb[i]

   # --- fit the mars model -----------------------------
   earth(
     age ~ sexI + sexF + height + diameter + length +
       weight + shuckedWeight + visceraWeight + shellWeight,
     data = estimateDF,
     degree = 3
   ) -> mrs

   estimateDF |>
     mutate(yhat = predict(mrs)) |>
     summarise( mae = mean(abs(age-yhat))) |>
     pull(mae) -> maeDF$in_mrs[i]

   validateDF |>
     mutate(yhat = predict(mrs, newdata=validateDF)) |>
     summarise( mae = mean(abs(age-yhat))) |>
     pull(mae) -> maeDF$out_mrs[i]
   }

saveRDS(maeDF , path(cache, "cv_mae.rds"))

copyToArchive(archive, path(cache, "cv_mae.rds"),
              name="cv_mae",
              tag="crab cross-validation of xgb and mars",
              replace=TRUE)

