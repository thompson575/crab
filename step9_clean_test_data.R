# --------------------------------------------------------------
# Clean the data files for the kaggle playground problem on
# predicting crab age
#
library(tidyverse)
library(fs)
library(fileArchive)
library(broom)

cache   <- "C:/Projects/Kaggle/Playground/crab/data/cache"
archive <- "C:/Projects/Kaggle/Playground/crab/data/repos"

readRDS( path(archive, "train.rds")) |>
  as_tibble() -> trainDF

readRDS( path(archive, "test.rds")) |>
  as_tibble() -> testDF

# --------------------------------------------------
# clean height
#
trainDF |>
  lm( height ~ sex + diameter + length + weight +
        shuckedWeight + visceraWeight + shellWeight,
      data = _) -> model


set.seed(8723)
model |>
  augment() |>
  mutate( id = trainDF$id) |>
  filter( abs(.std.resid) > 3 | height == 0) |>
  mutate( yhat = .fitted + rnorm(466, 0, 0.033)) |>
  select( id, yhat) |>
  right_join(trainDF, by = "id") |>
  mutate( height = ifelse(is.na(yhat), height, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=testDF) |>
  mutate( id = testDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3 | height == 0) |>
  mutate( yhat = .fitted + rnorm(264, 0, 0.033)) |>
  select( id, yhat) |>
  right_join(testDF, by = "id") |>
  mutate( height = ifelse(is.na(yhat), height, yhat)) |>
  select(-yhat)  -> cleanTestDF


# --------------------------------------------------
# clean diameter
#
cleanDF |>
  lm( diameter ~ sex + height + length + weight +
        shuckedWeight + visceraWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(849, 0, 0.033)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( diameter = ifelse(is.na(yhat), diameter, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(582, 0, 0.033)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( diameter = ifelse(is.na(yhat), diameter, yhat)) |>
  select(-yhat)  -> cleanTestDF
# --------------------------------------------------
# clean length
#
cleanDF |>
  lm( length ~ sex + height + diameter + weight +
        shuckedWeight + visceraWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(471, 0, 0.037)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( length = ifelse(is.na(yhat), length, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(343, 0, 0.037)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( length = ifelse(is.na(yhat), length, yhat)) |>
  select(-yhat)  -> cleanTestDF

# --------------------------------------------------
# clean weight
#
cleanDF |>
  lm( weight ~ sex + height + diameter + length +
        shuckedWeight + visceraWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(1143, 0, 1.44)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( weight = ifelse(is.na(yhat), weight, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(767, 0, 1.44)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( weight = ifelse(is.na(yhat), weight, yhat)) |>
  select(-yhat)  -> cleanTestDF

# --------------------------------------------------
# clean shuckedWeight
#
cleanDF |>
  lm( shuckedWeight ~ sex + height + diameter + length +
        weight + visceraWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(936, 0, 1.06)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( shuckedWeight = ifelse(is.na(yhat), shuckedWeight, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(635, 0, 1.06)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( shuckedWeight = ifelse(is.na(yhat), shuckedWeight, yhat)) |>
  select(-yhat)  -> cleanTestDF

# --------------------------------------------------
# clean visceraWeight
#
cleanDF |>
  lm( visceraWeight ~ sex + height + diameter + length +
        weight + shuckedWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(977, 0, 0.64)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( visceraWeight = ifelse(is.na(yhat), visceraWeight, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(685, 0, 0.64)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( visceraWeight = ifelse(is.na(yhat), visceraWeight, yhat)) |>
  select(-yhat)  -> cleanTestDF

# --------------------------------------------------
# clean shellWeight
#
cleanDF |>
  lm( shellWeight ~ sex + height + diameter + length +
        weight + shuckedWeight + visceraWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = cleanDF$id) |>
  filter( abs(.std.resid) > 3 ) |>
  mutate( yhat = .fitted + rnorm(979, 0, 0.72)) |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( shellWeight = ifelse(is.na(yhat), shellWeight, yhat)) |>
  select(-yhat)  -> cleanDF

model |>
  augment(newdata=cleanTestDF) |>
  mutate( id = cleanTestDF$id) |>
  filter( abs(.resid) / sd(.resid) > 3) |>
  mutate( yhat = .fitted + rnorm(636, 0, 0.72)) |>
  select( id, yhat) |>
  right_join(cleanTestDF, by = "id") |>
  mutate( shellWeight = ifelse(is.na(yhat), shellWeight, yhat)) |>
  select(-yhat)  -> cleanTestDF

saveRDS(cleanTestDF, path(cache, "clean_test.rds"))

copyToArchive(archive, path(cache, "clean_test.rds"),
              name="clean_test",
              tag="crab cleaned test data",
              replace=TRUE)
