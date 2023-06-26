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

# --------------------------------------------------
# clean height
#
trainDF |>
  lm( height ~ sex + diameter + length + weight +
        shuckedWeight + visceraWeight + shellWeight,
      data = _) -> model

model |>
  glance()

set.seed(8723)
model |>
  augment() |>
  mutate( id = trainDF$id) |>
  filter( abs(.std.resid) > 3 | height == 0) |>
  mutate( yhat = .fitted + rnorm(466, 0, 0.033)) |>
  select( id, height, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(trainDF, by = "id") |>
  mutate( height = ifelse(is.na(yhat), height, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF


cleanDF |>
  ggplot( aes(x=height, y=age, colour=sex)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Age by Height")

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
  select( id, diameter, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( diameter = ifelse(is.na(yhat), diameter, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF


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
  select( id, length, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( length = ifelse(is.na(yhat), length, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF

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
  select( id, weight, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( weight = ifelse(is.na(yhat), weight, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF

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
  select( id, shuckedWeight, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( shuckedWeight = ifelse(is.na(yhat), shuckedWeight, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF

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
  select( id, visceraWeight, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( visceraWeight = ifelse(is.na(yhat), visceraWeight, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF

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
  select( id, shellWeight, .fitted, .resid, .std.resid, yhat) |>
  print() |>
  select( id, yhat) |>
  right_join(cleanDF, by = "id") |>
  mutate( shellWeight = ifelse(is.na(yhat), shellWeight, yhat)) |>
  select(-yhat) |>
  print() -> cleanDF

saveRDS(cleanDF, path(cache, "clean.rds"))

copyToArchive(archive, path(cache, "clean.rds"),
              name="clean",
              tag="crab cleaned training data",
              replace=TRUE)
