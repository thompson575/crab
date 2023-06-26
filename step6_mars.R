# --------------------------------------------------------------
# MARS for the clean data files for the kaggle playground problem on
# predicting crab age
#
library(tidyverse)
library(fs)
library(fileArchive)
library(earth)

cache   <- "C:/Projects/Kaggle/Playground/crab/data/cache"
archive <- "C:/Projects/Kaggle/Playground/crab/data/repos"

# --- read split data --------------------------------------
estimateDF <- readRDS( path(archive, "estimate.rds"))

earth(
  age ~ sexI + sexF + height + diameter + length +
    weight + shuckedWeight + visceraWeight + shellWeight,
  data = estimateDF,
  degree = 3
) |>
  saveRDS( path(cache, "mars.rds"))

copyToArchive(archive, path(cache, "mars.rds"),
              name="mars",
              tag="crab mars model",
              replace=TRUE)
