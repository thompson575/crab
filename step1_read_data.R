# --------------------------------------------------------------
# Read the data files for the kaggle playground problem on
# predicting crab age
#
library(tidyverse)
library(fs)
library(fileArchive)
library(janitor)

rawData <- "C:/Projects/Kaggle/Playground/crab/data/rawData"
cache   <- "C:/Projects/Kaggle/Playground/crab/data/cache"
archive <- "C:/Projects/Kaggle/Playground/crab/data/repos"

# --------------------------------------------------------------
# Read training data, clean names and save in the archive
#
read.csv( path(rawData, "train.csv")) |>
  clean_names("lower_camel") |>
  saveRDS( path(cache, "train.rds"))

copyToArchive(archive, path(cache, "train.rds"),
              name="train",
              tag="crab training data",
              replace=TRUE)

# --------------------------------------------------------------
# Read test data, clean names and save in the archive
#
read.csv( path(rawData, "test.csv")) |>
  clean_names("lower_camel") |>
  saveRDS( path(cache, "test.rds"))

copyToArchive(archive, path(cache, "test.rds"),
              name="test",
              tag="crab test data",
              replace=TRUE)


# --------------------------------------------------------------
# Read sample submission and save in the archive
#
read.csv( path(rawData, "sample_submission.csv")) |>
  saveRDS( path(cache, "submission.rds"))

copyToArchive(archive, path(cache, "submission.rds"),
              name="submission",
              tag="crab sample submission",
              replace=TRUE)


