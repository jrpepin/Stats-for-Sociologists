
## Install packages not yet installed & load them
pacman::p_load(
  tidyverse,
  gssr, # load U.S. gss data
  haven, # work with labeled data
  labelled, # use labels as values
  summarytools, # easy descriptive statistics
  conflicted # declare primary packages
)

## Address important conflicts
conflict_prefer("is.labelled", "haven")
conflict_prefer("filter", "dplyr")
conflict_scout() # Identify the conflicts

## Load all gss
data(gss_all)

# Get the data only for the 2024 survey respondents
gss24 <- gss_get_yr(2024)


# 01.1 Intro -------------------------------------------------------------------

# 02.1 Terms -------------------------------------------------------------------

## GSS_ALL
gss_all |>
  select(premarsx, sex) |>
  saveRDS(file = "tutorials/SOC6302-02/data/gss_all.rds")

## GSS24
gss24_02 <- gss24 |>
  select(premarsx, sex, fefam) |>
  saveRDS(file = "tutorials/SOC6302-02/data/gss24.rds")

