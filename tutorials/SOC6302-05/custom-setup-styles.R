# custom-setup

## Install packages not yet installed & load them
pacman::p_load(
  tidyverse,
#  gssr, # load U.S. gss data
  haven, # work with labeled data
  labelled, # use labels as values
  kableExtra, # pretty tables
  flextable, # formatted pretty tables
  summarytools, # easy descriptive statistics
  learnr, # interactive practice questions & coding
  gradethis, # automated feedback for interactive exercises
  fontawesome, # icons
  gfonts, # shadows into light font
  conflicted # declare primary packages
)

## Address important conflicts
conflict_prefer("is.labelled", "haven")
conflict_prefer("filter", "dplyr")
conflict_scout() # Identify the conflicts


# custom-formatting

## Define color palette
c_palette <- c(
  "#3498DB",
  "#E74C3C",
  "#18BC9C",
  "#F39C12"
)

## Flextbale formatting
style_flextable <- function(ft) {
  ft %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    theme_zebra() %>%
    align(i = 1, align = "center", part = "header") %>%
    bg(bg = "#18BC9C", part = "header") %>%
    color(color = "white", part = "header")
}
