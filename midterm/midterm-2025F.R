# MIDTERM ----------------------------------------------------------------------

## load packages
library("here")
library("exams")

## a list of vectors of exam questions in R/Markdown (.Rmd) format

myexam <- list(c(
  here("midterm", "week01", "panes.Rmd"),
  here("midterm", "week01", "wheelan.Rmd"),
  here("midterm", "week02", "gandy.Rmd"),
  here("midterm", "week02", "install.Rmd"),
  here("midterm", "week02", "library.Rmd"),
  here("midterm", "week02", "package.Rmd"),
  here("midterm", "week02", "percentage.Rmd"),
  here("midterm", "week02", "type.Rmd"),
  here("midterm", "week05", "double-colon.Rmd")
))

## Create form codes -----------------------------------------------------------
form_codes <- list(
  list(FormCode = "A"),
  list(FormCode = "B")
)

## exams2pdf -------------------------------------------------------------------

midterm <- exams2pdf(
  myexam, 
  n = 2, # number of exam versions
  nsamp = 9, # number of exam questions
  name = "midterm_2025F", 
  dir = here("midterm"),
  template="myexam2",
  control = form_codes
  )
