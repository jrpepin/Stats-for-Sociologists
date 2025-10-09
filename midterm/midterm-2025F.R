# Setup ------------------------------------------------------------------------
# save the edited `myexam2.tex` file in `C:\R\library\exams\tex`
# reference: https://deskreject.com/2019/01/r-exams/

## load packages
library("here")
library("exams")
library("quarto")
library("qpdf")

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

# Create exam versions ---------------------------------------------------------

exams2pdf(
  myexam, 
  n = 2, # number of exam versions
  nsamp = 9, # number of exam questions
  name = "midterm_", 
  dir = here("docs", "midterm"),
  template="myexam2",
  title = NULL # merge with own later
  )

# Create title pages -----------------------------------------------------------

quarto_render(
  input = here("midterm", "title-page.qmd"),
  output_file = "title_1.pdf",
  execute_params = list(form_code = "A", captions = c("Intro caption"))
)

quarto_render(
  input = here("midterm", "title-page.qmd"),
  output_file = "title_2.pdf",
  execute_params = list(form_code = "B", captions = c("Instructions caption"))
)

# Combine documents ------------------------------------------------------------

## Form A
pdf_combine(
  input = c(
    here("docs", "midterm", "title_1.pdf"),
    here("docs", "midterm", "midterm_1.pdf")
  ),
  output = here("docs", "midterm", "midterm_A_AK.pdf")
)

## Form B
pdf_combine(
  input = c(
    here("docs", "midterm", "title_2.pdf"),
    here("docs", "midterm", "midterm_2.pdf")
  ),
  output = here("docs", "midterm", "midterm_B_AK.pdf")
)

# Exams without the answer key -------------------------------------------------

# Get total number of pages
n_pages_A <- pdf_length(here("docs", "midterm", "midterm_A_AK.pdf"))
n_pages_B <- pdf_length(here("docs", "midterm", "midterm_B_AK.pdf"))

# Keep all pages except the last
pdf_subset(
  input = here("docs", "midterm", "midterm_A_AK.pdf"),
  pages = 1:(n_pages_A - 1),
  output = here("docs", "midterm", "midterm_A.pdf")
)

pdf_subset(
  input = here("docs", "midterm", "midterm_B_AK.pdf"),
  pages = 1:(n_pages_B - 1),
  output = here("docs", "midterm", "midterm_B.pdf")
)

# Get rid of temporary docs ----------------------------------------------------

file.remove(here("docs", "midterm", "midterm_1.pdf"))
file.remove(here("docs", "midterm", "midterm_2.pdf"))
file.remove(here("docs", "midterm", "title_1.pdf"))
file.remove(here("docs", "midterm", "title_2.pdf"))
