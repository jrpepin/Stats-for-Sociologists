# Setup ------------------------------------------------------------------------
# save the edited `myexam2.tex` file in `C:\R\library\exams\tex`
# reference: https://deskreject.com/2019/01/r-exams/

## load packages
library("here")
library("exams")
library("quarto")
library("qpdf")
library("pdftools")
library("sak")

conflicted::conflicts_prefer(here::here)
conflicted::conflicts_prefer(dplyr::summarize)
conflicted::conflicts_prefer(dplyr::mutate)

## a list of vectors of exam questions in R/Markdown (.Rmd) format
myexam <- list(c(
# Week 01
  here("midterm", "week01", "wheelan-point.Rmd"),
  here("midterm", "week01", "panes.Rmd"),
  here("midterm", "week01", "rproj.Rmd"),
  here("midterm", "week01", "quarto.Rmd"),
# Week 02
  here("midterm", "week02", "gandy.Rmd"),
  here("midterm", "week02", "install.Rmd"),
  here("midterm", "week02", "library.Rmd"),
  here("midterm", "week02", "package.Rmd"),
  here("midterm", "week02", "percentage.Rmd"),
  here("midterm", "week02", "type.Rmd"),
  here("midterm", "week02", "operator.Rmd"),
  here("midterm", "week02", "relative-freq.Rmd"),
# Week 03
  here("midterm", "week03", "median-table.Rmd"),
  here("midterm", "week03", "sd.Rmd"),
  here("midterm", "week03", "sd-zero.Rmd"),
  here("midterm", "week03", "skew.Rmd"),
  here("midterm", "week03", "pipes-01.Rmd"),
  here("midterm", "week03", "pipes-02.Rmd"),
  here("midterm", "week03", "drop_na.Rmd"),
  here("midterm", "week03", "code-01.Rmd"),
  here("midterm", "week03", "summarize.Rmd"),
# Week 04
  here("midterm", "week04", "wheelen-CLT.Rmd"),
  here("midterm", "week04", "z-score.Rmd"),
  here("midterm", "week04", "z-score-02.Rmd"),
  here("midterm", "week04", "z-score-proportion.Rmd"),
  here("midterm", "week04", "sixty-eight.Rmd"),
  here("midterm", "week04", "sixty-eight-01.Rmd"),
  here("midterm", "week04", "mutate.Rmd"),
  here("midterm", "week04", "case_when.Rmd"),
# Week 05
  here("midterm", "week05", "wheelen-polling.Rmd"),
  here("midterm", "week05", "wheelen-inference.Rmd"),
  here("midterm", "week05", "sample-size.Rmd"),
  here("midterm", "week05", "ghosts.Rmd"),
  here("midterm", "week05", "hrsrelax.Rmd"),
  here("midterm", "week05", "p-value.Rmd"),
  here("midterm", "week05", "false-neg.Rmd"),
  here("midterm", "week05", "hypotheses.Rmd"),
  here("midterm", "week05", "ttest.Rmd"),
  here("midterm", "week05", "ttest-tv.Rmd"),
  here("midterm", "week05", "double-colon.Rmd"),
# Week 06
  here("midterm", "week06", "ASA.Rmd"),
  here("midterm", "week06", "cohen.Rmd"),
  here("midterm", "week06", "chi-square.Rmd"),
  here("midterm", "week06", "chi-square-formula.Rmd"),
  here("midterm", "week06", "chi-square-output.Rmd"),
  here("midterm", "week06", "r-relationship.Rmd"),
  here("midterm", "week06", "cor.test.Rmd"),
  here("midterm", "week06", "tbl_cross.Rmd"),
  here("midterm", "week06", "test-type01.Rmd"),
  here("midterm", "week06", "test-type02.Rmd")
))

## Number of available questions
n_questions <- length(myexam[[1]])

# Create exam versions ---------------------------------------------------------

## Create exams
exams2pdf(  myexam, 
  n = 1, # number of exam versions
  nsamp = n_questions, # number of exam questions
  name = "midterm_", 
  dir = here("docs", "midterm"),
  template="myexam2",
  title = NULL, # merge with own title page later
  verbose = TRUE
  )

## Convert a single PDF file
#convert_pdf_to_docx(here("docs", "midterm", "midterm_1.pdf"))

# Create title pages -----------------------------------------------------------

quarto_render(
  input = here("midterm", "title-page.qmd"),
  output_file = "title_1.pdf",
  )

# Combine documents ------------------------------------------------------------

pdf_combine(
  input = c(
    here("docs", "midterm", "title_1.pdf"),
    here("docs", "midterm", "midterm_1.pdf")
  ),
  output = here("docs", "midterm", "midterm_AK.pdf")
)


# Exam without the answer key -------------------------------------------------

# Load the PDF text
pdf_text_vec <- pdf_text(here("docs", "midterm", "midterm_AK.pdf"))

# Find the page number where "Answer Sheet" appears
answer_sheet_page <- which(sapply(pdf_text_vec, function(x) grepl("Answer Sheet", x)))

# Keep all pages except the answer sheets
pdf_subset(
  input = here("docs", "midterm", "midterm_AK.pdf"),
  pages = 1:(answer_sheet_page - 1),
  output = here("docs", "midterm", "midterm.pdf")
)

# Get rid of temporary docs ----------------------------------------------------

file.remove(here("docs", "midterm", "midterm_1.pdf"))
file.remove(here("docs", "midterm", "title_1.pdf"))


# problems ---------------------------------------------------------------------

## find bad files 
files <- unlist(myexam)

## (with - signs)
bad_files <- files[sapply(files, function(f) {
  any(grepl("\u2212", readLines(f, warn = FALSE)))
})]
bad_files


## (with α signs)
bad_files <- files[sapply(files, function(f) {
  any(grepl("\u03B1", readLines(f, warn = FALSE), useBytes = TRUE))
})]

bad_files

## Replace ≠ with \neq in your files
bad_files <- files[sapply(files, function(f) {
  any(grepl("\u2260", readLines(f, warn = FALSE), useBytes = TRUE))
})]

bad_files

## key word
find_something <- files[sapply(files, function(f) {
  any(grepl("as_kable_extra", readLines(f, warn = FALSE), useBytes = TRUE))
})]


find_something

## Try out to word

exams2html(  
  myexam, 
  n = 2, # number of exam versions
  nsamp = n_questions, # number of exam questions
  name = "midterm_", 
  dir = here("docs", "midterm")
)

# Convert HTML to Word using Pandoc
rmarkdown::pandoc_convert(
  input = here::here("docs", "midterm", "midterm_1.html"),
  to = "docx",
  output = here::here("docs", "midterm", "midterm_1.docx")
)

