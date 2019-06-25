library(rmarkdown)render("2-CustSatReport.Rmd", "html_document")
library(kableExtra)
render("2-CustSatReport-percents.Rmd", "html_document")
render("2-CustSatReport-percents.Rmd", "word_document")

