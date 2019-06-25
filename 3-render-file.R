library(rmarkdown)
library(kableExtra)
render("2-CustSatReport-percents.Rmd", "html_document")
render("2-CustSatReport.Rmd", "word_document")
render("2-CustSatReport-percents.Rmd", "word_document")

