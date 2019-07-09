library(rmarkdown)
library(kableExtra)
render("4-Comments-Report.RMD", "html_document")
render("2-CustSatReport.Rmd", "html_document")
render("2-CustSatReport-percents.Rmd", "html_document")
render("2-CustSatReport-percents.Rmd", "word_document")

