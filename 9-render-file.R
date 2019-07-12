library(rmarkdown)
library(kableExtra)
render("3-CustSatReport-comments.RMD", "html_document")
render("1-CustSatReport-base.Rmd", "html_document")
render("2-CustSatReport-percents.Rmd", "html_document")

