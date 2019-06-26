##############################################################################
#
# Customer Sat data Analysis Project - Comments sectioin
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/tsg-projects-2019_05-cust-sat.git
#
##############################################################################

#
# Library setups
#

# Import libraries
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open survey file
#

# Import and Open the data file / Establish the data set
data_filename <- "CustSatData.csv"
dat <- read.csv(data_filename, stringsAsFactors = FALSE)
names(dat)

comments_filename <- "CustSatComments.csv"
comms <- read.csv(comments_filename, stringsAsFactors = FALSE)

#
# Clean data file to set vector names
#

# Rename vectors from survey questions to variable names
dat <- rename(dat, replace = c("Tell.us.about.your.experience.working.with.us..TSG.makes.a.positive.contribution.to.my.work." = "PosContrib",
                               "Tell.us.about.your.experience.working.with.us..TSG.responds.to.my.requests.in.a.timely.manner." = "TimelyResp",
                               "Tell.us.about.your.experience.working.with.us..TSG.takes.accountability.for.my.requests." = "Accountability",
                               "Tell.us.about.your.experience.working.with.us..TSG.staff.are.knowledgable.." = "Knowledgeable",
                               "Please.rate.your.satisfaction.with.our.services...Account.manager.support." = "AcctMgrs",
                               "Please.rate.your.satisfaction.with.our.services...Audio.and.Video.Production.Services." = "BMPS",
                               "Please.rate.your.satisfaction.with.our.services...Business.applications.support..M.files..Salesforce..Enterprise..etc..." = "BusApps",
                               "Please.rate.your.satisfaction.with.our.services...Event.services." = "EventSvcs",
                               "Please.rate.your.satisfaction.with.our.services...Project.support." = "ProjSupp",
                               "Please.rate.your.satisfaction.with.our.services...Service.Desk." = "ServDesk",
                               "Please.rate.your.satisfaction.with.our.services...Studio.services." = "StudioSvcs",
                               "Please.rate.your.satisfaction.with.our.services...Vendor.management.services." = "VenMgmt",
                               "Is.there.anything.else.you.would.like.to.share." = "Comments"))

# Remove rows below the actual data
cleandat <- subset(dat, dat[ , 1] != "")

# Select the desired columns
wkgdat <- cleandat %>% select(Surveyed,	Comments, PosContrib,	TimelyResp,	
                              Accountability, Knowledgeable,	AcctMgrs,	
                              BMPS,	BusApps,	EventSvcs, ProjSupp,	ServDesk,	
                              StudioSvcs,	VenMgmt)

# Rename ratings so they will sort
wkgdat[wkgdat == "Always"]            <- "1-Always"
wkgdat[wkgdat == "Mostly"]            <- "2-Mostly"
wkgdat[wkgdat == "Sometimes"]         <- "3-Sometimes"
wkgdat[wkgdat == "Rarely"]            <- "4-Rarely"
wkgdat[wkgdat == "Never"]             <- "5-Never"

# Change survey names to shorter form
for(i in 1:length(unique(wkgdat$Surveyed))) {
  sssq <- str_sub(unique(wkgdat$Surveyed)[i],  6,  6)
  sssy <- str_sub(unique(wkgdat$Surveyed)[i], -2, -1)
  survey_name <- paste(as.character(0), as.character(i),
                       "-", sssq, as.character(sssy))
  survey_name <- str_replace_all(survey_name, " ", "")
  wkgdat[wkgdat == unique(wkgdat$Surveyed)[i]] <- survey_name
}

# Replace missing values (zeros) with NR and shorten "do not use"
wkgdat[wkgdat == 0]                   <- "NR"
wkgdat[wkgdat == ""]                  <- "NR"
wkgdat[wkgdat == "N/A do not use"]    <- "NA/DNU"

# Verify the number of unique values of the various factors
sapply(wkgdat, function(x)length(unique(x)))

#--------------------------------------------------------------------
#
# Vocabulary analysis - Cumulative
#
#--------------------------------------------------------------------

#
# Positive vocabulary elements
#

count_pos_vocab <- data.frame(Vocabulary_Word = 1:length(comms$Positive),
                              Count           = 1:length(comms$Positive))
                              
pct <- 0
for(i in 1:length(comms$Positive)) {
  x <- str_detect(wkgdat$Comments, comms$Positive[i])
  count_pos_vocab[i, 1] <- comms$Positive[i]
  count_pos_vocab[i, 2] <- length(x[x == TRUE])
  pct <- pct + length(x[x == TRUE])
}

cat("Number of positive comments is:", pct)
cat("Positive comment ratio is:     ", pct / length(comms$Positive))
cat("Summary of positive vocabulary word counts / occurrences")
kable(count_pos_vocab) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

#
# Negative vocabulary elements
#

count_neg_vocab <- data.frame(Vocabulary_Word = 1:length(comms$Negative),
                              Count           = 1:length(comms$Negative))

nct <- 0
for(i in 1:length(comms$Negative)) {
  x <- str_detect(wkgdat$Comments, comms$Negative[i])
  count_neg_vocab[i, 1] <- comms$Negative[i]
  count_neg_vocab[i, 2] <- length(x[x == TRUE])
  nct <- nct + length(x[x == TRUE])
}

cat("Number of negative comments is:", nct)
cat("Negative comment ratio is     :", nct / length(comms$Negative))
cat("Summary of negative vocabulary word counts / occurrences")
kable(count_neg_vocab) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

#--------------------------------------------------------------------
#
# Vocabulary analysis - by Survey
#
#--------------------------------------------------------------------

names(wkgdat)
wkgdat$Surveyed
dat01F18 <- wkgdat %>% filter(Surveyed == "01-F18")

q_dat <- unique(wkgdat$Surveyed)

#
# Positive vocabulary elements
#
pct <- 0
for(i in 1:length(comms$Positive)) {
  x <- str_detect(wkgdat$Comments, comms$Positive[i])
  pct <- pct + length(x[x == TRUE])
}
pct
cat("Positive comment ratio is:", pct / length(comms$Positive))

#
# Negative vocabulary elements
#
nct <- 0
for(i in 1:length(comms$Negative)) {
  x <- str_detect(wkgdat$Comments, comms$Negative[i])
  nct <- nct + length(x[x == TRUE])
}
nct
cat("Negative comment ratio is:", nct / length(comms$Negative))




#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


