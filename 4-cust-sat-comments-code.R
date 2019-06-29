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
library(rmarkdown)
library(kableExtra)
library(stringi)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open survey file
#

# Import and Open the data file / Establish the data set
data_filename <- "CustSatDataT.csv"
dat           <- read.csv(data_filename, stringsAsFactors = FALSE)
names(dat)

comments_filename <- "CustSatComments.csv"
comms             <- read.csv(comments_filename, stringsAsFactors = FALSE)

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

# Build dataframe
survey_df   <- data.frame(Survey         = survey_name,
                          Pos_vocab_Word = comms$Positive,
                          Count_pos_word = 1:length(comms$Positive),
                          Neg_vocab_word = comms$Negative,
                          Count_neg_word = 1:length(comms$Negative))

#
# Positive vocabulary elements
#

pct <- 0
for(i in 1:length(comms$Positive)) {
  x <- str_detect(wkgdat$Comments, comms$Positive[i])
  survey_df[i, 3] <- length(x[x == TRUE])
  pct <- pct + length(x[x == TRUE])
}

#
# Negative vocabulay elements
#

nct <- 0
for(i in 1:length(comms$Negative)) {
  x <- str_detect(wkgdat$Comments, comms$Negative[i])
  survey_df[i, 5] <- length(x[x == TRUE])
  nct <- nct + length(x[x == TRUE])
}

# Create a filename and write out the results
filename <- paste("cum_survey_data",".csv")
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(survey_df, file = filename)

cat("Vocabulary word counts / occurrences")
kable(survey_df) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

cat("Number of survey responses is :", nrow(wkgdat), "\n")
cat("Number of positive comments is:", pct, "\n")
cat("Positive comment ratio is     :", pct / nrow(wkgdat), "\n")
cat("Number of negative comments is:", nct, "\n")
cat("Negative comment ratio is     :", nct / nrow(wkgdat), "\n")

#--------------------------------------------------------------------
#
# Vocabulary analysis - by Survey
# 
#--------------------------------------------------------------------

# Set the number of surveys 
survey_num <- length(unique(wkgdat$Surveyed))

# Loop to examine each survey and assign values to a dataframe
for(i in 1:survey_num) {
  
  # Build dataframe
  survey_df   <- data.frame(Survey         = survey_name,
                            Pos_vocab_Word = comms$Positive,
                            Count_pos_word = 1:length(comms$Positive),
                            Neg_vocab_word = comms$Negative,
                            Count_neg_word = 1:length(comms$Negative))
  
  # Assign survey name to the dataframe
  survey_df[ ,1] = unique(wkgdat$Surveyed)[i]
  
  #
  # Positive vocabulary elements
  #
  
  # Set the specific survey data
  survey_dat  <- wkgdat %>% filter(Surveyed == unique(wkgdat$Surveyed)[i])
  
  # Lopp to count the occurrences of positive words
  pct <- 0
  for(j in 1:length(comms$Positive)) {
     x <- str_detect(survey_dat$Comments, comms$Positive[j])
     survey_df[j, 3] <- length(x[x == TRUE])
     pct <- pct + length(x[x == TRUE])
  }

  #
  # Negative vocabulary elements
  #

  # Lopp to count the occurrences of negative words
  nct <- 0
  for(j in 1:length(comms$Negative)) {
    x <- str_detect(survey_dat$Comments, comms$Negative[j])
    survey_df[j, 5] <- length(x[x == TRUE])
    nct <- nct + length(x[x == TRUE])
  }

  # Create the unique filename by survey and write out the results
  filename <- paste(survey_df[i,1],".csv")
  filename <- stri_replace_all_fixed(filename, " ", "")
  write.csv(survey_df, file = filename)
  
  #
  # Print results of individual surveys
  #
  
  cat("Number of rows in the survey is                :", nrow(survey_df), "\n")
  
  cat("Number of positive comments in",survey_name,"is:", pct, "\n")
  cat("Ratio of positive comments to responses is     :", pct / nrow(survey_dat), "\n")
  
  cat("Number of negative comments in",survey_name,"is:", nct, "\n")
  cat("Ratio of negative comments to responses is     :", nct / nrow(survey_dat), "\n")
  
}


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


