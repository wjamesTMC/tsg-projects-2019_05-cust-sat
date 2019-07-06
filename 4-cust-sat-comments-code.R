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
library(stringi)
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(rmarkdown)
library(kableExtra)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open survey file
#

# Import and Open the data file / Establish the data sets
data_filename <- "CustSatData.csv"
dat           <- read.csv(data_filename, stringsAsFactors = FALSE)

comments_filename <- "pos-neg-vocabulary.csv"
comms             <- read.csv(comments_filename, stringsAsFactors = FALSE)
pos_vocab         <- comms %>% filter(Tone == "P")
neg_vocab         <- comms %>% filter(Tone == "N")

# Establish the max dataframe size for later
if(nrow(pos_vocab) > nrow(neg_vocab)) {
  df_length <- nrow(pos_vocab) 
} else {
    df_length <- nrow(neg_vocab)
    }

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

# Calc the number of comments
num_comments <- length(unique(wkgdat$Comments))

#--------------------------------------------------------------------
#
# Vocabulary analysis - Cumulative
#
#--------------------------------------------------------------------

#
# Positive vocabulary elements
#

# Build dataframe for positives
pos_df   <- data.frame(Word  = pos_vocab$Term,
                       Count = 1:df_length,
                       Type  = "P")

# Loop to identify positive words in the comments field
pct <- 0
for(i in 1:df_length) {
  x <- str_detect(wkgdat$Comments, pos_vocab$Term[i])
  pos_df[i, 2] <- length(x[x == TRUE])
  pct <- pct + length(x[x == TRUE])
}

# Remove words with zero counts
pos_df  <- pos_df %>% filter(Count != 0)

# Sort from high to low
pos_df <- arrange(pos_df, desc(Count), Word)

# Append to the cumulative dataframe
cum_count_df <- rbind(cum_count_df, pos_df)

#
# Negative vocabulay elements
#

# Build dataframe for negatives
neg_df   <- data.frame(Word  = neg_vocab$Term,
                       Count = 1:df_length,
                       Type  = "N")


# Loop to identify negative words in the comments field
nct <- 0
for(i in 1:df_length) {
  x <- str_detect(wkgdat$Comments, neg_vocab$Term[i])
  neg_df[i, 2] <- length(x[x == TRUE])
  nct <- nct + length(x[x == TRUE])
}

# Remove words with zero counts
neg_df   <- neg_df %>% filter(Count != 0)

# Sort from high to low
neg_df <- arrange(neg_df, desc(Count), Word)

# Create datafrane and append negatives
cum_count_df <- pos_df
cum_count_df <- rbind(cum_count_df, neg_df)

# Determine overall positive and negative indexes (pos / neg words / comments
pos_index <- sum(pos_df$Count) / num_comments
neg_index <- sum(neg_df$Count) / num_comments

# Create a filename and write out the results
filename <- paste("cum_survey_data",".csv")
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(cum_count_df, file = filename)

cat("Number of survey responses      :", nrow(wkgdat), "\n")
cat("Number of survey comments       :", num_comments, "\n")
cat("Comments to responses ratio     :", num_comments / nrow(wkgdat), "\n")
cat("Number of positive words        :", pct, "\n")
cat("Positive words to comments ratio:", pos_index, "\n")
cat("Number of negative words        :", nct, "\n")
cat("Negative words to comments ratio:", neg_index, "\n")

# Display results and statistics
cat("Vocabulary word counts / occurrences")
# kable(cum_count_df) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
cum_count_df

#--------------------------------------------------------------------
#
# Vocabulary analysis - by Survey
# 
#--------------------------------------------------------------------

# Set the number of surveys 
survey_num <- length(unique(wkgdat$Surveyed))

# Dataframe to collect all the data and calculations
survey_inf <- data.frame(Survey        = survey_num,
                         num_resps     = survey_num,
                         num_comments  = survey_num,
                         c_to_r_ratio  = survey_num,
                         num_pos_words = survey_num,
                         pw_to_c_ratio = survey_num,
                         num_neg_words = survey_num,
                         nw_to_c_ratio = survey_num)

# Loop to examine each survey and assign values to a dataframe
for(i in 1:survey_num) {
  
  #
  # Positive vocabulary elements
  #
  
  # Build dataframe
  sur_pos_df <- data.frame(Survey = unique(wkgdat$Surveyed)[i],
                           Word   = pos_vocab$Term,
                           Count  = 0,
                           Type   = "P")
  
  # Set the specific survey data
  survey_dat  <- wkgdat %>% filter(Surveyed == unique(wkgdat$Surveyed)[i])
  survey_comments <- length(unique(survey_dat$Comments))
  
  # Loop to count the occurrences of positive words
  pct <- 0
  for(j in 1:df_length) {
     x <- str_detect(survey_dat$Comments, pos_vocab$Term[j])
     sur_pos_df[j, 3] <- length(x[x == TRUE])
     pct <- pct + length(x[x == TRUE])
  }

  # Remove words with zero counts
  sur_pos_df  <- sur_pos_df %>% filter(Count != 0)
  
  # Sort from high to low
  sur_pos_df <- arrange(sur_pos_df, desc(Count), Word)
  sur_pos_df
  
  #
  # Negative vocabulary elements
  #

  # Build dataframe
  sur_neg_df <- data.frame(Survey = unique(wkgdat$Surveyed)[i],
                           Word   = neg_vocab$Term,
                           Count  = 0,
                           Type   = "N")
  
  # Loop to count the occurrences of negative words
  nct <- 0
  for(j in 1:df_length) {
    x <- str_detect(survey_dat$Comments, neg_vocab$Term[j])
    sur_neg_df[j, 3] <- length(x[x == TRUE])
    nct <- nct + length(x[x == TRUE])
  }

  # Remove words with zero counts
  sur_neg_df  <- sur_neg_df %>% filter(Count != 0)
  
  # Sort from high to low
  sur_neg_df <- arrange(sur_neg_df, desc(Count), Word)
  sur_neg_df
  
  # Create combined dataframe
  sur_cum_df <- sur_pos_df
  
  # Append to the cumulative dataframe
  sur_cum_df <- rbind(sur_cum_df, sur_neg_df)
  
  # Create the unique filename by survey and write out the results
  filename <- paste(sur_cum_df[i,1],".csv")
  filename <- stri_replace_all_fixed(filename, " ", "")
  write.csv(sur_cum_df, file = filename)
  
  #
  # Print results of individual surveys
  #
  
  #kable(sur_cum_df) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
  # Populate summary dataframe
  survey_inf[i, 1] <- unique(wkgdat$Surveyed)[i]
  survey_inf[i, 2] <- nrow(survey_dat)
  survey_inf[i, 3] <- survey_comments
  survey_inf[i, 4] <- round(survey_comments / nrow(survey_dat), digits = 2)
  survey_inf[i, 5] <- pct
  survey_inf[i, 6] <- round(pct / survey_comments, digits = 2)
  survey_inf[i, 7] <- nct
  survey_inf[i, 8] <- round(nct / survey_comments, digits = 2)
    
  # Display results and statistics
  cat("Results for", survey_inf[i, 1], "\n")
  cat("Number of responses             :", survey_inf[i, 2], "\n")
  cat("Number of comments              :", survey_inf[i, 3], "\n")
  cat("Comments to responses ratio     :", survey_inf[i, 4], "\n")
  cat("Number of positive words        :", survey_inf[i, 5], "\n")
  cat("Positive words to comments ratio:", survey_inf[i, 6], "\n")
  cat("Number of negative words        :", survey_inf[i, 7], "\n")
  cat("Negative words to comments ratio:", survey_inf[i, 8], "\n")
  cat("\n")
  
}

survey_inf

#--------------------------------------------------------------------
#
# Build graphics from summary dataframe
#
#--------------------------------------------------------------------

# Number of coments and responses by survey
ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_resps, color = "Responses"), group=1, size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments"), group=1, size=2) +
  scale_colour_manual("", 
                      breaks = c("Responses", "Comments"),
                      values = c("#0072B2", "#E69F00")) +
  labs(title = "Number of Comments and Responses", subtitle = "Number By Survey") + ylab("Number")

# Ratio of comments to responses
ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=c_to_r_ratio, color = "green", group=1), size=2) +
  theme(legend.position = "none") +
  labs(title = "Ratio of Comments to Responses", subtitle = "Percentage By Survey") + ylab("% Comments") 

# Positive words vs. comments
ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_pos_words, color = "Positive Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Comments"),
                      values = c("#0072B2", "#E69F00")) +
  labs(title = "Positive Words vs. Comments", subtitle = "Number By Survey") + ylab("Occurences")

# Negative words vs. comments
ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=num_neg_words, color = "Negative Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=num_comments, color = "Comments", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Negative Words", "Comments"),
                      values = c("#0072B2", "#E69F00")) +
  labs(title = "Negative Words vs. Comments", subtitle = "Number By Survey") + ylab("Occurences")

# Positive and negative words to comments ratios
ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=pw_to_c_ratio, color = "Positive", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=nw_to_c_ratio, color = "Negative", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Positive", "Negative"),
                      values = c("#0072B2", "#E69F00")) +
  labs(title = "Positive / Negative Words Per Comment", subtitle = "Number Occurrences by Survey") +
  ylab("No. Words per Comment") 

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


