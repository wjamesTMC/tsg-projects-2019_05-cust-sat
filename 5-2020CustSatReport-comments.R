##############################################################################
#
# Customer Sat data Analysis Project - Comments section
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/tsg-projects-2019_05-cust-sat.git
#
##############################################################################

#--------------------------------------------------------------------
#
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
library(googlesheets)
library(purrr)
library(stringr)
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
data_filename <- gs_title("2019-2020 TSG Satisfaction Survey_T")
dat <- gs_read(data_filename, stringsAsFactors = FALSE)

vocab_filename <- gs_title("0_Input_Vocabulary")
comms          <- gs_read(vocab_filename, stringsAsFactors = FALSE)

pos_vocab      <- comms %>% filter(Tone == "Positive")
neg_vocab      <- comms %>% filter(Tone == "Negative")
neu_vocab      <- comms %>% filter(Tone == "Neutral")

#
# Clean data file to set vector names
#

dat <- rename(dat, replace = c("Timestamp" = "Surveyed",
                               "1A. TSG representatives are always able to understand my problems / needs. [Account Managers]" = "Q1A_AM",
                               "1A. TSG representatives are always able to understand my problems / needs. [Business Applications Support]" = "Q1A_BA",
                               "1A. TSG representatives are always able to understand my problems / needs. [B&MPS]" = "Q1A_BM",
                               "1A. TSG representatives are always able to understand my problems / needs. [Project Support / PMO]" = "Q1A_PS",
                               "1A. TSG representatives are always able to understand my problems / needs. [Service Desk]" = "Q1A_SD",
                               "1A. TSG representatives are always able to understand my problems / needs. [Vendor Managers]" = "Q1A_VM",
                               "1B. TSG representatives ask the right questions and listen to my input. [Account Managers]" = "Q1B_AM",
                               "1B. TSG representatives ask the right questions and listen to my input. [Business Applications Support]" = "Q1B_BA",
                               "1B. TSG representatives ask the right questions and listen to my input. [B&MPS]" = "Q1B_BM",
                               "1B. TSG representatives ask the right questions and listen to my input. [Project Support / PMO]" = "Q1B_PS",
                               "1B. TSG representatives ask the right questions and listen to my input. [Service Desk]" = "Q1B_SD",
                               "1B. TSG representatives ask the right questions and listen to my input. [Vendor Managers]" = "Q1B_VM",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Account Managers]" = "Q1C_AM",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Business Applications Support]" = "Q1C_BA",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [B&MPS]" = "Q1C_BM",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Project Support / PMO]" = "Q1C_PS",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Service Desk]" = "Q1C_SD",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Vendor Managers]" = "Q1C_VM",
                               "2A. TSG representatives always have the knowledge to address my needs. [Account Managers]" = "Q2A_AM",
                               "2A. TSG representatives always have the knowledge to address my needs. [Business Applications Support]" = "Q2A_BA",
                               "2A. TSG representatives always have the knowledge to address my needs. [B&MPS]" = "Q2A_BM",
                               "2A. TSG representatives always have the knowledge to address my needs. [Project Support / PMO]" = "Q2A_PS",
                               "2A. TSG representatives always have the knowledge to address my needs. [Service Desk]" = "Q2A_SD",
                               "2A. TSG representatives always have the knowledge to address my needs. [Vendor Managers]" = "Q2A_VM",
                               "2B.  TSG representatives address my needs completely and professionally. [Account Managers]" = "Q2B_AM",
                               "2B.  TSG representatives address my needs completely and professionally. [Business Applications Support]" = "Q2B_BA",
                               "2B.  TSG representatives address my needs completely and professionally. [B&MPS]" = "Q2B_BM",
                               "2B.  TSG representatives address my needs completely and professionally. [Project Support / PMO]" = "Q2B_PS",
                               "2B.  TSG representatives address my needs completely and professionally. [Service Desk]" = "Q2B_SD",
                               "2B.  TSG representatives address my needs completely and professionally. [Vendor Managers]" = "Q2B_VM",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Account Managers]" = "Q3_AM",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Business Applications Support]" = "Q3_BA",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [B&MPS]" = "Q3_BM",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Project Support / PMO]" = "Q3_PS",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Service Desk]" = "Q3_SD",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Vendor Managers]" = "Q3_VM",
                               "What additional comments do you have for TSG (pro or con) that might help us serve you better?" = "Comments",
                               "Email Address" = "Email"))

#
# Convert time stamps to survey names
#

# Strip off the clock time and leave just the date
dat$Surveyed <-gsub(" .*","",dat$Surveyed)

# Set survey groupings
for(i in 1:length(dat$Surveyed)) {
  if(str_detect(dat$Surveyed[i], "2019")) {
    dat$Surveyed[i] <- "1F19"
  }
  if(dat$Surveyed[i] != "1F19" & dat$Surveyed[i] > "1/1/2020" & dat$Surveyed[i] < "4/1/2020") {
    dat$Surveyed[i] <- "2W20"
  }
  if(dat$Surveyed[i] != "1F19" & dat$Surveyed[i] > "4/1/2020" & dat$Surveyed[i] < "6/30/2020") {
    dat$Surveyed[i] <- "3S20"
  }
  if(dat$Surveyed[i] != "1F19" & dat$Surveyed[i] > "7/1/2020" & dat$Surveyed[i] < "9/30/2020") {
    dat$Surveyed[i] <- "4S20"
  }
}
Q1_responses <- nrow(dat %>% filter(dat$Surveyed =="1F19"))
Q2_responses <- nrow(dat %>% filter(dat$Surveyed =="2W20"))
Q3_responses <- nrow(dat %>% filter(dat$Surveyed =="3S20"))
Q4_responses <- nrow(dat %>% filter(dat$Surveyed =="4S20"))

# Remove rows below the actual data / Clear out except for Comments
wkgdat <- subset(dat, dat[ , 1] != "") %>% select(Surveyed, Comments) 
wkgdat <- wkgdat[!is.na(wkgdat$Comments), ]
num_comments <- nrow(wkgdat)

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
                       Count = 0,
                       Type  = "Pos")

# Loop to identify positive words in the comments field
pct <- 0
for(i in 1:nrow(pos_vocab)) {
  x <- str_detect(wkgdat$Comments, pos_vocab$Term[i])
  pos_df[i, 2] <- length(x[x == TRUE])
  pct <- pct + length(x[x == TRUE])
}

# Remove words with zero counts
pos_df  <- pos_df %>% filter(Count != 0)

# Sort from high to low
pos_df <- arrange(pos_df, desc(Count), Word)

# Print out the top 10 words
pos_df[1:10, ]

#
# Negative vocabulary elements
#

# Build dataframe for negatives
neg_df   <- data.frame(Word  = neg_vocab$Term,
                       Count = 0,
                       Type  = "Neg")


# Loop to identify negative words in the comments field
nct <- 0
for(i in 1:nrow(neg_vocab)) {
  x <- str_detect(wkgdat$Comments, neg_vocab$Term[i])
  neg_df[i, 2] <- length(x[x == TRUE])
  nct <- nct + length(x[x == TRUE])
}

# Remove words with zero counts
neg_df   <- neg_df %>% filter(Count != 0)

# Sort from high to low
neg_df <- arrange(neg_df, desc(Count), Word)

# Print out the top 10 words
neg_df[1:10, ]

#
# Neutral vocabulary elements
#

# Build dataframe for negatives
neu_df   <- data.frame(Word  = neu_vocab$Term,
                       Count = 0,
                       Type  = "Neu")


# Loop to identify neutral words in the comments field
neu_ct <- 0
for(i in 1:nrow(neu_vocab)) {
  x <- str_detect(wkgdat$Comments, neu_vocab$Term[i])
  neu_df[i, 2] <- length(x[x == TRUE])
  neu_ct <- neu_ct + length(x[x == TRUE])
}

# Remove words with zero counts
neu_df   <- neu_df %>% filter(Count != 0)

# Sort from high to low
neu_df <- arrange(neu_df, desc(Count), Word)

# Print out the top 10 words
neu_df[1:10, ]

# Create datafrane and append negatives and neutrals
cum_count_df <- pos_df
cum_count_df <- rbind(cum_count_df, neg_df)
cum_count_df <- rbind(cum_count_df, neu_df)
cum_count_df <- cum_count_df %>% filter(Count != 0)

# Determine overall positive and negative indexes (pos / neg words / comments
pos_index <- sum(pos_df$Count) / num_comments
neg_index <- sum(neg_df$Count) / num_comments
neu_index <- sum(neu_df$Count) / num_comments

# Create a filename and write out the results
filename <- paste("2020_Output_cum_survey_data",".csv")
filename <- stri_replace_all_fixed(filename, " ", "")
write.csv(cum_count_df, file = filename)

cat(" Number of survey comments       :", num_comments, "\n", "\n",
    "Number of positive words        :", pct, "\n",
    "Positive words to comments ratio:", pos_index, "\n", "\n",
    "Number of negative words        :", nct, "\n",
    "Negative words to comments ratio:", neg_index, "\n", "\n",
    "Number of neutral words         :", neu_ct, "\n",
    "Neutral words to comments ratoo :", neu_index, "\n")


# Display results and statistics
cat("Vocabulary word counts / occurrences")
# kable(pos_df) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# kable(neg_df) %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

pos_df
neg_df
neu_df

#--------------------------------------------------------------------
#
# Vocabulary analysis - by Survey
# 
#--------------------------------------------------------------------

# Set the number of surveys 
survey_num <- length(unique(wkgdat$Surveyed))

# Dataframe to collect all the data and calculations
survey_inf <- data.frame(Survey         = survey_num,
                         Resp           = survey_num,
                         Comms          = survey_num,
                         c_to_r_ratio   = survey_num,
                         PosWds         = survey_num,
                         pw_to_c_ratio  = survey_num,
                         NegWds         = survey_num,
                         nw_to_c_ratio  = survey_num,
                         NeuWds         = survey_num,
                         neu_to_c_ratio = survey_num)

# Loop to examine each survey and assign values to a dataframe
for(i in 1:survey_num) {
  
  # Set the specific survey data
  survey_dat  <- wkgdat %>% filter(Surveyed == unique(wkgdat$Surveyed)[i])
  survey_comments <- length(unique(survey_dat$Comments))
  ifelse(str_detect(survey_dat$Comments, "NR"),
         survey_comments <- survey_comments- 1, survey_comments)
  
  #
  # Positive vocabulary elements
  #
  
  # Build dataframe
  sur_pos_df <- data.frame(Survey = unique(wkgdat$Surveyed)[i],
                           Word   = pos_vocab$Term,
                           Count  = 0,
                           Type   = "Pos")
  
  # Loop to count the occurrences of positive words
  pct <- 0
  for(j in 1:nrow(pos_vocab)) {
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
                           Type   = "Neg")
  
  # Loop to count the occurrences of negative words
  nct <- 0
  for(j in 1:nrow(neg_vocab)) {
    x <- str_detect(survey_dat$Comments, neg_vocab$Term[j])
    sur_neg_df[j, 3] <- length(x[x == TRUE])
    nct <- nct + length(x[x == TRUE])
  }

  # Remove words with zero counts
  sur_neg_df  <- sur_neg_df %>% filter(Count != 0)
  
  # Sort from high to low
  sur_neg_df <- arrange(sur_neg_df, desc(Count), Word)
  sur_neg_df
  
  #
  # Neutral vocabulary elements
  #
  
  # Build dataframe
  sur_neu_df <- data.frame(Survey = unique(wkgdat$Surveyed)[i],
                          Word   = neu_vocab$Term,
                          Count  = 0,
                          Type   = "Neu")
  
  # Loop to count the occurrences of neutral words
  neu_ct <- 0
  for(j in 1:nrow(neu_vocab)) {
     x <- str_detect(survey_dat$Comments, neu_vocab$Term[j])
     sur_neu_df[j, 3] <- length(x[x == TRUE])
     neu_ct <- neu_ct + length(x[x == TRUE])
  }
  
  # Remove words with zero counts
  sur_neu_df  <- sur_neu_df %>% filter(Count != 0)

  # Sort from high to low
  sur_neu_df <- arrange(sur_neu_df, desc(Count), Word)
  sur_neu_df
  
  # Create combined dataframe
  sur_cum_df <- sur_pos_df
  
  # Append to the cumulative dataframe
  sur_cum_df <- rbind(sur_cum_df, sur_neg_df)
  # sur_cum_df <- rbind(sur_cum_df, sur_neu_df)
  
  # Create the unique filename by survey and write out the results
  filename <- paste("0_Output_", sur_cum_df[i,1],".csv")
  filename <- stri_replace_all_fixed(filename, " ", "")
  write.csv(sur_cum_df, file = filename)
  
  #
  # Print results of individual surveys
  #
  
  #kable(sur_cum_df) %>%
  #  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
  
  # Populate summary dataframe
  survey_inf[i, 1] <- unique(wkgdat$Surveyed)[i]
  survey_inf[i, 2] <- nrow(dat)
  survey_inf[i, 3] <- survey_comments
  survey_inf[i, 4] <- round(survey_comments / nrow(dat), digits = 2)
  survey_inf[i, 5] <- pct
  survey_inf[i, 6] <- round(pct / survey_comments, digits = 2)
  survey_inf[i, 7] <- nct
  survey_inf[i, 8] <- round(nct / survey_comments, digits = 2)
  survey_inf[i, 9] <- neu_ct
  survey_inf[i,10] <- round(neu_ct / survey_comments, digits = 2)
    
  # Display results and statistics
  cat("Results for", survey_inf[i, 1], "\n")
  cat("Number of responses             :", survey_inf[i, 2], "\n")
  cat("Number of comments              :", survey_inf[i, 3], "\n")
  cat("Comments to responses ratio     :", survey_inf[i, 4], "\n")
  cat("Number of positive words        :", survey_inf[i, 5], "\n")
  cat("Positive words to comments ratio:", survey_inf[i, 6], "\n")
  cat("Number of negative words        :", survey_inf[i, 7], "\n")
  cat("Negative words to comments ratio:", survey_inf[i, 8], "\n")
  cat("Number of neutral words         :", survey_inf[i, 9], "\n")
  cat("Neutral words to comments ratio :", survey_inf[i,10], "\n")
  cat("\n")
  
}

survey_inf

#--------------------------------------------------------------------
#
# Build graphics from summary dataframe
#
#--------------------------------------------------------------------

# Number of coments and responses by survey
num_c_and_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=Resp, color = "Responses"), group=1, size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments"), group=1, size=2) +
  scale_colour_manual("", 
                      breaks = c("Responses", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Count of Comments and Responses", subtitle = "Numbers of each by Survey") + ylab("Number") +
  theme(legend.position = c(0.18,0.85))

# Ratio of comments to responses
ratio_c_to_r <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=c_to_r_ratio, color = "Ratios", group=1), size=2) +
  scale_colour_manual("", breaks = c("Ratios"), values = c("mediumblue")) +
  labs(title = "Ratio of Comments to Responses", subtitle = "Ratio By Survey") + ylab("Proportion of Comments") +
  theme(legend.position = c(0.15,0.88))

# Arrange the two plots for pasting into deck
grid.arrange(num_c_and_r, ratio_c_to_r, ncol = 2)

# Positive words vs. comments
pw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=PosWds, color = "Positive Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Positive Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.2,0.85))

# Negative words vs. comments
nw_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=NegWds, color = "Negative Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Negative Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Negative Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Neutral words vs. comments
neu_vs_c <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=NeuWds, color = "Neutral Words", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=Comms, color = "Comments", group=1), size=2) +
  scale_y_continuous(limits=c(0, 60)) +
  scale_colour_manual("", 
                      breaks = c("Neutral Words", "Comments"),
                      values = c("mediumblue", "indianred4")) +
  labs(title = "Neutral Words vs. Comments", subtitle = "Number of each by Survey") + ylab("Number of Each") +
  theme(legend.position = c(0.22,0.85))

# Arrange the two plots for pasting into deck
grid.arrange(pw_vs_c, nw_vs_c, neu_vs_c, ncol = 3)

# Positive and negative words to comments ratios
p_vs_n <- ggplot() +
  geom_line(data=survey_inf, aes(x=Survey, y=pw_to_c_ratio, color = "Positive", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=nw_to_c_ratio, color = "Negative", group=1), size=2) +
  geom_line(data=survey_inf, aes(x=Survey, y=neu_to_c_ratio, color = "Neutral", group=1), size=2) +
  scale_colour_manual("", 
                      breaks = c("Positive Words", "Negative Words", "Neutral Words"),
                      values = c("indianred4", "gray40", "green4")) +
  labs(title = "Ratios of Positive Negative & Neutral Words to Comments", subtitle = "Ratio Comparisons by Survey") +
  ylab("# Words / # Comments") 

# Arrange the two plots for pasting into deck
grid.arrange(p_vs_n, ncol = 2)


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


