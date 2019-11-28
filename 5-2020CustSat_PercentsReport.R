#*******************************************************************************
#
# Customer Sat data Analysis Project
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/tsg-projects-2019_05-cust-sat.git
#
#*******************************************************************************

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

################################################################################
#
# File open, cleanup, and set up for the analysis
#
################################################################################

#
# Download and open survey file
#

# Import and Open the data file / Establish the data set
data_filename <- gs_title("Test 2020 Survey")
dat <- gs_read(data_filename, stringsAsFactors = FALSE)

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

# Remove rows below the actual data / Remove Comments
dat <- subset(dat, dat[ , 1] != "") %>% select(-Comments, -Email) 

# Convert text to numbers
dat[dat == "1 - Strongly Disagree"]  <- 1
dat[dat == "4 - Disagree"]           <- 4
dat[dat == "7 - Agree"]              <- 7
dat[dat == "10 - Strongly Agree"]    <- 10
dat[dat == "NA or Do Not Use"]       <- 0
dat[is.na(dat)]                      <- 0
dat[dat == "0"]                      <- 0
dat[dat == ""]                       <- 0

################################################################################
#
# Create dataframes for reporting
#
################################################################################

# Create the dataframes to hold all the results
res_df     <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)

res_df_am  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)
res_df_ba  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)
res_df_bm  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)
res_df_ps  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)
res_df_sd  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)
res_df_vm  <- data.frame(Qtr = 1:6,   Grp = 1:6,   Q = 1:6,   Avg = 1:6)

grp_x_qtr  <- list(res_df_am, res_df_ba, res_df_bm, res_df_ps, res_df_sd, res_df_vm)
full_year  <- list(Q1 = grp_x_qtr, Q2 = grp_x_qtr, Q3 = grp_x_qtr, Q4 = grp_x_qtr)
groups <- c("AM", "BA", "BM", "PS", "SD", "VM")
questions <- c("1A", "1B", "1C", "2A", "2B", "3x")

#-------------------------------------------------------------------------------
#
# Work through each breakdown - by quarter, then group, then question
#
#-------------------------------------------------------------------------------

am_results  <- transform(dat[c(1, 2, 8,14,20,26,32)])
ba_results  <- transform(dat[c(1, 3, 9,15,21,27,33)])
bm_results  <- transform(dat[c(1, 4,10,16,22,28,34)])
ps_results  <- transform(dat[c(1, 5,11,17,23,29,35)])
sd_results  <- transform(dat[c(1, 6,12,18,24,30,36)])
vm_results  <- transform(dat[c(1, 7,13,19,25,31,37)])

results_list <- list(am_results, ba_results, bm_results, ps_results, sd_results, vm_results)



#
# Process each group, quarter by quarter
#

# Establish the quarter
for(a in 1:length(unique(dat$Surveyed))) {
  quarter <- unique(dat$Surveyed)[a]
  
  # Outer loop to establish the group and data
  for(i in 1:length(groups)) {
    group <- groups[i]
    group_data <- as.data.frame(results_list[i])
    group_data <- group_data %>% filter(Surveyed == quarter)
    group_data <- group_data %>% select(-Surveyed)
  
   # Establish the number of responses in this group
    x <- nrow(group_data)
    
    # Create a dataframe to hold the six answers as numberics
    new_df <- data.frame(Q1A = 1:x, Q1B = 1:x, Q1C = 1:x, Q2A = 1:x, Q2B = 1:x, Q3x = 1:x)
    
    # Next loop to write the numeric values to the new dataframe
    for(j in 1:x) {
      for(k in 1:6) {
        if(group_data[j,k] == "0") {
          new_df[j,k] = 0
          }
        if(group_data[j,k] == "1") {
          new_df[j,k] = 1
        }
        if(group_data[j,k] == "2") {
          new_df[j,k] = 2
        }
        if(group_data[j,k] == "3") {
          new_df[j,k] = 3
        }
        if(group_data[j,k] == "4") {
          new_df[j,k] = 4
        }
        if(group_data[j,k] == "5") {
          new_df[j,k] = 5
        }
        if(group_data[j,k] == "6") {
          new_df[j,k] = 6
        }
        if(group_data[j,k] == "7") {
          new_df[j,k] = 7
        }
        if(group_data[j,k] == "8") {
          new_df[j,k] = 8
        }
        if(group_data[j,k] == "9") {
          new_df[j,k] = 9
        }
        if(group_data[j,k] == "10") {
          new_df[j,k] = 10
        }
      }
    }
    for(m in 1:6) {
      
      # Clear out the NR and DNUs
      y <- new_df[m] == 0
      z <- length(y[y == TRUE])
      
      # Calculate the mean
      mean_new_df <- round(sum(new_df[m]) / (x - z), digits = 2)
      
      # write the results to the dataframe
      res_df[m,1]  <- quarter
      res_df[m,2]  <- group
      res_df[m,3]  <- questions[m]
      res_df[m,4]  <- mean_new_df
    }
    # Now we have the entire quarter for each group
    grp_x_qtr[[i]] <- res_df
  }
  # Until I can figure out how to access parts of a list by indexing alone, we 
  # loop through to make sure the results for the quarter to to the right place
  if(a == 1) {
    full_year$Q1 <- grp_x_qtr
  }
  if(a == 2) {
    full_year$Q2 <- grp_x_qtr
  }
  if(a == 3) {
    full_year$Q3 <- grp_x_qtr
  }
  if(a == 4) {
    full_year$Q4 <- grp_x_qtr
  }
}

cat("Summary for 1st Quarter")

kable(full_year$Q1) %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

cat("Summary for 2nd Quarter")

kable(full_year$Q2) %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

cat("Summary for 3rd Quarter")

kable(full_year$Q3) %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

cat("Summary for 4th Quarter")

kable(full_year$Q4) %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# Verify the number of unique values of the various factors
sapply(dat, function(x)length(unique(x)))

################################################################################
#
# Graphics 
#
################################################################################

#
# Set up dataframe to collect the mean for each question by group
#

fy_df <- data.frame(Qtr = 1:24, Group = 1:24, Avg = 1:24)

# Populate the first two colums
fy_df[ 1:6, 1]  <- unique(dat$Surveyed)[1]
fy_df[ 7:12,1]  <- unique(dat$Surveyed)[2]
fy_df[13:18,1]  <- unique(dat$Surveyed)[3]
fy_df[19:24,1]  <- unique(dat$Surveyed)[4]

fy_df[ 1:6, 2] <- t(groups)[ ,1:6]
fy_df[ 7:12,2] <- t(groups)[ ,1:6]
fy_df[13:18,2] <- t(groups)[ ,1:6]
fy_df[19:24,2] <- t(groups)[ ,1:6]

#
# Set up the dataframe to collect data for summary of questions
#

qs_df <- data.frame(Group = 1:24, Q1A = 1:24, Q1B = 1:24, Q1C = 1:24, Q2A = 1:24, Q2B = 1:24, Q3x = 1:24)
qs_df[ 1:6, 1] <- t(groups)[ ,1:6]
qs_df[ 7:12,1] <- t(groups)[ ,1:6]
qs_df[13:18,1] <- t(groups)[ ,1:6]
qs_df[19:24,1] <- t(groups)[ ,1:6]

#-------------------------------------------------------------------------------
#
# Part 1 - Quarterly Summaries
#    8 charts total (2 / quarter)
#
#    For each Quarter (4) Question Response Averages by group 
#    X = Group
#    Y = Average Score
#
#    For each quarter (4) Group Averages by Question
#    X = Question
#    Y - Average Score
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#
#  Response Averages by Group
#
#-------------------------------------------------------------------------------

# Q1 Assemble data
gbq_q1_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[1])
for(i in 1:6) {
  gbq_q1_df[i,3] <- round(mean(full_year$Q1[[i]]$Avg), digits = 2)
}

# Q1 Build plot
gbq_q1_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q1_df, stat = "identity") +
  geom_text(data = gbq_q1_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q1 (Fall 2019) - By Group")     

# Q2 Assemble data
gbq_q2_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[2])
for(i in 1:6) {
  gbq_q2_df[i,3] <- round(mean(full_year$Q2[[i]]$Avg), digits = 2)
}

# Q2 Build plot
gbq_q2_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q2_df, stat = "identity") +
  geom_text(data = gbq_q2_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q2 (Winter 2020) - By Group")  

# Q3 Assemble data
gbq_q3_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[3])
for(i in 1:6) {
  gbq_q3_df[i,3] <- round(mean(full_year$Q3[[i]]$Avg), digits = 2)
}

# Q3 Build plot
gbq_q3_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q3_df, stat = "identity") +
  geom_text(data = gbq_q3_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q3 (Spring 2020) - By Group") 

# Q4 Assemble data
gbq_q4_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[4])
for(i in 1:6) {
  gbq_q4_df[i,3] <- round(mean(full_year$Q4[[i]]$Avg), digits = 2)
}

# Q4 Build plot
gbq_q4_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q4_df, stat = "identity") +
  geom_text(data = gbq_q4_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q4 (Summer 2020) - By Group")

#-------------------------------------------------------------------------------
#
#  Response Averages by Question
#
#-------------------------------------------------------------------------------

# Q1 Gather data
qbq_q1_df <- data.frame(Question = 1:6, Avg = 1:6)

x = 0
for(i in 1:6) {
  qbq_q1_df[i,1] <- questions[i]
  for(j in 1:6) {
    x <- x + full_year$Q1[[j]]$Avg[i]
  }
  x <- x / 6
  qbq_q1_df[i,2] <- round(x, digits = 2)
  x = 0
}

# Q1 Build plot
qbq_q1_bar <- ggplot() +
  geom_bar(aes(x = Question, y = Avg, fill = "red"),
           data = qbq_q1_df, stat = "identity") +
  geom_text(data = qbq_q1_df, aes(x = Question, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q1 (Fall 2019) - By Question") 

# Q2 Gather data
qbq_q2_df <- data.frame(Question = 1:6, Avg = 1:6)

x = 0
for(i in 1:6) {
  qbq_q2_df[i,1] <- questions[i]
  for(j in 1:6) {
    x <- x + full_year$Q2[[j]]$Avg[i]
  }
  x <- x / 6
  qbq_q2_df[i,2] <- round(x, digits = 2)
  x = 0
}

# Q2 Build plot
qbq_q2_bar <- ggplot() +
  geom_bar(aes(x = Question, y = Avg, fill = "red"),
           data = qbq_q2_df, stat = "identity") +
  geom_text(data = qbq_q2_df, aes(x = Question, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q2 (Winter 2020) - By Question") 


# Q3 Gather data
qbq_q3_df <- data.frame(Question = 1:6, Avg = 1:6)

x = 0
for(i in 1:6) {
  qbq_q3_df[i,1] <- questions[i]
  for(j in 1:6) {
    x <- x + full_year$Q3[[j]]$Avg[i]
  }
  x <- x / 6
  qbq_q3_df[i,2] <- round(x, digits = 2)
  x = 0
}

# Q3 Build plot
qbq_q3_bar <- ggplot() +
  geom_bar(aes(x = Question, y = Avg, fill = "red"),
           data = qbq_q3_df, stat = "identity") +
  geom_text(data = qbq_q3_df, aes(x = Question, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q3 (Spring 2020) - By Question") 


# Q4 Gather data
qbq_q4_df <- data.frame(Question = 1:6, Avg = 1:6)

x = 0
for(i in 1:6) {
  qbq_q4_df[i,1] <- questions[i]
  for(j in 1:6) {
    x <- x + full_year$Q4[[j]]$Avg[i]
  }
  x <- x / 6
  qbq_q4_df[i,2] <- round(x, digits = 2)
  x = 0
}

# Q4 Build plot
qbq_q4_bar <- ggplot() +
  geom_bar(aes(x = Question, y = Avg, fill = "red"),
           data = qbq_q4_df, stat = "identity") +
  geom_text(data = qbq_q4_df, aes(x = Question, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Response Averages", subtitle = "Q4 (Summer 2020) - By Question") 


# Arrange the grids
grid.arrange(gbq_q1_bar, gbq_q2_bar, ncol = 2)
grid.arrange(gbq_q3_bar, gbq_q4_bar, ncol = 2)

grid.arrange(qbq_q1_bar, qbq_q2_bar, ncol = 2)
grid.arrange(qbq_q3_bar, qbq_q4_bar, ncol = 2)

#-------------------------------------------------------------------------------
#
# Part 2 - Quarterly Summaries by Group
#    24 charts
#    For each Quarter, 1 chart per group for Questions 1-6
#    X = Question
#    Y - Average Score
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#  Quarter 1
#    One chart per group
#-------------------------------------------------------------------------------

#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q1[1])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[1,2] <- "AM"
fy_df[1,3] <- ms

am_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q1[2])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[2,2] <- "BA"
fy_df[2,3] <- ms

ba_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q1[3])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[3,2] <- "B&MPS"
fy_df[3,3] <- ms

bm_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q1[4])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[4,2] <- "PS"
fy_df[4,3] <- ms

ps_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q1[5])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[5,2] <- "SD"
fy_df[5,3] <- ms

sd_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q1[6])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[6,2] <- "VM"
fy_df[6,3] <- ms

vm_bar_Q1 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q1 (Fall 2019) Average Score by Question - OA =",ms))

#-------------------------------------------------------------------------------
#  Quarter 2
#    One chart per group
#    X = Question
#    Y - Average Score
#-------------------------------------------------------------------------------

#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q2[1])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[7,2] <- "AM"
fy_df[7,3] <- ms

am_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q2[2])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[8,2] <- "BA"
fy_df[8,3] <- ms

ba_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q2[3])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[9,2] <- "B&MPS"
fy_df[9,3] <- ms

bm_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q2[4])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[10,2] <- "PS"
fy_df[10,3] <- ms

ps_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q2[5])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[11,2] <- "SD"
fy_df[11,3] <- ms

sd_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))


#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q2[6])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[12,2] <- "VM"
fy_df[12,3] <- ms

vm_bar_Q2 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q2 (Winter 2020) Average Score by Question - OA =",ms))

#-------------------------------------------------------------------------------
#  Quarter 3
#    One chart per group
#    X = Question
#    Y - Average Score
#-------------------------------------------------------------------------------

#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q3[1])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[13,2] <- "AM"
fy_df[13,3] <- ms

am_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q3[2])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[14,2] <- "BA"
fy_df[14,3] <- ms

ba_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q3[3])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[15,2] <- "B&MPS"
fy_df[15,3] <- ms

bm_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q3[4])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[16,2] <- "PS"
fy_df[16,3] <- ms

ps_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q3[5])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[17,2] <- "SD"
fy_df[17,3] <- ms

sd_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))


#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q3[6])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[18,2] <- "VM"
fy_df[18,3] <- ms

vm_bar_Q3 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q3 (Spring 2020) Average Score by Question - OA =",ms))


#-------------------------------------------------------------------------------
#  Quarter 4
#    One chart per group
#    X = Question
#    Y - Average Score
#-------------------------------------------------------------------------------

#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q4[1])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[19,2] <- "AM"
fy_df[19,3] <- ms

am_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q4[2])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[20,2] <- "BA"
fy_df[20,3] <- ms

ba_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q4[3])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[21,2] <- "B&MPS"
fy_df[21,3] <- ms

bm_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q4[4])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[22,2] <- "PS"
fy_df[22,3] <- ms

ps_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q4[5])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[23,2] <- "SD"
fy_df[23,3] <- ms

sd_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))


#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q4[6])
ms <- round(mean(grp_df$Avg), digits = 2)
fy_df[24,2] <- "VM"
fy_df[24,3] <- ms

vm_bar_Q4 <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q4 (Summer 2020) Average Score by Question - OA =",ms))

#-------------------------------------------------------------------------------
#
# Part 3 - Quarterly Summaries by Question
#    24 charts
#    For each Quarter, 1 chart per Question for Groups 1-6
#    X = Question
#    Y - Average Score
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#  Quarter 1
#    One chart per Question
#    For each of the 6 questions, X = Group, Y = Avg
#-------------------------------------------------------------------------------

#
# Question 1A
#

# Q1 Gather data
qbq_q1_q1a_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q1A <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q1A[1,  1] <- qbq_q1_q1a_df[1,  3]
Q1_Q1A[1,  2] <- qbq_q1_q1a_df[1,  4]
Q1_Q1A[2,  1] <- qbq_q1_q1a_df[1,  5]
Q1_Q1A[2,  2] <- qbq_q1_q1a_df[1,  6]
Q1_Q1A[3,  1] <- qbq_q1_q1a_df[1,  7]
Q1_Q1A[3,  2] <- qbq_q1_q1a_df[1,  8]
Q1_Q1A[4,  1] <- qbq_q1_q1a_df[1,  9]
Q1_Q1A[4,  2] <- qbq_q1_q1a_df[1, 10]
Q1_Q1A[5,  1] <- qbq_q1_q1a_df[1, 11]
Q1_Q1A[5,  2] <- qbq_q1_q1a_df[1, 12]
Q1_Q1A[6,  1] <- qbq_q1_q1a_df[1, 13]
Q1_Q1A[6,  2] <- qbq_q1_q1a_df[1, 14]

Q1_Q1A_ms <- round(mean(Q1_Q1A[2]), digits = 2)
  
# Q1 Build plot
Q1_Q1A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q1A, stat = "identity") +
  geom_text(data = Q1_Q1A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Q1A", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q1A_ms)) 

# Q2 Gather data
qbq_q2_q1a_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q1A <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q1A[1,  1] <- qbq_q2_q1a_df[1,  3]
Q2_Q1A[1,  2] <- qbq_q2_q1a_df[1,  4]
Q2_Q1A[2,  1] <- qbq_q2_q1a_df[1,  5]
Q2_Q1A[2,  2] <- qbq_q2_q1a_df[1,  6]
Q2_Q1A[3,  1] <- qbq_q2_q1a_df[1,  7]
Q2_Q1A[3,  2] <- qbq_q2_q1a_df[1,  8]
Q2_Q1A[4,  1] <- qbq_q2_q1a_df[1,  9]
Q2_Q1A[4,  2] <- qbq_q2_q1a_df[1, 10]
Q2_Q1A[5,  1] <- qbq_q2_q1a_df[1, 11]
Q2_Q1A[5,  2] <- qbq_q2_q1a_df[1, 12]
Q2_Q1A[6,  1] <- qbq_q2_q1a_df[1, 13]
Q2_Q1A[6,  2] <- qbq_q2_q1a_df[1, 14]

Q2_Q1A_ms <- round(mean(Q2_Q1A[2]), digits = 2)

# Q2 Build plot
Q2_Q1A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q1A, stat = "identity") +
  geom_text(data = Q2_Q1A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1A", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q1A_ms)) 

# Q3 Gather data
qbq_q3_q1a_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q1A <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q1A[1,  1] <- qbq_q3_q1a_df[1,  3]
Q3_Q1A[1,  2] <- qbq_q3_q1a_df[1,  4]
Q3_Q1A[2,  1] <- qbq_q3_q1a_df[1,  5]
Q3_Q1A[2,  2] <- qbq_q3_q1a_df[1,  6]
Q3_Q1A[3,  1] <- qbq_q3_q1a_df[1,  7]
Q3_Q1A[3,  2] <- qbq_q3_q1a_df[1,  8]
Q3_Q1A[4,  1] <- qbq_q3_q1a_df[1,  9]
Q3_Q1A[4,  2] <- qbq_q3_q1a_df[1, 10]
Q3_Q1A[5,  1] <- qbq_q3_q1a_df[1, 11]
Q3_Q1A[5,  2] <- qbq_q3_q1a_df[1, 12]
Q3_Q1A[6,  1] <- qbq_q3_q1a_df[1, 13]
Q3_Q1A[6,  2] <- qbq_q3_q1a_df[1, 14]

Q3_Q1A_ms <- round(mean(Q3_Q1A[2]), digits = 2)

# Q3 Build plot
Q3_Q1A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q1A, stat = "identity") +
  geom_text(data = Q3_Q1A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1A", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q1A_ms)) 

# Q4 Gather data
qbq_q4_q1a_df <- transform(as.data.frame(full_year$Q4)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q1A <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q1A[1,  1] <- qbq_q4_q1a_df[1,  3]
Q4_Q1A[1,  2] <- qbq_q4_q1a_df[1,  4]
Q4_Q1A[2,  1] <- qbq_q4_q1a_df[1,  5]
Q4_Q1A[2,  2] <- qbq_q4_q1a_df[1,  6]
Q4_Q1A[3,  1] <- qbq_q4_q1a_df[1,  7]
Q4_Q1A[3,  2] <- qbq_q4_q1a_df[1,  8]
Q4_Q1A[4,  1] <- qbq_q4_q1a_df[1,  9]
Q4_Q1A[4,  2] <- qbq_q4_q1a_df[1, 10]
Q4_Q1A[5,  1] <- qbq_q4_q1a_df[1, 11]
Q4_Q1A[5,  2] <- qbq_q4_q1a_df[1, 12]
Q4_Q1A[6,  1] <- qbq_q4_q1a_df[1, 13]
Q4_Q1A[6,  2] <- qbq_q4_q1a_df[1, 14]

Q4_Q1A_ms <- round(mean(Q4_Q1A[2]), digits = 2)

# Q4 Build plot
Q4_Q1A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q1A, stat = "identity") +
  geom_text(data = Q4_Q1A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1A", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q1A_ms)) 

# Arrange the Q1A grids
grid.arrange(Q1_Q1A_bar, Q2_Q1A_bar, ncol = 2)
grid.arrange(Q3_Q1A_bar, Q4_Q1A_bar, ncol = 2)

#
# Question 1B
#

# Q1 Gather data
qbq_q1_q1b_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q1B <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q1B[1,  1] <- qbq_q1_q1b_df[2,  3]
Q1_Q1B[1,  2] <- qbq_q1_q1b_df[2,  4]
Q1_Q1B[2,  1] <- qbq_q1_q1b_df[2,  5]
Q1_Q1B[2,  2] <- qbq_q1_q1b_df[2,  6]
Q1_Q1B[3,  1] <- qbq_q1_q1b_df[2,  7]
Q1_Q1B[3,  2] <- qbq_q1_q1b_df[2,  8]
Q1_Q1B[4,  1] <- qbq_q1_q1b_df[2,  9]
Q1_Q1B[4,  2] <- qbq_q1_q1b_df[2, 10]
Q1_Q1B[5,  1] <- qbq_q1_q1b_df[2, 11]
Q1_Q1B[5,  2] <- qbq_q1_q1b_df[2, 12]
Q1_Q1B[6,  1] <- qbq_q1_q1b_df[2, 13]
Q1_Q1B[6,  2] <- qbq_q1_q1b_df[2, 14]

Q1_Q1B_ms <- round(mean(Q1_Q1B[2]), digits = 2)

# Q1 Build plot
Q1_Q1B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q1B, stat = "identity") +
  geom_text(data = Q1_Q1B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1B", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q1B_ms)) 

# Q2 Gather data
qbq_q2_q1b_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q1B <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q1B[1,  1] <- qbq_q2_q1b_df[2,  3]
Q2_Q1B[1,  2] <- qbq_q2_q1b_df[2,  4]
Q2_Q1B[2,  1] <- qbq_q2_q1b_df[2,  5]
Q2_Q1B[2,  2] <- qbq_q2_q1b_df[2,  6]
Q2_Q1B[3,  1] <- qbq_q2_q1b_df[2,  7]
Q2_Q1B[3,  2] <- qbq_q2_q1b_df[2,  8]
Q2_Q1B[4,  1] <- qbq_q2_q1b_df[2,  9]
Q2_Q1B[4,  2] <- qbq_q2_q1b_df[2, 10]
Q2_Q1B[5,  1] <- qbq_q2_q1b_df[2, 11]
Q2_Q1B[5,  2] <- qbq_q2_q1b_df[2, 12]
Q2_Q1B[6,  1] <- qbq_q2_q1b_df[2, 13]
Q2_Q1B[6,  2] <- qbq_q2_q1b_df[2, 14]

Q2_Q1B_ms <- round(mean(Q2_Q1B[2]), digits = 2)

# Q2 Build plot
Q2_Q1B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q1B, stat = "identity") +
  geom_text(data = Q2_Q1B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1B", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q1A_ms)) 

# Q3 Gather data
qbq_q3_q1b_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q1B <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q1B[1,  1] <- qbq_q3_q1b_df[2,  3]
Q3_Q1B[1,  2] <- qbq_q3_q1b_df[2,  4]
Q3_Q1B[2,  1] <- qbq_q3_q1b_df[2,  5]
Q3_Q1B[2,  2] <- qbq_q3_q1b_df[2,  6]
Q3_Q1B[3,  1] <- qbq_q3_q1b_df[2,  7]
Q3_Q1B[3,  2] <- qbq_q3_q1b_df[2,  8]
Q3_Q1B[4,  1] <- qbq_q3_q1b_df[2,  9]
Q3_Q1B[4,  2] <- qbq_q3_q1b_df[2, 10]
Q3_Q1B[5,  1] <- qbq_q3_q1b_df[2, 11]
Q3_Q1B[5,  2] <- qbq_q3_q1b_df[2, 12]
Q3_Q1B[6,  1] <- qbq_q3_q1b_df[2, 13]
Q3_Q1B[6,  2] <- qbq_q3_q1b_df[2, 14]

Q3_Q1B_ms <- round(mean(Q3_Q1B[2]), digits = 2)

# Q3 Build plot
Q3_Q1B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q1B, stat = "identity") +
  geom_text(data = Q3_Q1B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1B", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q1B_ms)) 

# Q4 Gather data
qbq_q4_q1b_df <- transform(as.data.frame(full_year$Q4)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q1B <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q1B[1,  1] <- qbq_q4_q1b_df[2,  3]
Q4_Q1B[1,  2] <- qbq_q4_q1b_df[2,  4]
Q4_Q1B[2,  1] <- qbq_q4_q1b_df[2,  5]
Q4_Q1B[2,  2] <- qbq_q4_q1b_df[2,  6]
Q4_Q1B[3,  1] <- qbq_q4_q1b_df[2,  7]
Q4_Q1B[3,  2] <- qbq_q4_q1b_df[2,  8]
Q4_Q1B[4,  1] <- qbq_q4_q1b_df[2,  9]
Q4_Q1B[4,  2] <- qbq_q4_q1b_df[2, 10]
Q4_Q1B[5,  1] <- qbq_q4_q1b_df[2, 11]
Q4_Q1B[5,  2] <- qbq_q4_q1b_df[2, 12]
Q4_Q1B[6,  1] <- qbq_q4_q1b_df[2, 13]
Q4_Q1B[6,  2] <- qbq_q4_q1b_df[2, 14]

Q4_Q1B_ms <- round(mean(Q4_Q1B[2]), digits = 2)

# Q4 Build plot
Q4_Q1B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q1B, stat = "identity") +
  geom_text(data = Q4_Q1B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1B", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q1B_ms)) 

# Arrange the Q1B grids
grid.arrange(Q1_Q1B_bar, Q2_Q1B_bar, ncol = 2)
grid.arrange(Q3_Q1B_bar, Q4_Q1B_bar, ncol = 2)

#
# Question 1c
#

# Q1 Gather data
qbq_q1_q1c_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q1C <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q1C[1,  1] <- qbq_q1_q1c_df[3,  3]
Q1_Q1C[1,  2] <- qbq_q1_q1c_df[3,  4]
Q1_Q1C[2,  1] <- qbq_q1_q1c_df[3,  5]
Q1_Q1C[2,  2] <- qbq_q1_q1c_df[3,  6]
Q1_Q1C[3,  1] <- qbq_q1_q1c_df[3,  7]
Q1_Q1C[3,  2] <- qbq_q1_q1c_df[3,  8]
Q1_Q1C[4,  1] <- qbq_q1_q1c_df[3,  9]
Q1_Q1C[4,  2] <- qbq_q1_q1c_df[3, 10]
Q1_Q1C[5,  1] <- qbq_q1_q1c_df[3, 11]
Q1_Q1C[5,  2] <- qbq_q1_q1c_df[3, 12]
Q1_Q1C[6,  1] <- qbq_q1_q1c_df[3, 13]
Q1_Q1C[6,  2] <- qbq_q1_q1c_df[3, 14]

Q1_Q1C_ms <- round(mean(Q1_Q1C[2]), digits = 2)

# Q1 Build plot
Q1_Q1C_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q1C, stat = "identity") +
  geom_text(data = Q1_Q1C, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1C", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q1C_ms)) 

# Q2 Gather data
qbq_q2_q1c_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q1C <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q1C[1,  1] <- qbq_q2_q1c_df[3,  3]
Q2_Q1C[1,  2] <- qbq_q2_q1c_df[3,  4]
Q2_Q1C[2,  1] <- qbq_q2_q1c_df[3,  5]
Q2_Q1C[2,  2] <- qbq_q2_q1c_df[3,  6]
Q2_Q1C[3,  1] <- qbq_q2_q1c_df[3,  7]
Q2_Q1C[3,  2] <- qbq_q2_q1c_df[3,  8]
Q2_Q1C[4,  1] <- qbq_q2_q1c_df[3,  9]
Q2_Q1C[4,  2] <- qbq_q2_q1c_df[3, 10]
Q2_Q1C[5,  1] <- qbq_q2_q1c_df[3, 11]
Q2_Q1C[5,  2] <- qbq_q2_q1c_df[3, 12]
Q2_Q1C[6,  1] <- qbq_q2_q1c_df[3, 13]
Q2_Q1C[6,  2] <- qbq_q2_q1c_df[3, 14]

Q2_Q1C_ms <- round(mean(Q2_Q1C[2]), digits = 2)

# Q2 Build plot
Q2_Q1C_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q1C, stat = "identity") +
  geom_text(data = Q2_Q1C, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1C", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q1C_ms)) 


# Q3 Gather data
qbq_q3_q1c_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q1C <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q1C[1,  1] <- qbq_q3_q1c_df[3,  3]
Q3_Q1C[1,  2] <- qbq_q3_q1c_df[3,  4]
Q3_Q1C[2,  1] <- qbq_q3_q1c_df[3,  5]
Q3_Q1C[2,  2] <- qbq_q3_q1c_df[3,  6]
Q3_Q1C[3,  1] <- qbq_q3_q1c_df[3,  7]
Q3_Q1C[3,  2] <- qbq_q3_q1c_df[3,  8]
Q3_Q1C[4,  1] <- qbq_q3_q1c_df[3,  9]
Q3_Q1C[4,  2] <- qbq_q3_q1c_df[3, 10]
Q3_Q1C[5,  1] <- qbq_q3_q1c_df[3, 11]
Q3_Q1C[5,  2] <- qbq_q3_q1c_df[3, 12]
Q3_Q1C[6,  1] <- qbq_q3_q1c_df[3, 13]
Q3_Q1C[6,  2] <- qbq_q3_q1c_df[3, 14]

Q3_Q1C_ms <- round(mean(Q3_Q1C[2]), digits = 2)

# Q3 Build plot
Q3_Q1C_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q1C, stat = "identity") +
  geom_text(data = Q3_Q1C, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1C", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q1C_ms)) 


# Q4 Gather data
qbq_q4_q1c_df <- transform(as.data.frame(full_year$Q4)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q1C <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q1C[1,  1] <- qbq_q4_q1c_df[3,  3]
Q4_Q1C[1,  2] <- qbq_q4_q1c_df[3,  4]
Q4_Q1C[2,  1] <- qbq_q4_q1c_df[3,  5]
Q4_Q1C[2,  2] <- qbq_q4_q1c_df[3,  6]
Q4_Q1C[3,  1] <- qbq_q4_q1c_df[3,  7]
Q4_Q1C[3,  2] <- qbq_q4_q1c_df[3,  8]
Q4_Q1C[4,  1] <- qbq_q4_q1c_df[3,  9]
Q4_Q1C[4,  2] <- qbq_q4_q1c_df[3, 10]
Q4_Q1C[5,  1] <- qbq_q4_q1c_df[3, 11]
Q4_Q1C[5,  2] <- qbq_q4_q1c_df[3, 12]
Q4_Q1C[6,  1] <- qbq_q4_q1c_df[3, 13]
Q4_Q1C[6,  2] <- qbq_q4_q1c_df[3, 14]

Q4_Q1C_ms <- round(mean(Q4_Q1C[2]), digits = 2)

# Q4 Build plot
Q4_Q1C_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q1C, stat = "identity") +
  geom_text(data = Q4_Q1C, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q1C", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q1C_ms)) 


# Arrange the Q1C grids
grid.arrange(Q1_Q1C_bar, Q2_Q1C_bar, ncol = 2)
grid.arrange(Q3_Q1C_bar, Q4_Q1C_bar, ncol = 2)

#
# Question 2A
#

# Q1 Gather data
qbq_q1_q2a_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q2A <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q2A[1,  1] <- qbq_q1_q2a_df[4,  3]
Q1_Q2A[1,  2] <- qbq_q1_q2a_df[4,  4]
Q1_Q2A[2,  1] <- qbq_q1_q2a_df[4,  5]
Q1_Q2A[2,  2] <- qbq_q1_q2a_df[4,  6]
Q1_Q2A[3,  1] <- qbq_q1_q2a_df[4,  7]
Q1_Q2A[3,  2] <- qbq_q1_q2a_df[4,  8]
Q1_Q2A[4,  1] <- qbq_q1_q2a_df[4,  9]
Q1_Q2A[4,  2] <- qbq_q1_q2a_df[4, 10]
Q1_Q2A[5,  1] <- qbq_q1_q2a_df[4, 11]
Q1_Q2A[5,  2] <- qbq_q1_q2a_df[4, 12]
Q1_Q2A[6,  1] <- qbq_q1_q2a_df[4, 13]
Q1_Q2A[6,  2] <- qbq_q1_q2a_df[4, 14]

Q1_Q2A_ms <- round(mean(Q1_Q2A[2]), digits = 2)

# Q1 Build plot
Q1_Q2A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q2A, stat = "identity") +
  geom_text(data = Q1_Q2A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2A", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q2A_ms)) 

# Q2 Gather data
qbq_q2_q2a_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q2A <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q2A[1,  1] <- qbq_q2_q2a_df[4,  3]
Q2_Q2A[1,  2] <- qbq_q2_q2a_df[4,  4]
Q2_Q2A[2,  1] <- qbq_q2_q2a_df[4,  5]
Q2_Q2A[2,  2] <- qbq_q2_q2a_df[4,  6]
Q2_Q2A[3,  1] <- qbq_q2_q2a_df[4,  7]
Q2_Q2A[3,  2] <- qbq_q2_q2a_df[4,  8]
Q2_Q2A[4,  1] <- qbq_q2_q2a_df[4,  9]
Q2_Q2A[4,  2] <- qbq_q2_q2a_df[4, 10]
Q2_Q2A[5,  1] <- qbq_q2_q2a_df[4, 11]
Q2_Q2A[5,  2] <- qbq_q2_q2a_df[4, 12]
Q2_Q2A[6,  1] <- qbq_q2_q2a_df[4, 13]
Q2_Q2A[6,  2] <- qbq_q2_q2a_df[4, 14]

Q2_Q2A_ms <- round(mean(Q2_Q2A[2]), digits = 2)

# Q2 Build plot
Q2_Q2A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q2A, stat = "identity") +
  geom_text(data = Q2_Q2A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2A", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q2A_ms))
       
# Q3 Gather data
qbq_q3_q2a_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q2A <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q2A[1,  1] <- qbq_q3_q2a_df[4,  3]
Q3_Q2A[1,  2] <- qbq_q3_q2a_df[4,  4]
Q3_Q2A[2,  1] <- qbq_q3_q2a_df[4,  5]
Q3_Q2A[2,  2] <- qbq_q3_q2a_df[4,  6]
Q3_Q2A[3,  1] <- qbq_q3_q2a_df[4,  7]
Q3_Q2A[3,  2] <- qbq_q3_q2a_df[4,  8]
Q3_Q2A[4,  1] <- qbq_q3_q2a_df[4,  9]
Q3_Q2A[4,  2] <- qbq_q3_q2a_df[4, 10]
Q3_Q2A[5,  1] <- qbq_q3_q2a_df[4, 11]
Q3_Q2A[5,  2] <- qbq_q3_q2a_df[4, 12]
Q3_Q2A[6,  1] <- qbq_q3_q2a_df[4, 13]
Q3_Q2A[6,  2] <- qbq_q3_q2a_df[4, 14]

Q3_Q2A_ms <- round(mean(Q3_Q2A[2]), digits = 2)

# Q3 Build plot
Q3_Q2A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q2A, stat = "identity") +
  geom_text(data = Q3_Q2A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2A", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q2A_ms))

# Q4 Gather data
qbq_q4_q2a_df <- transform(as.data.frame(full_year$Q4)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q2A <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q2A[1,  1] <- qbq_q4_q2a_df[4,  3]
Q4_Q2A[1,  2] <- qbq_q4_q2a_df[4,  4]
Q4_Q2A[2,  1] <- qbq_q4_q2a_df[4,  5]
Q4_Q2A[2,  2] <- qbq_q4_q2a_df[4,  6]
Q4_Q2A[3,  1] <- qbq_q4_q2a_df[4,  7]
Q4_Q2A[3,  2] <- qbq_q4_q2a_df[4,  8]
Q4_Q2A[4,  1] <- qbq_q4_q2a_df[4,  9]
Q4_Q2A[4,  2] <- qbq_q4_q2a_df[4, 10]
Q4_Q2A[5,  1] <- qbq_q4_q2a_df[4, 11]
Q4_Q2A[5,  2] <- qbq_q4_q2a_df[4, 12]
Q4_Q2A[6,  1] <- qbq_q4_q2a_df[4, 13]
Q4_Q2A[6,  2] <- qbq_q4_q2a_df[4, 14]

Q4_Q2A_ms <- round(mean(Q4_Q2A[2]), digits = 2)

# Q4 Build plot
Q4_Q2A_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q2A, stat = "identity") +
  geom_text(data = Q4_Q2A, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2A", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q2A_ms))


# Arrange the Q2A grids
grid.arrange(Q1_Q2A_bar, Q2_Q2A_bar, ncol = 2)
grid.arrange(Q3_Q2A_bar, Q4_Q2A_bar, ncol = 2)

#
# Question 2B
#

# Q1 Gather data
qbq_q1_q2b_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q2B <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q2B[1,  1] <- qbq_q1_q2b_df[5,  3]
Q1_Q2B[1,  2] <- qbq_q1_q2b_df[5,  4]
Q1_Q2B[2,  1] <- qbq_q1_q2b_df[5,  5]
Q1_Q2B[2,  2] <- qbq_q1_q2b_df[5,  6]
Q1_Q2B[3,  1] <- qbq_q1_q2b_df[5,  7]
Q1_Q2B[3,  2] <- qbq_q1_q2b_df[5,  8]
Q1_Q2B[4,  1] <- qbq_q1_q2b_df[5,  9]
Q1_Q2B[4,  2] <- qbq_q1_q2b_df[5, 10]
Q1_Q2B[5,  1] <- qbq_q1_q2b_df[5, 11]
Q1_Q2B[5,  2] <- qbq_q1_q2b_df[5, 12]
Q1_Q2B[6,  1] <- qbq_q1_q2b_df[5, 13]
Q1_Q2B[6,  2] <- qbq_q1_q2b_df[5, 14]

Q1_Q2B_ms <- round(mean(Q1_Q2B[2]), digits = 2)

# Q1 Build plot
Q1_Q2B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q2B, stat = "identity") +
  geom_text(data = Q1_Q2B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2B", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q2B_ms)) 

# Q2 Gather data
qbq_q2_q2b_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q2B <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q2B[1,  1] <- qbq_q2_q2b_df[5,  3]
Q2_Q2B[1,  2] <- qbq_q2_q2b_df[5,  4]
Q2_Q2B[2,  1] <- qbq_q2_q2b_df[5,  5]
Q2_Q2B[2,  2] <- qbq_q2_q2b_df[5,  6]
Q2_Q2B[3,  1] <- qbq_q2_q2b_df[5,  7]
Q2_Q2B[3,  2] <- qbq_q2_q2b_df[5,  8]
Q2_Q2B[4,  1] <- qbq_q2_q2b_df[5,  9]
Q2_Q2B[4,  2] <- qbq_q2_q2b_df[5, 10]
Q2_Q2B[5,  1] <- qbq_q2_q2b_df[5, 11]
Q2_Q2B[5,  2] <- qbq_q2_q2b_df[5, 12]
Q2_Q2B[6,  1] <- qbq_q2_q2b_df[5, 13]
Q2_Q2B[6,  2] <- qbq_q2_q2b_df[5, 14]

Q2_Q2B_ms <- round(mean(Q2_Q2B[2]), digits = 2)

# Q2 Build plot
Q2_Q2B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q2B, stat = "identity") +
  geom_text(data = Q2_Q2B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2B", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q2B_ms)) 

# Q3 Gather data
qbq_q3_q2b_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q2B <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q2B[1,  1] <- qbq_q3_q2b_df[5,  3]
Q3_Q2B[1,  2] <- qbq_q3_q2b_df[5,  4]
Q3_Q2B[2,  1] <- qbq_q3_q2b_df[5,  5]
Q3_Q2B[2,  2] <- qbq_q3_q2b_df[5,  6]
Q3_Q2B[3,  1] <- qbq_q3_q2b_df[5,  7]
Q3_Q2B[3,  2] <- qbq_q3_q2b_df[5,  8]
Q3_Q2B[4,  1] <- qbq_q3_q2b_df[5,  9]
Q3_Q2B[4,  2] <- qbq_q3_q2b_df[5, 10]
Q3_Q2B[5,  1] <- qbq_q3_q2b_df[5, 11]
Q3_Q2B[5,  2] <- qbq_q3_q2b_df[5, 12]
Q3_Q2B[6,  1] <- qbq_q3_q2b_df[5, 13]
Q3_Q2B[6,  2] <- qbq_q3_q2b_df[5, 14]

Q3_Q2B_ms <- round(mean(Q3_Q2B[2]), digits = 2)

# Q3 Build plot
Q3_Q2B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q2B, stat = "identity") +
  geom_text(data = Q3_Q2B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2B", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q2B_ms)) 

# Q4 Gather data
qbq_q4_q2b_df <- transform(as.data.frame(full_year$Q4)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q2B <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q2B[1,  1] <- qbq_q4_q2b_df[5,  3]
Q4_Q2B[1,  2] <- qbq_q4_q2b_df[5,  4]
Q4_Q2B[2,  1] <- qbq_q4_q2b_df[5,  5]
Q4_Q2B[2,  2] <- qbq_q4_q2b_df[5,  6]
Q4_Q2B[3,  1] <- qbq_q4_q2b_df[5,  7]
Q4_Q2B[3,  2] <- qbq_q4_q2b_df[5,  8]
Q4_Q2B[4,  1] <- qbq_q4_q2b_df[5,  9]
Q4_Q2B[4,  2] <- qbq_q4_q2b_df[5, 10]
Q4_Q2B[5,  1] <- qbq_q4_q2b_df[5, 11]
Q4_Q2B[5,  2] <- qbq_q4_q2b_df[5, 12]
Q4_Q2B[6,  1] <- qbq_q4_q2b_df[5, 13]
Q4_Q2B[6,  2] <- qbq_q4_q2b_df[5, 14]

Q4_Q2B_ms <- round(mean(Q4_Q2B[2]), digits = 2)

# Q4 Build plot
Q4_Q2B_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q2B, stat = "identity") +
  geom_text(data = Q4_Q2B, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question Q2B", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q2B_ms)) 

# Arrange the Q2B grids
grid.arrange(Q1_Q2B_bar, Q2_Q2B_bar, ncol = 2)
grid.arrange(Q3_Q2B_bar, Q4_Q2B_bar, ncol = 2)

#
# Question 3x
#

# Q1 Gather data
qbq_q1_q3x_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q1_Q3x <- data.frame(Group = 1:6, Avg = 1:6)

Q1_Q3x[1,  1] <- qbq_q1_q3x_df[6,  3]
Q1_Q3x[1,  2] <- qbq_q1_q3x_df[6,  4]
Q1_Q3x[2,  1] <- qbq_q1_q3x_df[6,  5]
Q1_Q3x[2,  2] <- qbq_q1_q3x_df[6,  6]
Q1_Q3x[3,  1] <- qbq_q1_q3x_df[6,  7]
Q1_Q3x[3,  2] <- qbq_q1_q3x_df[6,  8]
Q1_Q3x[4,  1] <- qbq_q1_q3x_df[6,  9]
Q1_Q3x[4,  2] <- qbq_q1_q3x_df[6, 10]
Q1_Q3x[5,  1] <- qbq_q1_q3x_df[6, 11]
Q1_Q3x[5,  2] <- qbq_q1_q3x_df[6, 12]
Q1_Q3x[6,  1] <- qbq_q1_q3x_df[6, 13]
Q1_Q3x[6,  2] <- qbq_q1_q3x_df[6, 14]

Q1_Q3x_ms <- round(mean(Q1_Q3x[2]), digits = 2)

# Q1 Build plot
Q1_Q3x_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q1_Q3x, stat = "identity") +
  geom_text(data = Q1_Q3x, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question 3x", subtitle = paste("Q1 (Fall 2019) Averages by Group - OA =", Q1_Q3x_ms)) 

# Q2 Gather data
qbq_q2_q3x_df <- transform(as.data.frame(full_year$Q2)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q2_Q3x <- data.frame(Group = 1:6, Avg = 1:6)

Q2_Q3x[1,  1] <- qbq_q2_q3x_df[6,  3]
Q2_Q3x[1,  2] <- qbq_q2_q3x_df[6,  4]
Q2_Q3x[2,  1] <- qbq_q2_q3x_df[6,  5]
Q2_Q3x[2,  2] <- qbq_q2_q3x_df[6,  6]
Q2_Q3x[3,  1] <- qbq_q2_q3x_df[6,  7]
Q2_Q3x[3,  2] <- qbq_q2_q3x_df[6,  8]
Q2_Q3x[4,  1] <- qbq_q2_q3x_df[6,  9]
Q2_Q3x[4,  2] <- qbq_q2_q3x_df[6, 10]
Q2_Q3x[5,  1] <- qbq_q2_q3x_df[6, 11]
Q2_Q3x[5,  2] <- qbq_q2_q3x_df[6, 12]
Q2_Q3x[6,  1] <- qbq_q2_q3x_df[6, 13]
Q2_Q3x[6,  2] <- qbq_q2_q3x_df[6, 14]

Q2_Q3x_ms <- round(mean(Q2_Q3x[2]), digits = 2)

# Q2 Build plot
Q2_Q3x_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q2_Q3x, stat = "identity") +
  geom_text(data = Q2_Q3x, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question 3x", subtitle = paste("Q2 (Winter 2020) Averages by Group - OA =", Q2_Q3x_ms)) 

# Q3 Gather data
qbq_q3_q3x_df <- transform(as.data.frame(full_year$Q3)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q3_Q3x <- data.frame(Group = 1:6, Avg = 1:6)

Q3_Q3x[1,  1] <- qbq_q3_q3x_df[6,  3]
Q3_Q3x[1,  2] <- qbq_q3_q3x_df[6,  4]
Q3_Q3x[2,  1] <- qbq_q3_q3x_df[6,  5]
Q3_Q3x[2,  2] <- qbq_q3_q3x_df[6,  6]
Q3_Q3x[3,  1] <- qbq_q3_q3x_df[6,  7]
Q3_Q3x[3,  2] <- qbq_q3_q3x_df[6,  8]
Q3_Q3x[4,  1] <- qbq_q3_q3x_df[6,  9]
Q3_Q3x[4,  2] <- qbq_q3_q3x_df[6, 10]
Q3_Q3x[5,  1] <- qbq_q3_q3x_df[6, 11]
Q3_Q3x[5,  2] <- qbq_q3_q3x_df[6, 12]
Q3_Q3x[6,  1] <- qbq_q3_q3x_df[6, 13]
Q3_Q3x[6,  2] <- qbq_q3_q3x_df[6, 14]

Q3_Q3x_ms <- round(mean(Q3_Q3x[2]), digits = 2)

# Q3 Build plot
Q3_Q3x_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q3_Q3x, stat = "identity") +
  geom_text(data = Q3_Q3x, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question 3x", subtitle = paste("Q3 (Spring 2020) Averages by Group - OA =", Q3_Q3x_ms)) 

# Q4 Gather data
qbq_q4_q3x_df <- transform(as.data.frame(full_year$Q1)[c(1,3, 2,4,6,8,10,12,14,16,18,20,22,24)])
Q4_Q3x <- data.frame(Group = 1:6, Avg = 1:6)

Q4_Q3x[1,  1] <- qbq_q4_q3x_df[6,  3]
Q4_Q3x[1,  2] <- qbq_q4_q3x_df[6,  4]
Q4_Q3x[2,  1] <- qbq_q4_q3x_df[6,  5]
Q4_Q3x[2,  2] <- qbq_q4_q3x_df[6,  6]
Q4_Q3x[3,  1] <- qbq_q4_q3x_df[6,  7]
Q4_Q3x[3,  2] <- qbq_q4_q3x_df[6,  8]
Q4_Q3x[4,  1] <- qbq_q4_q3x_df[6,  9]
Q4_Q3x[4,  2] <- qbq_q4_q3x_df[6, 10]
Q4_Q3x[5,  1] <- qbq_q4_q3x_df[6, 11]
Q4_Q3x[5,  2] <- qbq_q4_q3x_df[6, 12]
Q4_Q3x[6,  1] <- qbq_q4_q3x_df[6, 13]
Q4_Q3x[6,  2] <- qbq_q4_q3x_df[6, 14]

Q4_Q3x_ms <- round(mean(Q4_Q3x[2]), digits = 2)

# Q4 Build plot
Q4_Q3x_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = Q4_Q3x, stat = "identity") +
  geom_text(data = Q4_Q3x, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(legend.position = "none") +
  labs(title = "Question 3x", subtitle = paste("Q4 (Summer 2020) Averages by Group - OA =", Q4_Q3x_ms)) 

# Arrange the Q3x grids
grid.arrange(Q1_Q3x_bar, Q2_Q3x_bar, ncol = 2)
grid.arrange(Q3_Q3x_bar, Q4_Q3x_bar, ncol = 2)

#-------------------------------------------------------------------------------
#
# Full Year Summary stacked bar charts
#
#-------------------------------------------------------------------------------

# Groiups Chart
full_year_groups <- ggplot(data = fy_df, aes(x = Group, y = Avg, fill = Qtr)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label = Avg), angle = 90, color="black",
            position = position_dodge(0.9), size=3.5) +
  labs(title = "Full Year Average Scores by Group") +
  scale_fill_brewer(palette="Reds")

# Questions Chart
fy_q1 <- qbq_q1_df %>% mutate(Quarter = unique(dat$Surveyed)[1])
fy_q2 <- qbq_q2_df %>% mutate(Quarter = unique(dat$Surveyed)[2])
fy_q3 <- qbq_q3_df %>% mutate(Quarter = unique(dat$Surveyed)[3])
fy_q4 <- qbq_q4_df %>% mutate(Quarter = unique(dat$Surveyed)[4])
fy_questions <- fy_q1
fy_questions <- rbind(fy_questions, fy_q2)
fy_questions <- rbind(fy_questions, fy_q3)
fy_questions <- rbind(fy_questions, fy_q4)

full_year_questions <- ggplot(data = fy_questions, aes(x = Question, y = Avg, fill = Quarter)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label = Avg), angle = 90, color="black",
            position = position_dodge(0.9), size=3.5) +
  labs(title = "Full Year Average Scores by Question") +
  scale_fill_brewer(palette="Reds")

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


