##############################################################################
#
# Customer Sat data Analysis Project
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
library(googlesheets)
library(purrr)

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
#
# Work through each breakdown - by quarter, then group, then question
#
#-------------------------------------------------------------------------------

# By Quarter
# q1_results  <- dat %>% filter(Surveyed == "1F19")
# q2_results  <- dat %>% filter(Surveyed == "2W20")
# q3_results  <- dat %>% filter(Surveyed == "3S20")
# q4_results  <- dat %>% filter(Surveyed == "4S20")

am_results  <- transform(dat[c(1, 2, 8,14,20,26,32)])
ba_results  <- transform(dat[c(1, 3, 9,15,21,27,33)])
bm_results  <- transform(dat[c(1, 4,10,16,22,28,34)])
ps_results  <- transform(dat[c(1, 5,11,17,23,29,35)])
sd_results  <- transform(dat[c(1, 6,12,18,24,30,36)])
vm_results  <- transform(dat[c(1, 7,13,19,25,31,37)])

results_list <- list(am_results, ba_results, bm_results, ps_results, sd_results, vm_results)

groups <- c("AM", "BA", "BM", "PS", "SD", "VM")
questions <- c("1A", "1B", "1C", "2A", "2B", "3x")

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
      mean_new_df <- round(sum(new_df[m]) / (x - z), digits = 1)
      
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

cat("Summary for 2nd Quarter")

kable(full_year$Q4) %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# Verify the number of unique values of the various factors
sapply(dat, function(x)length(unique(x)))

#--------------------------------------------------------------------
#
# Graphics - Groups by Quarter
#
#--------------------------------------------------------------------

# Set up dataframe to collect the mean for each question for each group for each quarter
fy_df <- data.frame(Qtr = 1:24, Group = 1:24, Avg = 1:24)

# Populate the first two colums
fy_df[ 1:6, 1]  <- unique(dat$Surveyed)[1]
fy_df[ 7:12,1]  <- unique(dat$Surveyed)[2]
fy_df[13:18,1]  <- unique(dat$Surveyed)[3]
fy_df[19:24,1]  <- unique(dat$Surveyed)[4]


#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q1[1])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[1,2] <- "AM"
fy_df[1,3] <- ms

am_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q1[2])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[2,2] <- "BA"
fy_df[2,3] <- ms

ba_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q1[3])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[3,2] <- "B&MPS"
fy_df[3,3] <- ms

bm_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q1[4])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[4,2] <- "PS"
fy_df[4,3] <- ms

ps_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q1[5])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[5,2] <- "SD"
fy_df[5,3] <- ms

sd_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q1[6])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[6,2] <- "VM"
fy_df[6,3] <- ms

vm_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q1 Average Score by Question - Overall Average is",ms))

#-------------------------------------------------------------------------------
#
# Quarter 2
#
#-------------------------------------------------------------------------------

#
# Account Managers
#

grp_df <- as.data.frame(full_year$Q2[1])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[7,2] <- "AM"
fy_df[7,3] <- ms

am_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))

#
# business Analysts
#

grp_df <- as.data.frame(full_year$Q2[2])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[8,2] <- "BA"
fy_df[8,3] <- ms

ba_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Business Analysts", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))

#
# B&MPS
#

grp_df <- as.data.frame(full_year$Q2[3])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[9,2] <- "B&MPS"
fy_df[9,3] <- ms

bm_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))

#
# Project Support
#

grp_df <- as.data.frame(full_year$Q2[4])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[10,2] <- "PS"
fy_df[10,3] <- ms

ps_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))

#
# Service Desk
#

grp_df <- as.data.frame(full_year$Q2[5])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[11,2] <- "SD"
fy_df[11,3] <- ms

sd_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))


#
# Vendor Managers
#

grp_df <- as.data.frame(full_year$Q2[6])
ms <- round(mean(grp_df$Avg), digits = 1)
fy_df[12,2] <- "VM"
fy_df[12,3] <- ms

vm_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Q2 Average Score by Question - Overall Average is",ms))


#-------------------
# Groups by quarter
#-------------------

#
# Quarter 1 (Fall)
#

# Assemble data
gbq_q1_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[1])

# Build plot
gbq_q1_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q1_df, stat = "identity") +
  geom_text(data = gbq_q1_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Quarterly Group Comparison", subtitle = "Q1F19")                 

#
# Quarter 2 (Winter)
#

# Assemble data
gbq_q2_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[2])

# Build plot
gbq_q2_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q2_df, stat = "identity") +
  geom_text(data = gbq_q2_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Quarterly Group Comparison", subtitle = "Q2W20")   

# Arrange the grids
grid.arrange(gbq_q1_bar, gbq_q2_bar, ncol = 2)

#
# Quarter 3 (Spring)
#

# Assemble data
gbq_q3_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[3])

# Build plot
gbq_q3_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q3_df, stat = "identity") +
  geom_text(data = gbq_q3_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Quarterly Group Comparison", subtitle = "Q3S20")   

#
# Quarter 4 (sUMMER)
#

# Assemble data
gbq_q4_df <- fy_df %>% filter(Qtr == unique(dat$Surveyed)[4])

# Build plot
gbq_q4_bar <- ggplot() +
  geom_bar(aes(x = Group, y = Avg, fill = "red"),
           data = gbq_q4_df, stat = "identity") +
  geom_text(data = gbq_q4_df, aes(x = Group, y = Avg, label = Avg), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Quarterly Group Comparison", subtitle = "Q4S20")   

# Arrange the grids
grid.arrange(gbq_q3_bar, gbq_q4_bar, ncol = 2)

#-------------------
# Multiple quarters bar charts
#-------------------

#-------------------
# Multiple quarters bar charts
#-------------------

grp_q1_df <- as.data.frame(full_year$Q1[6])
grp_q2_df <- as.data.frame(full_year$Q2[6])
grp_q3_df <- as.data.frame(full_year$Q3[6])
grp_q4_df <- as.data.frame(full_year$Q4[6])

ms_q1 <- round(mean(grp_q1_df$Avg), digits = 1)
ms_q2 <- round(mean(grp_q2_df$Avg), digits = 1)
ms_q3 <- round(mean(grp_q3_df$Avg), digits = 1)
ms_q4 <- round(mean(grp_q4_df$Avg), digits = 1)

vm_bar <- ggplot() +
  geom_bar(aes(x = Q, y = Avg, fill = "red"),
           data = grp_df, stat = "identity") +
  geom_text(data = grp_df, aes(x = Q, y = Avg, label = Qtr), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Managers", subtitle = paste("Average Score by Question - Overall Average is",ms))


ggplot(data=dat1, aes(x=time, y=total_bill, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())

# END HERE


#--------------------------------------------------------------------
#
# Create the set of pivot tables for group and question results
#
#--------------------------------------------------------------------





# Create a dataframe to hold the results
am_table <- data.frame(Surveyed = 4,
                       AM_1A = 4,
                       AM_1B = 4,
                       AM_1C = 4,
                       AM_2A = 4,
                       AM_2B = 4,
                       AM_3x = 4)

# Break data down by quarter
mv_AM_Q1    <- am_results %>% filter(Surveyed == unique(dat$Surveyed[1]))

# Break down the quarter into individual questions - we have to process each
# column individually to avoid dropping rows
mv_AM_Q1_1A <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q1A_AM) != 0) %>% select(Q1A_AM)
mv_AM_Q1_1B <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q1B_AM) != 0) %>% select(Q1B_AM)
mv_AM_Q1_1C <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q1C_AM) != 0) %>% select(Q1C_AM)
mv_AM_Q1_2A <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q2A_AM) != 0) %>% select(Q2A_AM)
mv_AM_Q1_2B <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q2B_AM) != 0) %>% select(Q2B_AM)
mv_AM_Q1_3x <- mv_AM_Q1 %>% filter(as.numeric(mv_AM_Q1$Q3_AM ) != 0) %>% select(Q3_AM)

# Take each AM rating for each question in the given survey and get the average
am_table[1,1] <- unique(dat$Surveyed)[1]
am_table[2,1] <- unique(dat$Surveyed)[2]
am_table[3,1] <- unique(dat$Surveyed)[3]
am_table[4,1] <- unique(dat$Surveyed)[4]
am_table[1,2] <- round(mean(as.numeric(mv_AM_Q1_1A$Q1A_AM)), digits = 1)
am_table[1,3] <- round(mean(as.numeric(mv_AM_Q1_1B$Q1B_AM)), digits = 1)
am_table[1,4] <- round(mean(as.numeric(mv_AM_Q1_1C$Q1C_AM)), digits = 1)
am_table[1,5] <- round(mean(as.numeric(mv_AM_Q1_2A$Q2A_AM)), digits = 1)
am_table[1,6] <- round(mean(as.numeric(mv_AM_Q1_2B$Q2B_AM)), digits = 1)
am_table[1,7] <- round(mean(as.numeric(mv_AM_Q1_3x$Q3_AM )), digits = 1)

# Print out summary of average scores                                  
cat("Summary for Account Managers")
kable(t(am_table)) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "left")


# Plot stacked bar for the experience factor results
am_table <- as.data.frame(t(am_table))

# Basic barplot
p <- ggplot(data = am_table, aes(x = Surveyed, y = am_table$1F19)) +
  geom_bar(stat="identity")
p

cat("Summary for Question 1A")
kable(q1a_results) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "left")

# Convert to a dataframe and set the frequency position
df <- as.data.frame(am_results)
df <- ddply(df, .('Q1A-AM'),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
pc_trends <- with(expfactors, table(PosContrib, Surveyed))
pc_trends <- as.data.frame(pc_trends)
pc_trends <- pc_trends %>% filter(PosContrib == "1-Always") 

# Plot stacked bar for the experience factor results
pc_plot <- ggplot() +
  geom_bar(aes(x = q1_results_AM, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = PosContrib, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
pc_table_perc <- data.frame(Survey = 1:length(unique(dat$Surveyed)),
                            Always = 1:length(unique(dat$Surveyed)),
                            PAS    = 1:length(unique(dat$Surveyed)),
                            PCS    = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  pc_table_perc[i,1] <- unique(dat$Surveyed)[i]
  pc_table_perc[i,2] <- pc_table[i,1]
  pc_table_perc[i,3] <- round((pc_table[i,1] / nrow(dat)), digits = 3) * 100
  pc_table_perc[i,4] <- round((pc_table[i,1] / sum(pc_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
pc_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = pc_table_perc, stat = "identity") +
  geom_text(data = pc_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
pc_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = pc_table_perc, stat = "identity") +
  geom_text(data = pc_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "% Always (By Survey)")

# Arrange and display the two plots for pasting into deck
# grid.arrange(pc_plot, pc_trends_plot_pcs, pc_trends_plot_pas, ncol = 3)

# Arrange and display the two plots for pasting into deck
grid.arrange(pc_plot, pc_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
pc_imp <- pc_table_perc[nrow(pc_table_perc),4] - pc_table_perc[1,4]
cat("Change in 'Always' rating over period:", pc_imp,"%")

#
# Experience Attribute 2
#

# Create the data table and display
tr_table <- with(expfactors, table(Surveyed, TimelyResp))

cat("Data Summary - All Surveys")
kable(tr_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(tr_table)
df <- ddply(df, .(TimelyResp),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
tr_trends <- with(expfactors, table(TimelyResp, Surveyed))
tr_trends <- as.data.frame(tr_trends)
tr_trends <- tr_trends %>% filter(TimelyResp == "1-Always") 

# Plot stacked bar for the experience factor results
tr_plot <- ggplot() +
  geom_bar(aes(x = TimelyResp, y = Freq, fill = Surveyed),
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = TimelyResp, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Timely Response", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
tr_table_perc <- data.frame(Survey = 1:length(unique(dat$Surveyed)),
                            Always = 1:length(unique(dat$Surveyed)),
                            PAS    = 1:length(unique(dat$Surveyed)),
                            PCS    = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  tr_table_perc[i,1] <- unique(dat$Surveyed)[i]
  tr_table_perc[i,2] <- tr_table[i,1]
  tr_table_perc[i,3] <- round((tr_table[i,1] / nrow(dat)), digits = 3) * 100
  tr_table_perc[i,4] <- round((tr_table[i,1] / sum(tr_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
tr_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = tr_table_perc, stat = "identity") +
  geom_text(data = tr_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Timely Response", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
tr_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = tr_table_perc, stat = "identity") +
  geom_text(data = tr_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Timely Response", subtitle = "% Always (By Survey)")

# Arrange the two plots for pasting into deck
# grid.arrange(tr_plot, tr_trends_plot_pcs, tr_trends_plot_pas, ncol = 3)

# Arrange and display the two plots for pasting into deck
grid.arrange(tr_plot, tr_trends_plot_pcs, ncol = 2)

tr_imp <- tr_table_perc[nrow(tr_table_perc),4] - tr_table_perc[1,4]
cat("Change in 'Always' rating over period:", tr_imp,"%")

#
# Experience Attribute 3
#

# Create the data table and display
ac_table <- with(expfactors, table(Surveyed, Accountability))

cat("Data Summary - All Surveys")
kable(ac_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(ac_table)
df <- ddply(df, .(Accountability),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
ac_trends <- with(expfactors, table(Accountability, Surveyed))
ac_trends <- as.data.frame(ac_trends)
ac_trends <- ac_trends %>% filter(Accountability == "1-Always") 

# Plot stacked bar for the experience factor results
ac_plot <- ggplot() +
  geom_bar(aes(x = Accountability, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = Accountability, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Accountability", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ac_table_perc <- data.frame(Survey = 1:length(unique(dat$Surveyed)),
                            Always = 1:length(unique(dat$Surveyed)),
                            PAS    = 1:length(unique(dat$Surveyed)),
                            PCS    = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  ac_table_perc[i,1] <- unique(dat$Surveyed)[i]
  ac_table_perc[i,2] <- ac_table[i,1]
  ac_table_perc[i,3] <- round((ac_table[i,1] / nrow(dat)), digits = 3) * 100
  ac_table_perc[i,4] <- round((ac_table[i,1] / sum(ac_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ac_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ac_table_perc, stat = "identity") +
  geom_text(data = ac_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Accountability", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
ac_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ac_table_perc, stat = "identity") +
  geom_text(data = ac_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Accountability", subtitle = "% Always (By Survey)")

# Arrange the two plots for pasting into deck
# grid.arrange(ac_plot, ac_trends_plot_pcs, ac_trends_plot_pas, ncol = 3)

# Arrange and display the two plots for pasting into deck
grid.arrange(ac_plot, ac_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
ac_imp <- ac_table_perc[nrow(ac_table_perc),4] - ac_table_perc[1,4]
cat("Change in 'Always' rating over period:", ac_imp,"%")


#
# Experience Attribute 4
#

# Create the data table and display
kn_table <- with(expfactors, table(Surveyed, Knowledgeable))

cat("Data Summary - All Surveys")
kable(kn_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(kn_table)
df <- ddply(df, .(Knowledgeable),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
kn_trends <- with(expfactors, table(Knowledgeable, Surveyed))
kn_trends <- as.data.frame(kn_trends)
kn_trends <- kn_trends %>% filter(Knowledgeable == "1-Always") 

# Plot stacked bar for the experience factor results
kn_plot <- ggplot() +
  geom_bar(aes(x = Knowledgeable, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = Knowledgeable, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Knowledgeable", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
kn_table_perc <- data.frame(Survey = 1:length(unique(dat$Surveyed)),
                            Always = 1:length(unique(dat$Surveyed)),
                            PAS    = 1:length(unique(dat$Surveyed)),
                            PCS    = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  kn_table_perc[i,1] <- unique(dat$Surveyed)[i]
  kn_table_perc[i,2] <- kn_table[i,1]
  kn_table_perc[i,3] <- round((kn_table[i,1] / nrow(dat)), digits = 3) * 100
  kn_table_perc[i,4] <- round((kn_table[i,1] / sum(kn_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
kn_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = kn_table_perc, stat = "identity") +
  geom_text(data = kn_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Knowledgeable", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
kn_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = kn_table_perc, stat = "identity") +
  geom_text(data = kn_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Knowledgeable", subtitle = "% Always (By Survey)")

# Arrange the two plots for pasting into deck
# grid.arrange(kn_plot, kn_trends_plot_pcs, kn_trends_plot_pas, ncol = 3)

# Arrange and display the two plots for pasting into deck
grid.arrange(kn_plot, kn_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
kn_imp <- kn_table_perc[nrow(kn_table_perc),4] - kn_table_perc[1,4]
cat("Change in 'Always' rating over period:", kn_imp,"%")

# 
# Prepare Summary Grids
#

# Arrange the four experience plots in a 2x2 format
# grid.arrange(pc_plot, tr_plot, ac_plot, kn_plot, ncol = 2)

# Arrange the 4 "Very Satisfied" specific survey plots in a 2x2 format
grid.arrange(pc_trends_plot_pcs, 
             tr_trends_plot_pcs, 
             ac_trends_plot_pcs, 
             kn_trends_plot_pcs, ncol = 2)

# Arrange the 4 "Very Satisfied" cumulative plots in a 2x2 format
# grid.arrange(pc_trends_plot_pas, 
#              tr_trends_plot_pas, 
#              ac_trends_plot_pas, 
#              kn_trends_plot_pas, ncol = 2)

# Summarize improvements across groups
cat("Summary of Percent Improvement Over All Surveys")
cat("Positive Contribution improvement to date:", pc_imp,"%")
cat("Timely Response improvement to date:      ", tr_imp,"%")
cat("Accountable improvement to date:          ", ac_imp,"%")
cat("Knowledgeable improvement to date:        ", kn_imp,"%")

#--------------------------------------------------------------------
#
# Create the set of pivot tables for TSG group ratings
#
#--------------------------------------------------------------------

# Get the subgroup data
grpfactors <- transform(dat[c(1, 6, 7, 8, 9, 10, 11, 12, 13)]) 
# summary(grpfactors)

#
# Subgroup 1
#

# Create the data table and display
am_table <- with(grpfactors, table(Surveyed, AcctMgrs))

cat("Data Summary - All Surveys")
kable(am_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(am_table)
df <- ddply(df, .(AcctMgrs),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
am_trends <- with(grpfactors, table(AcctMgrs, Surveyed))
am_trends <- as.data.frame(am_trends)
am_trends <- am_trends %>% filter(AcctMgrs == "1-Very Satisfied")

# Plot stacked bar for the group results
am_plot <- ggplot() +
  geom_bar(aes(x = AcctMgrs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = AcctMgrs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
am_table_perc <- data.frame(Survey = 1:length(unique(dat$Surveyed)),
                            Always = 1:length(unique(dat$Surveyed)),
                            PAS    = 1:length(unique(dat$Surveyed)),
                            PCS    = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  am_table_perc[i,1] <- unique(dat$Surveyed)[i]
  am_table_perc[i,2] <- am_table[i,1]
  am_table_perc[i,3] <- round((am_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  am_table_perc[i,4] <- round((am_table[i,1] / sum(am_table[i, 1:(ncol(am_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
am_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = am_table_perc, stat = "identity") +
  geom_text(data = am_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar and "Always" results for current survey
am_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = am_table_perc, stat = "identity") +
  geom_text(data = am_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(am_plot, am_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
am_imp <- am_table_perc[nrow(am_table_perc),4] - am_table_perc[1,4]
cat("Change in 'Always' rating over period:", am_imp,"%")

#
# Subgroup 2
#

# Create the data table and display
bmps_table <- with(grpfactors, table(Surveyed, BMPS))

cat("Data Summary - All Surveys")
kable(bmps_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(bmps_table)
df <- ddply(df, .(BMPS),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
bmps_trends <- with(grpfactors, table(BMPS, Surveyed))
bmps_trends <- as.data.frame(bmps_trends)
bmps_trends <- bmps_trends %>% filter(BMPS == "1-Very Satisfied")

# Plot stacked bar for the group results
bmps_plot <- ggplot() +
  geom_bar(aes(x = BMPS, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = BMPS, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
bmps_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                              Very_Sat = 1:length(unique(dat$Surveyed)),
                              PAS      = 1:length(unique(dat$Surveyed)),
                              PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  bmps_table_perc[i,1] <- unique(dat$Surveyed)[i]
  bmps_table_perc[i,2] <- bmps_table[i,1]
  bmps_table_perc[i,3] <- round((bmps_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  bmps_table_perc[i,4] <- round((bmps_table[i,1] / sum(bmps_table[i, 1:(ncol(bmps_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
bmps_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = bmps_table_perc, stat = "identity") +
  geom_text(data = bmps_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
bmps_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = bmps_table_perc, stat = "identity") +
  geom_text(data = bmps_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(bmps_plot, bmps_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
bmps_imp <- bmps_table_perc[nrow(bmps_table_perc),4] - bmps_table_perc[1,4]
cat("Change in 'Always' rating over period:", bmps_imp,"%")

#
# Subgroup 3
#

# Create the data table and display
ba_table <- with(grpfactors, table(Surveyed, BusApps))

cat("Data Summary - All Surveys")
kable(ba_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(ba_table)
df <- ddply(df, .(BusApps),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
ba_trends <- with(grpfactors, table(BusApps, Surveyed))
ba_trends <- as.data.frame(ba_trends)
ba_trends <- ba_trends %>% filter(BusApps == "1-Very Satisfied")

# Plot stacked bar for the group results
ba_plot <- ggplot() +
  geom_bar(aes(x = BusApps, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = BusApps, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Business Applications", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ba_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                              Very_Sat = 1:length(unique(dat$Surveyed)),
                              PAS      = 1:length(unique(dat$Surveyed)),
                              PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  ba_table_perc[i,1] <- unique(dat$Surveyed)[i]
  ba_table_perc[i,2] <- ba_table[i,1]
  ba_table_perc[i,3] <- round((ba_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ba_table_perc[i,4] <- round((ba_table[i,1] / sum(ba_table[i, 1:(ncol(ba_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ba_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ba_table_perc, stat = "identity") +
  geom_text(data = ba_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Busness Applications", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ba_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ba_table_perc, stat = "identity") +
  geom_text(data = ba_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Business Applications", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(ba_plot, ba_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
ba_imp <- ba_table_perc[nrow(ba_table_perc),4] - ba_table_perc[1,4]
cat("Change in 'Always' rating over period:", ba_imp,"%")

#
# Subgroup 4
#

# Create the data table and display
es_table <- with(grpfactors, table(Surveyed, EventSvcs))

cat("Data Summary - All Surveys")
kable(es_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(es_table)
df <- ddply(df, .(EventSvcs),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
es_trends <- with(grpfactors, table(EventSvcs, Surveyed))
es_trends <- as.data.frame(es_trends)
es_trends <- es_trends %>% filter(EventSvcs == "1-Very Satisfied")

# Plot stacked bar for the group results
es_plot <- ggplot() +
  geom_bar(aes(x = EventSvcs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = EventSvcs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
es_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                            Very_Sat = 1:length(unique(dat$Surveyed)),
                            PAS      = 1:length(unique(dat$Surveyed)),
                            PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  es_table_perc[i,1] <- unique(dat$Surveyed)[i]
  es_table_perc[i,2] <- es_table[i,1]
  es_table_perc[i,3] <- round((es_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  es_table_perc[i,4] <- round((es_table[i,1] / sum(es_table[i, 1:(ncol(es_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
es_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = es_table_perc, stat = "identity") +
  geom_text(data = es_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
es_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = es_table_perc, stat = "identity") +
  geom_text(data = es_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(es_plot, es_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
es_imp <- es_table_perc[nrow(es_table_perc),4] - es_table_perc[1,4]
cat("Change in 'Always' rating over period:", es_imp,"%")

#
# Subgroup 5
#

# Create the data table and display
ps_table <- with(grpfactors, table(Surveyed, ProjSupp))

cat("Data Summary - All Surveys")
kable(ps_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(ps_table)
df <- ddply(df, .(ProjSupp),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
ps_trends <- with(grpfactors, table(ProjSupp, Surveyed))
ps_trends <- as.data.frame(ps_trends)
ps_trends <- ps_trends %>% filter(ProjSupp == "1-Very Satisfied")

# Plot stacked bar for the group results
ps_plot <- ggplot() +
  geom_bar(aes(x = ProjSupp, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = ProjSupp, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ps_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                            Very_Sat = 1:length(unique(dat$Surveyed)),
                            PAS      = 1:length(unique(dat$Surveyed)),
                            PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  ps_table_perc[i,1] <- unique(dat$Surveyed)[i]
  ps_table_perc[i,2] <- ps_table[i,1]
  ps_table_perc[i,3] <- round((ps_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ps_table_perc[i,4] <- round((ps_table[i,1] / sum(ps_table[i, 1:(ncol(ps_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ps_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ps_table_perc, stat = "identity") +
  geom_text(data = ps_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ps_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ps_table_perc, stat = "identity") +
  geom_text(data = ps_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(ps_plot, ps_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
ps_imp <- ps_table_perc[nrow(ps_table_perc),4] - ps_table_perc[1,4]
cat("Change in 'Always' rating over period:", ps_imp,"%")

#
# Subgroup 6
#

# Create the data table and display
sd_table <- with(grpfactors, table(Surveyed, ServDesk))

cat("Data Summary - All Surveys")
kable(sd_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(sd_table)
df <- ddply(df, .(ServDesk),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
sd_trends <- with(grpfactors, table(ServDesk, Surveyed))
sd_trends <- as.data.frame(sd_trends)
sd_trends <- sd_trends %>% filter(ServDesk == "1-Very Satisfied")

# Plot stacked bar for the group results
sd_plot <- ggplot() +
  geom_bar(aes(x = ServDesk, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = ServDesk, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
sd_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                            Very_Sat = 1:length(unique(dat$Surveyed)),
                            PAS      = 1:length(unique(dat$Surveyed)),
                            PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  sd_table_perc[i,1] <- unique(dat$Surveyed)[i]
  sd_table_perc[i,2] <- sd_table[i,1]
  sd_table_perc[i,3] <- round((sd_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  sd_table_perc[i,4] <- round((sd_table[i,1] / sum(sd_table[i, 1:(ncol(sd_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
sd_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = sd_table_perc, stat = "identity") +
  geom_text(data = sd_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
sd_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = sd_table_perc, stat = "identity") +
  geom_text(data = sd_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(sd_plot, sd_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
sd_imp <- sd_table_perc[nrow(sd_table_perc),4] - sd_table_perc[1,4]
cat("Change in 'Always' rating over period:", sd_imp,"%")

#
# Subgroup 7
#

# Create the data table and display
ss_table <- with(grpfactors, table(Surveyed, StudioSvcs))

cat("Data Summary - All Surveys")
kable(ss_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(ss_table)
df <- ddply(df, .(StudioSvcs),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
ss_trends <- with(grpfactors, table(StudioSvcs, Surveyed))
ss_trends <- as.data.frame(ss_trends)
ss_trends <- ss_trends %>% filter(StudioSvcs == "1-Very Satisfied")

# Plot stacked bar for the group results
ss_plot <- ggplot() +
  geom_bar(aes(x = StudioSvcs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = StudioSvcs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ss_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                            Very_Sat = 1:length(unique(dat$Surveyed)),
                            PAS      = 1:length(unique(dat$Surveyed)),
                            PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  ss_table_perc[i,1] <- unique(dat$Surveyed)[i]
  ss_table_perc[i,2] <- ss_table[i,1]
  ss_table_perc[i,3] <- round((ss_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ss_table_perc[i,4] <- round((ss_table[i,1] / sum(ss_table[i, 1:(ncol(ss_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ss_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ss_table_perc, stat = "identity") +
  geom_text(data = ss_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ss_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ss_table_perc, stat = "identity") +
  geom_text(data = ss_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(ss_plot, ss_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
ss_imp <- ss_table_perc[nrow(ss_table_perc),4] - ss_table_perc[1,4]
cat("Change in 'Always' rating over period:", ss_imp,"%")

#
# Subgroup 8
#

# Create the data table and display
vm_table <- with(grpfactors, table(Surveyed, VenMgmt))

cat("Data Summary - All Surveys")
kable(vm_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to dataframe and set frequency position
df <- as.data.frame(vm_table)
df <- ddply(df, .(VenMgmt),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
vm_trends <- with(grpfactors, table(VenMgmt, Surveyed))
vm_trends <- as.data.frame(vm_trends)
vm_trends <- vm_trends %>% filter(VenMgmt == "1-Very Satisfied")

# Plot stacked bar for the group results
vm_plot <- ggplot() +
  geom_bar(aes(x = VenMgmt, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = VenMgmt, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Vendor Management", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
vm_table_perc <- data.frame(Survey   = 1:length(unique(dat$Surveyed)),
                            Very_Sat = 1:length(unique(dat$Surveyed)),
                            PAS      = 1:length(unique(dat$Surveyed)),
                            PCS      = 1:length(unique(dat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(dat$Surveyed))) {
  vm_table_perc[i,1] <- unique(dat$Surveyed)[i]
  vm_table_perc[i,2] <- vm_table[i,1]
  vm_table_perc[i,3] <- round((vm_table[i,1] / nrow(dat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  vm_table_perc[i,4] <- round((vm_table[i,1] / sum(vm_table[i, 1:(ncol(vm_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
vm_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = vm_table_perc, stat = "identity") +
  geom_text(data = vm_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Management", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
vm_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = vm_table_perc, stat = "identity") +
  geom_text(data = vm_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Management", subtitle = "% Very Sat (By Survey, no NR/DNU)")

# Arrange and display the two plots for pasting into deck
grid.arrange(vm_plot, vm_trends_plot_pcs, ncol = 2)

# Display the % improvement in "Always" to date
vm_imp <- vm_table_perc[nrow(vm_table_perc),4] - vm_table_perc[1,4]
cat("Change in 'Always' rating over period:", vm_imp,"%")

#
# Create summary grids for groups
#

# Arrange the 8 subgroup plots in two 2x2 formatted pieces - by survey
grid.arrange(
  am_trends_plot_pcs,
  bmps_trends_plot_pcs,
  ba_trends_plot_pcs,
  es_trends_plot_pcs, ncol = 2)

grid.arrange(
  ps_trends_plot_pcs,
  sd_trends_plot_pcs,
  ss_trends_plot_pcs,
  vm_trends_plot_pcs, ncol = 2)

# Arrange the 8 subgroup plots in two 2x2 formatted pieces - all surveys
# grid.arrange(
#   am_trends_plot_pas,
#   bmps_trends_plot_pas,
#   ba_trends_plot_pas,
#   es_trends_plot_pas, ncol = 2)
# 
# grid.arrange(
#   ps_trends_plot_pas,
#   sd_trends_plot_pas,
#   ss_trends_plot_pas,
#   vm_trends_plot_pas, ncol = 2)

cat("Account Managers improvement to date:     ", am_imp,"%")
cat("BMPS improvement to date:                 ", bmps_imp,"%")
cat("Business Applications improvement to date:", ba_imp,"%")
cat("Event Services improvement to date:       ", es_imp,"%")
cat("Project Support improvement to date:      ", ps_imp,"%")
cat("Service Desk improvement to date:         ", sd_imp,"%")
cat("Studio Services improvement to date:      ", ss_imp,"%")
cat("Vendor Management improvement to date:    ", vm_imp,"%")

#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------

# By survey question
q1a_results <- transform(dat[c(1,  2, 3, 4, 5, 6, 7)])
q1b_results <- transform(dat[c(1,  8, 9,10,11,12,13)])
q1c_results <- transform(dat[c(1, 14,15,16,17,18,19)])
q2a_results <- transform(dat[c(1, 20,21,22,23,24,25)])
q2b_results <- transform(dat[c(1, 26,27,28,29,30,31)])
q3x_results  <- transform(dat[c(1, 32,33,34,35,36,37)])

# Survey 1 (Fall) Group 1 Question 1A
q1_results_AM <- transform(q1_results[c(1, 2, 8,14,20,26,32)])
q2_results_AM <- transform(q2_results[c(1, 2, 8,14,20,26,32)])
q3_results_AM <- transform(q3_results[c(1, 2, 8,14,20,26,32)])
q4_results_AM <- transform(q4_results[c(1, 2, 8,14,20,26,32)])


