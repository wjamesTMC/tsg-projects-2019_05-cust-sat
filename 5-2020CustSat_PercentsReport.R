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

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open survey file
#

# Import and Open the data file / Establish the data set
data_filename <- gs_title("2019-2020 TSG Satisfaction Survey")
dat <- gs_read(data_filename, stringsAsFactors = FALSE)

#
# Convert time stamps to survey names
#

# Trap for 2018 survey
for(i in 1:length(dat$Timestamp)) {
  x <- str_detect(dat$Timestamp[i], "2019")
  if(x == TRUE) {
    dat$Timestamp[i] <- "2-1F19"
  }
}

# Label additional surveys
for(i in 1:length(dat$Timestamp)) {
  if(dat$Timestamp[i] > "1/1/2020" & dat$Timestamp[i] < "4/1/2020") {
    dat$Timestamp[i] <- "2-2W2020"
  }
  if(dat$Timestamp[i] > "4/1/2020" & dat$Timestamp[i] < "6/30/2020") {
    dat$Timestamp[i] <- "2-3S2020"
  }
  
  if(dat$Timestamp[i] > "7/1/2020" & dat$Timestamp[i] < "9/30/2020") {
    dat$Timestamp[i] <- "2-4S2020"
  }
}

#
# Clean data file to set vector names
#

dat <- rename(dat, replace = c("Timestamp" = "Surveyed",
                               "1A. TSG representatives are always able to understand my problems / needs. [Account Managers]" = "1A-AM",
                               "1A. TSG representatives are always able to understand my problems / needs. [Business Applications Support]" = "1A-BA",
                               "1A. TSG representatives are always able to understand my problems / needs. [B&MPS]" = "1A-BMPS",
                               "1A. TSG representatives are always able to understand my problems / needs. [Project Support / PMO]" = "1A-PS",
                               "1A. TSG representatives are always able to understand my problems / needs. [Service Desk]" = "1A-SD",
                               "1A. TSG representatives are always able to understand my problems / needs. [Vendor Managers]" = "1A-VM",
                               "1B. TSG representatives ask the right questions and listen to my input. [Account Managers]" = "1B-AM",
                               "1B. TSG representatives ask the right questions and listen to my input. [Business Applications Support]" = "1B-BA",
                               "1B. TSG representatives ask the right questions and listen to my input. [B&MPS]" = "1B-BMPS",
                               "1B. TSG representatives ask the right questions and listen to my input. [Project Support / PMO]" = "1B-PS",
                               "1B. TSG representatives ask the right questions and listen to my input. [Service Desk]" = "1B-SD",
                               "1B. TSG representatives ask the right questions and listen to my input. [Vendor Managers]" = "1B-VM",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Account Managers]" = "1CAM-",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Business Applications Support]" = "1C-BA",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [B&MPS]" = "1C-BMPS",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Project Support / PMO]" = "1C-PS",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Service Desk]" = "1C-SD",
                               "1C. TSG representatives stay in touch until my problems are resolved or my needs are met. [Vendor Managers]" = "1C-VM",
                               "2A. TSG representatives always have the knowledge to address my needs. [Account Managers]" = "2A-AM",
                               "2A. TSG representatives always have the knowledge to address my needs. [Business Applications Support]" = "2A-BA",
                               "2A. TSG representatives always have the knowledge to address my needs. [B&MPS]" = "2A-BMPS",
                               "2A. TSG representatives always have the knowledge to address my needs. [Project Support / PMO]" = "2A-PS",
                               "2A. TSG representatives always have the knowledge to address my needs. [Service Desk]" = "2A-SD",
                               "2A. TSG representatives always have the knowledge to address my needs. [Vendor Managers]" = "2A-VM",
                               "2B.  TSG representatives address my needs completely and professionally. [Account Managers]" = "2B-AM",
                               "2B.  TSG representatives address my needs completely and professionally. [Business Applications Support]" = "2B-BA",
                               "2B.  TSG representatives address my needs completely and professionally. [B&MPS]" = "2B-BMPS",
                               "2B.  TSG representatives address my needs completely and professionally. [Project Support / PMO]" = "2B-PS",
                               "2B.  TSG representatives address my needs completely and professionally. [Service Desk]" = "2B-SD",
                               "2B.  TSG representatives address my needs completely and professionally. [Vendor Managers]" = "2B-VM",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Account Managers]" = "3-AM",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Business Applications Support]" = "3-BA",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [B&MPS]" = "3-BMPS",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Project Support / PMO]" = "3-PS",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Service Desk]" = "3-SD",
                               "3. TSG representatives always resolve my needs or problems within my required timeframes. [Vendor Managers]" = "3-VM",
                               "What additional comments do you have for TSG (pro or con) that might help us serve you better?" = "Comments",
                               "Email Address" = "Email"))

# Remove rows below the actual data / Remove TComments
wkgdat <- subset(dat, dat[ , 1] != "") %>% select(-Comments, -Email)

# Rename ratings so they will sort properly
wkgdat[wkgdat == "1 - Strongly Disagree"]  <- "1"
wkgdat[wkgdat == "10 - Strongly Agree"]    <- "10"
wkgdat[wkgdat == "4 - Disagree"]           <- "4"
wkgdat[wkgdat == "7 - Agree"]              <- "7"
wkgdat[wkgdat == "NA or Do Not Use"]  <- "NA/DNU"
wkgdat[wkgdat == 0]                   <- "NR"
wkgdat[wkgdat == ""]                  <- "NR"


# Verify the number of unique values of the various factors
sapply(wkgdat, function(x)length(unique(x)))

#--------------------------------------------------------------------
#
# Create the set of pivot tables for Experience Attributes
#
#--------------------------------------------------------------------

# Get the Experience Attributes into a group
expfactors <- transform(wkgdat[c(1,2,3,4,5)]) 
# summary(expfactors)

#
# Experience Attribute 1
#

# Create the data table and display
pc_table <- with(expfactors, table(Surveyed, PosContrib))

cat("Data Summary - All Surveys")
kable(pc_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Convert to a dataframe and set the frequency position
df <- as.data.frame(pc_table)
df <- ddply(df, .(PosContrib),
            transform, pos = cumsum(Freq))

# Calc the number of "Always" responses
pc_trends <- with(expfactors, table(PosContrib, Surveyed))
pc_trends <- as.data.frame(pc_trends)
pc_trends <- pc_trends %>% filter(PosContrib == "1-Always") 

# Plot stacked bar for the experience factor results
pc_plot <- ggplot() +
  geom_bar(aes(x = PosContrib, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = PosContrib, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
pc_table_perc <- data.frame(Survey = 1:length(unique(wkgdat$Surveyed)),
                            Always = 1:length(unique(wkgdat$Surveyed)),
                            PAS    = 1:length(unique(wkgdat$Surveyed)),
                            PCS    = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  pc_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  pc_table_perc[i,2] <- pc_table[i,1]
  pc_table_perc[i,3] <- round((pc_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  pc_table_perc[i,4] <- round((pc_table[i,1] / sum(pc_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
pc_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = pc_table_perc, stat = "identity") +
  geom_text(data = pc_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
pc_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = pc_table_perc, stat = "identity") +
  geom_text(data = pc_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
tr_table_perc <- data.frame(Survey = 1:length(unique(wkgdat$Surveyed)),
                            Always = 1:length(unique(wkgdat$Surveyed)),
                            PAS    = 1:length(unique(wkgdat$Surveyed)),
                            PCS    = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  tr_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  tr_table_perc[i,2] <- tr_table[i,1]
  tr_table_perc[i,3] <- round((tr_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  tr_table_perc[i,4] <- round((tr_table[i,1] / sum(tr_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
tr_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = tr_table_perc, stat = "identity") +
  geom_text(data = tr_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Timely Response", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
tr_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = tr_table_perc, stat = "identity") +
  geom_text(data = tr_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
ac_table_perc <- data.frame(Survey = 1:length(unique(wkgdat$Surveyed)),
                            Always = 1:length(unique(wkgdat$Surveyed)),
                            PAS    = 1:length(unique(wkgdat$Surveyed)),
                            PCS    = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  ac_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  ac_table_perc[i,2] <- ac_table[i,1]
  ac_table_perc[i,3] <- round((ac_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  ac_table_perc[i,4] <- round((ac_table[i,1] / sum(ac_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ac_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ac_table_perc, stat = "identity") +
  geom_text(data = ac_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Accountability", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
ac_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ac_table_perc, stat = "identity") +
  geom_text(data = ac_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
kn_table_perc <- data.frame(Survey = 1:length(unique(wkgdat$Surveyed)),
                            Always = 1:length(unique(wkgdat$Surveyed)),
                            PAS    = 1:length(unique(wkgdat$Surveyed)),
                            PCS    = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  kn_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  kn_table_perc[i,2] <- kn_table[i,1]
  kn_table_perc[i,3] <- round((kn_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  kn_table_perc[i,4] <- round((kn_table[i,1] / sum(kn_table[i, 1:5])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
kn_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = kn_table_perc, stat = "identity") +
  geom_text(data = kn_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Knowledgeable", subtitle = "% Always (All Surveys)")

# Plot stacked bar and "Always" results for current survey
kn_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = kn_table_perc, stat = "identity") +
  geom_text(data = kn_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
grpfactors <- transform(wkgdat[c(1, 6, 7, 8, 9, 10, 11, 12, 13)]) 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
am_table_perc <- data.frame(Survey = 1:length(unique(wkgdat$Surveyed)),
                            Always = 1:length(unique(wkgdat$Surveyed)),
                            PAS    = 1:length(unique(wkgdat$Surveyed)),
                            PCS    = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  am_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  am_table_perc[i,2] <- am_table[i,1]
  am_table_perc[i,3] <- round((am_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  am_table_perc[i,4] <- round((am_table[i,1] / sum(am_table[i, 1:(ncol(am_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
am_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = am_table_perc, stat = "identity") +
  geom_text(data = am_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar and "Always" results for current survey
am_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = am_table_perc, stat = "identity") +
  geom_text(data = am_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
bmps_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                              Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                              PAS      = 1:length(unique(wkgdat$Surveyed)),
                              PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  bmps_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  bmps_table_perc[i,2] <- bmps_table[i,1]
  bmps_table_perc[i,3] <- round((bmps_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  bmps_table_perc[i,4] <- round((bmps_table[i,1] / sum(bmps_table[i, 1:(ncol(bmps_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
bmps_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = bmps_table_perc, stat = "identity") +
  geom_text(data = bmps_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "B&MPS", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
bmps_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = bmps_table_perc, stat = "identity") +
  geom_text(data = bmps_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Business Applications", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ba_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                              Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                              PAS      = 1:length(unique(wkgdat$Surveyed)),
                              PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  ba_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  ba_table_perc[i,2] <- ba_table[i,1]
  ba_table_perc[i,3] <- round((ba_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ba_table_perc[i,4] <- round((ba_table[i,1] / sum(ba_table[i, 1:(ncol(ba_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ba_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ba_table_perc, stat = "identity") +
  geom_text(data = ba_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Busness Applications", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ba_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ba_table_perc, stat = "identity") +
  geom_text(data = ba_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
es_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                            Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                            PAS      = 1:length(unique(wkgdat$Surveyed)),
                            PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  es_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  es_table_perc[i,2] <- es_table[i,1]
  es_table_perc[i,3] <- round((es_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  es_table_perc[i,4] <- round((es_table[i,1] / sum(es_table[i, 1:(ncol(es_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
es_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = es_table_perc, stat = "identity") +
  geom_text(data = es_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
es_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = es_table_perc, stat = "identity") +
  geom_text(data = es_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ps_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                            Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                            PAS      = 1:length(unique(wkgdat$Surveyed)),
                            PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  ps_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  ps_table_perc[i,2] <- ps_table[i,1]
  ps_table_perc[i,3] <- round((ps_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ps_table_perc[i,4] <- round((ps_table[i,1] / sum(ps_table[i, 1:(ncol(ps_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ps_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ps_table_perc, stat = "identity") +
  geom_text(data = ps_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ps_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ps_table_perc, stat = "identity") +
  geom_text(data = ps_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
sd_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                            Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                            PAS      = 1:length(unique(wkgdat$Surveyed)),
                            PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  sd_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  sd_table_perc[i,2] <- sd_table[i,1]
  sd_table_perc[i,3] <- round((sd_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  sd_table_perc[i,4] <- round((sd_table[i,1] / sum(sd_table[i, 1:(ncol(sd_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
sd_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = sd_table_perc, stat = "identity") +
  geom_text(data = sd_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
sd_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = sd_table_perc, stat = "identity") +
  geom_text(data = sd_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
ss_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                            Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                            PAS      = 1:length(unique(wkgdat$Surveyed)),
                            PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  ss_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  ss_table_perc[i,2] <- ss_table[i,1]
  ss_table_perc[i,3] <- round((ss_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  ss_table_perc[i,4] <- round((ss_table[i,1] / sum(ss_table[i, 1:(ncol(ss_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
ss_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = ss_table_perc, stat = "identity") +
  geom_text(data = ss_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
ss_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = ss_table_perc, stat = "identity") +
  geom_text(data = ss_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Vendor Management", subtitle = "All Attributes / All Surveys")

# Calc the percentage of "Always" responses
vm_table_perc <- data.frame(Survey   = 1:length(unique(wkgdat$Surveyed)),
                            Very_Sat = 1:length(unique(wkgdat$Surveyed)),
                            PAS      = 1:length(unique(wkgdat$Surveyed)),
                            PCS      = 1:length(unique(wkgdat$Surveyed)))

# Run through the data and summarize each survey; log number and %'s of "Always'
for(i in 1:length(unique(wkgdat$Surveyed))) {
  vm_table_perc[i,1] <- unique(wkgdat$Surveyed)[i]
  vm_table_perc[i,2] <- vm_table[i,1]
  vm_table_perc[i,3] <- round((vm_table[i,1] / nrow(wkgdat)), digits = 3) * 100
  # Subtract 2 from ncol to drop NA/DNU and NR responses
  vm_table_perc[i,4] <- round((vm_table[i,1] / sum(vm_table[i, 1:(ncol(vm_table)-2)])), digits = 3) * 100
}

# Plot stacked bar and "Always" results for all surveys
vm_trends_plot_pas <- ggplot() +
  geom_bar(aes(x = Survey, y = PAS, fill = Survey),
           data = vm_table_perc, stat = "identity") +
  geom_text(data = vm_table_perc, aes(x = Survey, y = PAS, label = PAS), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Management", subtitle = "% Very Sat (All Surveys)")

# Plot stacked bar for the group results
vm_trends_plot_pcs <- ggplot() +
  geom_bar(aes(x = Survey, y = PCS, fill = Survey),
           data = vm_table_perc, stat = "identity") +
  geom_text(data = vm_table_perc, aes(x = Survey, y = PCS, label = PCS), 
            vjust = 1.5, color = "black", size = 5) + 
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


