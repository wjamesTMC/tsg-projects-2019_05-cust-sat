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

#--------------------------------------------------------------------
#
# File open, cleanup, and set up for the analysis
#
#--------------------------------------------------------------------

#
# Download and open survey file
#

# Import and Open the data file / Establish the data set
data_filename <- "0_Input_CustSatData.csv"
dat <- read.csv(data_filename, stringsAsFactors = FALSE)

#
# Convert time stamps to survey names
#

# Trap for 2018 survey
for(i in 1:length(dat$Timestamp)) {
  x <- str_detect(dat$Timestamp[i], "2018")
  if(x == TRUE) {
    dat$Timestamp[i] <- "01 - Fall 2018"
  }
}

# Trap for 2019 surveys
for(i in 1:length(dat$Timestamp)) {
  if(dat$Timestamp[i] > "1/1/2019" & dat$Timestamp[i] < "4/1/2019") {
    dat$Timestamp[i] <- "02 - Winter 2019"
  }
  if(dat$Timestamp[i] > "4/1/2019" & dat$Timestamp[i] < "6/30/2019") {
    dat$Timestamp[i] <- "03 - Spring 2019"
  }

  if(dat$Timestamp[i] > "7/1/2019" & dat$Timestamp[i] < "9/30/2019") {
    dat$Timestamp[i] <- "04 - Summer 2019"
  }
}

#
# Clean data file to set vector names
#

dat <- rename(dat, replace = c("Timestamp" = "Surveyed",
                               "Tell.us.about.your.experience.working.with.us..TSG.makes.a.positive.contribution.to.my.work." = "PosContrib",
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

# Take out Timestamp and the comments fields for now
wkgdat <- cleandat %>% select(Surveyed,	PosContrib,	TimelyResp,	Accountability,
                              Knowledgeable,	AcctMgrs,	BMPS,	BusApps,	EventSvcs,
                              ProjSupp,	ServDesk,	StudioSvcs,	VenMgmt)

# Rename ratings so they will sort
wkgdat[wkgdat == "Always"]            <- "1-Always"
wkgdat[wkgdat == "Mostly"]            <- "2-Mostly"
wkgdat[wkgdat == "Sometimes"]         <- "3-Sometimes"
wkgdat[wkgdat == "Rarely"]            <- "4-Rarely"
wkgdat[wkgdat == "Never"]             <- "5-Never"

# Rename group ratings to be consistent
wkgdat[wkgdat == "Very Satisfied"]    <- "1-Very Satisfied"
wkgdat[wkgdat == "Satisfied"]         <- "2-Satisfied"
wkgdat[wkgdat == "Neutral"]           <- "3-Neutral"
wkgdat[wkgdat == "Dissatisfied"]      <- "4-Dissatisfied"
wkgdat[wkgdat == "Very Dissatisfied"] <- "5-Very Dissatisfied"

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
# Create the set of pivot tables for Experience Attributes
#
#--------------------------------------------------------------------

# Get the Experience Attributes into a group
expfactors <- transform(wkgdat[c(1,2,3,4,5)]) 
summary(expfactors)

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

# Convert to dataframe and set frequency position
df <- as.data.frame(pc_table)
df <- ddply(df, .(PosContrib),
            transform, pos = cumsum(Freq))

# Plot stacked bar and "Very Satisfied" detail
pc_plot <- ggplot() +
  geom_bar(aes(x = PosContrib, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = PosContrib, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Experience Attribute", subtitle = "Positive Contribution")

pc_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = pc_trends, stat = "identity") +
  geom_text(data = pc_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Positive Contribution", subtitle = "Always")

# Arrange the two plots for pasting into deck
grid.arrange(pc_plot, pc_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
pc_imp <- pc_trends[nrow(pc_trends),ncol(pc_trends)] - pc_trends[1,ncol(pc_trends)]
cat("Change in 'Always' rating over period:", pc_imp)

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

# Plot stacked bar and "Very Satisfied" detail
tr_plot <- ggplot() +
  geom_bar(aes(x = TimelyResp, y = Freq, fill = Surveyed),
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = TimelyResp, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Experience Attribute", subtitle = "Timely Response")

tr_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = tr_trends, stat = "identity") +
  geom_text(data = tr_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Timely Response", subtitle = "Always")

# Arrange the two plots for pasting into deck
grid.arrange(tr_plot, tr_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
tr_imp <- tr_trends[nrow(tr_trends),ncol(tr_trends)] - tr_trends[1,ncol(tr_trends)]
cat("Change in 'Always' rating over period:", tr_imp)

#
# Experience Attribute 3
#

# Create the data table and display
ac_table <- with(expfactors, table(Surveyed, Accountability))

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

# Plot stacked bar and "Very Satisfied" detail
ac_plot <- ggplot() +
  geom_bar(aes(x = Accountability, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = Accountability, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Experience Attribute", subtitle = "Accountability")

ac_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = ac_trends, stat = "identity") +
  geom_text(data = ac_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Accountability", subtitle = "Always")

# Arrange the two plots for pasting into deck
grid.arrange(ac_plot, ac_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
ac_imp <- ac_trends[nrow(ac_trends),ncol(ac_trends)] - ac_trends[1,ncol(ac_trends)]
cat("Change in 'Always' rating over period:", ac_imp)

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

# Plot stacked bar and "Very Satisfied" detail
kn_plot <- ggplot() +
  geom_bar(aes(x = Knowledgeable, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = Knowledgeable, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "Experience Attribute", subtitle = "Knowledgeable")

kn_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = kn_trends, stat = "identity") +
  geom_text(data = kn_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Knowledgeable", subtitle = "Always")

# Arrange the two plots for pasting into deck
grid.arrange(kn_plot, kn_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
kn_imp <- kn_trends[nrow(kn_trends),ncol(kn_trends)] - kn_trends[1,ncol(kn_trends)]
cat("Change in 'Always' rating over period:", kn_imp)

# Arrange the four experience plots in a 2x2 format
grid.arrange(pc_plot, tr_plot, ac_plot, kn_plot, ncol = 2)

# Arrange the 4 "Very Satisfied" plots in a 2x2 format
grid.arrange(pc_trends_plot, tr_trends_plot, ac_trends_plot, kn_trends_plot, ncol = 2)

# Summarize improvements across groups
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
summary(grpfactors)

#
# Subgroup 1
#

# # Create the data table and display
am_table <- with(grpfactors, table(Surveyed, AcctMgrs))

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

# Plot stacked bar and "Very Satisfied" detail
am_plot <- ggplot() +
  geom_bar(aes(x = AcctMgrs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = AcctMgrs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroups", subtitle = "Account Managers")

am_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = am_trends, stat = "identity") +
  geom_text(data = am_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Account Managers", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(am_plot, am_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
am_imp <- am_trends[nrow(am_trends),ncol(am_trends)] - am_trends[1,ncol(am_trends)]
cat("Change in 'Always' rating over period:", am_imp)

#
# Subgroup 2
#

# # Create the data table and display
bmps_table <- with(grpfactors, table(Surveyed, BMPS))

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

# Plot stacked bar and "Very Satisfied" detail
bmps_plot <- ggplot() +
  geom_bar(aes(x = BMPS, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = BMPS, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroups", subtitle = "B&MPS")

bmps_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = bmps_trends, stat = "identity") +
  geom_text(data = bmps_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "BMPS", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(bmps_plot, bmps_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
bmps_imp <- bmps_trends[nrow(bmps_trends),ncol(bmps_trends)] - bmps_trends[1,ncol(bmps_trends)]
cat("Change in 'Always' rating over period:", bmps_imp)

#
# Subgroup 3
#

# # Create the data table and display
ba_table <- with(grpfactors, table(Surveyed, BusApps))

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

# Plot stacked bar and "Very Satisfied" detail
ba_plot <- ggplot() +
  geom_bar(aes(x = BusApps, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = BusApps, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Business Applications")

ba_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = ba_trends, stat = "identity") +
  geom_text(data = ba_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Business Applications", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(ba_plot, ba_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
ba_imp <- ba_trends[nrow(ba_trends),ncol(ba_trends)] - ba_trends[1,ncol(ba_trends)]
cat("Change in 'Always' rating over period:", ba_imp)

#
# Subgroup 4
#

# # Create the data table and display
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

# Plot stacked bar and "Very Satisfied" detail
es_plot <- ggplot() +
  geom_bar(aes(x = EventSvcs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = EventSvcs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Event Services")

es_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = es_trends, stat = "identity") +
  geom_text(data = es_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Event Services", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(es_plot, es_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
es_imp <- es_trends[nrow(es_trends),ncol(es_trends)] - es_trends[1,ncol(es_trends)]
cat("Change in 'Always' rating over period:", es_imp)

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

# Plot stacked bar and "Very Satisfied" detail
ps_plot <- ggplot() +
  geom_bar(aes(x = ProjSupp, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = ProjSupp, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Project Support")

ps_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = ps_trends, stat = "identity") +
  geom_text(data = ps_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Project Support", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(ps_plot, ps_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
ps_imp <- ps_trends[nrow(ps_trends),ncol(ps_trends)] - ps_trends[1,ncol(ps_trends)]
cat("Change in 'Always' rating over period:", ps_imp)

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

# Plot stacked bar and "Very Satisfied" detail
sd_plot <- ggplot() +
  geom_bar(aes(x = ServDesk, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = ServDesk, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Service Desk")

sd_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = sd_trends, stat = "identity") +
  geom_text(data = sd_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Service Desk", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(sd_plot, sd_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
sd_imp <- sd_trends[nrow(sd_trends),ncol(sd_trends)] - sd_trends[1,ncol(sd_trends)]
cat("Change in 'Always' rating over period:", sd_imp)

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

# Plot stacked bar and "Very Satisfied" detail
ss_plot <- ggplot() +
  geom_bar(aes(x = StudioSvcs, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = StudioSvcs, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Studio Services")

ss_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = ss_trends, stat = "identity") +
  geom_text(data = ss_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Studio Services", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(ss_plot, ss_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
ss_imp <- ss_trends[nrow(ss_trends),ncol(ss_trends)] - ss_trends[1,ncol(ss_trends)]
cat("Change in 'Always' rating over period:", ss_imp)

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

# Plot stacked bar and "Very Satisfied" detail
vm_plot <- ggplot() +
  geom_bar(aes(x = VenMgmt, y = Freq, fill = Surveyed), 
           position = position_stack(reverse = TRUE), data = df, stat = "identity") +
  geom_text(data = df, aes(x = VenMgmt, y = pos, label = Freq), 
            vjust = 1.5, color = "black", size = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  labs(title = "TSG Subgroup", subtitle = "Vendor Management")

vm_trends_plot <- ggplot() +
  geom_bar(aes(x = Surveyed, y = Freq, fill = Surveyed),
           data = vm_trends, stat = "identity") +
  geom_text(data = vm_trends, aes(x = Surveyed, y = Freq, label = Freq), 
            vjust = 1.5, color = "black", size = 5) + 
  theme(legend.position = "none") +
  labs(title = "Vendor Mgmt", subtitle = "Very Satisfied")

# Arrange the two plots for pasting into deck
grid.arrange(vm_plot, vm_trends_plot, ncol = 2)

# Display the % improvement in "Always" to date
vm_imp <- vm_trends[nrow(vm_trends),ncol(vm_trends)] - vm_trends[1,ncol(vm_trends)]
cat("Change in 'Always' rating over period:", vm_imp)

# Arrange the 8 subgroup plots in two 2x2 formatted pieces
grid.arrange(am_trends_plot,
             bmps_trends_plot,
             ba_trends_plot,
             es_trends_plot,
             ncol = 2)

grid.arrange(
             ps_trends_plot,
             sd_trends_plot,
             ss_trends_plot,
             vm_trends_plot,
             ncol = 2)

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


