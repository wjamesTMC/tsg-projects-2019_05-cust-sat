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

# Change the survey date format so it will display better
wkgdat[wkgdat == "01 - Fall 2018"]    <- "01-F18"
wkgdat[wkgdat == "02 - Winter 2019"]  <- "02-W19"
wkgdat[wkgdat == "03 - Spring 2019"]  <- "03-S19"

# Replace missing values (zeros) with NR and shorten "do not use"
wkgdat[wkgdat == 0]                   <- "NR"
wkgdat[wkgdat == ""]                  <- "NR"
wkgdat[wkgdat == "N/A do not use"]    <- "NA/DNU"

# Verify the number of unique values of the various factors
sapply(wkgdat, function(x)length(unique(x)))
dim(wkgdat) # should be 129 14

#--------------------------------------------------------------------
#
# Vocabulary analysis - Cumulative
#
#--------------------------------------------------------------------

#
# Positive vocabularly elements
#
pct <- 0
for(i in 1:length(comms$Positive)) {
  x <- str_detect(wkgdat$Comments, comms$Positive[i])
  pct <- pct + length(x[x == TRUE])
}
pct
cat("Positive comment ratio is:", pct / length(comms$Positive))

#
# Negative vocabularly elements
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
# Vocabulary analysis - by Survey
#
#--------------------------------------------------------------------

names(wkgdat)
wkgdat$Surveyed
dat01F18 <- wkgdat %>% filter(Surveyed == "01-F18")

q_dat <- unique(wkgdat$Surveyed)

#
# Positive vocabularly elements
#
pct <- 0
for(i in 1:length(comms$Positive)) {
  x <- str_detect(wkgdat$Comments, comms$Positive[i])
  pct <- pct + length(x[x == TRUE])
}
pct
cat("Positive comment ratio is:", pct / length(comms$Positive))

#
# Negative vocabularly elements
#
nct <- 0
for(i in 1:length(comms$Negative)) {
  x <- str_detect(wkgdat$Comments, comms$Negative[i])
  nct <- nct + length(x[x == TRUE])
}
nct
cat("Negative comment ratio is:", nct / length(comms$Negative))




# Get the Experience Attributes into a group
expfactors <- transform(wkgdat[c(1,2,3,4,5)]) 
summary(expfactors)

#
# Experience Attribute 1
#

# Create the pivot
pc_table <- with(expfactors, table(Surveyed, PosContrib))
pc_table
round(prop.table(pc_table), digits = 3)

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
pc_imp <- round(((pc_trends[length(pc_trends$Surveyed),3] - pc_trends[1,3]) / 
                   pc_trends[length(pc_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Always' over period:", pc_imp,"%")

#
# Experience Attribute 2
#

# Create the pivot
tr_table <- with(expfactors, table(Surveyed, TimelyResp))
tr_table
str(tr_table)
round(prop.table(tr_table), digits = 3)

tr_trends <- with(expfactors, table(TimelyResp, Surveyed))
tr_trends <- as.data.frame(tr_trends)
tr_trends <- tr_trends %>% filter(TimelyResp == "1-Always") 

# Convert to dataframe and set frequency position
df <- as.data.frame(tr_table)
df <- ddply(df, .(TimelyResp),
            transform, pos = cumsum(Freq))

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
tr_imp <- round(((tr_trends[length(tr_trends$Surveyed),3] - tr_trends[1,3]) / 
                   tr_trends[length(tr_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Always' over period:", tr_imp,"%")

#
# Experience Attribute 3
#

# Create the pivot
ac_table <- with(expfactors, table(Surveyed, Accountability))
ac_table
round(prop.table(ac_table), digits = 3)

ac_trends <- with(expfactors, table(Accountability, Surveyed))
ac_trends <- as.data.frame(ac_trends)
ac_trends <- ac_trends %>% filter(Accountability == "1-Always") 

# Convert to dataframe and set frequency position
df <- as.data.frame(ac_table)
df <- ddply(df, .(Accountability),
            transform, pos = cumsum(Freq))

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
ac_imp <- round(((ac_trends[length(ac_trends$Surveyed),3] - ac_trends[1,3]) / 
                   ac_trends[length(ac_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Always' over period:", ac_imp,"%")

#
# Experience Attribute 4
#

# Create the pivot
kn_table <- with(expfactors, table(Surveyed, Knowledgeable))
kn_table
round(prop.table(kn_table), digits = 3)

kn_trends <- with(expfactors, table(Knowledgeable, Surveyed))
kn_trends <- as.data.frame(kn_trends)
kn_trends <- kn_trends %>% filter(Knowledgeable == "1-Always") 

# Convert to dataframe and set frequency position
df <- as.data.frame(kn_table)
df <- ddply(df, .(Knowledgeable),
            transform, pos = cumsum(Freq))

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
kn_imp <- round(((kn_trends[length(kn_trends$Surveyed),3] - kn_trends[1,3]) / 
                   kn_trends[length(kn_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Always' over period:", kn_imp,"%")

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

# Create the pivot
am_table <- with(grpfactors, table(Surveyed, AcctMgrs))
am_table
round(prop.table(am_table), digits = 3)

am_trends <- with(grpfactors, table(AcctMgrs, Surveyed))
am_trends <- as.data.frame(am_trends)
am_trends <- am_trends %>% filter(AcctMgrs == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(am_table)
df <- ddply(df, .(AcctMgrs),
            transform, pos = cumsum(Freq))

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

am_imp <- round(((am_trends[length(am_trends$Surveyed),3] - am_trends[1,3]) / 
                     am_trends[length(am_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", am_imp,"%")

#
# Subgroup 2
#

# Create the pivot
bmps_table <- with(grpfactors, table(Surveyed, BMPS))
bmps_table
round(prop.table(bmps_table), digits = 3)

bmps_trends <- with(grpfactors, table(BMPS, Surveyed))
bmps_trends <- as.data.frame(bmps_trends)
bmps_trends <- bmps_trends %>% filter(BMPS == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(bmps_table)
df <- ddply(df, .(BMPS),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
bmps_imp <- round(((bmps_trends[length(bmps_trends$Surveyed),3] - bmps_trends[1,3]) / 
                     bmps_trends[length(bmps_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", bmps_imp,"%")

#
# Subgroup 3
#

# Create the pivot
ba_table <- with(grpfactors, table(Surveyed, BusApps))
ba_table
round(prop.table(ba_table), digits = 3)

ba_trends <- with(grpfactors, table(BusApps, Surveyed))
ba_trends <- as.data.frame(ba_trends)
ba_trends <- ba_trends %>% filter(BusApps == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(ba_table)
df <- ddply(df, .(BusApps),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
ba_imp <- round(((ba_trends[length(ba_trends$Surveyed),3] - ba_trends[1,3]) / 
                   ba_trends[length(ba_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", ba_imp,"%")

#
# Subgroup 4
#

# Create the pivot
es_table <- with(grpfactors, table(Surveyed, EventSvcs))
es_table
round(prop.table(es_table), digits = 3)

es_trends <- with(grpfactors, table(EventSvcs, Surveyed))
es_trends <- as.data.frame(es_trends)
es_trends <- es_trends %>% filter(EventSvcs == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(es_table)
df <- ddply(df, .(EventSvcs),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
es_imp <- round(((es_trends[length(es_trends$Surveyed),3] - es_trends[1,3]) / 
                   es_trends[length(es_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", es_imp,"%")

#
# Subgroup 5
#

# Create the pivot
ps_table <- with(grpfactors, table(Surveyed, ProjSupp))
ps_table
round(prop.table(ps_table), digits = 3)

ps_trends <- with(grpfactors, table(ProjSupp, Surveyed))
ps_trends <- as.data.frame(ps_trends)
ps_trends <- ps_trends %>% filter(ProjSupp == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(ps_table)
df <- ddply(df, .(ProjSupp),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
ps_imp <- round(((ps_trends[length(ps_trends$Surveyed),3] - ps_trends[1,3]) / 
                   ps_trends[length(ps_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", ps_imp,"%")

#
# Subgroup 6
#

# Create the pivot
sd_table <- with(grpfactors, table(Surveyed, ServDesk))
sd_table
round(prop.table(sd_table), digits = 3)

sd_trends <- with(grpfactors, table(ServDesk, Surveyed))
sd_trends <- as.data.frame(sd_trends)
sd_trends <- sd_trends %>% filter(ServDesk == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(sd_table)
df <- ddply(df, .(ServDesk),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
sd_imp <- round(((sd_trends[length(sd_trends$Surveyed),3] - sd_trends[1,3]) / 
                   sd_trends[length(sd_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", sd_imp,"%")

#
# Subgroup 7
#

# Create the pivot
ss_table <- with(grpfactors, table(Surveyed, StudioSvcs))
ss_table
round(prop.table(ss_table), digits = 3)

ss_trends <- with(grpfactors, table(StudioSvcs, Surveyed))
ss_trends <- as.data.frame(ss_trends)
ss_trends <- ss_trends %>% filter(StudioSvcs == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(ss_table)
df <- ddply(df, .(StudioSvcs),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
ss_imp <- round(((ss_trends[length(ss_trends$Surveyed),3] - ss_trends[1,3]) / 
                   ss_trends[length(ss_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", ss_imp,"%")

#
# Subgroup 8
#

# Create the pivot
vm_table <- with(grpfactors, table(Surveyed, VenMgmt))
vm_table
round(prop.table(vm_table), digits = 3)

vm_trends <- with(grpfactors, table(VenMgmt, Surveyed))
vm_trends <- as.data.frame(vm_trends)
vm_trends <- vm_trends %>% filter(VenMgmt == "1-Very Satisfied")

# Convert to dataframe and set frequency position
df <- as.data.frame(vm_table)
df <- ddply(df, .(VenMgmt),
            transform, pos = cumsum(Freq))

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

# Display the % improvement in "Very Satisfied" to date
vm_imp <- round(((vm_trends[length(vm_trends$Surveyed),3] - vm_trends[1,3]) / 
                   vm_trends[length(vm_trends$Surveyed),3]) * 100, digits = 2)
cat("Improvement for 'Very Satisfied' over period:", vm_imp,"%")

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


