##############################################################################
#
# 2019-2020 Cust Sat Analysis
# Bill James / jamesw@csps.com
#
##############################################################################

#
# Library setups
#

# Import libraries
library(tidyverse)
library(dplyr)
library(plyr)
library(tidyr)
library(caret)
library(extrafont)
library(ggplot2)
library(scales)
library(googlesheets)
library(scales)
library(reshape2)
library(grid)
library(gridExtra)
library(janitor)
library(lattice)
library(rmarkdown)
library(kableExtra)
library(purrr)

#
# File open, cleanup, and set up for the analysis
#


# Import and Open the data file / Establish the data set
data_filename <- gs_title("2020 Survey All Users 20 02 05_T")
data_filename <- gs_title("2020 Survey Combined Recipients")
dat <- gs_read(data_filename, skip = 0, header = TRUE, stringsAsFactors = FALSE)
dat <- as.data.frame(dat)
dat[complete.cases(dat), ]
# dat <- dat %>% select(Dept, Email, `First Name`, `Last Name`, 'Survey_date')

# Consolidate Groups
dat[dat == "Office of the Clerk"]  <- "Clerk"
dat[dat == "Clerks Office"]  <- "Clerk"
dat[dat == "Technology Services Group"]  <- "TSG"
dat[dat == "Brdcst & Multimedia Svs"] <- "BMPS"
dat[dat == "Board Admin Office"] <- "Board Office"
dat[dat == "ComPub Manager's Office"] <- "Committee Office"
dat[dat == "Human Resources - SA"] <- "HR"
dat[dat == "Monitor Editorial"] <- "CSM"
dat[dat == "Bible Lesson Editorial & Products"]  <- "BLP"
dat[dat == "Bible Lesson Editorial and Products"] <- "BLP"
dat[dat == "Bible Lesson Products"] <- "BLP"
dat[dat == "Mary Baker Eddy Library"] <- "MBEL"
dat[dat == "Promotion & Design Services Group"] <- "Design Services"
dat[dat == "Office of Records Mgmt"] <- "ORM"
dat[dat == "Rights & Permissions Group"] <- "OGC"
dat[dat == "Office of the Clerk"] <- "Clerk"
dat[dat == "Clerks Office"] <- "Clerk"                                 
dat[dat == "Board of Education"] <- "BOE"
dat[dat == "Technology Services Group"] <- "TSG"                    
dat[dat == "Monitor Editorial"] <- "CSM"
dat[dat == "Bible Lesson Editorial & Products"] <- "BLP"              
dat[dat == "Treasurer's Office"] <- "TRO"                             
dat[dat == "Office of General Counsel"] <- "OGC"
dat[dat == "General Publications"] <- "CSPS"
dat[dat == "Board Admin Office"] <- "Board"                             
dat[dat == "JSH Editorial"] <- "JSH"
dat[dat == "Monitor Publishing"] <- "CSM"
dat[dat == "Office of Records Mgmt"] <- "ORM"                         
dat[dat == "Office of the Publishers Agent"] <- "OPA"
dat[dat == "TMC Reading Room"] <- "ISD/TMC RR"                               
dat[dat == "Bible Lesson Products"] <- "BLP"
dat[dat == "Manager's Office"] <- "CSPS"                               
dat[dat == "Church Activities"] <- "Church"
dat[dat == "Nursing, Branch and Practitioner"] <- "Church"               
dat[dat == "Facilities & Capital Proj"] <- "REPO"
dat[dat == "Monitor Editorial Dept"] <- "CSM"                         
dat[dat == "Committee on Publication"] <- "Committee Office"
dat[dat == "Monitor"] <- "CSM"                                      
dat[dat == "OCIO"] <- "TSG"
dat[dat == "Practitioner Services"] <- "Church"                          
dat[dat == "Christian Science Nursing Activities"] <- "Church"
dat[dat == "Human Resources"] <- "HR"
dat[dat == "Office of Language Svs"] <- "ISD/TMC RR"
dat[dat == "The Mary Baker Eddy Library"] <- "MBEL"                    
dat[dat == "Board of Lectureship"] <- "BOL"
dat[dat == "ComPub Managers Office"] <- "Committee Office"
dat[dat == "CSBD"] <- "Board"                                       
dat[dat == "Office of the Publisher's Agent"] <- "OPA"
dat[dat == "CSBD, Office of the Clerk"] <- "Clerk"                      
dat[dat == "Board of Trustees"] <- "Board"
dat[dat == "Customer Contact Center/Plaza Activities"] <- "CCC"       
dat[dat == "JSH GP Manufacturing"] <- "CSPS"
dat[dat == "CSM Jointly Managed"] <- "CSM"                            
dat[dat == "Security"] <- "REPO"
dat[dat == "Customer Contact Center/TMC Reading Rooms"] <- "CCC"     
dat[dat == "JSHEditorial"] <- "JSH"
dat[dat == "Real Estate Planning & Operations"] <- "REPO"              
dat[dat == "Office of Language Services"] <- "ISD/TMC RR"
dat[dat == "Practitioner Activities\\Plaza Activities"] <- "Church"      
dat[dat == "Office of Records Mgmt/Mary Baker Eddy Library"] <- "MBEL"
dat[dat == "CSM Marketing and Sales"] <- "CSM"                        
dat[dat == "Digital Publishing Services"] <- "CSM"
dat[dat == "CSPS Managers Office"] <- "CSPS"                           
dat[dat == "Office of the General Counsel, Plaza Activities"] <- "OGC"
dat[dat == "TSG"] <- "TSG"                                            
dat[dat == "JSHEd Herald"] <- "JSH"
dat[dat == "Site Svcs - InterOfficeServices"] <- "REPO"                
dat[dat == "JSH Publishing"] <- "JSH"                                 
dat[dat == "JSHQ Publishing"] <- "JSH"
dat[dat == "ISD"] <- "ISD/TMC RR"                                            
dat[dat == "JSH Production, Manufacturing"] <- "JSH"
dat[dat == "ComPub"] <- "Committee Office"
dat[dat == "Facilities &amp; Capital Proj"] <- "REPO"
dat[dat == "Site Services"] <- "REPO"                                  
dat[dat == "Monitor Editorial."] <- "CSM" 
dat[dat == "Customer Contact Center"] <- "CCC"
dat[dat == "Board Office"] <- "Board"
dat[dat == "Treasurers Office"] <- "TRO"
dat[dat == "Branch Activities"] <- "Church"
dat[dat == "Contractor"] <- "TSG"
dat[dat == "B&MPS / Clerk"] <- "BMPS"
dat[dat == "Office of the Treasurer"] <- "TRO"
dat[dat == "Clerk's Office"] <- "Clerk"
dat[dat == "Practitioner Activities"] <- "Clerk"

dept_list <- unique(dat$Dept)
dept_list <- sort(dept_list)
num_depts <- length(dept_list)

#-------------------------------------------------------------------------------
#
# Determine the departmental makeup of responders
# 
#-------------------------------------------------------------------------------

dept_df  <- data.frame(Department = 1:num_depts * 2, count = 1:num_depts * 2,   s_date = 1:num_depts * 2)
# count_3S20 = 1:num_depts, count_4S20 = 1:num_depts)

j = 1
for(i in 1:num_depts) {
  x <- dat %>% filter(Dept == unique(dat$Dept)[i])
  
  S1 <- x %>% filter(Survey_date == "1F19")
  S2 <- x %>% filter(Survey_date == "2W20")
  
  dept_df[j, 1] <- unique(dat$Dept)[i]
  dept_df[j, 2] <- nrow(S1)
  dept_df[j, 3] <- "1F19"
  
  j <- j + 1
  
  dept_df[j, 1] <- unique(dat$Dept)[i]
  dept_df[j, 2] <- nrow(S2)
  dept_df[j, 3] <- "2W20"
  
  j <- j + 1
}

dept_df <- dept_df[complete.cases(dept_df), ]
dept_df <- dept_df[order(dept_df$Department), ]
sort_df <- dept_df[order(rev(dept_df$Department)),]

# 
# Horizontal bar chart for result
#

ggplot(data = sort_df, aes(x = Department, y = count, fill = factor(s_date))) +
  geom_bar(position="dodge",stat="identity") +
  coord_flip() + 
  labs(title = "Number of Survey Invitations by Department by Quarter", subtitle = "Fall / Winter") +
  labs(y = "Count of Invitees") + 
  guides(fill = guide_legend(reverse=TRUE))


#-------------------------------------------------------------------------------
#
# Determine the set seed value for minimum overlaps
# 
#-------------------------------------------------------------------------------

# Get file for comparison
inpFile_F19 <- gs_title("TSG_CSS_F19_T")
F19_list <- gs_read(inpFile_F19, skip = 0, header = TRUE, stringsAsFactors = FALSE)

# W20 list starts with the full list
W20_list <- dat

# Set number of times to test for best seed
B <- 15000

# Set initial values for tests
min_dups  <- nrow(dat)
best_seed <- 1

for(i in 1:B) {
  set.seed(i)
  test_index  <- createDataPartition(y = dat$Dept, times = 1, p = 0.25, list = FALSE)
  W20_list    <- dat[test_index, ]
  dup_list    <- F19_list
  dup_list    <- rbind(dup_list, W20_list)
  dup_list    <- dup_list %>% mutate(dup = 0)
  dups        <- duplicated(dup_list)
  num_dups    <- length(dups[dups == TRUE])
  if(num_dups <  min_dups) {
    min_dups  <- num_dups
    best_seed <- i
  }
}

cat("Best seed value is:",best_seed)
cat("This value gives:",min_dups,"duplicates")

#  
# Random selection of 25% sample
#

set.seed(best_seed)

test_index  <- createDataPartition(y = dat$Dept, times = 1, p = 0.25, list = FALSE)
survey_list <- dat[test_index, ]

# Write out the distribution list
output_file <- gs_title("TSG_CSS_W20_T")
gs_edit_cells(output_file, ws = 1, input = survey_list, anchor = "A1", byrow = FALSE,
              col_names = NULL, trim = FALSE, verbose = TRUE)

#-------------------------------------------------------------------------------
#
# End
#
#-------------------------------------------------------------------------------
