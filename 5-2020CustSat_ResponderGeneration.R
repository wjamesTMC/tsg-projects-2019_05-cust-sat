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
library(caret)
library(googlesheets)


#
# File open, cleanup, and set up for the analysis
#


# Import and Open the data file / Establish the data set
data_filename <- gs_title("2020 Survey All Users 19 11 08")
dat <- gs_read(data_filename, skip = 0, header = TRUE, stringsAsFactors = FALSE)
dat <- as.data.frame(dat)
dat <- dat %>% select(Department, Email, `First Name`, `Last Name`)

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

#
# Random selection of 25% of the organization
#

# set.seed(1112)

test_index  <- createDataPartition(y = dat$Department, times = 1, p = 0.25, list = FALSE)
survey_list <- dat[test_index, ]

# Write out the distribvution list
output_file <- gs_title("TSG_CS_Nov2019")
gs_edit_cells(output_file, ws = 1, input = survey_list, anchor = "A1", byrow = FALSE,
              col_names = NULL, trim = FALSE, verbose = TRUE)


#--------------------------------------------------------------------
#
# End
#
#--------------------------------------------------------------------


