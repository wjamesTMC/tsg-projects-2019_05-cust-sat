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
data_filename <- gs_title("2020 Survey Recipients")
dat <- gs_read(data_filename, skip = 0, header = TRUE, stringsAsFactors = FALSE)
dat <- as.data.frame(dat)

# Set up for the partition
data(dat)

#
# Random selection of 25% of the organization
#

set.seed(1911)
test_index  <- createDataPartition(dat$Group, times = 1, p = 0.25, list = FALSE)
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


