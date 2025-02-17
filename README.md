---
author: BenjaminDecorte
title: "READme"
output: 
  html_document:
    self_contained: no

---

This file describes how run_analysis.R script works.

 *   First, unzip the data from [cloudfront](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
 *   Second, use source("run_analysis.R") command in RStudio.
 *   Third, you will find two output files are generated in the current working directory:
        "GOODdataset.txt" : it contains a data frame called GOODDataset with 10299*68 dimension.
        "GOODDatasetWithMean.txt" : it contains a data frame called result with 180*68 dimension.
 *   Finally, use data <- read.table("GOODDatasetWithMean.txt") command in RStudio to read the file. Since we are required to get the average of each variable for each activity and each subject, and there are 6 activities in total and 30 subjects in total, we have 180 rows with all combinations for each of the 66 features.

