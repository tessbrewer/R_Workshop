#read in the df
setwd('~/Documents/munich/R_Workshop/Proteomics/')
df = read.csv("data/raw_lfq_values.txt", sep = "\t", header = 1)
df

#drop the columns that aren't needed right now
df2 = df[,c(4,5,8:26)]
df2

install.packages("mgsub")
library(mgsub)

#add an underscore after the sample name and before the replicate number to facilitate tidying
column_names = colnames(df2)
for (column_number in 1:length(column_names)){
  column_name = column_names[column_number]
  if (grepl("WT", column_name) == TRUE) {
    new_column_name = mgsub::mgsub(column_name, c("WT", "min"), c("WT_", ""))
    column_names[column_number] = new_column_name
  }
  else if (grepl("DD", column_name) == TRUE) {
    new_column_name = mgsub::mgsub(column_name, c("DD", "min"), c("DD_", ""))
    column_names[column_number] = new_column_name
  }
}
column_names

#manually fix the name of the blank column
column_names[21] = "Blank_01_0"

#rearrange the column names of the df
colnames(df2) = column_names
df2

#convert the column names to variables
library(tidyr)
df3 = pivot_longer(df2, cols = 3:21, names_to = c("Sample", "Replicate", "Timepoint(min)"), names_sep = "_")
df3
