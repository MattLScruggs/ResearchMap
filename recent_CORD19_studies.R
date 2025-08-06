library(tidyverse)

setwd("~/Capstone/R code")
#### reading in the data ####
meta_data_sample<-read.csv("metadata.csv", nrows=10)
colnames(meta_data_sample)
all_cols<-as.list(apply(meta_data_sample, 2, class))
cols_needed <- c("doi","abstract","journal","publish_time")
all_cols[!names(all_cols) %in% cols_needed] = list(NULL)

meta_data<- read.csv("metadata.csv", header=TRUE, colClasses = all_cols)

meta_data$publish_time<-as.Date(meta_data$publish_time)

meta_data2<-meta_data[(meta_data$publish_time >= '2020-01-01' & meta_data$publish_time <='2020-06-30'),]%>%drop_na()

write.csv(meta_data2, "CORD_19_Abstracts_H1_2020.csv", row.names = F)
