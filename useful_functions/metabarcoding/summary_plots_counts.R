###Summary_plots_MiFish_counts
library(tidyr)
library(plyr)
library(ggplot2)
library(readr)
library(dplyr)
setwd("/Volumes/DannyBuss_5/PhD/Chapters/7. Diet Chapter/6. Outputs/Paper/Analysis/")
DIR<-read.csv("/Volumes/DannyBuss_5/PhD/Chapters/7. Diet Chapter/2. Data/Meta_faecal_samples.csv")
filepath<-("/Volumes/DannyBuss_5/PhD/Chapters/7. Diet Chapter/3. Databases/18S/v9/")

#Function - loads all CSVs from a directory
library(readr)
names(DIR)[1]<-"sample"
files<-list.files(path=filepath, pattern="*.csv",full.names=TRUE)
dat_Csv<-ldply(files, read_csv)
df<-merge(dat_Csv,DIR, by="sample",all=TRUE)
df<-df[!is.na(df$loci),]
df$LOCI<-sub(".*\\_","",df$loci)
x<-df$LOCI[100]
all<-df
df$Class<-as.factor(df$Class)
prey_nly<-df[!df$Class=="Mammalia",]
prey_nly<-prey_nly[!is.na(prey_nly$loci),]
y<-prey_nly$LOCI[10]
mam<-df[df$Class=="Mammalia",]
mam<-mam[!is.na(mam$loci),]
write.csv(all, file=paste0(x,"alldata.csv"))
write.csv(prey_nly, file=paste0(y,"prey_only.csv"))
write.csv(mam, file=paste0(y,"mammals_only.csv"))
