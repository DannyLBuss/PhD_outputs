apply_QC<-function(a,b,c,batchname){
  df_a<-read.csv(a,header=T)
  df_b<-read.csv(b,header=T)
  df_c<-read.csv(c,header=T)
  names(df_a)[2]<-"Item_a"
  names(df_b)[2]<-"Item_b"
  names(df_c)[2]<-"Item_c"
  a_tmp<-sub(".*[/@]","",a)
  df_a$Run<-sub("*.csv","",a_tmp)
  b_tmp<-sub(".*[/@]","",b)
  df_b$Run<-sub("*.csv","",b_tmp)
  df_b$Run<-batchname
  c_tmp<-sub(".*[/@]","",c)
  df_c$Run<-sub("*.csv","",c_tmp)
  df_c$Run<-batchname
  df_a$merge<-paste(df_a$Item_a,df_a$Row)
  df_b$merge<-paste(df_b$Item_b,df_b$Row)
  df_c$merge<-paste(df_c$Item_c,df_c$Row)
  df_a$CN_ratio<-((df_a$Amt./df_a$Amt..1)*(14/12))
  df_b$CN_ratio<-((df_b$Amt./df_b$Amt..1)*(14/12))
  df_c$CN_ratio<-((df_c$Amt./df_c$Amt..1)*(14/12))
  df_a<-df_a[!is.na(df_a$Row),]
  df_b<-df_b[!is.na(df_b$Row),]
  df_c<-df_c[!is.na(df_c$Row),]
  df_a_AP<-min(df_a$Ampl..44[df_a$Item_a=="Alanine"])
  df_b_AP<-min(df_b$Ampl..44[df_b$Item_b=="Alanine"])
  df_c_AP<-max(df_c$Ampl..44[df_c$Item_c=="Alanine"])
  X<-c("Alanine","Nylon","EMC","Caffeine","Protein 2")
  #Remove samples whose peaks are more than double or less than half reference Alanine
  for(i in 1:length(df_a$Row)){
    df_a$Amps[i]<-ifelse(df_a[i,2]%in%X,"STD","Sample")
  }
  for(i in 1:length(df_b$Row)){
    df_b$Amps[i]<-ifelse(df_b[i,2]%in%X,"STD","Sample")
  }
  for(i in 1:length(df_c$Row)){
    df_c$Amps[i]<-ifelse(df_c[i,2]%in%X,"STD","Sample")
  }
  df_a$Amps_b<-ifelse(df_a$Amps=="Sample" & df_a$Ampl..44 > (df_a_AP/2) | df_a$Amps=="Sample" & df_a$Ampl..44 < (df_a_AP*2), "Keep","Error")
  df_a<-df_a[df_a$Amps=="Sample" & df_a$Amps_b=="Keep"|df_a$Amps=="STD",]
  df_b$Amps_b<-ifelse(df_b$Amps=="Sample" & df_b$Ampl..44 > (df_b_AP/2) | df_b$Amps=="Sample" & df_b$Ampl..44 < (df_b_AP*2), "Keep","Error")
  df_b<-df_b[df_b$Amps=="Sample" & df_b$Amps_b=="Keep"|df_b$Amps=="STD",]
  df_c$Amps_b<-ifelse(df_c$Amps=="Sample" & df_c$Ampl..44 > (df_c_AP/2) | df_c$Amps=="Sample" & df_c$Ampl..44 < (df_c_AP*2), "Keep","Error")
  df_c<-df_c[df_c$Amps=="Sample" & df_c$Amps_b=="Keep"|df_c$Amps=="STD",]
  #(i) samples contained more than 13% and 5% of carbon and nitrogen, respectively
  df_a<-df_a[df_a$Amps=="Sample" & df_a$Amt. > 13|df_a$Amps=="Sample" & df_a$Amt..1 > 4.95|df_a$Amps=="STD",]
  df_b<-df_b[df_b$Amps=="Sample" & df_b$Amt. > 13|df_b$Amps=="Sample" & df_b$Amt..1 > 4.95|df_b$Amps=="STD",]
  df_c<-df_c[df_c$Amps=="Sample" & df_c$Amt. > 13|df_c$Amps=="Sample" & df_c$Amt..1 > 4.95|df_c$Amps=="STD",]
  cols_remove <- c(names(df_a[2]),names(df_b[2]),names(df_c[2]), "batchname","Run","merge","Amt.","d.13C.12C","Amt..1","d.15N.14N","CN_ratio")
  tmp_a<-df_a[, (colnames(df_a) %in% cols_remove)]
  tmp_b<-df_b[, (colnames(df_b) %in% cols_remove)]
  tmp_c<-df_c[, (colnames(df_c) %in% cols_remove)]
  d<-merge(tmp_a,tmp_b,by="merge", all=T)
  d<-merge(d,tmp_c,by="merge", all=T)
  names(d)<-recode(names(d),"Amt..x" = "Amt_Carbon_a", "d.13C.12C.x" = "delta_Carbon_a",
                   "Amt..1.x" = "Amt_Nitrogen_a","d.15N.14N.x" = "delta_Nitrogen_a",
                   "Run.x" = "Run_A", "CN_ratio.x" = "CN_ratio", "Amt..y" = "Amt_Carbon_b",
                   "d.13C.12C.y" = "delta_Carbon_b", "Amt..1.y" = "Amt_Nitrogen_b", 
                   "d.15N.14N.y" = "delta_Nitrogen_b", "Run.y" = "Run_B","CN_ratio.y" = "CN_ratio_B", 
                   "Amt." = "Amt_Carbon_c", "d.13C.12C" = "delta_Carbon_c", "Amt..1" = "Amt_Nitrogen_c", 
                   "d.15N.14N" = "delta_Nitrogen_c","CN_ratio" = "CN_ratio_c", "Run" = "Run_c")
  d$batchname<-batchname
  #(iv) create average from remaining samples
  mean_c<-rep(1.6,nrow(d))
  std_c<-rep(1.6,nrow(d))
  reps_c<-rep(1,nrow(d))
  for(i in 1:nrow(d)){
    mean_c[i]<-mean(c(d$delta_Carbon_a[i],d$delta_Carbon_b[i],d$delta_Carbon_c[i]),na.rm=T)
    std_c[i]<-sd(c(d$delta_Carbon_a[i],d$delta_Carbon_b[i],d$delta_Carbon_c[i]),na.rm=T)
  }
  d$mean_C<-mean_c
  d$StdDev_C<-std_c
  mean_n<-rep(1.6,nrow(d))
  std_n<-rep(1.6,nrow(d))
  mean_cn<-rep(1.6,nrow(d))
  d$delta_Nitrogen_b<-0.0
  d$delta_Nitrogen_b<-0.0
  d$delta_Carbon_b<-0.0
  d$delta_Nitrogen_b<-0.0
  for(i in 1:nrow(d)){
    mean_n[i]<-mean(c(d$delta_Nitrogen_a[i],d$delta_Nitrogen_b[i],d$delta_Nitrogen_c[i]),na.rm=T)
    std_n[i]<-sd(c(d$delta_Carbon_a[i],d$delta_Carbon_b[i],d$delta_Carbon_c[i]),na.rm=T)
    mean_cn[i]<-mean(c(d$CN_ratio_a[i],d$CN_ratio_b[i],d$CN_ratio_c[i]),na.rm=T)
  }
  d$mean_N<-mean_n
  d$StdDev_N<-std_n
  d$Mean_CN_ratio<-mean_cn
  d$reps<-0
  for(i in 1:length(d$Run_A)){
    d$reps[i]<-3-(na.count(c(d$Amt_Carbon_a[i],d$Amt_Carbon_b[i],d$Amt_Carbon_c[i])))
  }
  return(d)
}