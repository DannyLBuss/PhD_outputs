calc_Nitrogen_STDerror<-function(df,new.nylon=TRUE){
  ##Nitrogen_error_for_stds
  Alanine_range<-round(max(df$mean_N[df$Item_b=="Alanine"],na.rm=T)-min(df$mean_N[df$Item_b=="Alanine"],na.rm=T),2)
  EMC_range<-round(max(df$mean_N[df$Item_c=="EMC"],na.rm=T)-min(df$mean_N[df$Item_c=="EMC"],na.rm=T),2)
  Nylon_range<-round(max(df$mean_N[df$Item_b=="Nylon"],na.rm=T)-min(df$mean_N[df$Item_b=="Nylon"],na.rm=T),2)
  Protein_2_range<-round(max(df$mean_N[df$Item_c=="Protein 2"],na.rm=T)-min(df$mean_N[df$Item_c=="Protein 2"],na.rm=T),2)
  Caffeine_range<-round(max(df$mean_N[df$Item_c=="Caffeine"],na.rm=T)-min(df$mean_N[df$Item_c=="Caffeine"],na.rm=T),2)
  Nitrogen_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(Nitrogen_std_errors[!names(Nitrogen_std_errors)=="Nylon_range"])
  if (!new.nylon) return(Nitrogen_std_errors)
}
calc_Carbon_STDerror<-function(df,new.nylon=TRUE){  
  ##Carbon_error_for_stds
  Alanine_range<-round(max(df$mean_C[df$Item_c=="Alanine"],na.rm=T)-min(df$mean_C[df$Item_c=="Alanine"],na.rm=T),2)
  EMC_range<-round(max(df$mean_C[df$Item_a=="EMC"],na.rm=T)-min(df$mean_C[df$Item_a=="EMC"],na.rm=T),2)
  Nylon_range<-round(max(df$mean_C[df$Item_c=="Nylon"],na.rm=T)-min(df$mean_C[df$Item_c=="Nylon"],na.rm=T),2)
  Protein_2_range<-round(max(df$mean_C[df$Item_b=="Protein 2"],na.rm=T)-min(df$mean_C[df$Item_b=="Protein 2"],na.rm=T),2)
  Caffeine_range<-round(max(df$mean_C[df$Item_c=="Caffeine"],na.rm=T)-min(df$mean_C[df$Item_c=="Caffeine"],na.rm=T),2)
  Carbon_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(Carbon_std_errors[!names(Carbon_std_errors)=="Nylon_range"])
  if (!new.nylon) return(Carbon_std_errors)
}
calc_CN_STDerror<-function(df,new.nylon=TRUE){  
  ##Carbon_error_for_stds
  ##CN_ratio_checks_for_stds
  Alanine_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Alanine"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Alanine"],na.rm=T),2)
  Nylon_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Nylon"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Nylon"],na.rm=T),2)
  Caffeine_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Caffeine"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Caffeine"],na.rm=T),2)
  CN_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(CN_std_errors[!names(CN_std_errors)=="Nylon_range"])
  if (!new.nylon) return(CN_std_errors)
}