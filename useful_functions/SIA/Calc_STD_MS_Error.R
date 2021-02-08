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
  Protein_2_range<-NA
  EMC_range<-NA
  CN_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(CN_std_errors[!names(CN_std_errors)=="Nylon_range"])
  if (!new.nylon) return(CN_std_errors)
}

calc_Nitrogen_STD<-function(df,new.nylon=TRUE){
  ##Nitrogen_error_for_stds
  Alanine_range<-round(df$mean_N[df$Item_b=="Alanine"],2)
  Alanine_range_b<-Alanine_range[complete.cases(Alanine_range)]
  for(i in 1:length(Alanine_range_b)){
    if(Alanine_range_b[i] > -0.4 | Alanine_range_b[i] < -1.6){message_AL[i]<-"Warning! Alanine is outside expected range"}
    else(message_AL[i]<-"EMC is in normal range")
  }
  print(message_AL)
  EMC_range<-round(df$mean_N[df$Item_b=="EMC"],2)
  EMC_range_b<-EMC_range[complete.cases(EMC_range)]
  for(i in 1:length(EMC_range_b)){
    if(EMC_range_b[i] > -2 | EMC_range_b[i] < -1.5){message_EMC[i]<-"Warning! EMC is outside expected range"}
    else(message_EMC[i]<-"EMC is in normal range")
  }
  print(message_EMC)
  Nitrogen_std<-data.frame(Alanine_range,EMC_range)
  return(Nitrogen_std)
}
calc_Nitrogen_STD(df,new.nylon = T)
  EMC_range<-round(max(df$mean_N[df$Item_c=="EMC"],na.rm=T)-min(df$mean_N[df$Item_c=="EMC"],na.rm=T),2)
  Nylon_range<-round(max(df$mean_N[df$Item_b=="Nylon"],na.rm=T)-min(df$mean_N[df$Item_b=="Nylon"],na.rm=T),2)
  Protein_2_range<-round(max(df$mean_N[df$Item_c=="Protein 2"],na.rm=T)-min(df$mean_N[df$Item_c=="Protein 2"],na.rm=T),2)
  Caffeine_range<-round(max(df$mean_N[df$Item_c=="Caffeine"],na.rm=T)-min(df$mean_N[df$Item_c=="Caffeine"],na.rm=T),2)
  Nitrogen_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(Nitrogen_std_errors[!names(Nitrogen_std_errors)=="Nylon_range"])
  if (!new.nylon) return(Nitrogen_std_errors)
}
calc_Carbon_STD<-function(df,new.nylon=TRUE){  
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
calc_CN_STD<-function(df,new.nylon=TRUE){  
  ##Carbon_error_for_stds
  ##CN_ratio_checks_for_stds
  Alanine_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Alanine"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Alanine"],na.rm=T),2)
  Nylon_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Nylon"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Nylon"],na.rm=T),2)
  Caffeine_range<-round(max(df$Mean_CN_ratio[df$Item_c=="Caffeine"],na.rm=T)-min(df$Mean_CN_ratio[df$Item_c=="Caffeine"],na.rm=T),2)
  Protein_2_range<-NA
  EMC_range<-NA
  CN_std_errors<-data.frame(Alanine_range,EMC_range, Nylon_range, Protein_2_range, Caffeine_range)
  if (new.nylon) return(CN_std_errors[!names(CN_std_errors)=="Nylon_range"])
  if (!new.nylon) return(CN_std_errors)
}