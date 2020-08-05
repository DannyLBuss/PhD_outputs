calc_overlap_prey<-function(df_pred,df_prey){
  sample_C_TEF<-rnorm(100,mean=1.28,sd=0.38)
  sample_N_TEF<-rnorm(100,mean=1.52,sd=0.3)
  df1<-rep(0.00,15)
  df1<-list(df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,df1,
            df1,df1,df1,df1,df1,df1,df1,df1,df1,df1)
  df2<-rep(0.00,15)
  df2<-list(df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,df2,
            df2,df2,df2,df2,df2,df2,df2,df2,df2,df2)
  for(i in 1:100){
    df1[[i]]<-df_pred$mean_d13C-sample_C_TEF[i]
    df2[[i]]<-df_pred$Mean_d15N-sample_N_TEF[i]
  }
  for(i in 1:length(df1)){
    sei[[i]]<-data.frame(df1[[i]],df2[[i]],rep("sei",15),rep("Balaenoptera borealis",15))
    names(sei[[i]])<-c("mean_d13C","Mean_d15N","Species","item") 
  }
  df<-list()
  for(i in 1:length(sei)){
    df[[i]]<-bind_rows(df_prey,sei[[i]])
  }
  test.kin<-list()
  for(i in 1:length(sei)){
    test.kin[[i]]<-estMCP(data=df[[i]], x="mean_d13C", y="Mean_d15N", group="item",
                          levels=c(50,75,95))
  }
  for(i in 1:length(sei)){
    kin.area[[i]]<-getArea(test.kin[[i]])
    kin.olp[[i]]<-calcOverlap(test.kin[[i]])
  }
  Percent_overlap_50<-rep(0,length(sei))
  Percent_overlap_75<-rep(0,length(sei))
  Percent_overlap_95<-rep(0,length(sei))
  for(i in 1:length(sei)){
    Percent_overlap_50[i]<-kin.olp[[i]][1,5]
    Percent_overlap_75[i]<-kin.olp[[i]][2,6]
    Percent_overlap_95[i]<-kin.olp[[i]][3,7]
  }
  overlaps<-data.frame(Percent_overlap_50,Percent_overlap_75,Percent_overlap_95)
  return(overlaps)
}