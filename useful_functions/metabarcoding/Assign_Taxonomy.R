##This functions take a reference database in dada2 format and a fasta file of sequences with unknown taxonamy and converts it to dada2 format and assigns taxonomy
Assign_Taxonomy<-function(df,refFASTA){
  #names_keep<-sub("centroid=*","",names(df))
  names_keep<-names(df)
  tmp<-rep(as.character("NA"),length(df))
  for(i in 1:length(df)){
    tmp[[i]]<-as.character(df[[i]])
  }
  names(tmp)<-names_keep
  tmp<-toupper(tmp)
  spec<- assignTaxonomy(tmp, refFASTA)
  taxa.print <- spec # Removing sequence rownames for display only
  rownames(taxa.print) <- NULL
  #a<-data.frame(taxa.print)
  #plyr::count(a, vars=c("Order"))
  
  ###Put back together so abundance and taxanomic info in dataframe with sequences
  t<-unlist(taxa.print)
  meta_tax<-data.frame(names_keep,tmp,t)
  meta_tax$sample_size<-as.numeric(sub(".*size=","",names_keep))
  meta_tax$loci<-as.character(sub("\\..*","",names_keep))
  meta_tax$sample<-as.character(sub("\\_.*","",names_keep))
  char<-as.character(sub(".*\\.","",names_keep))
  meta_tax$seq_num<-as.numeric(sub("\\;.*","",char))
  rm(char)
  meta_tax$vsearchname<-as.character(sub("\\;.*","",names(df)))
  meta_tax$vsearchname_full<-as.character(names(df))
  char<-as.character(sub(".*seqs=","",names_keep))
  meta_tax$vscentroid_seqs<-as.numeric(sub("\\;.*","",char))
  rm(char)
  Year1<-as.character(c(paste("F",seq(1:17))))
  Year1<-gsub(" ","",Year1)
  Year2<-as.character(c(paste("F",seq(18,24))))
  Year2<-gsub(" ","",Year2)
  if (meta_tax$sample[1]%in%Year1){meta_tax$Year<-"2017"} else {meta_tax$Year<-"2018"}
  names(meta_tax)[2]<-"DNAstring"
  meta_tax$DNAstring[meta_tax$vscentroid_seqs<2]
  meta_tax$sample_size
  return(meta_tax)
}