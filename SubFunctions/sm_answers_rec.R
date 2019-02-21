#get all survey pages and questions for a recipient

answers_rec<-function(x,m){
  
  source("SubFunctions\\sm_answer_per_rec_spage.R")
  
  #x<-responses[[1]]$data$pages[[100]]$questions   all survey pages with questions for rec 8 on page 15
  #m<-meta_per_page[[1]][100,]
  
  
  meta<-apply(m,2,as.character)
  meta<-data.frame(t(data.frame(meta)))
  d<-lapply(x,answer_per_rec_spage)
  d2<-do.call(bind_rows,d)
  
  d3<-data.frame(t(d2))
  cols<-paste0(as.character(t(d3["q",])),".",as.character(t(d3["row_id",])))
  colnames(d3)<-cols
  d<-cbind(meta,d3[2,])
  cnames<-colnames(d)
  d2<-data.frame(d)
  colnames(d2)<-cnames
  d2
}

