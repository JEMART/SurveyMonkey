#Get resonses from all recipients on a data page

answers_page<-function(x,m){
  
  #x<-responses[[1]]$data$pages
  #m<-meta_per_page[[1]]
  
  msplit<-split(m,seq(nrow(m)))
  #no_cores <- detectCores() - 1  
  #cl <- parallel::makeCluster(no_cores)
  #doParallel::registerDoParallel(cl)
 
  f<-foreach(i=x,m=msplit,.packages=c('foreach','dplyr')) %dopar% {
    source("C:\\Users\\jespe\\Dropbox\\Min R mapp\\Rprojects\\SurveyMonkeyExtract\\SubFunctions\\sm_answer_per_rec_spage.R")
    x<-i$questions
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
    #answers_rec(i$questions,n)
  
  #parallel::stopCluster(cl)
  #registerDoSEQ()
  d<-do.call(bind_rows,f)
  
}