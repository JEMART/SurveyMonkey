#Get resonses from all recipients on a data page

answers_page<-function(x,m,Qm){
  
  #x<-responses[[2]]$data$pages
  #m<-meta_per_page[[2]]
  #Qm<-MetaData
  
  msplit<-split(m,seq(nrow(m)))
  Qm<-unique(Qm %>% select(question_family,question_id))

 
  f<-foreach(i=x,m=msplit,.packages=c('foreach','dplyr')) %dopar% {
    library(stringr)
    library(dplyr)
    source("C:\\Users\\jespe\\Dropbox\\Min R mapp\\Rprojects\\SurveyMonkeyExtract\\SubFunctions\\sm_answer_per_rec_spage.R")
    x<-i$questions
    meta<-apply(m,2,as.character)
    meta<-data.frame(t(data.frame(meta)))
    d<-lapply(x,answer_per_rec_spage)
    d2<-do.call(bind_rows,d)
    
    d3<-data.frame(t(d2))
    d4<-data.frame(t(d3))
    if("row_id" %in% colnames(d4)==FALSE) {
      d4<-d4 %>% inner_join(Qm,by=c("q"="question_id")) %>% select(question_family,q,choice_id)
      mrows<-which(d4$question_family == "multiple_choice")
      cols<-paste0(as.character(d4$q))
      cols[mrows]<-paste0(as.character(d4$q[mrows]),".",as.character(d4$choice_id[mrows]))
      nacols<-which(str_detect(cols,"NA")==TRUE)
      cols[nacols]<-d4$q[nacols]
    }else{
      d4<-d4 %>% inner_join(Qm,by=c("q"="question_id")) %>% select(question_family,q,choice_id,row_id)
      mrows<-which(d4$question_family == "multiple_choice")
      cols<-paste0(as.character(d4$q),".",as.character(d4$row_id))
      cols[mrows]<-paste0(as.character(d4$q[mrows]),".",as.character(d4$choice_id[mrows]))
      nacols<-which(str_detect(cols,"NA")==TRUE)
      cols[nacols]<-d4$q[nacols]
    }
    
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