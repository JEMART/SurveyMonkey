#answers per recipient and survey page
answer_per_rec_spage<-function(x) {
  
  #x<-responses[[1]]$data$pages[[100]]$questions[[26]]  #one recipient in survey page i.e. 18
  
  #Get all answers for each question on each survey page
 
  answers_page<-foreach(q=x$id,a=x$answers) %dopar% data.frame(q,a)
  answers_page_bind<-do.call(bind_rows,answers_page)
  
  #Place other_id as q 
  
  comb_other<-function(x){
    
    if("other_id" %in% colnames(x)){
      x$q<-as.numeric(as.character(x$q))
      other_pos<-which(is.na(x$other_id)==FALSE)
      x$q[other_pos]<-x$other_id[other_pos]
      x<-x[,-which(names(x) =="other_id")]
    }else{
      x
    }
    return(x)
  }
  
  answers_comb<-comb_other(answers_page_bind)
  if("choice_id" %in% colnames(answers_comb)){
    answers_comb$choice_id<-as.character(answers_comb$choice_id)
    text_pos<-which(is.na(answers_comb$text)==FALSE)
    if(length(text_pos)>0) {
      answers_comb$choice_id[text_pos]<-answers_comb$text[text_pos]
      data<-answers_comb[,-which(colnames(answers_comb)=="text")]
      
    }else{
      data<-answers_comb
      
    }
  }else{
    data<-answers_comb
    colnames(data)[which(colnames(data)=="text")]<-"choice_id"
  }
  return(data)
  
} 