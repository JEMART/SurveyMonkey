############  Get Recipient status for a collector in a survey #################
getRecipientsCollector<-function(survey_id,collector_id){
  base<-sm_get_recipient_info(auth_token = auth_token,
                              survey_id = survey_id,
                              collector_id = collector_id,
                              page=1)
  nr_pages<-ceiling(base$total/base$per_page)
  
  dlist<-foreach(i = 1:nr_pages) %do% {
    g<-sm_get_recipient_info(auth_token = auth_token,
                          survey_id = survey_id,
                          collector_id = collector_id,
                          page=i)
    d<-data.frame(g$data)
    d2<-d[,-which(colnames(d)=="custom_fields")]
    
    custom_fields<-data.frame(d$custom_fields)
    colnames(custom_fields)<-paste0("custom_field_",colnames(d$custom_fields))
    cbind(d2,custom_fields)
  }
  
  do.call(rbind,dlist)
  

}
