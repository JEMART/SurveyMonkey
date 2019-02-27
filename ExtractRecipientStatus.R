###### GET RECIPIENT STATUS FROM ALL COLLECTORS IN A SURVEY ##################
ExtractRecipientStatus<-function(survey_id){
  collectors<-getCollectors(survey_id = survey_id)
  selCollectorIds<-collectors  %>% select(id)
  getRec<-foreach(i=as.numeric(selCollectorIds[,1])) %do% getRecipientsCollector(survey_id,i)
  do.call(rbind,getRec)
}