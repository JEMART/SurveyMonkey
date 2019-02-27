############  Get Collector IDs #################
getCollectors<-function(survey_id){ 
  d<-sm_get_collector(auth_token = auth_token,survey_id = survey_id)
  d2<-d$data
  d2
}
