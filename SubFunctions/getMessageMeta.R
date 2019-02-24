################# Get message from collector in survey ###################

getMessage<-function(survey_id,collector_id) {
  d<-sm_get_message(auth_token = auth_token,survey_id = survey_id,collector_id = collector_id)
  d2<-d$data
  d2[,c(1,3:4)]
}  