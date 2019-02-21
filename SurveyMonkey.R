####### GET SURVEY RESPONSES FROM SURVEY MONKEY USING API ####################



####################################### LOAD DEPENDECIES ###############################################

library(httr)
library(dplyr)
library(foreach)
library(checkmate)
library(rlist)
library(tidyr)
library(doParallel)
library(parallel)
library(tidyverse)
library(keyring)
library(dplyr)

source("SurveyMonkeyFunctions.R")
service<-"SurveyMonkey"
keys<-key_list()
token<-keys$username[which(keys$service==service)]
auth_token<-token




#####Get surveys #########

surveys<-sm_get_surveys(auth_token = auth_token)$data[,c("id","title")]
tb<-surveys[1,]

### Get responses #########
responses_base<-sm_get_responses(auth_token = auth_token,survey_id = tb[1],page=1)

  #Get number of data pages
    pages<-ceiling(responses_base$total/responses_base$per_page)

  #Get pages with responses
    no_cores <- detectCores() - 1  
    cl <- parallel::makeCluster(no_cores)
    doParallel::registerDoParallel(cl)
    
    responses<-foreach(i=1:pages,.packages=c('foreach','httr')) %dopar% sm_get_responses(auth_token = auth_token,survey_id = tb[1],page=i)
    
    
 
  #Get metadata for each data page

  meta_per_page<-foreach(i=responses) %do% {
    data.frame(page=i$page,
             recipient_id = i$data$recipient_id,
             total_time = meta_id<-i$data$total_time,
             metadata = i$data$metadata,
             date_created = i$data$date_created,
             survey_id = i$data$survey_id,
             response_status = i$data$response_status)
    }

  #Get all data
  system.time(
    data<-answers_all_pages(responses,meta_per_page)
  )
  data<-data[,-length(colnames(data))]
  
  parallel::stopCluster(cl)
  registerDoSEQ()

## Get survey meta data
response_meta<-sm_get_response_details(auth_token = auth_token,survey_id = tb[1])
MetaData<-get_all_survey_meta(response_meta$pages[[3]])



#Map datafile to metadata, replace choice_id with choices_text and question_row ids with question_text/new_question_id



#Export file 
 
  keycol <- "question"
  valuecol <- "answer"
  sm_colnames<-colnames(data)
  gathercols <- as.character(sm_colnames[23:length(sm_colnames)])
  
  
  lookup_vals<-function(data,MetaData){
    x<-data.frame(choices_id=data)
    x<-x %>% left_join(unique(data.frame(choices_id=MetaData$choices_id,choices_position=MetaData$choices_position)),by="choices_id") %>% select(choices_position)
    x<-as.character(x[,1])
    x[is.na(x)]<-data[is.na(x)]
    data.frame(choices_position=x)
  }
  
  dataForLookup<-data[23:length(sm_colnames)]
  GetColOrder<-data.frame(question_rows_id=colnames(dataForLookup)) %>% 
               right_join(MetaData,by="question_rows_id") %>%
               select(question_rows_id,page,page_position)
  GetColOrder<-unique(GetColOrder)
  GetColOrder$bind<-as.numeric(as.character(paste0(GetColOrder$page,GetColOrder$page_position)))
  GetColOrder<-GetColOrder %>% arrange(GetColOrder$bind)
  
  metaframe<-data.frame(question_rows_id=GetColOrder$question_rows_id,
                        answer=rep(NA,length(GetColOrder$question_rows_id)))
  metaframe2<-data.frame(t(metaframe))
  colnames(metaframe2)<-metaframe$question_rows_id
  metaframe3<-metaframe2[2,]
  
  dataForLookup<-bind_rows(metaframe3,dataForLookup)
  dataForLookup<-dataForLookup[GetColOrder$question_rows_id][-1,]

  
  l<-apply(dataForLookup,2,lookup_vals,MetaData)
  DataLookUp<-bind_cols(l)
  GetColnames<-data.frame(question_rows_id = names(dataForLookup)) %>% 
              left_join(unique(data.frame(question_rows_id= MetaData$question_rows_id,
                                          question_heading_text=MetaData$question_heading_text)),by="question_rows_id") %>% 
              select(question_heading_text)
  
  GetColnames<-as.character(GetColnames[,1])
  
  CombColnames<-c(colnames(data[,1:22]),GetColnames)
  NewData<-data.frame(data[,1:22],DataLookUp)
  colnames(NewData)<-CombColnames
 
 
  write.csv2(NewData,paste0("C:\\Users\\jespe\\Desktop\\DataLookUp.",Sys.Date(),".csv"))





#To get summaries
#https://api.surveymonkey.com/v3/surveys/{id}/rollups
