############# SURVEY MONKEY FUNCTIONS ######################

##### ##### FUNCTIONS ###############################

sm_get_surveys <- function(auth_token, page = 1, per_page = 250) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste('https://api.surveymonkey.com/v3/surveys?page=', page, '&per_page=', per_page, sep='')
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  survey_list <- jsonlite::fromJSON(json)
  survey_list
}

############################################################################
sm_get_collector <- function(auth_token, survey_id) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste0('https://api.surveymonkey.com/v3/surveys/',survey_id,'/collectors')
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  responses <- jsonlite::fromJSON(json)
  responses
}

############################################################################
sm_get_message <- function(auth_token, survey_id,collector_id) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste0('https://api.surveymonkey.com/v3/surveys/',survey_id,'/collectors/',collector_id,'/messages')
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  responses <- jsonlite::fromJSON(json)
  responses
}

#############################################################################
sm_get_recipient_info <- function(auth_token, survey_id,collector_id,page,per_page = 100) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste0('https://api.surveymonkey.com/v3/surveys/',survey_id,'/collectors/',collector_id,'/recipients?include=survey_response_status,mail_status,custom_fields&page=', page, '&per_page=', per_page)
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  responses <- jsonlite::fromJSON(json)
  responses
}


################################################################################

sm_get_responses <- function(auth_token, survey_id,page = 1, per_page = 1000) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste0('https://api.surveymonkey.com/v3/surveys/',survey_id,'/responses/bulk?page=',page,'&per_page=',per_page)
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  responses <- jsonlite::fromJSON(json)
  responses
}

############################################################################

sm_get_response_details <- function(auth_token, survey_id) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your access token for SurveyMonkey: ')
  }
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste0('https://api.surveymonkey.com/v3/surveys/',survey_id,'/details')
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  json <- httr::content(survey_list_response, as = 'text')
  responses <- jsonlite::fromJSON(json)
  responses
}

############################################################################
get_survey_meta<-function(x,p){
  
  #x = response_meta$pages[[3]]
  #p= page
  
  #Extract from structure  
  cnames.func<- function(x,y) {
    colnames(x)<-y 
    x
  }
  
  m<-x[[p]][,,]
  d<-foreach(i=m) %do% data.frame(i)
  
  mlist<-list(NULL)
  for(i in 1:length(m)){
    
    if(checkList(m[[i]])==TRUE){
      mlist[[i]]<-data.frame(unlist(m[[i]]))
    }else{
      mlist[[i]]<-data.frame(m[[i]])
    }
    
    if(is.null(names(m[[i]]))==TRUE) {
      names(mlist[[i]])<-names(m)[i]
    }else{
      names(mlist[[i]])<-names(mlist[[i]])
    }
  }
  
  metaData1<-do.call(cbind,mlist)
  metaData1$headings<-foreach(i=metaData1$headings,.final=unlist) %do% sub('(?<=\\?).*$', '',i,perl=TRUE)
  
  if(is.null(metaData1$choices)==FALSE) {
    
    choices<-metaData1$choices
    rows<-if("rows" %in% names(metaData1)) metaData1$rows else NULL
    null_choice_df<-data.frame(choices_score=NA,choices_visible=NA,choices_text=NA,choices_position=NA,choices_id=NA)
    null_rows_df<-data.frame(rows_visible=NA,rows_text=NA,rows_position=NA,rows_id=NA)
    choices<-foreach(i=choices) %do% if(is.null(i))  null_choice_df else i
    if("rows" %in% names(metaData1)) {
      rows<-foreach(i=rows) %do% if(is.null(i))  null_rows_df else i
      
    }
    
    namesMetaData<-names(metaData1)
    pos.choices<-which(namesMetaData=="choices")
    pos.rows<-which(namesMetaData=="rows")
    
    if(is.null(metaData1$other)==FALSE) {   #if there is an other option in any of the questions on a page
      other<-metaData1$other
      names(other)<-paste0("other_",names(other))
      pos.other<-which(namesMetaData=="other")
      metaData<-if(is.null(pos.rows)==TRUE) metaData1[,-c(pos.choices,pos.other)] else metaData1[,-c(pos.choices,pos.other,pos.rows)]
     
      names(metaData)<-paste0("question_",names(metaData))
      names.choices<-paste0("choices","_",names(choices[[1]]))
      names.rows<-paste0("rows","_",names(rows[[1]]))
      
      choicesNew<-if(is.null(choices)|length(choices)<1)  NULL else lapply(choices,cnames.func,names.choices) 
      rowsNew<-if(is.null(rows)|length(row)<1) NULL else lapply(rows,cnames.func,names.rows) 
      
      
      metaData_list<-split(metaData, seq(nrow(metaData)))
      
      if(is.null(choicesNew)==FALSE & is.null(rowsNew)==FALSE) {
        meta3<-foreach(i=metaData_list,r=rowsNew) %do% data.frame(i,as.matrix(r))
        meta3_split<-foreach(i=meta3) %do% split(i, seq(nrow(i)))
        meta4<-foreach(i=meta3_split,b=choicesNew) %do% {
          lapply(i,data.frame,as.matrix(b))
        }
        metaData2<-do.call(bind_rows,meta4)
        
      }else{
        if(is.null(choicesNew)==TRUE) {
          metaData2<-foreach(i=metaData_list,r=rowsNew) %do% data.frame(i,as.matrix(r))
        }else{
          metaData2<-foreach(i=metaData_list,b=choicesNew) %do% data.frame(i,as.matrix(b))
        }
      }
        
        
      other_id_pos<-which(is.null(other$other_id)==FALSE)
      other_data<-foreach(i=metaData2,s=split(other,seq(nrow(other)))) %do% {
        
        unique(data.frame(question_type = 'other',
                   question_family = "other_open",
                   question_visible =s$other_visible,
                   question_id=s$other_id,
                   null_choice_df,
                   question_headings=paste0(i$question_headings," ",s$other_text),
                   question_position=(i$question_position+1)-0.1))
      }
      
      other_data<-other_data[other_id_pos]
      metaData<-c(metaData2,other_data)
      
      
      
    }else{
      
      metaData<- if(is.null(pos.choices)==TRUE)   metaData1 else {
        if(is.null(pos.rows)==TRUE) metaData1[,-c(pos.choices)] else metaData1[,-c(pos.choices,pos.rows)]
      }
      
      names(metaData)<-paste0("question_",names(metaData))
      metaData_list<-split(metaData, seq(nrow(metaData)))
    
      names.choices<-paste0("choices","_",names(choices[[1]]))
      names.rows<-paste0("rows","_",names(rows[[1]]))
     
      choicesNew<-if(is.null(choices)|length(choices)<1)  NULL else lapply(choices,cnames.func,names.choices) 
      rowsNew<-if(is.null(rows)|length(row)<1) NULL else lapply(rows,cnames.func,names.rows) 
      
      if(is.null(choicesNew)==FALSE & is.null(rowsNew)==FALSE) {
        meta3<-foreach(i=metaData_list,r=rowsNew) %do% data.frame(i,as.matrix(r))
        meta3_split<-foreach(i=meta3) %do% split(i, seq(nrow(i)))
        meta4<-foreach(i=meta3_split,b=choicesNew) %do% {
          lapply(i,data.frame,as.matrix(b))
        }
        metaData2<-do.call(bind_rows,meta4)
        
      }else{
        if(is.null(choicesNew)==TRUE) {
          metaData2<-foreach(i=metaData_list,r=rowsNew) %do% data.frame(i,as.matrix(r))
        }else{
          metaData2<-foreach(i=metaData_list,b=choicesNew) %do% data.frame(i,as.matrix(b))
        }
      }
      
      metaData<-metaData2
    }  
    
    metaData_bind<-if(checkList(metaData)==TRUE) do.call(bind_rows,metaData) else metaData
    metaData_bind$page<-p
    
    metaData_bind_sort<-metaData_bind %>% arrange(question_position)
    
    metaData_split<-split( metaData_bind_sort , f = metaData_bind_sort$question_position )
    
    metaDataPos<-list(NULL)
    for(i in 1:length(metaData_split)){
      metaDataPos[[i]]<-data.frame(metaData_split[[i]],page_position=i)
    }
    
    metaData<-do.call(bind_rows,metaDataPos)
    
  }else{
    names(metaData1)<-paste0("question_",names(metaData1))
    metaData<-data.frame(metaData1,page=p,page_position = metaData1$question_position)
    
  }
  return(metaData)
  
}

get_all_survey_meta<-function(x) {  #x = response_meta$pages[[3]] 
  
  QNr<-length(x) #Get number of questions
  Meta<-list(NULL)
  for(i in 1:QNr) {
    Meta[[i]]<-get_survey_meta(x,i)
  }
  
  d<-unique(do.call(bind_rows,Meta))
  d$question_rows_id<-paste0(d$question_id,".",d$rows_id)
  d$rows_text[is.na(d$rows_text)]<-" "
  d$question_heading_text<-paste0(d$question_headings," ",d$rows_text)
  d
  
}


#################################### GET RESPONSES ########################################################


#Get responses for all data pages

answers_all_pages<-function(x,m){
  
  #x<-responses 
  #m<-meta_per_page
  lp<-function(n,x,m){
    source("SubFunctions\\sm_answers_page.R")
    answers_page(x[[n]]$data$pages,m[[n]])
    }
 
  d<-foreach(n=1:length(x),.packages=c('doParallel')) %dopar% lp(n,x,m)
  do.call(bind_rows,d)
  
}

