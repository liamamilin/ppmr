
# Frist step is the calculation of the process indicators to be predicted

# Concerning the type of prediction, we can classify the existing prediction types into three main macro-categories:

# 1. predictions related to numeric or continuous measures of interest (numeric predictions). Typical examples in this configuration are predictions related to the *remaining time* of an ongoing execution and predictions related to the *duration* of an ongoing case or to its *cost*;
# 2. predictions related to categorical or boolean outcomes (categorical predictions). Typical examples in this configuration are predictions related to the class of risk of a given execution or to the outcome of a predicate along the lifecycle of a case;
# 3. predictions related to sequences of future activities (activity sequence predictions). Typical examples of predictions falling under this category refer to the prediction of the sequence of the future activities (and of their payload) of a process case upon its completion.

# dataset :sepsis,patients,traffic_fines,hospital,hospital_billing


# prefix and create indicators with prefix eventlog


#' Prefix And Create Indicators  For Evnetlog
#' @description This function transform original event log to prefix event log and create indicators for predicting.
#' @param eventLog A event log which class is eventlog from bupaR.
#' @param prefix_num Prefix length of trace.
#' @param mode   Create indicators,for now，have two option :activity or duration.
#' @return  Event log with prefix and indicators.
#' @examples
#' library(bupar)
#' library(ppmr)
#' eventdata <- createProcessIndicatorsWithPrefix(eventLog = patients,prefix_num = 4,mode = "activity")


createProcessIndicatorsWithPrefix <- function(eventLog,prefix_num = 5,mode="activity"){
  # if(!require(dplyr)){
  #   install.packages("dplyr")
  # }
  # if(!require(bupaR)){
  #   install.packages("bupaR")
  # }

  require(dplyr)
  require(bupaR)
  emap <- mapping(eventLog)
  # if predicate is null, it is mean some trace length is shorter than prfix_num

  if(mode == "activity"){
    label <-  eventLog %>%
      group_by_case() %>% slice_events((prefix_num+1)) %>%
      ungroup_eventlog() %>% select(emap[["case_identifier"]],emap[["activity_identifier"]],force_df = TRUE)

    label <- rename(label,predicate = emap[["activity_identifier"]])
  }else if(mode == "duration"){
    label <- eventLog  %>% throughput_time(level = "case",units = "hours")
    label <- rename(label,predicate = throughput_time)

  }
  emap <- mapping(eventLog)
  case_id <- eventLog %>% case_id()

  eventLog_prefix <- eventLog %>% group_by(across(all_of(case_id))) %>% first_n(prefix_num) %>% ungroup_eventlog()
  eventLog_prefix <- eventLog_prefix %>% left_join(label,by = case_id)

  return(eventLog_prefix)
}




# enrich data

# 1. time perspective
# 2. resource perspective
# 3. data flow perspective data flow 由于太过于宽泛，
# 4. Conformance perspective  暂时不加入

# 目前提供两个角度的enrich data

#' Enrich Evenlog
#' @description This function create new feature for predicting.For now only consider two perspective:time perspective and resource perspective.
#' New feature include:idle time,processing time,throughput time,resource frequency,resource involvement.
#' @param eventLog A event log which class is eventlog from bupaR.
#' @param prefix_num prefix length of trace.
#' @param mode   create indicators,for now，have two option :activity or duration.
#' @return  event log with prefix , indicators and new feature.
#' @examples
#' library(bupar)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")

enrichEventlog <- function(eventLog,prefix_num,mode = "activity"){
  require(edeaR)
  require(dplyr)

  prefix_eventLog <- createProcessIndicatorsWithPrefix(eventLog,prefix_num,mode)
  # time perspective
  prefix_eventLog <- prefix_eventLog %>% idle_time(level = "case",units = "hours") %>% edeaR::augment(prefix_eventLog)
  prefix_eventLog <- prefix_eventLog %>% processing_time(level = "case",units = "hours")%>% edeaR::augment(prefix_eventLog)
  prefix_eventLog <- prefix_eventLog %>% throughput_time(level = "case",units = "hours")%>% edeaR::augment(prefix_eventLog)
  # resource
  prefix_eventLog <- prefix_eventLog %>% resource_frequency(level = "case")%>% edeaR::augment(prefix_eventLog)
  prefix_eventLog <- prefix_eventLog %>% resource_involvement(level = "case")%>% edeaR::augment(prefix_eventLog)


  return(prefix_eventLog)
}


