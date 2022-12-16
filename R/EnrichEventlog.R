
# Frist step is the calculation of the process indicators to be predicted

# Concerning the type of prediction, we can classify the existing prediction types into three main macro-categories:

# 1. predictions related to numeric or continuous measures of interest (numeric predictions). Typical examples in this configuration are predictions related to the *remaining time* of an ongoing execution and predictions related to the *duration* of an ongoing case or to its *cost*;
# 2. predictions related to categorical or boolean outcomes (categorical predictions). Typical examples in this configuration are predictions related to the class of risk of a given execution or to the outcome of a predicate along the lifecycle of a case;
# 3. predictions related to sequences of future activities (activity sequence predictions). Typical examples of predictions falling under this category refer to the prediction of the sequence of the future activities (and of their payload) of a process case upon its completion.

# dataset :eventlog,patients,traffic_fines,hospital,hospital_billing


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
    label$predicate <- as.numeric(label$predicate)
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



#' Analyse event in control-flow perspective, time perspective and resource perspective
#'
#' @param eventlog A event log
#' @param perspective Analysis perspective, option: control-flow,time and resource
#' @return A list of result
#' @examples
#' x <- eventAnalyse(eventlog = patients,perspective = "time")

eventAnalyse <- function(eventlog,
                         perspective=c("controlflow","time","resource"),
                         coverage.p=0.2){
  require(bupaR)
  require(dplyr)
  if(any(perspective=="controlflow")){
    x.1 <- resource_frequency(eventlog,level = "log")
    x.2 <- resource_frequency(eventlog,level = "case")
    x.3 <- resource_frequency(eventlog,level = "activity")
    x.4 <- resource_frequency(eventlog,level = "resource")
    x.5 <- resource_frequency(eventlog,level = "resource-activity")
    x.6 <- resource_involvement(eventlog,level = "case")
    x.7 <- resource_involvement(eventlog,level = "resource")
    x.8 <- resource_specialisation(log = eventlog,level = "log")
    x.9 <- resource_specialisation(log = eventlog,level = "activity")
    x.10 <- resource_specialisation(log = eventlog,level = "resource")
    p.1 <- resource_map(log = eventlog)
    p.2 <- plot(resource_matrix(eventlog))

    return(list("resource distribution"=x.1,
                "resource distribution in different case"=x.2,
                "resource distribution in different activity"=x.3,
                "resource frequency"=x.4,
                "analyze resource with activity"=x.5,
                "resource involvement in diifferent case"=x.6,
                "resource involvement"=x.7,
                "resource specialisation distribution"=x.8,
                "resource specialisation distribution in different case"=x.9,
                "resource specialisation"=x.10,
                "resource map of an event log based on handover of work"=p.1,
                "resource precedence matrix"=p.2))

  }else if(any(perspective=="time")){
    x.1 <- start_activities(log = eventlog,level = "log")
    x.2 <- start_activities(log = eventlog,level = "case")
    x.3 <- start_activities(log = eventlog,level = "activity")
    x.4 <- start_activities(log = eventlog,level = "resource")
    x.5 <- start_activities(log = eventlog,level = "resource-activity")

    x.6 <- end_activities(log = eventlog,level = "log")
    x.7 <- end_activities(log = eventlog,level = "case")
    x.8 <- end_activities(log = eventlog,level = "activity")
    x.9 <- end_activities(log = eventlog,level = "resource")
    x.10 <- end_activities(log = eventlog,level = "resource-activity")

    y.1 <- list("number of start activities"=x.1,
                "start activity in different case"=x.2,
                "start activity"=x.3,
                "start activity in different resource"=x.4,
                "start activity with resource" = x.5)
    y.2 <- list("number of end activities"=x.6,
                "end activity in different case"=x.7,
                "end activity"=x.8,
                "end activity in different resource"=x.9,
                "end activity with resource" = x.10)

    x.11 <- trace_coverage(log = eventlog,level = "log")
    x.12 <- trace_coverage(log = eventlog,level = "trace")
    x.13 <- trace_coverage(log = eventlog,level = "case")

    y.3 <- list("trace coverage distribution "=x.11,
                "trace coverage"=x.12,
                "trace coverage in different case"=x.13)

    x.14 <- trace_length(log = eventlog,level = "log")
    x.15 <- trace_length(log = eventlog,level = "trace")
    x.16 <- trace_length(log = eventlog,level = "case")
    y.4 <- list("trace length distribution"=x.14,
                "trace length"=x.15,
                "trace length in  different case"=x.16)

    x.17 <- number_of_repetitions(log = eventlog,level = "log")
    x.18 <- number_of_repetitions(log = eventlog,level = "case")
    x.19 <- number_of_repetitions(log = eventlog,level = "activity")
    x.20 <- number_of_repetitions(log = eventlog,level = "resource")
    x.21 <- number_of_repetitions(log = eventlog,level = "resource-activity")

    y.5 <- list("repetitions distribution"=x.17,
                "repetitions in  different case"=x.18,
                "repetitions in different activity"=x.19,
                "repetitions in different resource"=x.20,
                "repetitions in different resource-activity"=x.21)

    x.22 <- number_of_selfloops(log = eventlog,level = "log")
    x.23 <- number_of_selfloops(log = eventlog,level = "case")
    x.24 <- number_of_selfloops(log = eventlog,level = "activity")
    x.25 <- number_of_selfloops(log = eventlog,level = "resource")
    x.26 <- number_of_selfloops(log = eventlog,level = "resource-activity")

    y.6 <- list("selfloops distribution"=x.22,
                "selfloops in  different case"=x.23,
                "selfloops in different activity"=x.24,
                "selfloops in different resource"=x.25,
                "selfloops in different resource-activity"=x.26)

    x.27 <- activity_presence(log = eventlog)
    x.28 <- activity_frequency(log = eventlog,level = "activity")
    y.7 <- list("activity presence"=x.27,
                "activity frequency"=x.28)

    p.1 <- process_map(log = eventlog)
    p.2 <- trace_explorer(log = eventlog,coverage = coverage.p)

    x.29 <- process_matrix(eventlog)
    p.3 <- plot(process_matrix(eventlog))

    y.8 <- list("process map"=p.1,
                "trace explorer"=p.2,
                "precedence matrix"=x.29,
                "precedence matrix plot"=p.3)

    result <- list("start activity"=y.1,"end activity"=y.2,"trace coverage"=y.3,"trace length"=y.4,"repetitions"=y.5,"selfloops"=y.6,"actiivity presence"=y.7,"visualization"=y.8)

    return(result)


  }else{

    x.1 <- throughput_time(log = eventlog,level = "log")
    x.2 <- throughput_time(log = eventlog,level = "trace")
    x.3 <- throughput_time(log = eventlog,level = "case")

    y.1 <- list("throughput time"=x.1,
                "throughput time in trace"=x.2,
                "throughput time in case"=x.3)

    x.4 <- processing_time(log = eventlog,level = "log")
    x.5 <- processing_time(log = eventlog,level = "trace")
    x.6 <- processing_time(log = eventlog,level = "case")

    y.2 <- list("processing time"=x.4,
                "processing time in trace"=x.5,
                "processing time in case"=x.6)

    x.7 <- idle_time(log = eventlog,level = "log")
    x.8 <- idle_time(log = eventlog,level = "trace")
    x.9 <- idle_time(log = eventlog,level = "case")

    y.3 <- list("idle time"=x.7,
                "idle time in trace"=x.8,
                "idle time in case"=x.9)

    p.1 <- dotted_chart(eventlog)

    result <- list("throughput time"=y.1,"processing time"=y.2,"idle time"=y.3,"visualization"=p.2)

    return(result)


  }
}
