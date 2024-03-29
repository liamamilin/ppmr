# 1. Last state encoding  ：只考虑最后第m个event 对应的属性，
# 2. aggregation encoding ：与last state encoding 相比，aggregation encoding 考虑all event in the prefix of the case 。根据属性的类型不同，聚合方式也不一样，如果是数值，则可以使用sum，mean ，sd，最大，最小，等等。对于分类变量可以是计数
# 3. index-based encoding ：大概意思是将每一个event中的属性全部组合起来
# 4. Tensor encoding： 转化成为n乘以t乘以p的形式t是是event 的数量p是属性数量。n表示训练数据集的数量

# 5. window encoding ： 根据窗口大小将一个trace 分解成为多个(这是另外一种方法，不需要prefix)




# 1. Last state encoding

#' Last State Encoding
#' @description In this encoding method, only the last available snapshot of the data is used.
#' @param eventLog A event log with prefix.
#' @return  event log with encoding.
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- lastStateEncoding(prefix_eventLog = eventdata)

lastStateEncoding <- function(prefix_eventLog){
  #pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
  require(bupaR)
  require(dplyr)
  encoding_envetlog <- prefix_eventLog %>% group_by_case() %>% last_n(1)
  lifecyle_ID <- encoding_envetlog %>% lifecycle_id()
  lc <- encoding_envetlog %>% select(all_of(lifecyle_ID),force_df = TRUE) %>% pull()

  if(all(is.na(lc))){
    stop("lifecycle_id is NA")
  }else{
    encoding_envetlog <- encoding_envetlog %>% filter(across(all_of(lifecyle_ID))=="complete")
  }


  return(encoding_envetlog)
}



# 2. Last N State Encoding

#' Last N State Index Based Encoding
#' @description The last M available snapshot of the data is used ,And The idea of indexbased encoding is to use all possible information (including the order) in the trace, generating one feature per each event attribute per each executed event (each index).
#' @param eventLog A event log with prefix.
#' @param Window The Window length of Trace.
#' @return  event log with encoding.
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- lastNStateIndexBasedEncoding(prefix_eventLog = eventdata,Window=3)

lastNStateIndexBasedEncoding <- function(prefix_eventLog,Window=3){
  #pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
  require(dplyr)
  require(bupaR)

  if(Window >(trace_length(prefix_eventLog)$max)){
    stop("wrong window parameters")
  }

  lifecyle_ID <- prefix_eventLog %>% lifecycle_id()
  lc <- prefix_eventLog %>% select((all_of(lifecyle_ID)),force_df = TRUE) %>% pull()

  if(all(is.na(lc))){
    encoding_envetlog <- prefix_eventLog %>% group_by_case() %>% last_n(Window)
  }else{
    encoding_envetlog <- prefix_eventLog  %>% filter(across(all_of(lifecyle_ID))=="complete") %>% group_by_case() %>% last_n(Window)
  }
  EventMap <- mapping(encoding_envetlog)
  encoding_envetlog <- encoding_envetlog %>% data.frame()

  # 将activity设置成为因子变量
  encoding_envetlog[EventMap[["activity_identifier"]]] <- as.factor(encoding_envetlog[[EventMap[["activity_identifier"]]]])

  if(length(EventMap) == dim(encoding_envetlog)[2]){
    message("you did not enrich the eventlog")
    event_data <- encoding_envetlog %>% select(EventMap[["case_identifier"]],EventMap[["activity_identifier"]],EventMap[["timestamp_identifier"]],EventMap[["resource_identifier"]])

    for(i in 1:Window){
      event_data_1 <- event_data %>% group_by(across(all_of(EventMap[["case_identifier"]]))) %>% dplyr::slice(i)

      new <- c(paste(EventMap[["activity_identifier"]],i,sep = "_"),
               paste(EventMap[["timestamp_identifier"]],i,sep = "_"),
               paste(EventMap[["resource_identifier"]],i,sep = "_"))
      old <- c(EventMap[["activity_identifier"]],
               EventMap[["timestamp_identifier"]],
               EventMap[["resource_identifier"]])
      event_data_1 <- event_data_1 %>% rename_with(~new,all_of(old))



      assign(paste("encoding_envetlog",i,sep = "_"),event_data_1)
    }
    result <- get(paste("encoding_envetlog",1,sep = "_"))
    for(i in 2:Window){
      result <- result %>% left_join(get(paste("encoding_envetlog",i,sep="_")),by = EventMap[["case_identifier"]])
    }
    return(result)

  }else{
    other_data <- encoding_envetlog %>% select(all_of(EventMap[[1]]),(length(EventMap)+1):(dim(encoding_envetlog)[2]))
    other_data <- other_data  %>% distinct()


    event_data <- encoding_envetlog %>% select(EventMap[["case_identifier"]],EventMap[["activity_identifier"]],EventMap[["timestamp_identifier"]],EventMap[["resource_identifier"]])

    for(i in 1:Window){
      event_data_1 <- event_data %>% group_by(across(all_of(EventMap[["case_identifier"]]))) %>% dplyr::slice(i)

      new <- c(paste(EventMap[["activity_identifier"]],i,sep = "_"),
               paste(EventMap[["timestamp_identifier"]],i,sep = "_"),
               paste(EventMap[["resource_identifier"]],i,sep = "_"))
      old <- c(EventMap[["activity_identifier"]],
               EventMap[["timestamp_identifier"]],
               EventMap[["resource_identifier"]])
      event_data_1 <- event_data_1 %>% rename_with(~new,all_of(old))



      assign(paste("encoding_envetlog",i,sep = "_"),event_data_1)
    }
    result <- get(paste("encoding_envetlog",1,sep = "_"))
    for(i in 2:Window){
      result <- result %>% left_join(get(paste("encoding_envetlog",i,sep="_")),by = EventMap[["case_identifier"]])
    }

    result <- result %>% left_join(other_data,by =  EventMap[["case_identifier"]])

    return(result)

  }





}


# 3. aggregation encoding


#' Aggregation Encoding
#' @description Aggregation encoding. In contrast to the last-state encoding, aggregation encoding considers all events in the prefix of a case, rather than considering only the last m events. Each attribute is encoded into one or multiple features using different aggregation functions depending on the datatype of the attribute.
#' If an attribute is of numerical type then we map this attribute into one feature by means of a numerical aggregation function such as sum, average, minimum, or maximum.
#' f an attribute is categorical we map this attribute into one feature for every value in the attribute’s domain by applying the “count” aggregation function.
#' @param eventLog A event log with prefix.
#' @return  event log with encoding.
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- aggregation_encoding(prefix_eventLog = eventdata)

aggregation_encoding <-  function(prefix_eventLog){
  #pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
  require(caret)
  require(bupaR)
  require(dplyr)
  require(lubridate)

  lifecyle_ID <- prefix_eventLog %>% lifecycle_id()
  lc <- prefix_eventLog %>% select((all_of(lifecyle_ID)),force_df = TRUE) %>% pull()

  if(all(is.na(lc))){
    stop("lifecycle_id is NA")
  }else{
    encoding_envetlog <- prefix_eventLog  %>% filter(across(all_of(lifecyle_ID))=="complete")
  }
  Map <- mapping(encoding_envetlog)
  encoding_envetlog <- encoding_envetlog %>% data.frame()



  traceData <- encoding_envetlog %>% select(-predicate,
                                        -Map[["lifecycle_identifier"]],
                                        -Map[["case_identifier"]],
                                        -Map[["activity_instance_identifier"]])%>%
    select(where(is.factor))
  formu <- formula(paste("~.",sep=""))

  dummy <- dummyVars(formu, data=traceData)
  newdata <- data.frame(predict(dummy, newdata = traceData))
  newdata <- newdata %>% mutate(case_id = encoding_envetlog %>% select(Map[["case_identifier"]]) %>% pull())
  #newdata1 <- newdata %>% mutate(case_id = traceData[,1])
  newdata <- newdata %>% group_by(case_id) %>% summarise_all(sum)

  other_data <- encoding_envetlog %>% select(-Map[["activity_identifier"]],
                                             -Map[["activity_instance_identifier"]],
                                             -Map[["lifecycle_identifier"]],
                                             -Map[["timestamp_identifier"]],
                                             -predicate
                                             )
  other_data$idle_time <- as.numeric(other_data$idle_time)
  other_data$processing_time <- as.numeric(other_data$processing_time)
  other_data$throughput_time <- as.numeric(other_data$throughput_time)

  other_data <-  other_data %>%
    group_by(across(all_of(Map[["case_identifier"]]))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

  other_data <- rename(other_data,case_id = Map[["case_identifier"]])

  result <- newdata %>% left_join(other_data,by="case_id") %>%
    left_join((encoding_envetlog %>% select(Map[["case_identifier"]],predicate)),by = c("case_id" = Map[["case_identifier"]]))

  return(list("one hot rule"=dummy,"encoding event log"=unique(result)))

}

