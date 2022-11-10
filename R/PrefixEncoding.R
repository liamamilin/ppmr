# 1. Last state encoding  ：只考虑最后第m个event 对应的属性，
# 2. aggregation encoding ：与last state encoding 相比，aggregation encoding 考虑all event in the prefix of the case 。根据属性的类型不同，聚合方式也不一样，如果是数值，则可以使用sum，mean ，sd，最大，最小，等等。对于分类变量可以是计数
# 3. index-based encoding ：大概意思是将每一个event中的属性全部组合起来
# 4. Tensor encoding： 转化成为n乘以t乘以p的形式t是是event 的数量p是属性数量。n表示训练数据集的数量
# 5. window encoding ： 根据窗口大小将一个trace 分解成为多个


lastStateEncoding <- function(prefix_eventLog){
  encoding_envetlog <- prefix_eventLog %>% group_by_case() %>% last_n(1)
  lifecyle_ID <- encoding_envetlog %>% lifecycle_id()
  lc <- encoding_envetlog %>% select((lifecyle_ID),force_df = TRUE) %>% pull()

  if(all(is.na(lc))){
    message("lifecycle_id is NA")
  }else{
    encoding_envetlog <- encoding_envetlog %>% filter(across(all_of(lifecyle_ID))=="complete")
  }


  return(encoding_envetlog)
}




windowEncoding <- function(prefix_eventLog,Window=3){

  if(Window >(prefix_eventLog %>% n_traces())){
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

  other_data <- encoding_envetlog %>% select(all_of(EventMap[[1]]),(length(EventMap)+1):(dim(encoding_envetlog)[2]))
  other_data <- other_data  %>% distinct()


  event_data <- encoding_envetlog %>% select(EventMap[["case_identifier"]],EventMap[["activity_identifier"]],EventMap[["timestamp_identifier"]],EventMap[["resource_identifier"]])

  for(i in 1:Window){
    event_data_1 <- event_data %>% group_by(across(all_of(EventMap[["case_identifier"]]))) %>% slice(i)

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

# Case identifier:		patient
# Activity identifier:		handling
# Resource identifier:		employee
# Activity instance identifier:	handling_id
# Timestamp:			time
# Lifecycle transition:		registration_type
