# Interpretative_model


#' Interpretative Model
#' @description Interpretative Model,This function is base on DALEXtra packages.
#' @param ModelResult Model result.
#' @param train  Train data without lablee.
#' @param trainlabel Lable of train dataset .
#' @param newdata one slice data.
#' @return  Return the interpretation of Model.
#' @examples
#' library(bupar)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- lastStateEncoding(prefix_eventLog = eventdata)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding %>% as.data.frame()
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-patient,-handling_id,-.order,-registration_type)
#' library(lubridate)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% mutate(year=year(time),month=month(time),day = day(time),week = week(time),hour = hour(time))
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-time)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% filter(!is.na(predictor))
#' enrichEventlogEncoding.1$predictor <- enrichEventlogEncoding.1$predictor %>% as.character() %>% as.factor()
#' EventLogModel <- BuildModel(TheModel = rand_forest,engine = "ranger",PrefixData = enrichEventlogEncoding.1,predictmode = "classification")
#' pre <- predict(object = x[[3]], x[["test"]],type = "class")
#' result <- InterpretativeModel(ModelResult = EventLogModel[[3]],traindata=EventLogModel[[1]][,-3],trainlabel = EventLogModel[[1]][,3],newdata = EventLogModel[[2]][1,])


InterpretativeModel <- function(ModelResult,traindata,trainlabel,
                                 newdata=NULL){
  require(DALEXtra)
  require(ingredients)
  require(iBreakDown)


  # vr_age  <- model_profile(expmodel,variables = var)
  # plot(vr_age)

  if(!is.null(newdata)){

    expmodel <- explain_tidymodels(ModelResult,traindata,trainlabel)

    imp <- model_parts(expmodel)
    imp
    plot(imp)

    sp_rf <- predict_parts(expmodel,new_observation = newdata)
    sp_rf
    plot(sp_rf)

    return(list(imp,sp_rf))
  }else{
    expmodel <- explain_tidymodels(ModelResult,traindata,trainlabel)

    imp <- model_parts(expmodel)
    imp
    plot(imp)

    return(imp)
  }




}




# usethis::use_package(package = "ingredients",type = "Imports")
