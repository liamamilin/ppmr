# Interpret_model


#' Interpret Model
#' @description Interpret Model,This function is base on DALEXtra packages.
#' @param ModelResult Model result.
#' @param train  Train data without lablee.
#' @param trainlabel Lable of train dataset .
#' @param newdata one slice data.
#' @return  Return the interpretation of Model.
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- lastStateEncoding(prefix_eventLog = eventdata)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding %>% as.data.frame()
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-patient,-handling_id,-.order,-registration_type)
#' library(lubridate)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% mutate(year=year(time),month=month(time),day = day(time),week = week(time),hour = hour(time))
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-time)
#' enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% filter(!is.na(predicate))
#' enrichEventlogEncoding.1$predicate <- enrichEventlogEncoding.1$predicate %>% as.character() %>% as.factor()
#' EventLogModel <- BuildModel(TheModel = rand_forest,engine = "ranger",PrefixData = enrichEventlogEncoding.1,predictmode = "classification")
#' pre <- predict(object = EventLogModel[[3]], EventLogModel[["test"]],type = "class")
#' result <- InterpretativeModel(ModelResult = EventLogModel[[3]],traindata=EventLogModel[[1]][,-3],trainlabel = EventLogModel[[1]][,3],newdata = EventLogModel[[2]][1,])


InterpretativeModel <- function(ModelResult,traindata,trainlabel,
                                 newdata=NULL){
  require(DALEXtra)
  require(ingredients)
  require(iBreakDown)


  if(!is.null(newdata)){

    expmodel <- explain_tidymodels(ModelResult,traindata,trainlabel)

    imp <- model_parts(expmodel)


    # BD plot

    sp_rf <- predict_parts(expmodel,new_observation = newdata,type = "break_down")


    # Ceteris-paribus Profiles

    cp_titanic_rf <- predict_profile(explainer = (expmodel),
                                     new_observation = newdata)

    # Ceteris-paribus Oscillations
    bd_lm <- predict_parts(explainer = expmodel,
                           new_observation = newdata,
                           type = "oscillations_uni")

    return(list(importance=imp,BDplot=sp_rf,Ceteris_paribus_Profiles=cp_titanic_rf,
                Ceteris_paribus_Oscillations=bd_lm))
  }else{
    expmodel <- explain_tidymodels(ModelResult,traindata,trainlabel)

    imp <- model_parts(expmodel)
    cat("feature importance")
    imp
    plot(imp)

    return(imp)
  }




}




# usethis::use_package(package = "ingredients",type = "Imports")
