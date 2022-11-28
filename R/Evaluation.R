

#' Evaluate Model
#' @description Evaluate model.
#' @param ModelResult Model result.
#' @param train  Train Data.
#' @param test Test Data.
#' @param predictmode Type of model,regression or classification.
#' @return  Return model evaluate metrics.
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
#' result <- evaluateModel(ModelResult = EventLogModel[[3]],train = EventLogModel[[1]],test = EventLogModel[[2]],predictmode = "classification"

evaluateModel <- function(ModelResult,train,test,predictmode="regression"){
  require(dplyr)
  require(tidymodels)

  if(predictmode=="regression"){

    train_result <- c(rep(0,4))
    names(train_result) <- c("rmse","rsq","mae","mape")
    test_result <- c(rep(0,4))
    names(test_result) <- c("rmse","rsq","mae","mape")

    training_pred <- predict(ModelResult, train) %>%
      bind_cols(predict(ModelResult, train, type = "conf_int")) %>%
      # Add the true outcome data back in
      bind_cols(train %>%
                  select(predicate))

    train_result[1] <- rmse(training_pred,predicate,.pred_class)[[3]]
    train_result[2] <- rsq(training_pred,predicate,.pred_class)[[3]]
    train_result[3] <- mae(training_pred,predicate,.pred_class)[[3]]
    train_result[4] <- mape(training_pred,predicate,.pred_class)[[3]]




    testing_pred <- predict(ModelResult, test) %>%
      bind_cols(predict(ModelResult, test, type = "conf_int")) %>%
      # Add the true outcome data back in
      bind_cols(test %>%
                  select(predicate))


    test_result[1] <- rmse(training_pred,predicate,.pred_class)[[3]]
    test_result[2] <- rsq(training_pred,predicate,.pred_class)[[3]]
    test_result[3] <- mae(training_pred,predicate,.pred_class)[[3]]
    test_result[4] <- mape(training_pred,predicate,.pred_class)[[3]]

    return(list(train_result,test_result))



  }else if(predictmode=="classification"){
    train_result <- c(rep(0,5))
    names(train_result) <- c("precision","accuracy","sensitive","specificity","f_measure")
    test_result <- c(rep(0,5))
    names(test_result) <- c("precision","accuracy","sensitive","specificity","f_measure")

    training_pred <- predict(ModelResult, train) %>%
      bind_cols(predict(ModelResult, train, type = "prob")) %>%
      # Add the true outcome data back in
      bind_cols(train %>%
                  select(predicate))
    train_result[1] <- precision(training_pred,predicate,.pred_class)[[3]]
    train_result[2] <- accuracy(training_pred,predicate,.pred_class)[[3]]
    train_result[3] <- sens(training_pred,predicate,.pred_class)[[3]]
    train_result[4] <- spec(training_pred,predicate,.pred_class)[[3]]
    train_result[5] <- f_meas(training_pred,predicate,.pred_class)[[3]]

    testing_pred <- predict(ModelResult, test) %>%
      bind_cols(predict(ModelResult, test, type = "prob")) %>%
      # Add the true outcome data back in
      bind_cols(test %>%
                  select(predicate))

    test_result[1] <- precision(testing_pred,predicate,.pred_class)[[3]]
    test_result[2] <- accuracy(testing_pred,predicate,.pred_class)[[3]]
    test_result[3] <- sens(testing_pred,predicate,.pred_class)[[3]]
    test_result[4] <- spec(testing_pred,predicate,.pred_class)[[3]]
    test_result[5] <- f_meas(testing_pred,predicate,.pred_class)[[3]]

    return(list(train=train_result,test=test_result))

  }






}
