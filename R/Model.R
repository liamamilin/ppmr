

# ClusterBucketingModel <- function(BuckingData,
#                                   TheModel=rand_forest,
#                                   engine="ranger",
#                                   predictmode = "regression"){
#
#
# }



# predicmode  = classification or regression
# recive data frame

#' Build Model with Encoding Event Log Data
#' @description Build all kind of regression and classification model.This function is base on Tidymodel package.
#' @param TheModel Model function in Tidymodels,such as boost_tree,rand_forest.
#' @param engine  engine of model.
#' @param PrefixData Encoding event log data.
#' @param predictmode Type of model,regression or classification.
#' @return  Return A list about train dataset,test dataset and model result.
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


BuildModel <- function(TheModel=rand_forest,engine="ranger",PrefixData,predictmode = "regression"){

  # if(class(TheModel)=="character"){
  #   stop("model class should be function, use function in parsnip,
  #        such as linear_reg()/logistic_reg() ")
  # }
  require(tidymodels)
  require(dotwhisker)

  # Emap <- mapping(PrefixData)


  # Put 3/4 of the data into the training set
  data_split <- initial_split(PrefixData, prop = 3/4)

  # Create data frames for the two sets:
  train_data <- training(data_split)
  test_data  <- testing(data_split)


  dataRecipe <- recipe(predicate~ ., data = train_data) %>%
    step_zv(all_predicates()) %>%
    step_naomit()


  Mymodel <- TheModel() %>%
    set_engine(engine) %>%
    set_mode(mode = predictmode)

  Myworkflow <- workflow() %>%
    add_model(Mymodel) %>%
    add_recipe(dataRecipe)

  modelfit <-
    Myworkflow %>%
    fit(data = train_data)

  return(list(train=train_data,test=test_data,model=modelfit))

}





