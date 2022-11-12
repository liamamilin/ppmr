# ppmr

Predictive business process monitoring methods exploit historical process execution logs to generate predic- tions about running instances (called cases) of a business process, such as the prediction of the outcome, next activity, or remaining cycle time of a given process case. 

This R packages support A series of function  about Predictive process monitoring.

# install packages

```
devtools::install_github("liamamilin/ppmr")
```

# Enrich ,Labeling Event Log Data

```
library(bupar)
library(ppmr)

patients %>% head(5)
# Log of 5 events consisting of:
1 trace 
5 cases 
5 instances of 1 activity 
1 resource 
Events occurred from 2017-01-02 11:41:53 until 2017-01-04 16:07:47 
 
# Variables were mapped as follows:
Case identifier:		patient 
Activity identifier:		handling 
Resource identifier:		employee 
Activity instance identifier:	handling_id 
Timestamp:			time 
Lifecycle transition:		registration_type 

# A tibble: 5 × 7
  handling     patient employee handling_id registration_type time                .order
  <fct>        <chr>   <fct>    <chr>       <fct>             <dttm>               <int>
1 Registration 1       r1       1           start             2017-01-02 11:41:53      1
2 Registration 2       r1       2           start             2017-01-02 11:41:53      2
3 Registration 3       r1       3           start             2017-01-04 01:34:05      3
4 Registration 4       r1       4           start             2017-01-04 01:34:04      4
5 Registration 5       r1       5           start             2017-01-04 16:07:47      5

names(patients)
[1] "handling"          "patient"           "employee"          "handling_id"      
[5] "registration_type" "time"              ".order"           

eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")

names(eventdata)
 [1] "patient"           "handling"          "employee"          "handling_id"      
 [5] "registration_type" "time"              ".order"            "predictor"        
 [9] "idle_time"         "processing_time"   "throughput_time"   "nr_of_resources"  
[13] "min"               "q1"                "mean"              "median"           
[17] "q3"                "max"               "st_dev"            "iqr"              
[21] "total"             "absolute"          "relative" 
```


# Encoding Event Log Data

Implemented three type of encoding method.

```
enrichEventlogEncoding <- lastStateEncoding(prefix_eventLog = eventdata)
```

# Bucketing Event Log

```
eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")

clusternum <- optimalNumberOfCluster(prefix_eventLog = eventdata,max_cluster_num=5,B=100)

clusternum
Clustering Gap statistic ["clusGap"] from call:
clusGap(x = str_dist, FUNcluster = get(cluster_methods), K.max = max_cluster_num, B = 100)
B=100 simulated reference sets, k = 1..5; spaceH0="scaledPCA"
 --> Number of clusters (method 'firstSEmax', SE.factor=1): 5
          logW   E.logW        gap     SE.sim
[1,] 11.119066 10.84650 -0.2725612 0.01383700
[2,] 10.285399 10.55173  0.2663286 0.01222086
[3,]  9.618844 10.47968  0.8608386 0.01807247
[4,]  7.031667 10.44631  3.4146403 0.03080576
[5,]  5.122363 10.44572  5.3233526 0.03043616

```

# Build Regression Or Classification Model


```
# Data processing
enrichEventlogEncoding <- lastStateEncoding(prefix_eventLog = eventdata)
enrichEventlogEncoding.1 <- enrichEventlogEncoding %>% as.data.frame()
enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-patient,-handling_id,-.order,-registration_type)
library(lubridate)
enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% mutate(year=year(time),month=month(time),day = day(time),week = week(time),hour = hour(time))
enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% select(-time)
enrichEventlogEncoding.1 <- enrichEventlogEncoding.1 %>% filter(!is.na(predictor))
enrichEventlogEncoding.1$predictor <- enrichEventlogEncoding.1$predictor %>% as.character() %>% as.factor()

# Build model
EventLogModel <- BuildModel(TheModel = rand_forest,engine = "ranger",PrefixData = enrichEventlogEncoding.1,predictmode = "classification")

══ Workflow [trained] ═════════════════════════════════════════════════════════════════════
Preprocessor: Recipe
Model: rand_forest()

── Preprocessor ───────────────────────────────────────────────────────────────────────────
2 Recipe Steps

• step_zv()
• step_naomit()

── Model ──────────────────────────────────────────────────────────────────────────────────
Ranger result

Call:
 ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1), probability = TRUE) 

Type:                             Probability estimation 
Number of trees:                  500 
Sample size:                      373 
Number of independent variables:  14 
Mtry:                             3 
Target node size:                 10 
Variable importance mode:         none 
Splitrule:                        gini 
OOB prediction error (Brier s.):  0.006691683 

pre <- predict(object = x[[3]], x[["test"]],type = "class")
```

# Evaluate Model

```

result <- evaluateModel(ModelResult = EventLogModel[[3]],train = EventLogModel[[1]],test = EventLogModel[[2]],predictmode = "classification"

result
[[1]]
  precision    accuracy   sensitive specificity   f_measure 
  0.9982729   0.9973190   0.7500000   0.9986188   0.9991342 

[[2]]
  precision    accuracy   sensitive specificity   f_measure 
  0.9830508   0.9840000   0.6666667   0.9926471   0.9913793 
```


# Interpretative model

```
result <- InterpretativeModel(ModelResult = EventLogModel[[3]],traindata=EventLogModel[[1]][,-3],trainlabel = EventLogModel[[1]][,3],newdata = EventLogModel[[2]][1,])
Preparation of a new explainer is initiated
  -> model label       :  workflow  (  default  )
  -> data              :  373  rows  22  cols 
  -> target variable   :  373  values 
  -> predict function  :  yhat.workflow  will be used (  default  )
  -> predicted values  :  No value for predict function target column. (  default  )
  -> model_info        :  package tidymodels , ver. 1.0.0 , task multiclass (  default  ) 
  -> predicted values  :  predict function returns multiple columns:  4  (  default  ) 
  -> residual function :  difference between 1 and probability of true class (  default  )
  -> residuals         :  numerical, min =  0.004280851 , mean =  0.02342005 , max =  0.7290669  
  A new explainer has been created! 


result[[1]]
          variable mean_dropout_loss    label
1     _full_model_          9.539765 workflow
2              min          9.539765 workflow
3               q1          9.539765 workflow
4             mean          9.539765 workflow
5           median          9.539765 workflow
6               q3          9.539765 workflow
7              max          9.539765 workflow
8           st_dev          9.539765 workflow
9              iqr          9.539765 workflow
10 nr_of_resources          9.725712 workflow
11        relative          9.765216 workflow
12        absolute          9.780544 workflow
13           total          9.797688 workflow
14            year         10.586507 workflow
15             day         10.614065 workflow
16           month         10.897857 workflow
17            week         11.177043 workflow
18            hour         11.639669 workflow
19 throughput_time         12.131740 workflow
20 processing_time         12.371743 workflow
21       idle_time         12.390275 workflow
22        handling        118.232268 workflow
23        employee        152.382308 workflow
24      _baseline_        783.842091 workflow

plot(result[[1]])
```

![](/Users/milin/ppmr/ip.png)

```
result[[2]]
workflow.Check-out: intercept                                             0.514
workflow.Check-out: employee = r6                                         0.240
workflow.Check-out: handling = Discuss Results                            0.208
workflow.Check-out: hour = 1                                             -0.025
workflow.Check-out: processing_time = 19.2352777777778                    0.010
workflow.Check-out: idle_time = 133.375277777778                         -0.008
workflow.Check-out: throughput_time = 152.610555555556                    0.011
workflow.Check-out: week = 16                                            -0.003
workflow.Check-out: year = 2017                                           0.007
workflow.Check-out: day = 18                                              0.006
workflow.Check-out: month = 4                                             0.002
workflow.Check-out: total = 4                                             0.000
workflow.Check-out: absolute = 4                                          0.000
workflow.Check-out: relative = 0.6667                                     0.000
workflow.Check-out: nr_of_resources = 4                                   0.000
workflow.Check-out: min = 1                                               0.000
workflow.Check-out: q1 = 1                                                0.000
workflow.Check-out: mean = 1                                              0.000
workflow.Check-out: median = 1                                            0.000
workflow.Check-out: q3 = 1                                                0.000
workflow.Check-out: max = 1                                               0.000
workflow.Check-out: st_dev = 0                                            0.000
workflow.Check-out: iqr = 0                                               0.000
workflow.Check-out: prediction                                            0.961
workflow.Discuss Results: intercept                                       0.481
workflow.Discuss Results: employee = r6                                  -0.254
workflow.Discuss Results: handling = Discuss Results                     -0.196
workflow.Discuss Results: hour = 1                                        0.006
workflow.Discuss Results: processing_time = 19.2352777777778             -0.007
workflow.Discuss Results: idle_time = 133.375277777778                   -0.002
workflow.Discuss Results: throughput_time = 152.610555555556             -0.003
workflow.Discuss Results: week = 16                                      -0.003
workflow.Discuss Results: year = 2017                                     0.000
workflow.Discuss Results: day = 18                                       -0.003
workflow.Discuss Results: month = 4                                      -0.002
workflow.Discuss Results: total = 4                                       0.000
workflow.Discuss Results: absolute = 4                                    0.000
workflow.Discuss Results: relative = 0.6667                               0.000
workflow.Discuss Results: nr_of_resources = 4                             0.000
workflow.Discuss Results: min = 1                                         0.000
workflow.Discuss Results: q1 = 1                                          0.000
workflow.Discuss Results: mean = 1                                        0.000
workflow.Discuss Results: median = 1                                      0.000
workflow.Discuss Results: q3 = 1                                          0.000
workflow.Discuss Results: max = 1                                         0.000
workflow.Discuss Results: st_dev = 0                                      0.000
workflow.Discuss Results: iqr = 0                                         0.000
workflow.Discuss Results: prediction                                      0.017
workflow.Registration: intercept                                          0.003
workflow.Registration: employee = r6                                      0.002
workflow.Registration: handling = Discuss Results                         0.001
workflow.Registration: hour = 1                                           0.018
workflow.Registration: processing_time = 19.2352777777778                -0.003
workflow.Registration: idle_time = 133.375277777778                       0.010
workflow.Registration: throughput_time = 152.610555555556                -0.008
workflow.Registration: week = 16                                          0.004
workflow.Registration: year = 2017                                       -0.006
workflow.Registration: day = 18                                          -0.002
workflow.Registration: month = 4                                          0.001
workflow.Registration: total = 4                                          0.000
workflow.Registration: absolute = 4                                       0.000
workflow.Registration: relative = 0.6667                                  0.000
workflow.Registration: nr_of_resources = 4                                0.000
workflow.Registration: min = 1                                            0.000
workflow.Registration: q1 = 1                                             0.000
workflow.Registration: mean = 1                                           0.000
workflow.Registration: median = 1                                         0.000
workflow.Registration: q3 = 1                                             0.000
workflow.Registration: max = 1                                            0.000
workflow.Registration: st_dev = 0                                         0.000
workflow.Registration: iqr = 0                                            0.000
workflow.Registration: prediction                                         0.022
workflow.Triage and Assessment: intercept                                 0.002
workflow.Triage and Assessment: employee = r6                             0.013
workflow.Triage and Assessment: handling = Discuss Results               -0.013
workflow.Triage and Assessment: hour = 1                                  0.001
workflow.Triage and Assessment: processing_time = 19.2352777777778        0.000
workflow.Triage and Assessment: idle_time = 133.375277777778              0.000
workflow.Triage and Assessment: throughput_time = 152.610555555556       -0.001
workflow.Triage and Assessment: week = 16                                 0.001
workflow.Triage and Assessment: year = 2017                              -0.001
workflow.Triage and Assessment: day = 18                                  0.000
workflow.Triage and Assessment: month = 4                                -0.001
workflow.Triage and Assessment: total = 4                                 0.000
workflow.Triage and Assessment: absolute = 4                              0.000
workflow.Triage and Assessment: relative = 0.6667                         0.000
workflow.Triage and Assessment: nr_of_resources = 4                       0.000
workflow.Triage and Assessment: min = 1                                   0.000
workflow.Triage and Assessment: q1 = 1                                    0.000
workflow.Triage and Assessment: mean = 1                                  0.000
workflow.Triage and Assessment: median = 1                                0.000
workflow.Triage and Assessment: q3 = 1                                    0.000
workflow.Triage and Assessment: max = 1                                   0.000
workflow.Triage and Assessment: st_dev = 0                                0.000
workflow.Triage and Assessment: iqr = 0                                   0.000
workflow.Triage and Assessment: prediction                                0.000

plot(result[[2]])
```

![](/Users/milin/ppmr/Rplot.png)
