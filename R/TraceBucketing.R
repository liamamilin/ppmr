# 优化的点：1. 更多的enrich 数据的方法。2. 不同的encoding data 的方法 3. bucketing 4. 聚类分开预测的方法。


# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# x <- c("bupaR","tidymodels","tidyverse","cluster","daqapo","edeaR","eventdataR","heuristicsmineR","reticulate","processanimateR","processcheckR","processmapR","processmonitR","petrinetR","xesreadR")
# stringdist,cluster
# for(i in x[-c(2,3)]){
#   print(i)
#   usethis::use_package(package = i,type = "Imports")
# }

#


# 1 single bucketing ： 就是不拆分
# 2 KNN 计算prefix trace 得k近邻，然后用K近邻来构建模型，因此这种方法是动态的，每一个预测都重新训练模型
# 3 cluster ： 根据trace 进行聚类
# 4 Prefix length ：根据不同的prefix trace 长度进行划分
# 5 state ：流程模型中每一个状态构建一个分类器
# 6 domain knowledge ：根据专家的经验手动进行划分


# cluster_methods : clara,fanny,pam
# c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
# "soundex")

# encoding 之前的数据
# Kmean

#' Kmeans Cluster Bucketing
#' @description Cluster trace by Kmeans Cluster.
#' @param prefix_eventLog A event log with prefix.
#' @param dist_methods Measure the distance of trace.Option inclcude:"osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
# "soundex".
#' @param cluster_methods Cluster methods.Option include:clara,fanny,pam.
#' @param cluster_num Cluster number.
#' @return  event log with Cluster label
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- KMmeanclusterBucketing(prefix_eventLog = eventdata)


KMmeanclusterBucketing <- function(prefix_eventLog,
                             dist_methods="lv",
                             cluster_methods = "clara",
                             cluster_num = 3,...){
  require(stringdist)
  require(dplyr)
  require(Rfast)
  require(cluster)


  eMap <- mapping(prefix_eventLog)

  cluster_prefix_eventlog <- prefix_eventLog %>%
    group_by_case()  %>%
    summarise(Log_trace =paste0(.data[[eMap[["activity_identifier"]]]],collapse = ""))
  message("calculate string distance")
  str_dist <- stringdistmatrix(cluster_prefix_eventlog$Log_trace ,
                               cluster_prefix_eventlog$Log_trace,method = dist_methods)
  rownames(str_dist) <- cluster_prefix_eventlog[[1]]
  colnames(str_dist) <- cluster_prefix_eventlog[[1]]
    # str_cluster <- fanny(str_dist,cluster_num)
    #
    # prefix_eventLog <- prefix_eventLog %>% mutate(str_cluster$clustering)

    expre_cluster <- parse(text=paste("str_cluster <- ",cluster_methods,"(x=str_dist,","k=",cluster_num,...,")",sep = ""))

    eval(expre_cluster)

    cluster_data <- data.frame(names(str_cluster$clustering),str_cluster$clustering)
    names(cluster_data) <- c(eMap[["case_identifier"]],"cluster_id")

    prefix_eventLog <- prefix_eventLog %>% left_join(cluster_data,by = eMap[["case_identifier"]])


    return(prefix_eventLog)




}


# hierarchical clustering
# agnes,diana

#' Hierarchical Cluster Bucketing
#' @description Cluster trace by hierarchical clustering.
#' @param prefix_eventLog A event log with prefix.
#' @param dist_methods Measure the distance of trace.Option inclcude:"osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
# "soundex".
#' @param cluster_methods Cluster methods.Option include:agnes,diana.
#' @param cluster_num Cluster number.
#' @return  event log with Cluster label
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' enrichEventlogEncoding <- hierarchicalClusteringBucketing(prefix_eventLog = eventdata)



hierarchicalClusteringBucketing <- function(prefix_eventLog,
                                            dist_methods="lv",
                                            cluster_num = 3,
                                            cluster_methods = "agnes",...){
  require(stringdist)
  require(dplyr)
  require(Rfast)
  require(cluster)


  eMap <- mapping(prefix_eventLog)

  cluster_prefix_eventlog <- prefix_eventLog %>%
    group_by_case()  %>%
    summarise(Log_trace =paste0(.data[[eMap[["activity_identifier"]]]],collapse = ""))
  message("calculate string distance")
  str_dist <- stringdistmatrix(cluster_prefix_eventlog$Log_trace ,
                               cluster_prefix_eventlog$Log_trace,method = dist_methods)
  rownames(str_dist) <- cluster_prefix_eventlog[[1]]
  colnames(str_dist) <- cluster_prefix_eventlog[[1]]
  # str_cluster <- fanny(str_dist,cluster_num)
  #
  # prefix_eventLog <- prefix_eventLog %>% mutate(str_cluster$clustering)
  message("clustering . . . . . ")
  expre_cluster <- parse(text=paste("str_cluster <- ",cluster_methods,"(","x=str_dist",...,")",sep = ""))

  eval(expre_cluster)
  #plot(str_cluster)
  num_Cluster <- cutree(str_cluster,k = cluster_num)

  cluster_data <- data.frame(rownames(str_dist),num_Cluster)
  names(cluster_data) <- c(eMap[["case_identifier"]],"cluster_id")

  prefix_eventLog <- prefix_eventLog %>% left_join(cluster_data,by = eMap[["case_identifier"]])


  return(prefix_eventLog)


}







#' Gap Statistic for Estimating the Number of Clusters
#' @description Use gap statistic to estimating the number of clusters
#' @param prefix_eventLog A event log with prefix.
#' @param dist_methods Measure the distance of trace.Option inclcude:"osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
# "soundex".
#' @param cluster_methods Cluster methods.Option include:clara,fanny,pam.
#' @param max_cluster_num Max cluster number.
#' @return  The number of cluster.
#' @examples
#' library(bupaR)
#' library(ppmr)
#' eventdata <- enrichEventlog(eventLog = patients,prefix_num = 4,mode = "activity")
#' clusternum <- optimalNumberOfCluster(prefix_eventLog = eventdata,max_cluster_num=5,B=100)



optimalNumberOfCluster <- function(prefix_eventLog,cluster_methods="fanny",dist_methods="lv",max_cluster_num=5,...){

  require(stringdist)
  require(dplyr)
  require(Rfast)
  require(cluster)


  eMap <- mapping(prefix_eventLog)

  cluster_prefix_eventlog <- prefix_eventLog %>%
    group_by_case()  %>%
    summarise(Log_trace =paste0(.data[[eMap[["activity_identifier"]]]],collapse = ""))
  message("calculate string distance")
  str_dist <- stringdistmatrix(cluster_prefix_eventlog$Log_trace ,
                               cluster_prefix_eventlog$Log_trace,method = dist_methods)
  rownames(str_dist) <- cluster_prefix_eventlog[[1]]
  colnames(str_dist) <- cluster_prefix_eventlog[[1]]

  message("calculate optimal number of clusters, It would take some time !")
  Gap_num <- clusGap(str_dist, FUN =  get(cluster_methods), K.max = max_cluster_num,...) # 20


  #plot(Gap_num)





  cluster_num <- which(Gap_num$Tab[,3]==max(Gap_num$Tab[,3]))

  message(paste("optimal number of clusters is:",cluster_num))
  return(Gap_num)
  # cluster_num <- cluster_num[1]
  # expre_cluster <- parse(text=paste("str_cluster <- ",cluster_methods,"(str_dist,cluster_num)",sep = ""))
  #
  # eval(expre_cluster)
  #
  # cluster_data <- data.frame(names(str_cluster$clustering),str_cluster$clustering)
  # names(cluster_data) <- c(eMap[["case_identifier"]],"cluster_id")
  #
  # prefix_eventLog <- prefix_eventLog %>% left_join(cluster_data,by = eMap[["case_identifier"]])
  #
  # return(prefix_eventLog)

}




