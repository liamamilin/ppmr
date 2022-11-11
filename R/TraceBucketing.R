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
#
# for(i in x[-c(2,3)]){
#   print(i)
#   usethis::use_package(package = i,type = "Imports")
# }

# purrr


# 1 single bucketing ： 就是不拆分
# 2 KNN 计算prefix trace 得k近邻，然后用K近邻来构建模型，因此这种方法是动态的，每一个预测都重新训练模型
# 3 cluster ： 根据trace 进行聚类
# 4 Prefix length ：根据不同的prefix trace 长度进行划分
# 5 state ：流程模型中每一个状态构建一个分类器
# 6 domain knowledge ：根据专家的经验手动进行划分


# 输入是
cluster_bucketing <- function(){

}



prefix_len_bucketing <- function(){

}

patients %>% group_by_case() %>% summarise(x =paste0(handling,collapse = ""))


patients %>% group_by_case() %>% mutate(x =paste0(handling,collapse = ""))



data <- data.frame(foo=c(1, 1, 2, 3, 3, 3), bar=c('a', 'b', 'a', 'b', 'c', 'd'))

data %>%
  group_by(foo) %>%
  mutate(bars_by_foo = paste0(bar, collapse = ""))
