#' A function to simulate coefficient in Logit model with cluster sd .
#'
#' This function simulate the post esitmation distribution
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param n.sim numeric. Specify the number of simulations
#' @param data data.frame. The name of the data frame in logit model
#' @param clusterid character. The cluster id
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export

# Generate 1000 new datasets #
sim_cluster <- function(ModelResults, n.sim = 1000, data, clusterid){
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       cluster <- data[,clusterid]
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)
       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)
       return(sim)
}
