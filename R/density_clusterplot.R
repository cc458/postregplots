#' A function to plot Predicted Probability in Logit model with cluster sd .
#'
#' This function creates a  density plot for a binary variable in Logit model using simulations
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param n.sim numeric. Specify the number of simulations
#' @param varname character vector. A vector contains the name of variables
#' @param data data.frame. The name of the data frame in logit model
#' @param label vector. Specify the labels
#' @param clusterid character. The cluster id
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export



####function to make density plot based on simulation from clustering standard errors
density_clusterplot <- function(ModelResults, n.sim = 1000, varname,
                                data, clusterid, label = c("lab1", "lab2")){
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       cluster <- data[,clusterid]
       #Cluster by ccode
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)
       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)
       X1 <- model.matrix(ModelResults)
       X2 <- model.matrix(ModelResults)
       X1[,varname] <- 0
       non <- apply(apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean) #1 indicates row, 2= columns
       X2[,varname] <- 1
       yes <- apply(apply(X2, 1, function (x) plogis(sim %*% x)), 1, mean)
       simprodicted <- data.frame(non = non,
                                  yes = yes)
       require(reshape2)
       require(ggplot2)
       simprodicted <- melt(simprodicted)
       levels(simprodicted$variable)
       levels(simprodicted$variable) <- label
       library(ggthemes)
       legend_title <- ""
       p <- ggplot(simprodicted, aes(x=value, fill=variable)) + theme_tufte() +
              geom_density(alpha=0.4) +
              labs(x="Predicted Probability", title="",fill="") +
              theme(legend.position="bottom") + labs(x = "", caption =  paste0("Note: density plots are based on ", n.sim," MC iterations")) +
              scale_fill_manual(legend_title, values = c("gold","blue"))
       return(p)
}
