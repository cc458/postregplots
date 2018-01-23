#' A function to plot first difference in Logit model with cluster sd .
#'
#' This function creates a ridge density plot for variables in Logit model using simulations
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param n.sim numeric. Specify the number of simulations
#' @param varname character vector. A vector contains the name of variables
#' @param data data.frame. The name of the data frame in logit model
#' @param val1 numeric. Specify the min value of percentile
#' @param val2 numeric. Specify the max value of percentile
#' @param clusterid character. The cluster id
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export



###function to make a coefficient plot
density_ridgeplot = function(ModelResults, n.sim = 1000, varname, data,
                        val1, val2, clusterid){
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(gridExtra)
       library(viridis)
       library(ggridges)
       library(dplyr)
       library(ggplot2)
       library(tidyr)
       library(stringr)
       library(purrr)
       cluster <- data[,clusterid]
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)

       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)

       fd <- list()

       for (i in 1:length(varname)){
                     X1 <- model.matrix(ModelResults)
                     X2 <- model.matrix(ModelResults)
                     X = model.matrix(ModelResults)
                     value = as.numeric(quantile(X[, varname[i]], c(val1, val2)))
                     #max = as.numeric(quantile(X[, varname[i]], val2))
                     X1[, varname[i]] = value[1]
                     #X1[, varname[i]] = min(X1[, varname[i]])
                     #X1[, varname[i]] = quantile(X1[, varname[i]], val1)
                     X2[, varname[i]] = value[2]
                     #X2[, varname[i]] = max(X2[, varname[i]])
                     #X2[, varname[i]] = quantile(X2[, varname[i]], val2)

       fd[[varname[i]]] =  apply(apply(X2, 1, function (x) plogis(sim %*% x)) -
                                apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean)
              }

       df_plot <- map(fd, data.frame) %>%
                     map2_df(., names(.), ~mutate(.x, id = .y))

       names(df_plot)[1] <- "value"


         p =    ggplot(df_plot, aes(x = value, y = id)) +
                     geom_density_ridges(col = "grey70", scale = .8, show.legend = F) +
                     scale_fill_viridis(discrete = TRUE) +
                     geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
                     theme_ridges(font_size = 13, grid = F, center_axis_labels = T) +
                     theme(axis.title.y = element_blank())
         return(p)
}



