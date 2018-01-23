#' A function to plot the first difference in the predicted probability of the interaction between two binary variables in Logit model with cluster sd .
#'
#' This function creates a density ridge plot for the interaction terms in Logit model using simulations
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param n.sim numeric. Specify the number of simulations
#' @param varname1 character. A binary variable
#' @param varname2 character. An interval vaiable
#' @param data data.frame. The name of the data frame in logit model
#' @param val1 numeric. Specify the min value
#' @param val2 numeric. Specify the max value
#' @param intervals numeric. Specify the intervals for the sequence
#' @param clusterid character. The cluster id
#' @param xlabs character. The x label for the plot
#' @param ylabs character. The y label for the plot
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export


plot_binary_FDridge = function(ModelResults, n.sim = 1000, data, clusterid, varname1, varname2,
                            val1, val2, intervals, label = c("lab1", "lab2")){
       #get a sim objective
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       library(dplyr)
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

       ##set simulation
       varname2_val = seq(val1, val2, by =intervals)
       fd <- array(NA, c(n.sim, length(varname2_val)))

       for (i in 1:length(varname2_val)){
              X1 <- model.matrix(ModelResults)
              X2 <- model.matrix(ModelResults)
              X1[, varname1] = 0
              X1[, varname2] = varname2_val[i]
              X1[, paste(varname1, varname2, sep = ":")] = 0*varname2_val[i]

              X2[,varname1] = 1
              X2[,varname2] = varname2_val[i]
              X2[, paste(varname1, varname2, sep = ":")] = 1*varname2_val[i]

              fd[, i] =  apply(apply(X2, 1, function (x) plogis(sim %*% x)) -
                                      apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean)
       }

       df_plot <- fd
       colnames(df_plot) <- c("no", "yes")
       simprodicted <- as.data.frame(df_plot)
       simprodicted = simprodicted %>% dplyr::mutate(diff = yes - no)

       require(reshape2)
       require(ggplot2)
       simprodicted <- melt(simprodicted)
       levels(simprodicted$variable)
       levels(simprodicted$variable) <- label

       p = ggplot(simprodicted, aes(x = value, y = variable)) +
              geom_density_ridges(col = "grey70", scale = .8, show.legend = F) +
              scale_fill_viridis(discrete = TRUE) +
              geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
              theme_ridges(font_size = 13, grid = F, center_axis_labels = T) +
              theme(axis.title.y = element_blank())
       return(p)
}

