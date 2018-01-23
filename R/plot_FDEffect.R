#' A function to plot the first difference in the predicted probability of the interaction between a binary and interval variables in Logit model with cluster sd .
#'
#' This function creates a facet plot for the interaction terms in Logit model using simulations
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

##function for first difference

plot_FDEffect = function(ModelResults, n.sim = 1000, data, clusterid, varname1, varname2,
                    val1, val2, intervals, xlabs, ylabs){
       #get a sim objective
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       cluster <- data[,clusterid]
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)
       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)

       ##set simulation
       varname2_val = seq(val1, val2, by =intervals)
       df <- array(NA, c(length(varname2_val), 4))

       for (i in 1:length(varname2_val)){
              X1 <- model.matrix(ModelResults)
              X2 <- model.matrix(ModelResults)
              X1[, varname1] = 0
              X1[, varname2] = varname2_val[i]
              X1[, paste(varname1, varname2, sep = ":")] = 0*varname2_val[i]

              X2[,varname1] = 1
              X2[,varname2] = varname2_val[i]
              X2[, paste(varname1, varname2, sep = ":")] = 1*varname2_val[i]

              fd =  apply(apply(X2, 1, function (x) plogis(sim %*% x)) -
                                 apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean)

              df[i, 1] <- varname2_val[i]
              df[i, 2] <- mean(fd)
              df[i, 3:4] <- quantile(fd, probs = c(.05,.95))
       }

       df_plot <- df
       colnames(df_plot) <- c("X", "mean", "lo", "hi")
       df_p <- as.data.frame(df_plot)

       p =   ggplot(df_p, aes(X)) +
              geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey70") +
              geom_line(aes(y = mean)) +  theme_gray() +
              geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
              scale_x_continuous(name = xlabs, breaks = seq(val1, val2, by =2)) +
              ylab(paste("First Difference in",ylabs, sep = " ")) +
              theme(axis.title.y = element_text(margin = margin(1,1,1,1)),
                    axis.text = element_text(size=14),
                    axis.title=element_text(size=14),
                    strip.text = element_text(size=15))
       return(p)

}
