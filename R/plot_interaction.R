#' A function to plot interaction effect between a binary and interval variables in Logit model with cluster sd .
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




plot_interaction = function(ModelResults, n.sim = 1000, data, clusterid, varname1, varname2,
                            var1val1, var1val2, intervals1, var2val1, var2val2, intervals2, 
                            lenlabel, var1xlabel, var2ylabel, ncut = 5, ndig= 3, colpal="Set1"){
       #get a sim objective
       require(arm)
       library(multiwayvcov)
       library(lmtest)
       library(ggplot2)
       library(RColorBrewer)
       cluster <- data[,clusterid]
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)
       set.seed(12345)
       sim <- mvrnorm(n= n.sim, coef(ModelResults), vcov_cluster)
       
       ##set simulation
       varname1_val = seq(var1val1, var1val2, by =intervals1)
       varname2_val = seq(var2val1, var2val2, by =intervals2)
       
       df_plot <- list()
       df <- array(NA, c(length(varname2_val),5))
    
for (j in 1:length(varname1_val)){
              
       for (i in 1:length(varname2_val)){
              X1 <- model.matrix(ModelResults)
              X1[, varname1] = varname1_val[j]
              X1[, varname2] = varname2_val[i]
              X1[, paste(varname1, varname2, sep = ":")] = varname1_val[j]*varname2_val[i]

              fd = apply(apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean)
              df[i, 1] <- varname1_val[j]
              df[i, 2] <- varname2_val[i]
              df[i, 3] <- mean(fd)
              df[i, 4:5] <- quantile(fd, probs = c(.05,.95))
              df_plot[[j]] <- list(df)
              }
}

library(purrr)

df_plot <- map(df_plot, data.frame) %>%
              map_df(., rbind)

colnames(df_plot) <- c("predx", "modx",  "mean", "lo", "hi")
       # Create ten segments to be colored in
       df_plot$breaks <- cut(round(df_plot$mean,ndig), ncut)
       # Sort the segments in ascending order
       breaks <- levels(unique(df_plot$breaks))
       # Plot
cols = brewer.pal(ncut, colpal)

p <- ggplot(df_plot,
            aes(x = predx, y = modx, z = mean)) +
                       coord_fixed() + 
       geom_tile(aes(fill = breaks)) + theme_bw() +
       xlab(var1xlabel) +
       ylab(var2ylabel) +
       scale_fill_manual(values = cols,
                         name = lenlabel, 
                         breaks = breaks, labels = breaks) + 
       theme(axis.title.y = element_text(margin = margin(1,1,1,1)),
             axis.text = element_text(size=14),
             axis.title=element_text(size=14),
             strip.text = element_text(size=14))
return(p)
}

