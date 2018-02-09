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



plot_interEffect = function(ModelResults, n.sim = 1000, data, clusterid, varname1, varname2,
                            val1, val2, intervals, label = c("lab1", "lab2"), xlabs, ylabs, facet = F){
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
       df_no <- array(NA, c(length(varname2_val), 5))
       df_yes <- array(NA, c(length(varname2_val), 5))

       for (i in 1:length(varname2_val)){
              X1 <- model.matrix(ModelResults)
              X2 <- model.matrix(ModelResults)
              X1[, varname1] = 0
              X1[, varname2] = varname2_val[i]
              X1[, paste(varname1, varname2, sep = ":")] = 0*varname2_val[i]

              X2[,varname1] = 1
              X2[,varname2] = varname2_val[i]
              X2[, paste(varname1, varname2, sep = ":")] = 1*varname2_val[i]

              fd_no = apply(apply(X1, 1, function (x) plogis(sim %*% x)), 1, mean)

              fd_yes = apply(apply(X2, 1, function (x) plogis(sim %*% x)), 1, mean)

              df_no[i, 1] <- varname2_val[i]
              df_no[i, 2] <- mean(fd_no)
              df_no[i, 3:4] <- quantile(fd_no, probs = c(.05,.95))
              df_no[i, 5] <- 0
              df_yes[i, 1] <- varname2_val[i]
              df_yes[i, 2] <- mean(fd_yes)
              df_yes[i, 3:4] <- quantile(fd_yes, probs = c(.05,.95))
              df_yes[i, 5] <- 1
       }

       df_plot <- rbind(df_no, df_yes)
       colnames(df_plot) <- c("X", "mean", "lo", "hi", "type")
       df_p <- as.data.frame(df_plot)
       df_p$type <- as.factor(df_p$type)
       levels(df_p$type) <- label

       if(facet == F){
              p =   ggplot(df_p, aes(x=X, y=mean, group = type, color = type, linetype=type)) +
                     theme_gray() +
                     geom_ribbon(aes(ymin = lo, ymax = hi), alpha=.2, color = NA) +
                     geom_line(aes(y = mean, x = X), size = 0.5, alpha = 0.75) +
                     ylab(ylabs) +
                     scale_color_manual( "", labels= label,
                                         values=c("#2c7bb6","#d7191c")
                     ) +
                     scale_linetype_manual("",
                                           labels=label,
                                           values=c(1,2)) +
                     scale_x_continuous(name = xlabs, breaks = seq(val1, val2, by = 2)) +
                     theme(axis.title.y = element_text(vjust=1,size=12),
                           axis.title.x = element_blank(),
                           legend.position="bottom",
                           legend.margin=margin(0,0,0,0),
                           legend.text=element_text(size=12),
                           axis.text.y=element_text(size=12),
                           axis.title=element_text(size=14),
                           axis.text.x=element_text(size=12),
                           plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))
              return(p)}
       else { p =   ggplot(df_p, aes(x=X, y=mean)) + theme_gray() +
              geom_ribbon(aes(x = X, ymin = lo, ymax = hi), alpha=.4) +
              stat_smooth(color = "#2c7bb6") + facet_wrap(~type, scales = "free_y") +
              scale_x_continuous(name = xlabs, breaks = seq(val1, val2, by = 2)) +
              ylab(ylabs) +
              theme(axis.title.y = element_text(margin = margin(1,1,1,1)),
                    axis.text = element_text(size=14),
                    axis.title=element_text(size=14),
                    strip.text = element_text(size=15))
       return(p)
       }

}

