#' A function to plot ROC plots
#' This function creates a ggplot for ROC
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param linetypes character. A vector of charactors indicting the line types
#' @param interval numeric. Specify the intervals for the sequence
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export

##function for first difference
plot_roc <- function(ModelResults,linetypes = c("solid", "dotted"), interval){
       require(plotROC)
       require(pROC)
       require(dplyr)
       require(ggplot2)
       library(purrr)
       library(tibble)
       library(magrittr)

       pred_dv =lapply(ModelResults, function(x)
                     FUN = predict(x, type = "response"))

       Y = lapply(ModelResults, function(x) FUN = x$y)

       roc = Map(function(x, y) roc(x,y), Y, pred_dv)

       roc_df  = lapply(roc, function(x)
              FUN= data.frame(plotx = x$specificities,
                             ploty = rev(x$sensitivities),
                             name = paste("AUC =",
                                          sprintf("%.3f",x$auc)))) %>%
              map_df(., rbind)

       breaks = seq(0, 1, interval)

       p = ggplot(roc_df, aes(x = plotx, y = ploty)) +
              geom_line(aes(linetype = name)) +
              scale_linetype_manual(name = '',
                                    values=linetypes) +
              geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.5) +
              #geom_step(aes(linetype = name)) +
              scale_x_continuous(name = "1-Specificity",limits = c(0,1),
                                 breaks = breaks, expand = c(0.001,0.001)) +
              scale_y_continuous(name = "Sensitivity", limits = c(0,1),
                                 breaks = breaks, expand = c(0.001,0.001)) +
              theme_grey() +
              theme(legend.position=c(.8, 0.1),
                    axis.title.y = element_text(margin = margin(1,1,1,1)),
                    axis.text = element_text(size=13),
                    axis.title=element_text(size=13))
        return(p)

}


#plot_roc(ModelResults = list(c, f1), linetypes = c("solid", "dotted"), interval=0.2)
