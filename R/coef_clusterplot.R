#' A function to plot coefficients in Logit model with cluster sd .
#'
#' This function creates a  coefficient plot for Logit model
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param varnames character vector. A vector contains the name of variables
#' @param data data.frame. The name of the data frame in logit model
#' @param clusterid character. The cluster id
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export


###function to make a coefficient plot
coef_clusterplot = function(ModelResults, varnames = NULL, data, clusterid){
       library(multiwayvcov)
       library(lmtest)
       library(dplyr)
       require(ggplot2)
       library(gridExtra)
       cluster <- data[,clusterid]
       vcov_cluster <- cluster.vcov(ModelResults, cluster)
       coef_cluster <- coeftest(ModelResults, vcov = vcov_cluster)

       coef_cluster <- round(coef_cluster, digits = 4)
       modelcoef = coef_cluster[,"Estimate"]
       modelse = coef_cluster[,"Std. Error"]
       ylo = modelcoef - 1.96*(modelse)
       yhi = modelcoef + 1.96*(modelse)
       names = rownames(coef_cluster)
       dfplot = data.frame(names, modelcoef, modelse, ylo, yhi)
       #dfplot$names <- as.character(dfplot$names)
       dfplot <- dfplot %>%
              dplyr::mutate(color_sig = ifelse(ylo < 0 & yhi > 0, "#ef8a62", "#2c7bb6"))
       if(!is.null(varnames)){
              dfplot <- dfplot[match(varnames, dfplot$names),]
       }
       else{dfplot <- dfplot}

       p <- ggplot(dfplot, aes(x=names, y=modelcoef, ymin=ylo, ymax=yhi)) +
              geom_pointrange(size=1, shape=16, colour=dfplot$color_sig) +
              coord_flip() +
              geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
              xlab('') + ylab('') +
              theme(legend.position="none",
                    axis.text = element_text(size=14),
                    axis.title=element_text(size=14))
       return(p)
}
