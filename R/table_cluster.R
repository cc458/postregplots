#' A function to make a latex table from Logit model with cluster sd .
#'
#' This function creates a  latex table for Logit model
#'
#' @param ModelResults list. Specify model name of the logit model.
#' @param varDef character vector. A vector contains the name of variables
#' @param data data.frame. The name of the data frame in logit model
#' @param clusterid character. The cluster id
#' @import ggplot2 arm multiwayvcov lmtest gridExtra viridis ggridges dplyr
#' ggplot2 tidyr stringr purrr
#' @export

table_cluster = function(ModelResults, varDef, digs=3, robustse = TRUE, data, clusterid){

       library(multiwayvcov)
       library(lmtest)
       library(stringi)

       if(robustse){
              cluster <- data[,clusterid]
              vcov_cluster = lapply(ModelResults,function(x) FUN = cluster.vcov(x, cluster))
              modSumm = mapply(function(x, y) coeftest(x,y), x = ModelResults, y = vcov_cluster)
              noModels=length(modSumm)
              varsTable=VarDef[-1,1] #remove Dv
              tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)
              tableResults[,1]=rep(varsTable,2)
              colnames(tableResults) = c('Variable',paste('Model',1:noModels))
       }

       else { modSumm=lapply(ModelResults,function(x) FUN= summary(x)$coefficients[,c('Estimate','Std. Error','Pr(>|z|)')])
       noModels=length(modSumm)
       varsTable=VarDef[-1,1] #remove Dv
       tableResults = matrix('', nrow=2*length(varsTable), ncol=1+noModels)
       tableResults[,1]=rep(varsTable,2)
       colnames(tableResults) = c('Variable',paste('Model',1:noModels))
       }

       for(i in 2:ncol(tableResults)){
              temp = modSumm[[i-1]]
              n <-  nrow(ModelResults[[i-1]]$model)
              AIC <- ModelResults[[i-1]]$aic
              Deviance <- ModelResults[[i-1]]$deviance
              LogLikelihood <-logLik(ModelResults[[i-1]])
              #countries <- nrow(summary(ModelResults[[i-1]])$res.eff[[1]]$res.sfe)
              # n = ModelResults[[2-1]]$n-length(ModelResults[[2-1]]$CoefTable)
              temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
              estims = temp[1:length(varsTable),'Estimate']
              estims = round(as.numeric(as.character(estims)),digs)
              pvals = abs(temp[1:length(varsTable),'Pr(>|z|)'])
              pvals = round(as.numeric(as.character(pvals)),digs)
              estims = ifelse(pvals<=0.10 & !is.na(pvals) & pvals>0.05,
                              paste('$', estims,'^{\\ast}$',sep=''), estims)
              estims = ifelse(pvals<0.10 & !is.na(pvals) & pvals<=0.05,
                              paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
              estims = ifelse(is.na(estims),'',estims)
              tableResults[1:length(varsTable),i] = estims
              serrors = temp[(length(varsTable)+1):nrow(tableResults),'Std. Error']
              serrors = round(as.numeric(as.character(serrors)),digs)
              serrors = paste('(',serrors,')',sep='')
              serrors = ifelse(serrors=='(NA)','',serrors)
              tableResults[(length(varsTable)+1):nrow(tableResults),i] = serrors
       }

       # Reorganizing rows and variable labels
       tableFinal = NULL
       for(i in 1:length(varsTable)){
              temp = cbind('', t(tableResults[i+length(varsTable), 2:ncol(tableResults)]))
              tableFinal = rbind(tableFinal, tableResults[i,], temp)
       }


       # Adding other info
       sSize = cbind('Num. obs.', t(as.vector(mapply(x=ModelResults,
                                                     function(x) FUN=nrow(x$model)))))

       AIC <- cbind('AIC', t(as.vector(round(as.numeric(as.character(mapply(x=ModelResults,
                                                                            function(x) FUN=x$aic))), digs))))
       Deviance <- cbind('Deviance', t(as.vector(round(as.numeric(as.character(mapply(x=ModelResults,
                                                                                      function(x) FUN=x$deviance))), digs))))
       LogLikelihood <- cbind('Log Likelihood', t(as.vector(round(as.numeric(as.character(mapply(x=ModelResults,
                                                                                                 function(x) FUN=logLik(x)))), digs))))

       if(digs<=2){ digBotRow = digs } else { digBotRow = digs -1 }

       tableFinal = rbind(tableFinal, sSize, AIC, Deviance, LogLikelihood)
       #assign labels
       temp=VarDef[match(tableFinal[,'Variable'], VarDef[,1]),2]
       temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
       tableFinal[,'Variable']=temp

       ##filter empty variable
       tableFinal =  tableFinal[apply(apply(tableFinal[,2:dim(tableFinal)[2]], 1, function(x) stri_isempty(x)), 2, sum) < noModels,]
}
