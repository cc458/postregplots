marginaleffect_comparison = function(ModelResults, n.sim = 1000, varname, data,
                           val1 = 0 , val2 =1, clusterid){
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
       
       vcov_cluster = lapply(ModelResults, function(x) FUN = cluster.vcov(x, cluster))
       modSumm = mapply(function(x, y) coeftest(x,y), x = ModelResults, y = vcov_cluster)
       noModels=length(modSumm)
      # model_names = paste('Model',1:noModels)
       
       set.seed(12345)
       sim = Map(function(x, y) mvrnorm(n= n.sim, coef(x), y), ModelResults, vcov_cluster)
       
      
       
       fd <- list()
       
       for (i in 1:length(ModelResults)){
              X1 <- model.matrix(ModelResults[[i]])
              X2 <- model.matrix(ModelResults[[i]])
              X = model.matrix(ModelResults[[i]])
              value = as.numeric(quantile(X[, varname[i]], c(val1, val2)))
              X1[, varname[i]] = value[1]
              X2[, varname[i]] = value[2]
              fd[[varname[i]]] =  apply(apply(X2, 1, function (x) plogis(sim[[i]] %*% x)) -
                                                   apply(X1, 1, function (x) plogis(sim[[i]] %*% x)), 1, mean) ##1 indicates row, 2= columns
       }
       
       
       df_plot <- map(fd, data.frame) %>%
              map2_df(., names(.), ~mutate(.x, id = .y))
       
       names(df_plot)[1] <- "value"
       ##change the order of the variable
       
       df_plot$id <- factor(df_plot$id, levels = rev(varname))
       
       
       p =    ggplot(df_plot, aes(x = value, y = id, height=..density.., fill = id)) +
              geom_density_ridges(col = "grey70", scale = .8, show.legend = F) +
              scale_fill_viridis(discrete = TRUE) +
              geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
              theme_ridges(font_size = 13, grid = F, center_axis_labels = T) +
              theme(axis.title.y = element_blank())
       return(p)
       
}


