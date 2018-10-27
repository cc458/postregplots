#function for predicted probs
predict_probs <- function (ModelResults, n.sim = 1000, varname, data, val1 = 0,
         val2 = 1,intervals, clusterid){
              library(arm)
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

##set simulation
sim_b1 <- sim_cluster(ModelResults, n.sim, data = data, clusterid)

varname_val = seq(val1, val2, by = intervals)
df <- array(NA, c(length(varname_val), 4))


for (i in 1:length(varname_val)){
       X1 <- model.matrix(ModelResults)
       X1[, varname] = varname_val[i]
       fd = apply(apply(X1, 1, function (x) plogis(sim_b1 %*% x)), 1, mean)
       df[i, 1] <- varname_val[i]
       df[i, 2] <- mean(fd)
       df[i, 3:4] <- quantile(fd, probs = c(.05,.95))
}

colnames(df) <- c("X", "mean", "lo", "hi")
df <- as.data.frame(df)

p = ggplot(df, aes(x=X, y=mean)) +
       theme_minimal() +
       geom_ribbon(aes(ymin = lo, ymax = hi), alpha=.2, color = NA) +
       geom_line(aes(y = mean, x = X), size = 1.5) +
       theme(axis.text = element_text(size=12),
             plot.title = element_text(hjust = 0,size = 30, face = "bold"),
             axis.title=element_text(size=12))
return(p)
}

