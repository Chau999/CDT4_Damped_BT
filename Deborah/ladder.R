library(BradleyTerryScalable)
library(igraph)

source('functions.R')
 
###############################################################################
#SETTING

n = 50 # number of players

pi = numeric(n) # vector of strengths
# for (i in seq_len(n)){
#   pi[n-i+1] = runif(1,min=i-1,max=i+1)
# }
#pi = seq(from=n,to=1,by=-1)
# 
# pi=sort(runif(n,0,100))
# pi = pi / sum(pi) # normalizing

pi = sort(exp(rnorm(n)), decreasing=T)
pi = pi / sum(pi)

K = 10 # number of games between 2 players
pi = pi * K
V = generate_ladder(pi, K) # matrices of victories: Vij= # i beats j 


################################################################################
# Fit with Bradley terry model

bt_object = btdata(V, return_graph = T)

par(mar = c(0, 0, 0, 0) + 0.1)  
plot.igraph(bt_object$graph, vertex.size = 6, edge.arrow.size = 1)

str_mle <- btfit(bt_object, a = 1)   ## compute the MLE
str_mle $ pi [[1]] ## extract the Bradley-Terry "ability" scores

df_coef <- coef(str_mle, as_df=T)
df_coef$"item" <- as.numeric(df_coef$"item")
df_coef <- df_coef[order(df_coef$"item"),]

pi_est <- exp(df_coef["coef"][[1]])

mse <- mean((pi-pi_est)^2)

