library(BradleyTerryScalable)
library(igraph)

source('functions.R')

pi = sort(exp(rnorm(n)), decreasing=T)
pi = pi / sum(pi)

K = 10 # number of games between 2 players
pi = pi * K
V = generate_ladder(pi, K) # matrices of victories: Vij= # i beats j

fict_game = sort(seq_len(n), decreasing=T) / n
V = rbind(rep(1,n), V)
V = cbind(c(0,rep(1,n)),V)


bt_object = btdata(V, return_graph = T)

par(mar = c(0, 0, 0, 0) + 0.1)  
plot.igraph(bt_object$graph, vertex.size = 6, edge.arrow.size = 1)

str_mle <- btfit(bt_object, a = 1)
str_mle $ pi [[1]]

df_coef <- coef(str_mle, as_df=T)
df_coef$"item" <- as.numeric(df_coef$"item")
df_coef <- df_coef[order(df_coef$"item"),]

pi_est <- exp(df_coef["coef"][[1]])