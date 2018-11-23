library(BradleyTerryScalable)
source('functions.R')

n = 50
pi = sort(exp(rnorm(n)), decreasing=T)
pi = pi / mean(pi)

K = c(10,100,1000,10^4,10^5,10^6,10^7)
# K = 1 doesnt work
mse = numeric(length(K))

for (k in seq_along(K)){
  
  print(k)
  
  V = generate_ladder(pi, K[k])
  bt_object = btdata(V)

  str_f <- btfit(bt_object, a = 1)
  df_coef <- coef(str_f, as_df=T)
  df_coef$"item" <- as.numeric(df_coef$"item")
  df_coef <- df_coef[order(df_coef$"item"),]
  
  pi_est <- exp(df_coef["coef"][[1]])
  
  mse[k] <- mean((pi-pi_est)^2)
  
}

plot(K,mse,log="x")