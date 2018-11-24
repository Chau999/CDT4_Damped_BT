# playing around with the functions to see how the fit is
prob_generator <- function(pi1,pi2,outcome,theta){
  denom <- pi1 + pi2 + theta * sqrt(pi1*pi2)
  if (outcome =='D'){
    return(theta * sqrt(pi1*pi2)/denom)
    
  } else if (outcome =='W1'){
    return(pi1/denom)
    
  } else {
    return(pi2/denom)
  }
}

teleporter <- function(df,delta,year){
  year_df<-yeardf(year)
  year_df$ranking <- seq_len(nrow(year_df))
  alphabet_df <- year_df[sort(rownames(year_df)),]
  df_boatnames <- unique(c(df[,1],df[,2]))
  rank_vec <- alphabet_df[rownames(alphabet_df) %in% df_boatnames ,]$ranking
  rank_vec <- max(rank_vec)-rank_vec
  rank_vec_norm <- rank_vec/sum(rank_vec)
  df_4_col <- codes_to_counts(df,c('W1','W2','D'))
  df_btdata <- btdata(df_4_col,return_graph=FALSE)
  df_mat <- as.matrix(df_btdata$wins)
  col_sums <- colSums(df_mat)
  col_sums[col_sums==0] <- 1

  # might get a problem if a boat hasn't lost at all in df
  df_mat_norm <- t(t(df_mat)/col_sums)
  df_mat_norm<- delta *df_mat_norm + (1-delta)*rank_vec_norm
  df_teleport_mat <- t(t(df_mat_norm)*col_sums) 
  df_teleport_mat <- as.matrix(df_teleport_mat)
  print(c(length(rank_vec_norm),nrow(df_mat_norm)))
  return(df_teleport_mat)
}


method_comparison <- function(years=5, torpids=FALSE, draw=TRUE, cutoff=50, plot=TRUE, delta = 0.85){
  pad_prob_vec <- c()
  no_pad_prob_vec <- c()
  tele_prob_vec<-c()
  start = 2018-years
  for (from_year in seq(start, start + years)){
    print(from_year)
    pad_row_df <- pad_row(years=5,from=from_year, torpids=torpids, draw=draw, cutoff=cutoff, plot=FALSE, delta = delta)
    no_pad_df <- dataGather(years=5,from=from_year,cutoff=cutoff)
    no_pad_4_col <- codes_to_counts(no_pad_df,c('W1','W2','D'))
    no_pad_btdata <- btdata(no_pad_4_col,return_graph=FALSE)
    no_pad_fit <- btfit(no_pad_btdata, a = 1.1)
    pad_btdata <- btdata(pad_row_df)
    pad_fit <- btfit(pad_btdata, a=1)
    pad_pi <- exp(coef(pad_fit))
    no_pad_pi <- exp(coef(no_pad_fit))
    tele_wins<- teleporter(no_pad_df,delta,year=from_year-years)
    tele_btdata <- btdata(tele_wins)
    tele_fit <- btfit(tele_btdata,a=1)
    coeffs<- coef(tele_fit)
    # for some reason coeffs is sometimes a list
    if (is.list(coef(tele_fit))){
      coeffs <- coef(tele_fit)[[1]]
    }
    tele_pi <- exp(coeffs)
    current_df <- dataGather(years=1,from=from_year+1,cutoff=50)
    #observed draw prob, help us choose theta
    draw_prob<-sum(no_pad_df[,'outcome']=='D')/nrow(no_pad_df)
    
    theta = 2* draw_prob /(1-draw_prob)
    
    boatNames <- rownames(pad_row_df)
    
    current_df <- current_df[current_df[,'Boat_1'] %in% boatNames & current_df[,'Boat_2'] %in% boatNames,]
    
    log_prob_pad <- sum(sapply(seq_len(nrow(current_df)), function(x) log(prob_generator(pad_pi[current_df[x,"Boat_1"]],
                                                                                         pad_pi[current_df[x,'Boat_2']],
                                                                                         current_df[x,'outcome'],
                                                                                         theta))))
    log_prob_no_pad <- sum(sapply(seq_len(nrow(current_df)), function(x) log(prob_generator(no_pad_pi[current_df[x,"Boat_1"]],
                                                                                            no_pad_pi[current_df[x,'Boat_2']],
                                                                                            current_df[x,'outcome'],
                                                                                            theta))))
    log_prob_tele <- sum(sapply(seq_len(nrow(current_df)), function(x) log(prob_generator(tele_pi[current_df[x,"Boat_1"]],
                                                                                            tele_pi[current_df[x,'Boat_2']],
                                                                                            current_df[x,'outcome'],
                                                                                            theta))))
    pad_prob_vec <- c(pad_prob_vec, log_prob_pad)
    
    no_pad_prob_vec <- c(no_pad_prob_vec,log_prob_no_pad)
    tele_prob_vec <- c(tele_prob_vec , log_prob_tele)
    log_mat <- matrix(c(pad_prob_vec,no_pad_prob_vec,tele_prob_vec),ncol=3)
  }
  if (plot==TRUE){
    plot(seq(start,start+years), log_mat[,1],'b', col='red',ylim=c(min(log_mat),max(log_mat)),xlab='year',ylab='Log-likelihood')
    lines(seq(start,start+years),log_mat[,2],'b',col='blue')
    lines(seq(start,start+years),log_mat[,3],'b',col='green')
    legend('bottomright',c('MAP','Padding','Teleport'),col=c('red','blue','green'))

  }
  return(matrix(c(pad_prob_vec,no_pad_prob_vec),ncol=2))
}



