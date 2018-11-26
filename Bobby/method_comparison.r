prob_generator <- function(pi1,pi2,outcome,theta){
  # generate probability of outcome given team 1 strength pi1 and team 2 strength pi2 under 
  # Davidson model, with parameter theta
  denom <- pi1 + pi2 + theta * sqrt(pi1*pi2)
  if (outcome =='D'){
    return(theta * sqrt(pi1*pi2)/denom)
    
  } else if (outcome =='W1'){
    return(pi1/denom)
    
  } else {
    return(pi2/denom)
  }
}

obtain_kendall <- function(true, pred){
  # Input two vectors of coef
  rank_true <- names(sort(true, decreasing = TRUE))
  rank_pred <- names(sort(pred, decreasing = TRUE))
  
  # Create Dummy
  dummy <- c(1:length(true))
  names(dummy) <- names(true)
  
  # Encode
  dummy_true <- dummy[rank_true]
  dummy_pred <- dummy[rank_pred]
  
  # Kendall tau score
  return(Kendall(dummy_true, dummy_pred)$tau[[1]])
}

teleporter <- function(df,delta,year,gender,torpids){
  # df is the results that we create into a wins matrix
  # 1-delta is the teleportation probability
  # year is the year at start of df
  
  # gather rankings at beginning the results depicted in df
  year_df<-yeardf(year, sex= gender,torp = torpids)
  year_df$ranking <- seq_len(nrow(year_df))
  alphabet_df <- year_df[sort(rownames(year_df)),]
  df_boatnames <- unique(c(df[,1],df[,2]))
  
  # create rankings vector in alphabetical order
  temp_rank_vec <- sort(alphabet_df[rownames(alphabet_df) %in% df_boatnames ,]$ranking, index.return=TRUE)
  rank_vec<-temp_rank_vec$ix
  rank_vec[temp_rank_vec$ix] <- rev(seq_len(length(rank_vec)))

  rank_vec_norm <- rank_vec/sum(rank_vec)
  
  
  # change df format into a matrix of wins/losses
  df_4_col <- codes_to_counts(df,c('W1','W2','D'))
  df_btdata <- btdata(df_4_col,return_graph=FALSE)
  df_mat <- as.matrix(df_btdata$wins)
  avg_wins <- sum(df_mat)/nrow(df_mat)
  # modify normalised rank vec such that it has correct length
  # this counts for new boats that haven't been observed before
  rank_vec_norm[which(rownames(df_mat)%in% rownames(alphabet_df))] <- rank_vec_norm
  rank_vec_norm[which(!rownames(df_mat)%in% rownames(alphabet_df))] <- mean(rank_vec_norm)
  
  # avoid diving by zero
  col_sums <- colSums(df_mat)
  col_sums[col_sums==0] <- 1
  
  # create the transition matrix and add the teleportation, then renormalies
  df_mat_norm <- t(t(df_mat)/col_sums)
  df_mat_norm<- delta *df_mat_norm + (1-delta)*rank_vec_norm
  df_teleport_mat <- t(t(df_mat_norm)*col_sums) 
  df_teleport_mat <- as.matrix(df_teleport_mat)
  
  #return(df_teleport_mat)
  return(scrooge(df_mat_norm))
}

######################################################################################

method_comparison <- function(years=5, torpids=FALSE, draw=TRUE, cutoff=50, logPlot=TRUE, rankPlot=TRUE, delta_vec = 0.85, gender='m'){
  pad_rank_mat <- matrix(0,nrow = length(delta_vec),ncol=years)
  tele_rank_mat <-matrix(0,nrow=length(delta_vec), ncol=years)
  no_pad_rank_vec <- rep(0,years)
  comparison_counts <- 0
  nuffin <- rep(0,years)
  rownames(pad_rank_mat)<- delta_vec
  rownames(tele_rank_mat) <- delta_vec
  start = 2018-years
  names(nuffin)<- seq(start+1,start+years)
  colnames(pad_rank_mat)<- seq(start+1,start+years)
  colnames(tele_rank_mat) <- seq(start+1,start+years)
  for (delta in delta_vec){
    paste0('Now looking at delta=',delta)
    pad_prob_vec <- c()
    no_pad_prob_vec <- c()
    tele_prob_vec<-c()
    start = 2018-years
    for (from_year in seq(start+1, start + years)){
      print(from_year)
      
      # get the unppadded data frame
      no_pad_df <- dataGather(years=5,from=from_year,cutoff=cutoff, gender=gender,torpids=torpids)
      no_pad_4_col <- codes_to_counts(no_pad_df,c('W1','W2','D'))
      no_pad_btdata <- btdata(no_pad_4_col,return_graph=FALSE)
      no_pad_fit <- btfit(no_pad_btdata, a = 1.1)
      
      # get the padded win/loss matrix  
      pad_row_df <- pad_row(years=5,from=from_year, torpids=torpids, draw=draw, cutoff=cutoff, plot=FALSE, delta = delta, gender)
      pad_btdata <- btdata(pad_row_df)
      pad_fit <- btfit(pad_btdata, a=1.1)
      
      # apply teleportation to the unpadded results
      #tele_wins<- teleporter(no_pad_df,delta,year=from_year-5,gender, torpids=torpids)
      #tele_btdata <- btdata(tele_wins)
      #tele_fit <- btfit(tele_btdata,a=1.1)
      tele_fit <- teleporter(no_pad_df,delta,year=from_year-5,gender,torpids=torpids)
      # get pi coefficients
      pad_pi <- exp(coef(pad_fit))
      no_pad_pi <- exp(coef(no_pad_fit))
      coeffs<- coef(tele_fit)
      # for some reason coeffs is sometimes a list
      if (is.list(coef(tele_fit))){
        coeffs <- coef(tele_fit)[[1]]
      }
      tele_pi <- exp(coeffs)
      
      # observed draw prob, help us choose theta
      draw_prob<-sum(no_pad_df[,'outcome']=='D')/nrow(no_pad_df)
      theta = 2* draw_prob /(1-draw_prob)
      
      # get new dataset to test predictive abilities of our models, remove data points for which we observe a new boat
      current_df <- dataGather(years=1,from=from_year+1,cutoff=cutoff, gender=gender,torpids=torpids)
      boatNames <- rownames(pad_row_df)
      current_df <- current_df[current_df[,'Boat_1'] %in% boatNames & current_df[,'Boat_2'] %in% boatNames,]
      
      # summed log probabilities for the three methods
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
      
      if (rankPlot==TRUE){
        # organise the data from each year before adding to df
        #print(paste0('Currently downloading data from the year: ', year))
        year_df <- yeardf(from_year, sex=gender,torp=FALSE)
        year_df$ranking <- seq_len(nrow(year_df))
        year_df$finalRanking <- sapply(seq_len(nrow(year_df)),function(x) -sum(year_df[x,1:4])+x)
        
        common_names <- intersect(rownames(year_df),names(pad_pi))
        common_final_rank <- 1/year_df[common_names,'finalRanking']
        names(common_final_rank) <- common_names 
        
        common_initial_rank <- 1/year_df[common_names,'ranking']
        names(common_initial_rank) <- common_names
        
        pad_rank_mat[toString(delta),toString(from_year)] <- obtain_kendall(common_final_rank,pad_pi[common_names])
        tele_rank_mat[toString(delta),toString(from_year)] <- obtain_kendall(common_final_rank,coeffs[common_names])
        no_pad_rank_vec[toString(from_year)]<- obtain_kendall(common_final_rank,no_pad_pi[common_names])
        nuffin[toString(from_year)] <- obtain_kendall(common_final_rank,common_initial_rank)
        
        
      }
      ##################################################################
    }
    if (logPlot==TRUE){
      name <- paste0(gender,delta*100,'comparison.pdf')
      pdf(name,width=8,height=5,paper='special') 
      plot(seq(start+1,start+years), log_mat[,1],'b', col='red',ylim=c(min(log_mat),max(log_mat)),xlab='year',ylab='Log-likelihood',xaxt='n')
      axis(1, at=seq(2008,2018,by=1), las=2)
      lines(seq(start+1,start+years),log_mat[,2],'b',col='blue')
      lines(seq(start+1,start+years),log_mat[,3],'b',col='green')
      legend('bottomright',c('Padding','MAP','Teleport'),col=c('red','blue','green'),lty=1)
      dev.off()
    }

  }
  if (rankPlot==TRUE){
    pad_rank_vec<-rowSums(pad_rank_mat)/years
    tele_rank_vec <- rowSums(tele_rank_mat)/years
    baseline <- sum(no_pad_rank_vec)/years
    nuffin_baseline <-sum(nuffin)/years
    name <- paste0(gender,'deltacomparison.pdf')
    pdf(name,width=8,height=5,paper='special') 
    plot(delta_vec, pad_rank_vec,'b', col='red',ylim=c(min(c(pad_rank_vec,tele_rank_vec,nuffin_baseline,baseline)),max(c(pad_rank_vec,tele_rank_vec,nuffin_baseline))),
         xlab='Delta',ylab='Kendall rank correlation coefficient')
    lines(delta_vec,tele_rank_vec,'b',col='blue')
    abline(h=nuffin_baseline, col='darkgreen')
    abline(h=baseline, col='darkviolet')
    legend('right',c('Padding','Teleport', 'No Change','MAP'),col=c('red','blue','darkgreen','darkviolet'),lty=1)
    dev.off()
  }

  
  
  return(comparison_counts)
}


