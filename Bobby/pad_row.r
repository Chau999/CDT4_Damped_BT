#download the package
install.packages('BradleyTerryScalable')
library(BradleyTerryScalable)
library(igraph)


pad_row <- function(years=5,from=2019, torpids=FALSE, draw=TRUE, cutoff=50, plot=FALSE, delta = 0.85){
  row_df <- dataGather(years,from=from,cutoff=cutoff,draw=draw,torpids=torpids)
  if (draw==TRUE){
    codes <- c('W1','W2','D')
  } else{
    codes <- c('W1', 'W2')
  }
  
  row_df_4col <- codes_to_counts(row_df, codes)
  
  row_btdata <- btdata(row_df_4col,return_graph=plot)
  
  if (plot){
    par(mar=c(0,0,0,0) + 0.1)
    
    plot.igraph(row_btdata$graph, vertex.size=3, edge.arrow.size = 0.5)
  }
  
  row_pad <- as.matrix(row_btdata$wins)
  
  avg_wins <- sum(row_pad)/nrow(row_pad)
  
  prior_year <- from - years
  
  prior_year_df <- yeardf(prior_year)
  
  prior_year_df$ranking <- seq_len(nrow(prior_year_df))
  
  currentBoats <- rownames(row_pad)
  
  priorRanks <- prior_year_df[rownames(row_pad),'ranking']
  sortRanks <- sort(priorRanks, index.return= TRUE,na.last=TRUE)
  
  index <- sortRanks$ix
  
  ghostCol <- c()
  
  ghostRow <- c()
  
  ghostRow[index[!is.na(index)]] <- seq(0,1,length.out=length(which(!is.na(priorRanks))))
  
  #ghostRow[which(!is.na(priorRanks))] <- seq(1,0,length.out=length(which(!is.na(priorRanks))))[!is.na(priorRanks)]
  
  ghostRow[which(is.na(priorRanks))] <- 1/2
  
  ghostCol[index[(!is.na(index))]] <- seq(1,0,length.out=length(which(!is.na(priorRanks))))
  
  ghostCol[which(is.na(priorRanks))] <- 1/2
  
  ghostCol<- c(0,ghostCol)
  
  pad <- avg_wins * 2 * (1/delta - 1)
  
  row_pad <- cbind(ghostCol*pad, rbind(ghostRow*pad, row_pad))
  
  rownames(row_pad)[1]='ghostShip'
  colnames(row_pad)[1]='ghostShip'
  return(row_pad)
}

# how about scrooge
scrooge <- function(m){
  
  ## Matrix m has its rows indexed by the "cited" items, 
  ## and columns by "citing" items
  
  ## First convert m into a transition matrix, with column sums 1
  column_totals <- colSums(m)
  m_transition <- m / matrix(column_totals, 54, 54, byrow = TRUE)
  
  ## Now we get the eigenvector that corresponds to eigenvalue 1 
  ## (the first eigenvalue, so we want the first column from the matrix of eigenvectors):
  pagerank_vector <- (eigen(m_transition) $ vectors)[, 1]
  
  ## And then _scale_ the pagerank vector by the journals' out-citation counts, 
  ## and then (arbitrarily) re-normalize to have mean 1:
  pagerank_scaled <- pagerank_vector / column_totals
  pagerank_scaled <- pagerank_scaled / mean(pagerank_scaled)
  return(sort(log(pagerank_scaled),decreasing=TRUE)-mean(log(pagerank_scaled)))
}



