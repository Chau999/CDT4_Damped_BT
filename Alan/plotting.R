  library(igraph)
  library(BradleyTerryScalable)
  
  # Load the toy_data
  data(toy_data)
  
  # Format the data into correct format
  toy_data_4col <- codes_to_counts(toy_data, c("W1", "W2", "D"))
  toy_btdata <- btdata(toy_data_4col, return_graph = TRUE)
  
  # Plot
  par(mar=c(0,0,0,0) + 0.1)
  plot.igraph(toy_btdata$graph, vertex.size=28, edge.arrow.size=0.5, edge.color = "blue", vertex.color="lightblue")
  
  #-------------------------------------------------------------------
  
  # Plot Padding
  padding_fit_grpah_mat <- function(mat, pad_lvl=1, a=1, rm_pad_coef=TRUE){
    # Initialisation
    len <- nrow(mat)
    n_mat <- cbind(rep(pad_lvl, (len + 1)), rbind(rep(pad_lvl, len), mat))
    rownames(n_mat) <- c("Pad", rownames(mat))
    colnames(n_mat) <- c("Pad", colnames(mat))
    
    # Model Fitting using the MLE method
    pad_btdata <- btdata(n_mat, return_graph = TRUE)
    return(pad_btdata$graph)
  }
  
  pad_graph <- padding_fit_grpah_mat(toy_btdata$wins)
  
  plot.igraph(pad_graph, vertex.size=28, edge.arrow.size=0.5, edge.color = "blue", vertex.color="lightblue")
  
  #--------------------------------------------------------------------
  
  # Plot Teleport
  damping_fit_plot_graph <- function(mat, damp_lvl, a=1){
    len = nrow(mat)
    n_mat <- (1 - damp_lvl) * mat + damp_lvl * matrix(1, len, len)
    
    pad_btdata <- btdata(n_mat, return_graph = TRUE)
    return(pad_btdata$graph)
  }
  
  damp_graph <- damping_fit_plot_graph(toy_btdata$wins, damp_lvl = 0.85)
  plot.igraph(damp_graph, vertex.size=28, edge.arrow.size=0.5, edge.color = "blue", vertex.color="lightblue")
