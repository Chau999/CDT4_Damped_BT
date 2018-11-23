library(igraph)
library(BradleyTerryScalable)

# Load the true data
data(citations)
citations

# Load the toy_data
data(toy_data)
?toy_data

# Format the data into correct format
citations_btdata <- btdata(citations, return_graph = TRUE)
toy_data_4col <- codes_to_counts(toy_data, c("W1", "W2", "D"))
toy_btdata <- btdata(toy_data_4col, return_graph = TRUE)

# Plot
par(mar=c(0,0,0,0) + 0.1)
plot.igraph(citations_btdata$graph, vertex.size=58, edge.arrow.size=0.5)
plot.igraph(toy_btdata$graph, vertex.size=28, edge.arrow.size=0.5)

# Extract summary
summary(toy_btdata)

# Fit the method
citations_fit <- btfit(citations_btdata, 1) # Use MLE, fully connected, no worries
toy_fit_MLE <- btfit(toy_btdata, 1) # Try MLE
toy_fit_Bayes <- btfit(toy_btdata, 1.1)

# Methods for btfit object
summary(citations_fit)
summary(toy_fit_Bayes)

#############################
# Trying the Padding method #
#############################

toy_pad <- as.matrix(toy_btdata$wins)
toy_newpad <- cbind(rep(1, 9),rbind(rep(1, 8), toy_pad))
rownames(toy_newpad) <- c("Pad",rownames(toy_pad))
colnames(toy_newpad) <- c( "Pad", colnames(toy_pad))

pad_btdata <- btdata(toy_newpad, return_graph = TRUE) # so the graph becomes a fully connected graph
plot.igraph(pad_btdata$graph, vertex.size=30, edge.arrow.size=0.7, edge.width=3 * E(pad_btdata$graph)$weight)

pad_fit <- btfit(pad_btdata, 1)

summary(pad_fit)

# Compare with Bayesian method
cbind(names(coef(pad_fit))[-4], names(coef(toy_fit_Bayes))) # Only getting 1 disagreement

summary(toy_fit_MLE) # Not comparable


############################################
## Trying with different level of padding ##
############################################
padding_fit <- function(mat, pad_lvl=1, a=1, rm_pad_coef=TRUE){
  # Initialisation
  len <- nrow(mat)
  n_mat <- cbind(rep(pad_lvl, (len + 1)), rbind(rep(pad_lvl, len), mat))
  rownames(n_mat) <- c("Pad", rownames(mat))
  colnames(n_mat) <- c("Pad", colnames(mat))
  
  # Model Fitting using the MLE method
  pad_btdata <- btdata(n_mat)
  pad_fit <- btfit(pad_btdata, a) # Use the MLE method when a == 1, else bayesian

  if (rm_pad_coef) return(coef(pad_fit)[-which(names(coef(pad_fit)) == "Pad")])
  else return(coef(pad_fit))
}

padding_fit(toy_pad, pad_lvl = 100) # Pad_lvl does not concern the overall ranking

######################################################
## Turning the graph into a fully connected network ##
######################################################
damping_fit <- function(mat, damp_lvl, a=1){
  len = nrow(mat)
  n_mat <- (1 - damp_lvl) * mat + damp_lvl * matrix(1, len, len)
  
  pad_btdata <- btdata(n_mat)
  pad_fit <- btfit(pad_btdata, a) # Use the MLE method when a == 1, else bayesian
  
  return(coef(pad_fit))
}

damping_fit(toy_pad, damp_lvl=0.85) # seems alright
damping_fit(toy_pad, damp_lvl=1)

