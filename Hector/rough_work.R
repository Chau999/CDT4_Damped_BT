library(BradleyTerryScalable)
library(igraph)

# Download data with Bobby's function
spring2018 <- dataGather(draw = FALSE)

# Rearrange torpids data such that its suitable for btdata object
spring2018 <- cbind(spring2018[c(2,1)],1)
# Create a btdata object for torpids: matrix containing number of times
# row i beats column j
spring2018_mat <- btdata(spring2018, return_graph = TRUE)
spring2018_mat$wins

# Visualise
plot.igraph(spring2018_mat$graph, 
            vertex.size = 10, 
            edge.arrow.size = 0.3,
            shape="circle", 
            curved=TRUE
)

summary(spring2018_mat)
# What is a fully-connected component?
# What is a component of size 1? 2?
# Fully-connected: FALSE => can't find MLE for full data set

# Write function to visualise data
visualise <- function(years = 1, from = 2019, torpids = FALSE, draw = FALSE,
                      vertex_size = 5, edge_arrow_size = 0.3){
  data <- dataGather(years = years, from = from, torpids = torpids, draw = draw)
  data <- cbind(data[c(2,1)],1)
  data_mat <- btdata(data, return_graph = TRUE)
  plot.igraph(data_mat$graph, 
              vertex.size = vertex_size, 
              edge.arrow.size = edge_arrow_size,
              shape = "sphere", 
              curved=TRUE)
}
visualise(1)
visualise(2)
visualise(3)
visualise(4)

# Find the MLE
spring2018_fit_MLE <- btfit(spring2018_mat, a = 1)
summary(spring2018_fit_MLE)
# Interpretation?

# Mind the MAP
spring2018_fit_MAP <- btfit(spring2018_mat, a = 2)
spring2018_fit_MAP$pi
# Note that Pembroked 

