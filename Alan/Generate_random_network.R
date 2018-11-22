library(BradleyTerryScalable)
library(igraph)
library(stringdist)
#set.seed(1234)

# 
Players <- LETTERS[1:26]
skills <- exp(rnorm(26, sd=3))
print(skills)
names(skills) <- Players

gen_random_competition <- function(num_games=100, player_vec=Players, skill_vec=skills){
  col_1 <- sample(x = player_vec, size = num_games, replace = TRUE)
  col_2 <- sample(x = player_vec, size = num_games, replace = TRUE) 
  score_mat <- cbind(col_1, col_2)
  for (i in 1:num_games){
    if (score_mat[i,1] == score_mat[i, 2]){
      score_mat[i,] = sample(x = player_vec, size = 2, replace = FALSE)
    }
  }
  prob_win_vec <- skill_vec[score_mat[, 1]]/(skill_vec[score_mat[,1]] + skill_vec[score_mat[, 2]])
  win_vec <- rep(0, num_games)
  for (j in 1:num_games){
    win_vec[j] <- sample(c("W1", "W2"), prob = c(prob_win_vec[j], 1 - prob_win_vec[j]))
  }
  
  score_mat <- as.data.frame(cbind(score_mat, win_vec))
  colnames(score_mat) <- c("Player_1", "Player_2", "Result")
  return(score_mat)
}

score_mat <- gen_random_competition()

# Format Data
sim_data <- codes_to_counts(score_mat, c("W1","W2"))
sim_btdata <- btdata(sim_data, return_graph = TRUE)

# Plot data
par(mar=c(0,0,0,0) + 0.1)
plot.igraph(sim_btdata$graph, edge.arrow.size=0.5, edge.width=3 * E(sim_btdata$graph)$weight)
dev.off()

# Get summary
summary(sim_btdata) # Have connected component

# Compare different padding
score_vec <- rep(0, 50)
ls_0 <- paste(names(sort(skills, decreasing = TRUE)), collapse = '')
print(ls_0)
for (pad in 1:50){
  ls_1 <- paste(names(padding_fit(as.matrix(sim_btdata$wins), pad_lvl = pad)), collapse = '')
  print(ls_1)
  score_vec[pad] = stringdist(ls_1, ls_0, method='osa')/nchar(ls_0)
}

plot(1:50,score_vec)

# Try it for values less than 1 
score_vec <- rep(0, 500)
pad_vec <- seq(from=0.01, to = 10, length.out = 500)
ls_0 <- paste(names(sort(skills, decreasing = TRUE)), collapse = '')
print(ls_0)
for (pad in 1:500){
  ls_1 <- paste(names(padding_fit(as.matrix(sim_btdata$wins), pad_lvl = pad_vec[pad])), collapse = '')
  print(ls_1)
  score_vec[pad] = stringdist(ls_1, ls_0, method='osa')/nchar(ls_0)
}
plot(pad_vec, score_vec)

# Compare differnt 

# Normalise the shit
# Try it for values less than 1 
score_vec <- rep(0, 500)
pad_vec <- seq(from=0.01, to = 10, length.out = 500)
ls_0 <- paste(names(sort(skills, decreasing = TRUE)), collapse = '')
score_0 <- sort(skills, decreasing = TRUE)
score_0 <- score_0/sum(score_0)

for (pad in 1:500){
  score_1 <- padding_fit(as.matrix(sim_btdata$wins), pad_lvl = pad_vec[pad])
  score_1 <- score_1/sum(score_1)
  score_1 <- score_1[LETTERS[1:26]]
  score_vec[pad] = sqrt(mean((score_1 - score_0)^2))
}

plot(pad_vec, score_vec)




