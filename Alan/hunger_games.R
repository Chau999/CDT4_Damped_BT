##################
## Hunger Games ##
##################
library("Kendall")
library('RColorBrewer')
library(randomNames)
library(igraph)
library(BradleyTerryScalable)
set.seed(1234)

# Generate players
num_players <- 13
#players <- randomNames(num_players, sample.with.replacement = FALSE, which.names = "first",name.sep=" ")
players <- c("Alan","Maud","Natalia","Valarie","James","Bobby","Deborah","Lorenzo","Lucy","Emanualle","Ana","William","Hector")

# Generate skills
skills <- exp(rnorm(num_players, 0, 1))
names(skills) <- players
hist(skills) # just a glimpse

# Lets say the players are dead after getting defeated 4 times
hunger_games <- function(num_rounds, player_vec, skill_vec, health_limit){
  num_players <- length(player_vec)
  death_list <- c()
  current_players <- rep(player_vec)
  health_vec <- rep(health_limit, num_players)
  names(health_vec) <- player_vec
  score_mat <- matrix(0, nrow=num_rounds, ncol=3)
  colnames(score_mat) <- c("Player1", "Player2", "Outcome")
  for (i in 1:num_rounds){
    # Sample two player
    if (length(current_players) == 1){
      return(list(score = score_mat[1:(i-1),], death_ls = c(death_list, current_players)))
    }
    else{
      curr_P <- sample(current_players, size = 2, replace = FALSE)
      score_mat[i, 1:2] <- curr_P
      skill_1 <- skill_vec[curr_P[1]]
      skill_2 <- skill_vec[curr_P[2]]
      alpha <- skill_1 / (skill_1 + skill_2)
      outcome <- sample(c("W1","W2"), size = 1, prob = c(alpha, 1 - alpha))
      score_mat[i, 3] <- outcome
      if (outcome == "W1") {
        health_vec[curr_P[2]] = health_vec[curr_P[2]] - 1
        if (health_vec[curr_P[2]] == 0){
          current_players = current_players[-which(current_players == curr_P[2])]
          death_list <- c(death_list, curr_P[2])
        }
      }
      else if (outcome == "W2"){
        health_vec[curr_P[1]] = health_vec[curr_P[1]] - 1
        if (health_vec[curr_P[1]] == 0){
          current_players = current_players[-which(current_players == curr_P[1])]
          death_list <- c(death_list, curr_P[1])
        }
      }
    }
  }
  return(list(score = score_mat, death_ls = c(death_list, current_players)))
}

# generate hunger frame
hunger_list <- hunger_games(500, players, skills, 1)
hunger_death <- hunger_list$death_ls
hunger_sim <- as.data.frame(hunger_list$score)

# Generate data into correct format
hunger_4col <- codes_to_counts(hunger_sim, c("W1", "W2"))
bthunger <- btdata(hunger_4col, return_graph = TRUE)

# Visualise
plot.igraph(bthunger$graph, 
            vertex.size=28, 
            edge.arrow.size=0.5, 
            #edge.width=3 * E(bthunger$graph)$weight, 
            shape="circle", 
            curved=TRUE,
            edge.color = "blue",
            vertex.color = "lightblue"
            #layout = layout_in_circle(bthunger$graph, order = hunger_death)
            )

tkplot(bthunger$graph)

# Highest kill
degree(bthunger$graph, mode = "out")
degree(bthunger$graph, mode = "in")

###########################
## Stop Playing Games now #
###########################
skills
summary(bthunger) # Not fully connected

# Apply padding
coef_pad <- padding_fit(bthunger$wins, pad_lvl=1)
coef_pad[names(skills)]

# Apply Damping
coef_damp <- damping_fit(bthunger$wins, damp_lvl = 0.15, a = 1)
coef_damp[names(skills)]

# Apply Bayesian model
Bayes_model <- btfit(bthunger, 2)
coef_bayes <- Bayes_model$pi$full_dataset

ranks <- cbind(names(sort(skills, decreasing = TRUE)), names(sort(coef_pad, decreasing = TRUE)))
ranks <- cbind(ranks, names(sort(coef_damp, decreasing = TRUE)))
ranks <- cbind(ranks, names(sort(coef_bayes, decreasing = TRUE)))
colnames(ranks) <- c("TRUE", "Padding", "Damping", "Bayesian")
ranks

true_skill <- skills[players]/sum(skills)
estimate_skill <- coef_pad[players]/sum(coef_pad)

sqrt(mean((true_skill - estimate_skill)^2))

tst <- Kendall(c(1,2,3), c(3,1,2))




