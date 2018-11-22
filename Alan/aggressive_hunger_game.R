############################
# Preferential Hunger Game #
############################

set.seed(1234)

pref_hunger_games <- function(num_rounds, player_vec, skill_vec, health_limit, aggresive_factor=2, depression_factor = 0.5){
  # Initialisation
  num_players <- length(player_vec)
  death_list <- c()
  fight_vec <- rep(1, num_players)
  names(fight_vec) <- player_vec
  current_players <- rep(player_vec)
  health_vec <- rep(health_limit, num_players)
  names(health_vec) <- player_vec
  score_mat <- matrix(0, nrow=num_rounds, ncol=3)
  colnames(score_mat) <- c("Player1", "Player2", "Outcome")
  # Run the for loop
  for (i in 1:num_rounds){
    # Sample two player
    if (length(current_players) == 1){
      return(list(score = score_mat[1:(i-1),], death_ls = c(death_list, current_players)))
    }
    else{
      curr_P <- sample(player_vec, size = 2, replace = FALSE, prob = fight_vec)
      score_mat[i, 1:2] <- curr_P
      skill_1 <- skill_vec[curr_P[1]]
      skill_2 <- skill_vec[curr_P[2]]
      alpha <- skill_1 / (skill_1 + skill_2)
      outcome <- sample(c("W1","W2"), size = 1, prob = c(alpha, 1 - alpha))
      score_mat[i, 3] <- outcome
      if (outcome == "W1") {
        health_vec[curr_P[2]] = health_vec[curr_P[2]] - 1
        fight_vec[curr_P[1]] = fight_vec[curr_P[1]] * aggresive_factor
        fight_vec[curr_P[2]] = fight_vec[curr_P[2]] * depression_factor
        if (health_vec[curr_P[2]] == 0){
          current_players = current_players[-which(current_players == curr_P[2])]
          death_list <- c(death_list, curr_P[2])
          fight_vec[curr_P[2]] = 0
        }
      }
      else if (outcome == "W2"){
        health_vec[curr_P[1]] = health_vec[curr_P[1]] - 1
        fight_vec[curr_P[2]] = fight_vec[curr_P[2]] * aggresive_factor
        fight_vec[curr_P[1]] = fight_vec[curr_P[1]] * depression_factor
        if (health_vec[curr_P[1]] == 0){
          current_players = current_players[-which(current_players == curr_P[1])]
          death_list <- c(death_list, curr_P[1])
          fight_vec[curr_P[1]] = 0
        }
      }
    }
  }
  return(list(score = score_mat, death_ls = c(death_list, current_players)))
}

# Testing
# Generate players
num_players <- 50
players <- randomNames(num_players, sample.with.replacement = FALSE, which.names = "first",name.sep=" ")

# Generate skills
skills <- exp(rnorm(num_players, 0, 1))
names(skills) <- players

pref_hgame <- pref_hunger_games(500, players, skills, health_limit = 2)
score_mat <- as.data.frame(pref_hgame$score)
death_order <- pref_hgame$death_ls

hunger_4col <- codes_to_counts(score_mat, c("W1", "W2"))
bthunger <- btdata(hunger_4col, return_graph = TRUE)

degree(bthunger$graph, mode="out")

V(bthunger$graph)["Alejandro"]$color <- "red"

plot.igraph(bthunger$graph, 
            vertex.size=15, 
            edge.arrow.size=0.1, 
            #edge.width=3 * E(bthunger$graph)$weight, 
            shape="circle", 
            curved=TRUE
            #layout = layout_in_circle(bthunger$graph, order = hunger_death)
)

dev.off()
barplot(degree(bthunger$graph, mode="out")[death_order], xlab="Order of Death", ylab="Number of Deaths")

names(sort(skills, decreasing = TRUE))

