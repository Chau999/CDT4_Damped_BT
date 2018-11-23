############################
# Preferential Hunger Game #
############################
library("Kendall")
set.seed(134)

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
num_players <- 13
#players <- randomNames(num_players, sample.with.replacement = FALSE, which.names = "first",name.sep=" ")
players <- c("Alan","Maud","Natalia","Valarie","James","Bobby","Deborah","Lorenzo","Lucy","Emanualle","Ana","William","Hector")

# Generate skills
skills <- exp(rnorm(num_players, 0, 1))
names(skills) <- players
hist(skills)

pref_hgame <- pref_hunger_games(500, players, skills, health_limit = 1,aggresive_factor = 1,depression_factor = 1)
score_mat <- as.data.frame(pref_hgame$score)
death_order <- pref_hgame$death_ls

hunger_4col <- codes_to_counts(score_mat, c("W1", "W2"))
bthunger <- btdata(hunger_4col, return_graph = TRUE)

# The design of the network display
dev.off()
colfunc <- colorRampPalette(c("Red","orange"))
V(bthunger$graph)$color <- "pink"
V(bthunger$graph)[names(sort(degree(bthunger$graph, mode="out"), decreasing = TRUE))[1:3]]$color <- colfunc(3)

#V(bthunger$graph)[names(sort(degree(bthunger$graph, mode="out"), decreasing = TRUE))]$color <- colfunc(num_players)

edge.start <- ends(bthunger$graph, es=E(bthunger$graph), names=F)[,1]
edge.col <- V(bthunger$graph)$color[edge.start]
E(bthunger$graph)$width <- 0.1 + E(bthunger$graph)$weight
par(mar=c(0,0,0,0) + 0.1)
plot.igraph(bthunger$graph, 
            vertex.size=20, 
            edge.arrow.size=0.2, 
            edge.width=3 * E(bthunger$graph)$weight, 
            shape="circle",
            edge.color=edge.col,
            curved=TRUE
            #layout = layout_in_circle(bthunger$graph, order = hunger_death)
)
title("Hunger Games Illustration")

# tkplot(bthunger$graph)


# Barplot to display the number of deaths
dev.off()
end_point = 0.5 + length(death_order) + length(death_order)-1
par(mar = c(5, 4, 2, 2) + 0.2)
barplot(degree(bthunger$graph, mode="out")[death_order], xlab="Order to Death",ylab="Number of Deaths", space=1, xaxt="n", mgp=c(3,1,0))
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(death_order), cex=1)


names(sort(skills, decreasing = TRUE))

########################################
## Analysis on Aggressive hunger game ##
########################################

padd_coef <- padding_fit(bthunger$wins)
ranks <- cbind(names(sort(skills, decreasing=TRUE)),names(sort(padd_coef, decreasing = TRUE)))
colnames(ranks) <- c("True","Padding")
ranks


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
ranks <- cbind(ranks, sample(players))
colnames(ranks) <- c("TRUE", "Padding", "Damping", "Bayesian","Control")

ranks

#----------------------------------------------------------------------#
dummy_vec <- c(1:num_players)
names(dummy_vec) <- players

ken_vec <- rep(0, 4)
names(ken_vec) <- c("Padding", "Damping", "Bayesian","Control")

for (j in 2:5){
  ken_vec[j-1] <- abs(Kendall(dummy_vec[ranks[,1]], dummy_vec[ranks[,j]])$S[[1]])/num_players
}

?Kendall

ken_vec

dummy_vec[ranks[,1]]
dummy_vec[ranks[,2]]

kend <- Kendall(dummy_vec[ranks[,1]], dummy_vec[ranks[,2]])

kend$S[[1]]


dummy_vec[sample(ranks[, 1])]
kend_control <- Kendall(dummy_vec[ranks[,1]], dummy_vec[sample(ranks[, 1])])

kend_control$S
