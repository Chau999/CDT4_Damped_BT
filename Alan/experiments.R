#################
## Experiments ##
#################
set.seed(142)
#set.seed(141)
# Analyse the network structure of Aggresive hunger games

# Testing
# Generate players
num_players <- 30
players <- randomNames(num_players, sample.with.replacement = FALSE, which.names = "first",name.sep=" ")
#players <- c("Alan","Maud","Natalia","Valarie","James","Bobby","Deborah","Lorenzo","Lucy","Emanualle","Ana","William","Hector")

# Generate skills
skills <- exp(rnorm(num_players, 0, 1))
names(skills) <- players
hist(skills)
dev.off()

# Run the game
health_bar <- rep(1, num_players)
names(health_bar) <- players
pref_hgame1 <- pref_hunger_games(2000, players, skills, health_vec = health_bar, aggresive_factor = 1, depression_factor = 1)
pref_hgame2 <- pref_hunger_games(2000, players, skills, health_vec = health_bar, aggresive_factor = 4, depression_factor = 0.5)
pref_hgame3 <- pref_hunger_games(2000, players, skills, health_vec = ceiling(2 * skills), aggresive_factor = 4, depression_factor = 0.5)
#pref_hgame <- pref_hunger_games(1000, players, skills, health_vec = health_bar, aggresive_factor = 4,depression_factor = 0.5)
score_mat1 <- as.data.frame(pref_hgame1$score)
score_mat2 <- as.data.frame(pref_hgame2$score)
score_mat3 <- as.data.frame(pref_hgame3$score)

death_order <- pref_hgame$death_ls


hunger_4col1 <- codes_to_counts(score_mat1, c("W1", "W2"))
hunger_4col2 <- codes_to_counts(score_mat2, c("W1", "W2"))
hunger_4col3 <- codes_to_counts(score_mat3, c("W1", "W2"))

bthunger1 <- btdata(hunger_4col1, return_graph = TRUE)
bthunger2 <- btdata(hunger_4col2, return_graph = TRUE)
bthunger3 <- btdata(hunger_4col3, return_graph = TRUE)


pref_hgame1$survived

# The design of the network display
dev.off()
plotting_graph <- function(bthunger, pref_hgame){
  colfunc <- colorRampPalette(c("Darkred","red","pink"))
  V(bthunger$graph)$color <- "pink"
  V(bthunger$graph)[names(sort(degree(bthunger$graph, mode="out"), decreasing = TRUE))[1:length(degree(bthunger$graph, mode="out"))]]$color <- colfunc(length(degree(bthunger$graph, mode="out")))
  V(bthunger$graph)[pref_hgame$survived]$color <- "blue"
  V(bthunger$graph)[pref_hgame$survived]$frame.color <- "gold"
  V(bthunger$graph)$shape <- "circle"
  V(bthunger$graph)[pref_hgame$survived]$shape <- "circle"
  V(bthunger$graph)$size <- 7
  V(bthunger$graph)[pref_hgame$survived]$size <- 14
  #V(bthunger$graph)[names(sort(degree(bthunger$graph, mode="out"), decreasing = TRUE))]$color <- colfunc(num_players)
  
  
  edge.start <- ends(bthunger$graph, es=E(bthunger$graph), names=F)[,1]
  edge.col <- V(bthunger$graph)$color[edge.start]
  E(bthunger$graph)$width <- 0.1 + E(bthunger$graph)$weight
  par(mar=c(0,0,0,0) + 0.1)
  plot.igraph(bthunger$graph, 
              vertex.label = NA,
              edge.arrow.size=0.5, 
              edge.width= E(bthunger$graph)$weight,
              edge.color=edge.col,
              curved=TRUE
              #layout = layout_in_circle(bthunger$graph, order = death_order)
  )
}





par(mfrow=c(1,3))

plotting_graph(bthunger = bthunger1, pref_hgame = pref_hgame1)
plotting_graph(bthunger = bthunger2, pref_hgame = pref_hgame2)
plotting_graph(bthunger = bthunger3, pref_hgame = pref_hgame3)



length(degree(bthunger1$graph, mode="out"))


#-------------------------------------------------------------------------------
# Analyse this dataset
plot_degree_dis <- function(preferential_game, main_title){
  dg_dist <- degree_distribution(preferential_game$graph, mode="total")[-1]
  plot(dg_dist, xlab="Degree",ylab="Frequency")
  title(main_title)
  
  #plot(log(dg_dist), log(1:39))
  #fit <- lm(log(dg_dist)[which(dg_dist !=0)] ~ log(which(dg_dist !=0)))
  #fit$coefficients
  
  #plot(dg_dist, type="l", xlab="Degree",ylab="Degree Frequency")
  #title("Power-law Behavior (Hugar Games)")
  #lines(c(1:37), c(1:37)^fit$coefficients[2]-0.07,col="red")
}
dev.off()
par(mfrow=c(1,3))
plot_degree_dis(bthunger1, main_title="Game 1")
plot_degree_dis(bthunger2, main_title="Game 2")
plot_degree_dis(bthunger3, main_title="Game_3")

dev.off()


#########################
## Fitting the BT Model #
#########################
#' you run the padding for 3 different games while changing the alpha parameter
#' 
#' 

compare_coef <- function(true, pred){
  name <- names(true)
  # Normalise so it sums to 1
  a <- (exp(pred)/sum(exp(pred)))[name]
  b <- (true/sum(true))[name]
  
  # Return RMSE
  sqrt(mean((a - b)^2))
}

padding_rmse_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))
damping_rmse_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))
bayesian_rmse_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))


# Obtain RMSE and store it to a list
obtain_rmse <- function(rmse_list, fitting_method, hyper_param_range=c(1:30), bayesian=FALSE){
  
  if (bayesian){
    for (i in 1: length(hyper_param_range)){
      rmse <- rep(0, 3)
      rmse[1] <- compare_coef(skills, fitting_method(bthunger1$wins, 0.1, hyper_param_range[i]))
      rmse[2] <- compare_coef(skills, fitting_method(bthunger2$wins, 0.1, hyper_param_range[i]))
      rmse[3] <- compare_coef(skills, fitting_method(bthunger3$wins, 0.1, hyper_param_range[i]))
      
      game_vec <- c("g1",'g2',"g3")
      for (game_ind in 1:3){
        rmse_list[[game_vec[game_ind]]][i] =  rmse[game_ind]
      }
    }
    return(rmse_list)
  }
  else {
    for (i in 1: length(hyper_param_range)){
      rmse <- rep(0, 3)
      rmse[1] <- compare_coef(skills, fitting_method(bthunger1$wins, hyper_param_range[i]))
      rmse[2] <- compare_coef(skills, fitting_method(bthunger2$wins, hyper_param_range[i]))
      rmse[3] <- compare_coef(skills, fitting_method(bthunger3$wins, hyper_param_range[i]))
      
      game_vec <- c("g1",'g2',"g3")
      for (game_ind in 1:3){
        rmse_list[[game_vec[game_ind]]][i] =  rmse[game_ind]
      }
    }
    return(rmse_list)
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
  
  # Kendall distance
  abs((Kendall(dummy_true, dummy_pred)$S[[1]])/length(true))
}

# Obtain kendall list
obtain_kendall_list <- function(kendall_list, fitting_method, hyper_param_range=c(1:30), bayesian=FALSE){
  
  if (bayesian){
    for (i in 1: length(hyper_param_range)){
      ken <- rep(0, 3)
      ken[1] <- obtain_kendall(skills, fitting_method(bthunger1$wins, 0.1, hyper_param_range[i]))
      ken[2] <- obtain_kendall(skills, fitting_method(bthunger2$wins, 0.1, hyper_param_range[i]))
      ken[3] <- obtain_kendall(skills, fitting_method(bthunger3$wins, 0.1, hyper_param_range[i]))
      
      game_vec <- c("g1",'g2',"g3")
      for (game_ind in 1:3){
        kendall_list[[game_vec[game_ind]]][i] =  ken[game_ind]
      }
    }
    return(kendall_list)
  }
  else {
    for (i in 1: length(hyper_param_range)){
      ken <- rep(0, 3)
      ken[1] <- obtain_kendall(skills, fitting_method(bthunger1$wins, hyper_param_range[i]))
      ken[2] <- obtain_kendall(skills, fitting_method(bthunger2$wins, hyper_param_range[i]))
      ken[3] <- obtain_kendall(skills, fitting_method(bthunger3$wins, hyper_param_range[i]))
      
      game_vec <- c("g1",'g2',"g3")
      for (game_ind in 1:3){
        kendall_list[[game_vec[game_ind]]][i] =  ken[game_ind]
      }
    }
    return(kendall_list)
  }
}

padding_kendall_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))
damping_kendall_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))
bayesian_kendall_list <- list(g1=rep(0, 30), g2=rep(0, 30), g3=rep(0, 30))

# Obtain Kendall List for all 3 methods!
padding_kendall_list <- obtain_kendall_list(padding_rmse_list, padding_fit)
damping_kendall_list <- obtain_kendall_list(damping_rmse_list, damping_fit, seq(from=0.1,to=1,length.out = 30))
bayesian_kendall_list <- obtain_kendall_list(bayesian_rmse_list, damping_fit, seq(from=1.1, to=5, length.out=30), bayesian = TRUE)

# Obtain RMSE list for all 3 methods!  
padding_rmse_list <- obtain_rmse(padding_rmse_list, padding_fit)
damping_rmse_list <- obtain_rmse(damping_rmse_list, damping_fit, seq(from=0.1,to=1,length.out = 30))
bayesian_rmse_list <- obtain_rmse(bayesian_rmse_list, damping_fit, seq(from=1.1, to=5, length.out=30), bayesian = TRUE)

# Plot Resulst
plot_rmse <- function(rmse_list, x_axis, xlabel, main_title, bayesian=FALSE){

  plot(x_axis, rmse_list[["g1"]], type="l",lwd=2, ylim=c(0, 0.1),lty=1, ylab="RMSE", xlab=xlabel)
  lines(x = x_axis, y = rmse_list[["g3"]], ylim=c(0,0.1),type="l",lwd=2, col="red", lty=3)
  
  lines(x = x_axis, y = rmse_list[["g2"]], type="l",lwd=2, col="blue", lty=2)
  
  legend("bottomright", legend=c("Game 1", "Game 2", "Game 3"), col = c("black","blue","red"), lty=1:3)
  title(main_title)

}

plot_kendall <- function(kendall_list, x_axis, xlabel, main_title, bayesian=FALSE){
  
  plot(x_axis, kendall_list[["g1"]], type="l",ylim=c(0, 6),lwd=2,lty=1, ylab="Kendall.Dist", xlab=xlabel)
  lines(x = x_axis, y = kendall_list[["g3"]], ylim=c(0,0.1),type="l",lwd=2, col="red", lty=3)
  
  lines(x = x_axis, y = kendall_list[["g2"]], type="l",lwd=2, col="blue", lty=2)
  
  legend("topright", legend=c("Game 1", "Game 2", "Game 3"), col = c("black","blue","red"), lty=1:3)
  title(main_title)
  
}



par(mfrow=c(1,3))
plot_rmse(x_axis = c(1:30),rmse_list = padding_rmse_list, "Padding_level", main_title = "Padding")
plot_rmse(x_axis = seq(from=0.1,to=1,length.out = 30), rmse_list = damping_rmse_list, "Damping_level", main_title = "Damping")
plot_rmse(x_axis = seq(from=1.1, to=5, length.out=30), rmse_list = bayesian_rmse_list, "Gamma Prior Input",main_title="Bayesian Method (2012)",bayesian=TRUE)

par(mfrow=c(1,3))
plot_kendall(x_axis = c(1:30),kendall_list =  padding_kendall_list, "Padding_level", main_title = "Padding")
plot_kendall(x_axis = seq(from=0.1,to=1,length.out = 30), kendall_list = damping_kendall_list, "Damping_level", main_title = "Damping")
plot_kendall(x_axis = seq(from=1.1, to=5, length.out=30), kendall_list = bayesian_kendall_list, "Gamma Prior Input",main_title="Bayesian Method (2012)",bayesian=TRUE)




