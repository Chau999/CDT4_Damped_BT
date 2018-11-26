#################
## Experiments ##
#################
set.seed(140)

# Analyse the network structure of Aggresive hunger games

# Testing
# Generate players
num_players <- 35
players <- randomNames(num_players, sample.with.replacement = FALSE, which.names = "first",name.sep=" ")
#players <- c("Alan","Maud","Natalia","Valarie","James","Bobby","Deborah","Lorenzo","Lucy","Emanualle","Ana","William","Hector")

# Generate skills
skills <- exp(rnorm(num_players, 0, 1))
names(skills) <- players
hist(skills)
dev.off()

# Run the game
pref_hgame <- pref_hunger_games(1000, players, skills, health_vec = ceiling(2 * skills),aggresive_factor = 2,depression_factor = 0.5)
score_mat <- as.data.frame(pref_hgame$score)
death_order <- pref_hgame$death_ls


hunger_4col <- codes_to_counts(score_mat, c("W1", "W2"))
bthunger <- btdata(hunger_4col, return_graph = TRUE)

# The design of the network display
dev.off()
colfunc <- colorRampPalette(c("Darkred","red","pink"))
V(bthunger$graph)$color <- "pink"
V(bthunger$graph)[names(sort(degree(bthunger$graph, mode="out"), decreasing = TRUE))[1:num_players]]$color <- colfunc(num_players)
V(bthunger$graph)[pref_hgame$survived]$frame.color <- "gold"
V(bthunger$graph)$shape <- "circle"
V(bthunger$graph)[pref_hgame$survived]$shape <- "rectangle"
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
            edge.width=1 * E(bthunger$graph)$weight,
            edge.color=edge.col,
            curved=TRUE
#            layout = layout_in_circle(bthunger$graph, order = death_order)
)
#-------------------------------------------------------------------------------
# Analyse this dataset
preferential_game = rep(bthunger) # set life to 10 aggresive to 3 and depression to 0.5

dev.off()
dg_dist <- degree_distribution(preferential_game$graph, mode="total")[-1]
plot(dg_dist, type="l", xlab="Degree",ylab="Frequency")
title("Power-law Behavior on the Degreee Distribution")

which(dg_dist !=0)

plot(log(dg_dist), log(1:39))
fit <- lm(log(dg_dist)[which(dg_dist !=0)] ~ log(which(dg_dist !=0)))
fit$coefficients

plot(dg_dist, type="l", xlab="Degree",ylab="Degree Frequency")
title("Power-law Behavior (Hugar Games)")
lines(c(1:37), c(1:37)^fit$coefficients[2]-0.07,col="red")

#########################
## Fitting the BT Model #
#########################

# Padding
pad_coef <- padding_fit(bthunger$wins)
name <- names(skills)
a <- (exp(pad_coef)/sum(exp(pad_coef)))[name]
b <- (sort(skills, decreasing = TRUE)/sum(skills))[name]

sqrt(mean((a - b)^2))


# Damping
damp_coef <- damping_fit(bthunger$wins, damp_lvl = 0.15)
damp_coef
sum(damp_coef)

a2 <- (exp(damp_coef)/sum(exp(damp_coef)))[name]
sqrt(mean((a2 - b)^2))


