# Load data
library(igraph)

setwd('~/Documents/DS/tp1-ars/')
karate <- read.graph("data/karate.gml.txt", format="gml")
football <- read.graph("data/football.gml.txt", format="gml")
dolphins <- read.graph("data/dolphins.gml.txt", format="gml")
polbooks <- read.graph("data/polbooks.gml.txt", format="gml")

# Question 1
topology <- function(g) {
  cat("nombre de noeuds : ",  vcount(g), "\n")
  cat("densité du graphe : ", graph.density(g), "\n")
  cat("nombre de noeuds : ", degree(g), "\n")
  cat("degrés de chaque noeud : ", degree.distribution(g), "\n")
  cat("distribution de degrés : ", transitivity(g), "\n")
  cat("la moyenne des plus courts chemins : ", average.path.length(g), "\n")
  cat("matrice des plus courts chemins : ", shortest.paths(g), "\n")
  cat("centralité d'intermédialité : ", betweenness(g), "\n")
  cat("centralité de proximité : ", closeness(g), "\n")
  cat("graphe connexe : ", is.connected(g), "\n")
  cat("liste des composantes connexes: ", clusters(g), "\n")
  #neighbors(g,x)
}

# question 2
distribution = function (g) {
  cat("Degré de distribution", degree.distribution(g), "\n")
}


# main
topology(karate)
distribution(karate)


