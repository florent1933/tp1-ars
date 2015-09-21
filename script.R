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
  cat("la moyenne des plus courts chemins : ", average.path.length(g), "\n")
  cat("matrice des plus courts chemins : ", shortest.paths(g), "\n")
  cat("centralité d'intermédialité : ", betweenness(g), "\n")
  cat("centralité de proximité : ", closeness(g), "\n")
  cat("graphe connexe : ", is.connected(g), "\n")
  
  print("graphe connexe : ")
  print(clusters(g))
  #neighbors(g,x)
}

# question 2
distribution = function (g) {
  cat("Degré de distribution", degree.distribution(g), "\n")
  plot(degree.distribution(g))
}

# question 3
comparator = function (n) {
  er_graph <- erdos.renyi.game(n, 2/100)
  ws_graph <- watts.strogatz.game(1, n, 4, 0.05)
  ba_graph <- barabasi.game(n)
  
  print("erdos.renyi.game")
  cat("Diamètre", diameter(er_graph))
  cat("Transitivité", transitivity(er_graph), "\n")
  
  print("watts.strogatz.game")
  cat("Diamètre", diameter(ws_graph))
  cat("Transitivité", transitivity(ws_graph), "\n")
  
  print("barabasi.game")
  cat("Diamètre", diameter(ba_graph))
  cat("Transitivité", transitivity(ba_graph), "\n")

}


# main
topology(karate)
distribution(karate)

comparator(100)


