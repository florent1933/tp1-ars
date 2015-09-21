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
  cat("DegrÃ© de distribution", degree.distribution(g), "\n")
  plot(degree.distribution(g))
}

# question 3
comparator = function (n) {
  er_graph <- erdos.renyi.game(n, 2/100)
  ws_graph <- watts.strogatz.game(1, n, 4, 0.05)
  ba_graph <- barabasi.game(n)
  
  
  print("erdos.renyi.game")
  cat("Diamètre:", diameter(er_graph), " ")
  cat("Transitivité:", transitivity(er_graph), "\n")
  
  print("watts.strogatz.game")
  cat("Diamètre:", diameter(ws_graph), " ")
  cat("Transitivité:", transitivity(ws_graph), "\n")
  
  print("barabasi.game")
  cat("Diamètre:", diameter(ba_graph), " ")
  cat("Transitivité:", transitivity(ba_graph), "\n")
  
  #comparaison des diamètres
  barplot(c(diameter(er_graph), diameter(ws_graph), diameter(ba_graph)), names.arg = c("er_graph", "ws_graph", "ba_graph"))
  
  #comparaison en fonction du nombre de noeuds
  #diametres = diameterE(erdos.renyi.game(45, 2/100))
  diametresE <- as.vector(NULL)
  diametresW <- as.vector(NULL)
  diametresB <- as.vector(NULL)
  for (i in seq(50, n, by=5)) { 
    diametresE = c(diametresE, diameter(erdos.renyi.game(n, 2/100)))
    diametresW = c(diametresW, diameter(watts.strogatz.game(1, n, 4, 0.05)))
    diametresB = c(diametresB, diameter(barabasi.game(n)))
  }
  
  plot(diametresE, type = "l")
  plot(diametresW, type = "l")
  plot(diametresB, type = "l")
}


# question 4
couleur <- as.vector(c("aquamarine", "white", "chartreuse", "darkorchid", "darkorange", "darkgreen", "darkgray", "darkblue", "darkcyan", "cyan", "beige", "deeppink"))
community = function(g) {
  for(valeur in unique(V(g)$value)){
    V(g)[value == valeur]$color <- couleur[valeur] 
  }
  plot(g)
}



#question 5
ssGraphe <- function(g) {
  b <- betweenness(g)
  c <- closeness(g)
  deg <- degree(g)
  
  bIndex <- which.max(b)
  cIndex <- which.max(c)
  degIndex <- which.max(deg)
  
  ssg1 <- graph.neighborhood(g,1,bIndex)[[1]]
  ssg2 <- graph.neighborhood(g,1,cIndex)[[1]]
  ssg3 <- graph.neighborhood(g,1,degIndex)[[1]]
  
  plot(ssg1, main="Centralité de l'intermédiarité")
  plot(ssg2, main="Centralité de proximité")
  plot(ssg3, main="Degrés")
}

# main
# 1
topology(karate)
# 2
distribution(karate)
# 3
comparator(100)
# 4
community(dolphins)
community(karate)
community(polbooks)
# 5
ssGraphe(karate)
ssGraphe(dolphins)
ssGraphe(polbooks)
ssGraphe(football)


