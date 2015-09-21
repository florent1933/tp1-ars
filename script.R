# Load data
library(igraph)

setwd('~/Documents/DS/tp1-ars/')
karate <- read.graph("data/karate.gml.txt", format="gml")
football <- read.graph("data/football.gml.txt", format="gml")
dolphins <- read.graph("data/dolphins.gml.txt", format="gml")
polbooks <- read.graph("data/polbooks.gml.txt", format="gml")

topology <- function(g) {
  cat("nombre de noeuds : ",  vcount(g), "\n")
  cat("nombre de noeuds : ", graph.density(g), "\n")
  cat("nombre de noeuds : ", degree(g), "\n")
  cat("nombre de noeuds : ", degree.distribution(g), "\n")
  cat("nombre de noeuds : ", transitivity(g), "\n")
  cat("nombre de noeuds : ", average.path.length(g), "\n")
  cat("nombre de noeuds : ", shortest.paths(g), "\n")
  cat("nombre de noeuds : ", betweenness(g), "\n")
  cat("nombre de noeuds : ", closeness(g), "\n")
  cat("nombre de noeuds : ", is.connected(g), "\n")
  cat("nombre de noeuds : ", clusters(g), "\n")
  #neighbors(g,x)
}

topology(karate)

