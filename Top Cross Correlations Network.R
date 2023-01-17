###########################################################################
###########################################################################
###                                                                     ###
###              TOP CROSS CORRELATIONS NETWORK                         ###
###                                                                     ###
###########################################################################
###########################################################################


# ::::::::::::::::::::::::::::::::::::::::::::::::
##################################################
## Project: Networks Course
## Script purpose: Data Analysis
## Date: Sept/27/2022
## Author: Antonieta Martínez-Guerrero
## Copyright (c) Antonieta Martínez-Guerrero, 2022
## Email: antonietamgdata@gmail.com
##################################################
# ::::::::::::::::::::::::::::::::::::::::::::::::


library(corrplot)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)  
library(corrr)
library(igraph)
library(lares)
library(qgraph)


data <- your_data # dataframe with variables as columns 

# Ordena y pone nombres a las filas
rn <- data[,1] # toma la primera columna con los nombres de las filas
dats <- data.frame(data[,-1]) # elimina la primera columna con los nombres de las filas
rownames(dats) <- rn[,1] # cambia los nombres de las filas

# omite NAs (es necesario quitar variables con NAs)
dats2 <- na.omit(dats)

# Genera ruido uniformemente distribuido
set.seed(365)
nCols <- ncol(dats2)
nRows <- nrow(dats2)
ruido <- runif(nRows*nCols, -0.00001, 0.00001)
#plot(ruido, c(1:5280))
ruido <- matrix(ruido, nrow = nRows, ncol = nCols)
dats2 <- dats2 + ruido

######### TOP CROSS CORRELATIONS VALUES ----------------------------------

Cross <- corr_cross(dats2, # name of dataset
                    max_pvalue = 0.05, # display only significant correlations (at 5% level)
                    top = 20, # display top 10 couples of variables (by correlation coefficient)
                    plot = FALSE,
                    method = "spearman"
)

Cross <- as.data.frame(Cross)


names_cross <- unique(c(Cross$key, Cross$mix))
ndf <- dats2[,names_cross]


# Crea matriz de correlación

mm <- cor(ndf, method = "spearman") #metodo pearson (parametric) o spearman (non-parametric)
res1 <- cor.mtest(ndf, conf.level = .95, method="spearman")
signif <- as.matrix(res1[[1]])
sig_m <- mm
sig_m[signif >= 0.05] <- 0

mat <- sig_m #matriz de correlaciones significativas

boxplot(mat)

# Keep only high correlations
mat[mat<0.5 & mat > 0] <- 0
mat[mat < 0 & mat > (-0.5)] <- 0

# Make an Igraph object from this matrix:
network <- graph.adjacency(mat, mode = "undirected", weighted = TRUE, diag = FALSE)

E(network)$cor <- E(network)$weight

E(network)$cor <- ifelse(E(network)$weight==1, 
                         E(network)$weight - runif(length(E(network)$weight), 0.00001, 0.0001),
                         E(network)$weight + 0)
E(network)$cor <- ifelse(E(network)$cor==-1, 
                         E(network)$cor + runif(length(E(network)$cor), 0.00001, 0.0001),
                         E(network)$cor + 0)

E(network)$color <- ifelse(E(network)$weight < 0, "#27408B","#CD0000")
E(network)$width <- 3*atanh(abs(E(network)$cor))
V(network)$size <- 1.8*abs(rowSums(mat))


e <- get.edgelist(network,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(network))

png("name_net.png", width = 5000, height = 4000)
par(bg="#FFFFFF", mar=c(0,0,0,0))
set.seed(4)
plot(network,
     layout=l,
     #vertex.size=12,
     vertex.color="#C1CDCD", 
     vertex.label.cex=4.5,
     vertex.label.color="black",
     vertex.frame.color="transparent",
     edge.curved=.1,
     alpha=0.5
)

dev.off()