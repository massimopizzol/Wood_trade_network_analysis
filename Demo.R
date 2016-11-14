#You need the igraph and plyr packages for this demo
library("igraph")
library("plyr")

#get data in
#import database and prepare it
db <- read.csv("Forestry_Trade_Flows_E_All_Data_reduced.csv", header = TRUE, sep = ";")
keep <- c(1:10, seq(12, (dim(db)[2]-1), 2)) #remove duplicate columns (labelled with "F")
db.1 <- db[, keep]; rm(keep)
db.1[is.na(db.1)] <- 0 #substitute NA with zeros
head(db.1)
dim(db.1)
products <- levels(db.1$Item)     #exact names of the products (only "Sawnwood (C)" because I uploaded a reduced version of the database)
countries <- levels(db.1$Reporter.Countries) #exact names of the countries
print(paste (length(countries), "countries"))
levels(db.1$Element) #exact names of trade flows




#function so subset the database
selection <- function(trade, product, year) {
  
  db.sel <- subset.data.frame(db.1, Element == trade & Item == product,
                            select = c(Reporter.Countries, Partner.Countries,
                                        #Item, Element, Unit, 
                                        get(paste("Y",year,sep = ""))),
                                        drop = FALSE)
  db.sel
}


#select a year and product for the analysis
year = 2013
product = "Sawnwood (C)"
sel <- selection("Export Quantity", product, year)
#sel <- data.frame(sel[,2], sel[,1], sel[,3]) #remove this line if using export values!
names(sel) <- c("From", "To", "weight") #add meaningful heading to the dataframe
head(sel) 
sel[50:60,]
dim(sel)



#create graph from dataframe using the igraph package
g. = graph_from_data_frame(sel, directed = TRUE)
g. <- delete_vertices(g., c("Others (adjustment)",
                            "Unspecified", 
                            "World", 
                            "Total FAO")) #remove these vertices
summary(g.)
adj <- as_adjacency_matrix(g.,attr='weight', sparse=FALSE, names = TRUE) #adjacency matrix
g.bis = graph.adjacency(adj, mode = "directed", weighted = TRUE, diag = FALSE)  #this was maybe redundant but reduces the size of the graph
                                                                                #(removes the useless connections with value zero)
paste((length(E(g.bis)$weight)/ length(E(g.)$weight))*100,
       " % of total connections remaning (zero values removed)")

paste(sum(E(g.)$weight), "trade before removing zeros", 
      sum(E(g.bis)$weight), "trade after removing zeros")

#write.csv(adj, file = paste("adiacency_",product, "_",year,".csv", sep = ""))
#this is if you want to export the graph as adjacency matrix...



#filter graph based on size of trades
cutoff <- mean(sel[,3])/4; cutoff #cutoff defined here

g <- delete.edges(g.bis, which(E(g.bis)$weight < cutoff))
g <- delete.vertices(g, which(degree(g) < 1)) 
g1 <- decompose.graph(g, max.comps = 1, min.vertices = 3)
g <- g1[[1]]

paste(length(E(g.bis)), "edges before cutoff;", length(E(g)), "edges after cutoff")
paste(length(V(g.bis)), "vertices before cutoff;", length(V(g)), "vertices after cutoff")
paste((sum(E(g)$weight)/sum(E(g.)$weight))*100, "% of total trade remaining after cutoff")




#plot the graph
plot(g, vertex.label = V(g)$names,
     vertex.label.cex = 0.4,
     vertex.label.family = "sans",
     vertex.label.color = "black",
     vertex.size = 8,
     edge.color = "lightgrey", 
     edge.width=((E(g)$weight)^0.5)/250, 
     edge.arrow.size = 0.2,
     mark.groups = NULL,
     rescale = TRUE)



#find communities (clusters) and calculate various indicators (look at the differences)
tt <- cluster_spinglass(g, spins=20, gamma = 1)
communities(tt) #look at the communities
sort(sizes(tt)) #look at their size


transitivity(g, type="global")
transitivity(g, type="global", weights = NULL)
transitivity(g, type="global", weights = c(E(g)$weight))
transitivity(g, type="barrat")
transitivity(g, type="weighted", weights = NULL)
transitivity(g, type="barrat", weights = c(E(g)$weight))


modularity(g, membership(tt))
modularity(g, membership(tt), weights = NULL)
modularity(g, membership(tt), weights = E(g)$weight)


#loop cluster and find most frequent one
clu <- list() #list of clusters
los <- list() #list of sizes
simulation = 20 #you can increase this number but the code will take more time

for (i in 1:simulation){
  tt <- cluster_spinglass(g, spins=20, gamma = 1)
  
  clu[[i]] <- tt
  los[[i]] <- data.frame(t(as.vector(sort(sizes(tt)))))
  print(i)
  
}

out <- do.call(rbind.fill,los) #combine communities (vectors of different length) in matrix
out. <- count(out, vars = names(out)) #counting how many times each community occurs
out. #Look at the list fo results after the iterative simulation.

best <- out.[order(out.$freq, decreasing = TRUE), ][1,-max(dim(out.)[2])]
best = best[!is.na(best)] ; best        #most frequent cluster
frequency <- max(out.$freq) ; frequency #how many times the cluster was produced
frequency/simulation                    #how many times on total runs

xxx <- sapply(los, function(x)all(x==best)) #match most frequent cluster with the list
tt <- clu[xxx][1][[1]] #finally take this out as the conclusive clusterization
aa <- communities(tt)
aa #the final communities


###


#Global stats
cat(paste("\n Modularity (Q) =", modularity(g, membership(tt)), "\n", #Good Q values are high (e.g. higher than 0.3 and close to 0.7)
      "Transitivity =", transitivity(g, type="global"), "\n",  #Global Transitivity values close to 1 indicates no clustering (one big group)
      "Density = ", graph.density(g)) )                         #Density (max interconnectendesss density = 1)




#plot network with clusters highlighted
plot(tt,g, vertex.label = V(g)$names, #use V(g) for having numbers only
     vertex.frame.color = "white",
     vertex.label.cex = 0.4,
     vertex.label.family = "Arial",
     vertex.label.color = "black",
     vertex.size = 7,
     edge.color = "lightgrey", 
     edge.width=((E(g)$weight)^0.5)/250, 
     edge.arrow.size = 0.1,
     mark.groups = NULL,
     rescale = TRUE) 




#plot all single clusters
for (i in 1:length(communities(tt))){
  
  g2 <- induced_subgraph(g, communities(tt)[[i]])
  print(summary(g2))               
  
  
  plot(g2, vertex.label = V(g2)$name,
       vertex.label.cex = 0.8,
       vertex.label.family = "sans",
       vertex.size = 8,
       vertex.label.color = "black",
       vertex.label.dist = 0.6,
       vertex.color = i,
       vertex.frame.color = "black",
       edge.color = "lightgray", 
       edge.width=((E(g2)$weight)^0.5)/150, 
       edge.arrow.size = 0.5,
       mark.groups = NULL,
       rescale = TRUE)
}


 


#plot one single vertex

One_country <- function(country, limit){
  
  g <- delete.edges(g., which(E(g.)$weight < limit))
  
  a <- E(g)[inc(which(V(g)$name == country))] #found here:http://stackoverflow.com/questions/20209303/how-to-get-vertex-ids-back-from-graph
  g123 <- subgraph.edges(g, a)
  plot(g123, vertex.label = V(g123)$name,
       vertex.label.cex = 0.8,
       vertex.label.family = "sans",
       vertex.label.color = "black",
       vertex.label.dist = 0.5,
       vertex.size = 8,
       vertex.color = "lightblue",
       vertex.frame.color = "black",
       edge.color = "lightgrey", 
       edge.width=((E(g123)$weight)^0.5)/150, 
       edge.arrow.size = ((E(g123)$weight)^0.5)/500,
       mark.groups = NULL,
       rescale = TRUE,
       edge.curved = NULL)
  
}

One_country("Denmark", 1000)
#One_country("Canada", 10000)


#end here
