
library("igraph")
library("plyr")


#Import database and prepare it
db <- read.csv("Forestry_Trade_Flows_E_All_Data_reduced.csv", header = TRUE, sep = ";") ;dim(db)
keep <- c(1:10, seq(12, (dim(db)[2]-1), 2)) #remove duplicate columns (labelled with "F")
db <- db[, keep]; rm(keep); dim(db)
db <- subset(db, Reporter.Countries != "Others (adjustment)" & Reporter.Countries !=  "Unspecified" & Reporter.Countries != "World" & Reporter.Countries !="Total FAO") ; dim(db)
db <- subset(db, Partner.Countries != "Others (adjustment)" & Partner.Countries !=  "Unspecified" & Partner.Countries != "World" & Partner.Countries !="Total FAO") ; dim(db)
db[is.na(db)] <- 0 #substitute NA with zeros

db$Reporter.Countries <- revalue(db$Reporter.Countries, #Change long names
                                 c("Bolivia (Plurinational State of)" = "Bolivia",
                                   "Central African Republic" =  "Central African Rep", 
                                   "Democratic People's Republic of Korea" = "North Korea",
                                   "Democratic Republic of the Congo" = "Congo",
                                   "Iran (Islamic Republic of)" = "Iran", 
                                   "Lao People's Democratic Republic" = "Laos", 
                                   "Netherlands Antilles" = "Neth Antilles",
                                   "Saint Vincent and the Grenadines" = "S Vincent Grenadines",
                                   "Venezuela (Bolivarian Republic of)" = "Venezuela",
                                   "United Republic of Tanzania" = "Tanzania"))

db$Partner.Countries <- revalue(db$Partner.Countries, 
                                 c("Bolivia (Plurinational State of)" = "Bolivia",
                                   "Central African Republic" =  "Central African Rep", 
                                   "Democratic People's Republic of Korea" = "North Korea",
                                   "Democratic Republic of the Congo" = "Congo",
                                   "Iran (Islamic Republic of)" = "Iran", 
                                   "Lao People's Democratic Republic" = "Laos", 
                                   "Netherlands Antilles" = "Neth Antilles",
                                   "Saint Vincent and the Grenadines" = "S Vincent Grenadines",
                                   "Venezuela (Bolivarian Republic of)" = "Venezuela",
                                   "United Republic of Tanzania" = "Tanzania"))

products <- levels(db$Item)     #exact names of the products
countries <- levels(db$Reporter.Countries) #exact names of the countries
dim(db)



#function so subset the database
selection <- function(trade, product, year) {
  
  db.sel <- subset.data.frame(db, Element == trade & Item == product,
                            select = c(Reporter.Countries, Partner.Countries,
                                        #Item, Element, Unit, 
                                        get(paste("Y",year,sep = ""))),
                                        drop = FALSE)
  
  db.sel
}




#function to create graph from dataframe
graphing <- function(sel, cutoff){
  
  g. = graph_from_data_frame(sel, directed = TRUE)
  
  #filter network based on desired size of trade
  cutoff_value <- exp(mean(log((E(g.)$weight))))*cutoff
  #print(plot(sort(log((E(g.)$weight)))))
  g <- delete.edges(g., which(E(g.)$weight < cutoff_value))
    
  g <- delete.vertices(g, which(degree(g) < 1)) 
  g1 <- decompose.graph(g, max.comps = 1, min.vertices = 3)
  g <- g1[[1]]
  percent <- sum(E(g)$weight)/sum(E(g.)$weight)*100
  rm(g1, g.)
  
  output <- list("Graph" = g, "Percentage" = percent, "Cutoff_value" = cutoff_value)
  }



#Function to loop cluster
clusterloop <- function(g, iterations){
  
  clu <- list() #list of clusters
  los <- list() #list of sizes
  
  for (i in 1:iterations){
  
    print(i)
    tt <- cluster_spinglass(g, spins=20, gamma = 1)
    clu[[i]] <- tt #add each set of communities to a big list
    los[[i]] <- data.frame(t(as.vector(sort(sizes(tt)))))
    
  }
  
  output <- list("Clusters" = clu, "Sizes" = los)

}



#Huge loop, calculate graph for each year/product/cutoff combination, 
#and respective communities with 100 simulations
#WARNING: takes 37 hours on my pc...

products
products2 <- products[-which(products == "Forest Products" |  products == "Paper+-Board Ex Newsprnt")]

ptm <- proc.time()

glo_g_list <- list() #list of graphs
glo_percent_list <- list() #list of percentages
glo_clu_list <- list() #list of list of communities
glo_los_list <- list() #list of community sizes

iterations = 100

cutoffs <- list("Y" = 1, "N" = 0)

for (y in 1998:2013){ 
  
  for (p in products2){
    
    sel <- selection("Export Quantity", p, y)
    #sel <- data.frame(sel[,2], sel[,1], sel[,3]) #remove this line if using export values!
    names(sel) <- c("From", "To", "weight")
    sel <- sel[sel$weight != 0,]
    
    
    for (cutoff in cutoffs){
      
      cutoff_name <- names(cutoffs[match(cutoff, cutoffs)])
      print(paste(y, p, cutoff_name, sep = " "))
      
      graphing_output <- graphing(sel, cutoff) #create the graph
      g <- graphing_output$Graph
      percent <- graphing_output$Percentage
      cutoff_value <- graphing_output$Cutoff_value
      
      clusterloop_output <- clusterloop(g, iterations) #loop the clustering (spinglass)
      clu <- clusterloop_output$Clusters
      los <- clusterloop_output$Sizes
      
      identifier = paste(p,y,cutoff_name,cutoff_value, sep = "_") #attach every output to a list
      glo_g_list[[identifier]] <- g
      glo_percent_list[[identifier]] <- percent
      glo_clu_list[[identifier]] <- clu
      glo_los_list[[identifier]] <- los
      
      rm(identifier, los, clu, clusterloop_output, g, percent, graphing_output, cutoff_name, cutoff_value)
      
    }
    
    rm(sel)
    
  }
  
}

rm(p, y, cutoff)
proc.time() - ptm #this is just for timing it
rm(ptm)





##Exporting results

#loop over all clusters and export for simulation each community (30min)
glo_comm_list <- list()

for (i in 1:length(glo_clu_list)){                  #loop across clusters
  
  for (iteration in 1:length(glo_clu_list[[i]])){   #loop across the 100 simulations
    
    aa <- communities(glo_clu_list[[i]][iteration][[1]])
  
    for (community in 1:length(aa)){                #loop across communities of the simulation 
      
      comm_i <- as.vector(aa[[community]])
      ID <- paste(names(glo_los_list[i])[[1]],"_simulation_", iteration,"_community_", names(aa)[community], sep ="")
      glo_comm_list[[ID]] <- comm_i
      rm(ID, community)
      
    }
    
    rm(aa, iteration)
    
  }
  
  rm(i)
  
}

rm(comm_i, community)



#export all communities in a .txt file

getOption("max.print") #current print limit, too low
options(max.print = 99999999) #in order to print all of it

sink(file = "Communities_all.txt", type = c("output"))
glo_comm_list
sink()

options(max.print = 10000) #set print limit back to normal




#Export stats for all simulations (takes some time...)
glo_stat<- data.frame()

for (i in 1:length(glo_g_list)){

  #get the identifiers
  IDs <- strsplit(names(glo_g_list[i]), "_") 
  product <- IDs[[1]][1]
  year <- IDs[[1]][2]
  cutoff <- IDs[[1]][3]
  cutoff_value <- IDs[[1]][4]
  
  #calculate stats for each graph
  Nvertices <- length(V(glo_g_list[[i]]))
  Nedges <- length(E(glo_g_list[[i]]))
  #Transitivity <- transitivity(glo_g_list[[i]], type="global")
  Density <-  graph.density(glo_g_list[[i]])
  TOTWeight<-  sum(E(glo_g_list[[i]])$weight)
  Percent <- glo_percent_list[[i]]
  
  
  #Modularity for each iteration on the same graph
  for(Iteration in 1:length(glo_clu_list[[i]])){
    
    Modularity <- modularity(glo_g_list[[i]], membership(glo_clu_list[[i]][[Iteration]]))
    Modularity_w <- modularity(glo_g_list[[i]], membership(glo_clu_list[[i]][[Iteration]]), weights = E(glo_g_list[[i]])$weight)
    
    glo_stat_x <- data.frame(year, product, cutoff, cutoff_value, TOTWeight, Percent, Nvertices, Nedges, Modularity, Modularity_w, Density, Iteration)
    print(glo_stat_x)
  
    glo_stat <- rbind(glo_stat, glo_stat_x) #appending all stats to the empty dataframe
	  
    rm(glo_stat_x, Modularity, Modularity_w, Iteration)
  }
    
  rm(TOTWeight, Percent, Density, Nedges, Nvertices, cutoff, cutoff_value, year, product, IDs, i)
  
}

write.csv(glo_stat, "global_stats_100.csv")







#if one wants to do this...but optional...

#function to select best cluster based on list of sizes and clusters 
bestcluster <- function(los, clu){
  
  out <- do.call(rbind.fill,los) #combine vectors of different length in matrix
  #out[is.na(out)] <- 0 #not necessary
  out <- count(out, vars = names(out)) #counting how many times each vector occurs
  
  best <- out[order(out$freq, decreasing = TRUE), ][1,-max(dim(out)[2])]
  best = best[!is.na(best)]        #best cluster
  frequency <- max(out$freq)       #how many times the cluster was produced
  
  matching <- sapply(los, function(x)all(x==best)) #match best cluster with the list
  #clu[matching][1][[1]][4] #change this numbers to check that clusters are actually identical
  #clu[matching][2][[1]][4]
  tt_sel <- clu[matching][1][[1]]   #finally take this out as the conclusive clusterization
  aa_sel <- communities(tt_sel)
  
  Output <- list("Outcounts" = out, "Bestclu" <- best, "Frequency" = frequency, "tt_sel" = tt_sel, "aa_sel" = aa_sel)
  
}   




#loop over all clusters and export the selected community
for (i in 1:length(glo_clu_list)){
  
  #get the identifiers
  IDs <- strsplit(names(glo_los_list[i]), "_") 
  p <- IDs[[1]][1]
  y <- IDs[[1]][2]
  cutoff_name <- IDs[[1]][4]
  cutoff <- IDs[[1]][5]
  
  #print best clusters each in a separate file
  bclu <- bestcluster(glo_los_list[[i]], glo_clu_list[[i]])
  bclu_sel <- bclu$aa_sel
  
  for (community_sel in 1:length(bclu_sel)){
    
    xx <- as.vector(bclu_sel[[community_sel]])
    write.table(xx, file = paste(getwd(),"/Outputs/Communities_sel/", p, "_",y, "_cutoff_", cutoff_name,"_", cutoff,"_",
                                 "community_", names(bclu_sel)[community_sel], ".txt", sep =""), 
                quote = TRUE, row.names = FALSE, col.names = FALSE)
    
  }
  
  #print frequency table each in a separate file
  write.table(bclu$Outcounts, file = paste(getwd(),"/Outputs/Countstables/", p, "_",y, "_cutoff_", cutoff_name,"_", cutoff,
                                           "_Countstable.txt", sep =""))
  
  rm(xx, community_sel, bclu_sel, bclu, cutoff, cutoff_name, y, p, IDs, i)
}




#loop over all selected clusters and return stats
glo_stat_sel <- data.frame()

for (i in 1:length(glo_clu_list)){
  
  #get the identifiers
  IDs <- strsplit(names(glo_los_list[i]), "_") 
  p <- IDs[[1]][1]
  y <- IDs[[1]][2]
  cutoff_name <- IDs[[1]][4]
  cutoff <- IDs[[1]][5]
  
  #calculate stats
  Transitivity <- transitivity(glo_g_list[[i]], type="global")
  Density <-  graph.density(glo_g_list[[i]])
  
  bclu <- bestcluster(glo_los_list[[i]], glo_clu_list[[i]])
  Modularity_sel <- modularity(glo_g_list[[i]], membership(bclu$tt_sel))
  Modularity_w_sel <- modularity(glo_g_list[[i]], membership(bclu$tt_sel), weights = E(glo_g_list[[i]])$weight)
  
  Frequency <- bclu$Frequency
  Percent <- glo_percent_list[[i]]
  
  glo_stat_x_sel <- data.frame(y, p, cutoff, cutoff_name, Percent, Modularity_sel, 
                               Modularity_w_sel, Transitivity, Density, Frequency)
  
  glo_stat_sel <- rbind(glo_stat_sel, glo_stat_x_sel)
  
  rm(glo_stat_x_sel, Percent, Frequency, Modularity_sel, Modularity_w_sel, bclu, Density, Transitivity,
     cutoff, cutoff_name, y, p, IDs, i)
  
}

write.csv(glo_stat_sel, "global_stats_100_sel.csv")

