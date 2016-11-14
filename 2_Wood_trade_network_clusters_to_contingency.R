#create contingency table from list of communities

#first convert communities from list to dataframe
glo_com <- matrix(nrow = length(glo_comm_list), ncol = 9)
for (i in 1:length(glo_comm_list)){ 
  
  row <- strsplit(names(glo_comm_list[i]), "_")[[1]]
  glo_com[i,] <- c(row, names(glo_comm_list[i]))
  print(i)
  print(row)
  rm(i, row)
  
}

head(glo_com)
glo_com <- glo_com[,-c(5,7)] 
glo_com <- data.frame(glo_com) #you can only have one class type in matrices...
names(glo_com) <- c("product", "year", "cutoff_f", "cutoff", "simulation", "community", "UNIQUEID")
glo_com$cutoff <- as.numeric(as.character(glo_com$cutoff))
glo_com$cutoff_f <- as.factor(glo_com$cutoff_f)


#function: select specific networks from the list
cut_list <- function(cutoff, product){ 
  glo_com_p <- glo_com[glo_com$product == product,] #filtered for product
  
  
  glo_com_p_c <- glo_com_p[glo_com_p$cutoff_f == cutoff,] #filtered for cutoff
  
  cutted_list <- glo_comm_list[names(glo_comm_list) %in% glo_com_p_c$UNIQUEID] #filtered for cutoff
  
}


#function: from cutted list to long dataframe 
cutted_to_long <- function(cutted_list){
  
  
  names_all = c()
  for (i in 1:length(cutted_list)){
    names_i <- rep(names(cutted_list[i]),length(unlist(cutted_list[i])) )
    names_all <- append(names_all, names_i)
  }
  
  long_list <- unlist(cutted_list)
  long_df <- data.frame(long_list)
  long_df$nm <- names_all
  names(long_df) <- c("country", "cluster")
  rownames(long_df) <- NULL
  rm(names_all, names_i,i)
  long_df
}



#function: from longdataframe to contingency matrix
long_to_adj <- function(long_df, product){
  m <- table(long_df)
  M <- as.matrix(m)
  Mrow <- M %*% t(M) #this works!
  
  title <- paste(product, " (n = ", dim(Mrow)[1],")", sep = "")
  
  svg(filename = paste("Heatmap_",product,".svg",sep = ""), width = 4.5, height = 4.5)
  par(cex.main=0.7) 
  #heatmap(Mrow, main = title, labRow = NA, labCol = NA)
  heatmap(Mrow, main = title, Rowv = NA, Colv = NA, labRow = NA, labCol = NA)
  dev.off()
  par(cex.main=0.7) 
  #print(heatmap(Mrow, main = title, labRow = NA, labCol = NA)) 
  print(heatmap(Mrow, main = title, Rowv = NA, Colv = NA, labRow = NA, labCol = NA))
  
  rm(M, m, title)
  Mrow
}


#loop the above functions for all products
results <- list()
for (p in products2){
  
  cutted_list <- cut_list("Y", p)
  long_df <- cutted_to_long(cutted_list)
  Adjm <- long_to_adj(long_df, p)
  results[[p]] <- Adjm
  print(p)
  rm(p, Adjm, long_df, cutted_list)
}


#export all contingency matrices
for (i in products2){
  prodd <- results[[i]]
  dir.create("adj_output", showWarnings = FALSE)
  write.csv(prodd, file = paste("adj_output/",i,"_adiacencymatrix.csv", sep = ""))
  rm(prodd, i)
}



#Ways to look at these data.
names(results)
prod <- results[[products[1]]]
prod[1:10, 1:10]
country_res <- data.frame(sort(prod["Denmark",]))
names(country_res) = products[1]
country_res


#rm(Adjm)

# #other nice plotting
my_palette <- colorRampPalette(c("black", "yellow", "green"))(n = 299)
col_breaks = c(seq(0,0.25,length=100), # for red
               seq(0.25,0.5,length=100), # for yellow
               seq(0.5,1,length=100)) # for green
heatmap(results[[13]]/1600, Rowv = NA, Colv = NA, symm = TRUE, revC = TRUE,
        col=my_palette, breaks = col_breaks, main = products[13]) #gives the nice heatmap


products
indicators$Threshold/1600







#Stats and cluster analysis on contingency table
library("igraph")

indicators <- data.frame(matrix(NA, nrow = 12, ncol = 9))
names(indicators) <- c("NC", "SF", "NC/SF", "Mean", "SD", "Modularity", "Weighted Modularity", "Density", "Transitivity")
row.names(indicators) <- products2

for (i in 1:length(products2)){
  
  prodadj <- results[[i]]
  prodadj[lower.tri(prodadj)] <- 0
  
  indicators[i,1] <- dim(prodadj)[1]
  indicators[i, 2] <- sum(prodadj)
  indicators[i, 3] <- sum(prodadj)/dim(prodadj)[1]
  prodadj2 <- prodadj[prodadj != 0]
  indicators[i, 4] <- mean(prodadj2)
  indicators[i, 5] <- sd(prodadj2)
  
  gg = graph.adjacency(prodadj, mode = "upper", weighted = TRUE, diag = FALSE) 
  gg1 <- decompose.graph(gg, max.comps = 1, min.vertices = 3)
  gg <- gg1[[1]]
  gtt <- cluster_spinglass(gg, spins=20, gamma = 1)
  
  indicators[i, 6] <- modularity(gg, membership(gtt))
  indicators[i, 7] <- modularity(gg, membership(gtt), weights = E(gg)$weight)
  indicators[i, 8] <- graph.density(gg)
  indicators[i, 9] <- transitivity(gg, type="global")
  
  
  print(indicators[i,])
  rm(prodadj2 ,prodadj, gtt, gg, gg1)
}

#indicators$Units <- c("m3","m3","m3","m3","m3","tons","m3","m3","m3","m3","m3","m3","tons")

indicators

write.csv(indicators, "Table1.csv")





