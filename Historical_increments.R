#Forestry marginals
#Massimo Pizzol, 2015

#load libraries needed
library(ggplot2)
library(reshape)
#library(ggthemes)
library(scales)




#import database and prepare it
db <- read.csv("Forestry_E_All_Data_reduced.csv", header = TRUE, sep = ";")
head(db)
c <- c(1:8, seq(10, 112, 2)) #remove duplicate columns (labelled with "F")
db <- db[, c]; rm(c)

timeserie <- as.character(1961:2013) 
names(db) <- c(names(db[,1:7]), timeserie) #rename top row

countries<- read.table(file = "countries_selection.txt")[,1] #exact names of the countries
db <- db[db$Country %in% countries,]                         #Use "countries_selection_ROW.txt" above 
                                                             #to get the same charts but with e.g. "World", "Northern America", etc.(regions)
head(db)
dim(db)
products <- levels(db$Item)      #exact names of the products




#Function to subset database according to product under analysis
selection <- function(trade, product) {
  
  db.c <- subset.data.frame(db, Element == trade & Item == product,
                            select = -c(Unit, Country.Code, Item.Code, Element.Code,
                                        drop = FALSE))
  db.c
}



#Function to calculate prod increment per country
incremental <- function(trade, product, startyear, endyear) {
  
  db.c <- selection(trade, product)
  
  
  #calculate regression stats and marginal increments
  xes <- c(startyear:endyear) #years under analysis
  slope <- c()
  se <- c()
  lCI <- c()
  hCI <- c()
  deg_fri <- c()
  R2 <- c()
  marginals <- matrix(nrow = dim(db.c)[1], ncol = (length(xes)-1))
  
  
  for (i in 1:dim(db.c)[1]){
    
    serie <- as.numeric(db.c[i, (dim(db.c)[2]-(length(xes)-1)):dim(db.c)[2]])
    serie[serie == 0] <- NA
    
    #if more than one third are NAs, reset the serie
    if (sum(is.na(serie)) >= length(serie)/3){
            
      slope[i] <- 0
      se[i] <- 0
      lCI[i] <- 0
      hCI[i] <- 0
      deg_fri[i] <- 0
      R2[i] <- 0
      marginals[i,] <- 0
    }
    
    else {
      
      fit1 <- lm(serie ~ xes) #regression model
      a <- summary(fit1)
      slope[i]<- a$coefficients[2,1] #regression coefficient (slope)
      se[i] <- a$coefficients[2,2] #standard error of the slope
      lCI[i] <- a$coefficients[2,1] - a$coefficients[2,2]*qt(0.975, a$df[2]) #same as: confint(fit1, 'xes', level=0.95)
      hCI[i] <- a$coefficients[2,1] + a$coefficients[2,2]*qt(0.975, a$df[2]) #same as: confint(fit1, 'xes', level=0.95) #qt gives the t value
      deg_fri[i] <- a$df[2]
      R2[i] <- a$r.squared
      marginals[i,] <- serie[-1] - serie[-length(serie)] #yearly marginal increments (year "t+1" minus year "t")
      
    }
    
  } 
  
  print(length(slope) == dim(db.c)[1]) #sanity check
  
  print(a)
  print(serie)
  print(xes)
  
  results <- data.frame(db.c[,1:3], slope, se, lCI, hCI, deg_fri, R2, marginals)
  names(results) <- c("Country", "Product", "Trade", "Increment", 
                      "SE", "lCI", "hCI", "df", "Rsquared", 
                      paste(rep("M", (length(xes)-1)), c(1:(length(xes)-1)), sep = ""))
  
  results
  
}



#Function to clean results and extract and plot top/bottom 10
tb10 <- function(trade, product, startyear, endyear){
  
  
  results <- incremental(trade, product, startyear, endyear)
  results <- results[order(-results$Increment), ,drop = FALSE]
  top10 <- results[1:10,]
  bottom10 <- results[(dim(results)[1]-10):dim(results)[1], ]
  
  
  #Plot top 10
  top10plot <- ggplot(top10, aes(x = factor(Country), y = Increment)) + 
    geom_bar(stat = "identity", position="dodge", fill="brown2") +
    geom_errorbar(aes(ymin=lCI, ymax=hCI), width=.3, color="darkblue")+
    geom_point()+
    theme_classic()+
    #geom_text(label = top10$Country, size = 4, angle = 0, 
    #          hjust = 0, vjust = 1)+
    scale_x_discrete(limits=rev(top10$Country))+ #to order them
#     ggtitle(paste(top10$Product[1],
#                   " - top10 production increment (",
#                   startyear, " - ", endyear,")", sep = ""))+
    scale_y_continuous(labels=function(x)x/10^6)+
    theme(plot.title = element_text(size=15, face="bold", vjust=1),
          axis.ticks.y = element_blank(), 
          axis.text.y=element_text(face="bold", color = "black", vjust = 0, hjust = 1),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()
          #panel.background = element_rect(fill = 'grey75')
    )+
    labs(x=NULL, y="Increment (Mm3)")+
    coord_flip()
  print(top10plot)
  
  
  #plot bottom 10
  bottom10plot <- ggplot(bottom10, aes(x = factor(Country), y = Increment)) + 
    geom_bar(stat = "identity", position="dodge", fill="brown2") +
    geom_errorbar(aes(ymin=hCI, ymax=Increment+lCI), width=.3, color="darkblue")+
    geom_point()+
    #geom_text(label = top10$Country, size = 4, angle = 0, 
    #          hjust = 0, vjust = 1)+
    scale_x_discrete(limits=rev(bottom10$Country))+ #to order them
    ggtitle(paste(top10$Product[1],
                  " - bottom10 production increment (",
                  startyear, " - ", endyear,")", sep = ""))+
    scale_y_continuous(labels=function(x)x/10^6)+
    theme(plot.title = element_text(size=15, face="bold", vjust=1),
          axis.ticks.y = element_blank(), 
          axis.text.y=element_text(face="bold", color = "black", vjust = 0, hjust = 1),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()
          #panel.background = element_rect(fill = 'grey75')
    )+
    labs(x=NULL, y="Increment (Mm3)")+
    coord_flip()
    print(bottom10plot)
    
    
  return(list("top10" = top10, 
                "bottom10" = bottom10, 
                "top" = top10plot, 
                "bottom" = bottom10plot))
    
}



#function to plot an historical of production quantities
history <- function(trade, product, country, startyear, endyear){
  
  db.c2 <- selection(trade, product)
  
  prod <- t(db.c2[which(db.c2$Country == country), (startyear-1957):(endyear-1957)])
  year <- row.names(prod)
  prod <- data.frame(year, prod)
  names(prod) <- c("Year", "Amount")
  rownames(prod) <- NULL
  head(prod)
  
  ggplot(data = prod, aes(x = Year, y = Amount)) + 
    geom_bar(color = "Black", fill="indianred2", width=.8,stat = "identity")+
    ggtitle(paste(country, product, trade,startyear, "-", endyear))+
    scale_y_continuous(labels=function(y)y/10^6)+
    theme(plot.title = element_text(size=15, face="bold", vjust=1),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()
    )+
    labs(y="Mm3")
  
} 
  



#Examples: calculate increments, plot, and export results
products

trade = "Production"
product = "Sawnwood (C)" 

increment_rank <- incremental(trade, product, 1998, 2013)
increment_rank <- increment_rank[order(increment_rank$Increment, decreasing = TRUE),]
write.csv(increment_rank, file = paste("increment_rank_", product, "_", trade, ".csv", sep = ""))

topbottom <- tb10(trade, product, 1998, 2013)
topbottom$top10 #to access single elements
topbottom$bottom10 

history(trade, product, "Denmark", 1998, 2013)



#this below will work only if you get the full database from Faostat (See README file)
#loop over all products considered in the paper.
products2 <- c("Chips and Particles", 
               "Fibreboard",
               "Industrial Roundwood(C)",
               "Industrial Roundwood(NC)",
               "Particle Board", 
               "Plywood", 
               "Sawnwood (C)",
               "Sawnwood (NC)", 
               "Veneer Sheets",
               "Wood Pulp")


products2 %in% products

for (product in products2){
  
  print(product)
  trade = "Production"
  increment_rank <- incremental(trade, product, 1998, 2013)
  increment_rank <- increment_rank[order(increment_rank$Increment, decreasing = TRUE),]
  write.csv(increment_rank, file = paste("increment_rank_", product, "_", trade, ".csv", sep = ""))
  rm(product, trade, increment_rank)
  
}


for (product in products2){
  
  print(product)
  trade = "Production"
  topbottom <- tb10(trade, product, 1998, 2013)

}



