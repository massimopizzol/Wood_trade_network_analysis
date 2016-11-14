library("plyr")
library("ggplot2")



#Import data
glo_stat2 <- read.csv("global_stats_100.csv")
glo_stat2 <- glo_stat2[,-1]
names(glo_stat2) <- c("year", "product", "cutoff_f", "cutoff", "TOTweight", "Percent", "Nvertices", "Nedges", "Modularity","Modularity_W", "Density", "Iteration")
glo_stat2 <- glo_stat2[glo_stat2$product != "Paper+-Board Ex Newsprnt",]
glo_stat2$cutoff_f <- factor(glo_stat2$cutoff_f)
glo_stat2$year <- factor(glo_stat2$year)
glo_stat2$product <- factor(glo_stat2$product)
glo_stat2$prod_year <- interaction(glo_stat2$year, glo_stat2$product)
head(glo_stat2)




#Plots for the paper

glo_stat_Y <- subset(glo_stat2, cutoff_f == "Y")
glo_stat_Y$prod_year <- interaction(glo_stat_Y$year, glo_stat_Y$product)
dim(glo_stat_Y); names(glo_stat_Y);levels(glo_stat_Y$product); head(glo_stat_Y)



#Modularity per product per year in different panels
ggplot(glo_stat_Y, aes(x = factor(year), y = Modularity)) + 
  geom_point(size = 2, col = "red")+
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Modularity")

ggsave("Modularity.eps", width = 13, height = 15, units = "cm") 

#Weighted Modularity per product per year in different panels
setEPS()
postscript("WModularity.eps", width = 7, height = 8, horizontal = FALSE, onefile = FALSE, paper = "special")

ggplot(glo_stat_Y, aes(x = factor(year), y = Modularity_W)) + 
  geom_point(size = 1, col = "red")+
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Weighted Modularity")
dev.off()

#ggsave("Weighted Modularity.eps", width = 13, height = 15, units = "cm") 



#Modularity per product per year in one panel
ggplot(glo_stat_Y, aes(x = prod_year, y = Modularity, col = factor(product))) + geom_point() +
  labs(y="Modularity") + theme_classic() + theme(legend.title=element_blank(), legend.position="bottom", axis.title.x = element_blank(), axis.text.x=element_blank(),axis.ticks.x = element_blank() )
ggplot(glo_stat_Y, aes(x = prod_year, y = Modularity_W, col = factor(product))) + geom_point() + 
  labs(y="Weighted Modularity") + theme_classic() + theme(legend.title=element_blank(), legend.position="bottom", axis.title.x = element_blank(), axis.text.x=element_blank(),axis.ticks.x = element_blank() )




#Additional plots for the SI

ggplot(glo_stat2, aes(x = factor(year), y = Modularity_W, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Weighted Modularity")
  
ggplot(glo_stat2, aes(x = factor(year), y = Density, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Density")


ggplot(glo_stat2, aes(x = factor(year), y = Modularity, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Modularity")


ggplot(glo_stat2, aes(x = factor(year), y = TOTweight, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Total Weight")


ggplot(glo_stat2, aes(x = factor(year), y = Percent, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Percent")

ggplot(glo_stat2, aes(x = factor(year), y = Nvertices, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Number of vertices")

ggplot(glo_stat2, aes(x = factor(year), y = Nedges, colour = cutoff_f)) + 
  geom_point(size = 2)+ scale_colour_discrete(name="Cutoff", labels=c("No", "Yes"))+ 
  facet_wrap(~product, nrow = 4)+
  theme_bw() + #nice one there is also classic, light, minimal, etc. super nice.
  theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
  labs(x="", y = "Number of edges")




