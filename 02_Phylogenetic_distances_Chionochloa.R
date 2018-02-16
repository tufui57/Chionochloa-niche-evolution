acaena <- read.nexus("Y:\\Niche change of lineages\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")


### Extract the set of terminal edge lengths associated with these tips.
# Extract names of edges (i.e. tips and taxa)
tips <- acaena$tip.label
## first get the node numbers of the tips
nodes <- sapply(tips, function(x,y) which(y==x), y=acaena$tip.label)
## then get the edge lengths for those nodes
#edge.lengths<-setNames(acaena$edge.length[sapply(nodes, function(x,y) which(y==x),y=acaena$edge[,2])],names(nodes))

## Distance between all combinations of tips
distances <- dist.nodes(acaena)

dis <- distances[nodes,nodes]

colnames(dis) <- names(nodes)
rownames(dis) <- names(nodes)

write.csv(dis, "Y:\\Niche change of lineages\\phylogenetic_distances_Chionochloa.csv")


###################################################################
### Linear model test for phylogenetic distances and Schoener's D
###################################################################
phy <- read.csv("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\phylogenetic_distances_Chionochloa.csv")
d <- read.csv("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\schoennerD_Chionochloa.csv")

# Check Chionochloa names
levels(d$spname1)
levels(phy$X)

# Use "subsp." and "var."
a <- gsub("subsp", "subsp.", as.character(phy$X))
phy$X <- gsub("var", "var.", a)
colnames(phy) <- c("X", phy$X)



phy[as.character(phy$X) %in% d$spname1, colnames(phy) %in% d$spname1]
dmat <- d[d$spname1 %in% as.character(phy$X), ]
dmat2 <- dmat[dmat$spname2 %in% as.character(phy$X), ]

dmat2$pd <- NA
for(i in 1:nrow(dmat2)){
  dmat2$pd[i] <- phy[as.character(phy$X) == as.character(dmat2$spname1[i]), as.character(dmat2$spname2[i])]
}

write.csv(dmat2, "Y:\\Niche change of lineages\\ShonnerD_phylogenetic_distances_Chionochloa.csv")

########################################################
### Function for plotting
########################################################
library(ggplot2)

dmat <- read.csv("Y:\\Niche change of lineages\\ShonnerD_phylogenetic_distances_Chionochloa.csv")

plotAnalysis <- function(data, 
                         m, # linear model object
                         xv, yv, # column names of responce and explanatory variable
                         xlabname, ylabname, # axes names for plot
                         showStats = T # TRUE; Show p value and slope of linear model and colour points, FALSE; No stat values and black points
){
  
  if(showStats == T){
    myplot <- ggplot(data, aes_string(x = xv, y = yv, colour = "spname1")) +
      geom_point(aes(colour = spname1)) +
      # change xy labels
      labs(x = xlabname, y = ylabname) +
      # change text size
      theme(text = element_text(size = 20),
            axis.text.x = element_text(size = 20)) +
      # drow LM line & confident intervals 
      stat_smooth(method = "lm", col = "red") +
      # show stats result as title
      labs(title = paste("Adj R2 =", signif(summary(m)$adj.r.squared, digits = 2),
                         "Intercept =", signif(m$coef[[1]], 2),
                         " Slope =", signif(m$coef[[2]], 2),
                         " P =", signif(summary(m)$coef[2, 4], 2))) +
      theme(panel.background = element_rect(fill = "gray95"))
  } else {
    myplot <- ggplot(data, aes_string(x = xv, y = yv)) +
      geom_point(aes(colour = spname1)) +
      # change xy labels
      labs(x = xlabname, y = ylabname) +
      # change text size
      theme(text = element_text(size = 20),
            axis.text.x = element_text(size = 20)) +
      # drow LM line & confident intervals 
      stat_smooth(method = "lm", col = "red") +
      theme(panel.background = element_rect(fill = "gray95"), legend.position="none")
  }
  return(myplot)
}

#########################################################################
### Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

m <- lm(pd ~ ecospat.corrected, dmat)

myplot <- plotAnalysis(data = dmat, m=m, xv = "ecospat.corrected", yv = "pd", showStats = T,
                       xlabname = "Schonner's D of occurrence records", ylabname = "Phylogenetic distances")

# save
ggsave(paste("Y:\\Chion_pd_occD.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


