#########################################################################################################
### Tests for phylogenetic distances, species ages, niche volume and Schoener's D
#########################################################################################################

setwd("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands")

###################################################################
### Linear model test for phylogenetic distances and Schoener's D
###################################################################

# ## Import data
# phy <- read.csv("phylogenetic_distances_Chionochloa.csv")
# d <- read.csv("schoennerD_Chionochloa.csv")
# 
# # Modify species name
# spname <- gsub("subsp.", "subsp", unique(c(as.character(d$spname1), as.character(d$spname2))))
# # Chionochloa flaviscans in phylogeny tree and Chionochloa flaviscans f. flaviscans in occurrence records are treated as the same species?
# spname2 <- gsub("_f._flavicans", "", gsub("var.", "var", spname))
# splist <- unique(spname2[spname2 %in% phy$X])
# 
# # Extract species pairs whose phylogeentic distance is available
# phy2 <- phy[phy$X[phy$X %in% splist], c("X", colnames(phy)[colnames(phy) %in% splist])]
# 
# # Modify species name in Shoenners'D list
# d$spname1 <- gsub("_f._flavicans", "", gsub("var.", "var", gsub("subsp.", "subsp", d$spname1)))
# d$spname2 <- gsub("_f._flavicans", "", gsub("var.", "var", gsub("subsp.", "subsp", d$spname2)))
# 
# # Extract species pairs whose Shoenners'D is available
# d2 <- d[d$spname1 %in% splist,]
# d3 <- d2[d2$spname2 %in% splist,]
# 
# 
# # Put phylogenetic distance and niche overlap into one dataframe
# d3$pd <- NA
# for(i in 1:nrow(d3)){
#   te <- d3$spname2[i]
#   d3$pd[i] <- phy2[phy2$X == as.character(d3$spname1[i]), as.character(te)]
# }
# 
# write.csv(d3, "Chionochloa\\ShonnerD_phylogenetic_distances_Chionochloa2018.csv")

########################################################
### Function for plotting
########################################################
library(ggplot2)

dmat <- read.csv("Chionochloa\\ShonnerD_phylogenetic_distances_Chionochloa2018.csv")

source("C:\\Users\\nomur\\Documents\\Acaena niche evolution\\plotAnalysis_clade_niche.R")

#########################################################################
### Phylogenetic distances ~ niche overlap of occurrence records
#########################################################################

m <- lm(pd ~ ecospat.corrected, dmat)

myplot <- plotAnalysis(data = dmat, m = m,
                       xv = "ecospat.corrected", yv = "pd", 
                       showStats = F, nodeNumber = NULL,
                       xlabname = "Schonner's D of occurrence records", ylabname = "Phylogenetic distances")

# save
ggsave(paste("Y:\\pd_occD_legend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


########################################################################################
### Node ages
########################################################################################
library(ape)
# Import phylogenetic tree data
chion <- read.nexus("Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")

### Extract the set of terminal edge lengths associated with these tips.
# Extract names of edges (i.e. tips and taxa)
tips <- chion$tip.label
## first get the node numbers of the tips
nodes <- sapply(tips, function(x,y) which(y==x), y=chion$tip.label)

## Distance between all combinations of tips
distances <- dist.nodes(chion)

## Speices age
ages <- sapply(nodes, function(i){
  min(distances[distances[,i]>0,i])
}
)

write.csv(ages, "species_ages_chionochloa.csv")

# species age
ages <- read.csv("Chionochloa\\species_ages_chionochloa.csv")
# Niche volume
d <- read.csv("Chionochloa\\Chionochloa_nicheVolume.csv")


# Modify species name in niche volume list
spname <- gsub("subsp.", "subsp", d$X)
d$X <- gsub("_f._flavicans", "", gsub("var.", "var", spname))

dat <- cbind(d[as.character(d$X) %in% ages$X, c("X","corrected.D")], 
      ages[ages$X %in% as.character(d$X),"x"]
      )

colnames(dat) <- c("spname1", "occ.corrected.D", "sp.age")

#########################################################################
### Node ages ~ niche volume
#########################################################################

m <- lm(occ.corrected.D ~ sp.age, dat)

myplot <- plotAnalysis(data=dat, m=m, xv = "occ.corrected.D", yv = "sp.age", showStats = F,
                       xlabname = "Species niche volume", ylabname = "Species age")

# save
ggsave(paste("Y:\\chionochloa_nicheVolume_spAge_noLegend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

summary(lm(occ.corrected.D ~ sp.age, dat))