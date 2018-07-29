
library(phytools)

########################################################################################
### Linage through time
########################################################################################
# Plot speciation rate through time

# Output of TreeAnnotator
# TreeAnnotator output is the one chosen among tree candidates estimated on the process of MCMC sampling.
# Species names must not have space. Replace space with _ (underscore).
acaena <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\Phylogenetic data and trees\\From Angela\\NZ_Acaena_BEAST_output_6gene.tree")

ltt(acaena)

ltt(chion)
 
########################################################################################
### Calculation of niche evolution rate
########################################################################################
library(ouch)

### Calculate trait data to estimate its evolution rate
### Trait = species climate nihce = mean value of PC1 and 2 of species


###################
### Acaena
###################

# Output of TreeAnnotator
# TreeAnnotator output is the one chosen among tree candidates estimated on the process of MCMC sampling.
# Species names must not have space. Replace space with _ (underscore).
acaena <- read.nexus("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\Phylogenetic data and trees\\Phylogenetic data and trees\\From Angela\\NZ_Acaena_BEAST_output_6gene.tree")

alld <- read.csv("Y:\\1st chpater_Acaena project\\meta data\\Acaena_bioclim_landcover_1km.csv")
aca.d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
aca.sname <- colnames(aca.d)[grepl("^Acaena", colnames(aca.d))]

format_trait_data <- function(
  d, # occurrence record data
  sname # species name
){
  # Replace NA with 0
  for(i in sname){d[is.na(d[,i]),i] <- 0}
  
  # Species that doesn't have < 5 records can't be used in this analysis.
  sname2 <- sname[!(sapply(sname, function(i){sum(d[,i]) < 5}))]
  
  # get env. corrdinates (PCA axes)
  pca <- prcomp(d[, paste("bioclim", c(1, 6, 12, 15), sep = "")],
                center = TRUE,
                scale. = TRUE)
  
  scores <- data.frame(d[, c(paste("bioclim", c(1, 6, 12, 15), sep = ""), "x", "y", sname2)], pca$x[, 1:2])
  
  # Get mean of PC1
  trait <- sapply(sname2, function(i){
    occ <- scores[scores[,i] == 1,]
    mean(occ$PC1)
  }
  )
  
  return(trait)
}

trait.aca <- format_trait_data(aca.d, aca.sname)

# Prune terminal nodes missing their trait data
tipname <- sapply(acaena$tip.label, function(i){
  strsplit(i, "_Ac")[[1]][1]
})
acaena$tip.label <- gsub("_AY634821", "", gsub("_EU352216", "", tipname))

pruned.aca<- drop.tip(acaena, acaena$tip.label[(acaena$tip.label %in% names(trait)) == F])
# Convert tree object created by read.nexus() into ouch tree object
pruned.aca2 <- ape2ouch(pruned.aca)

match_trait_tree <- function(trait, tree){
  
  # Adjust order of trait data
  label <- tree@nodelabels
  nicheMean <- rep(NA, length(label))
  label2 <- data.frame(c(label), nicheMean)
  
  nasp <- label2[!(label2$c.label %in% names(trait)),]
  traitsp <- label2[label2$c.label %in% names(trait),]
  
  traitsp[order(traitsp$c.label.), "nicheMean"] <- trait[names(trait) %in% label2$c.label]
  
  trait2 <- rbind(nasp, traitsp)
  trait2[as.numeric(rownames(trait2)),]
  
  trait2[,"c.label."] <- as.character(trait2[,"c.label."])
  
  # Order and rownames of trait data must match with tree node names (tree@nodes)
  traitd <- data.frame(trait2[match(label, trait2$c.label.), "nicheMean"])
  rownames(traitd) <- tree@nodes
  
  return(traitd)
}

trait.aca2 <- match_trait_tree(trait.aca, pruned.aca2)

### Brownian motion model
# Treminal nodes mustn't have missing data
brown(data = trait.ac2, tree = pruned.aca2)
plot(brown(data = trait.aca2, tree = pruned.aca2))

###################
### Chionochloa
###################

# Output of BEAST
# BEAST output has all of tree candidates estimated on the process of MCMC sampling.
chion <- read.nexus("Y:\\Niche change of lineages\\Niche evolution of open habitat species in islands\\Phylogenetic data and trees\\Chionochloa_genetic_data\\Chiono_summary.trees")

alld <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

trait <- format_trait_data(d, sname)

# Prune terminal nodes missing their trait data
chion$tip.label <- gsub("subsp", "subsp.", chion$tip.label)
chion$tip.label <- gsub("flavicans", "flavicans_f._flavicans", chion$tip.label)
chion$tip.label[(chion$tip.label %in% names(trait)) == F]

pruned.chion <- drop.tip(chion, chion$tip.label[(chion$tip.label %in% names(trait)) == F])
# Convert tree object created by read.nexus() into ouch tree object
pruned.chion2 <- ape2ouch(pruned.chion)

traitd <- match_trait_tree(trait, pruned.chion2)

### Brownian motion model
# Treminal nodes mustn't have missing data
brown(data = traitd, tree = pruned.chion2)
plot(brown(data = traitd, tree = pruned.chion2))

write.csv()
