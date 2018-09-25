#########################################################
## Create list of species features
#########################################################

library(dplyr)

genus_name <- "Acaena"

#########################################################
## Count number of 1km occurrence cells
#########################################################

# Data import
alld <- read.csv(paste("Y:\\", genus_name, "_bioclim_landcover_history_worldclim1_1km_24sep.csv", sep = ""))
d <- alld[is.na(alld$bioclim1) == F, ]

### Make species name codes
spname <- grepl(genus_name, colnames(d)) %>% colnames(d)[.]
source(".//functions//Create_Package_speciseNameCleaning.r")
codes <- makeTag_separate(spname, genus_name, "_")

# Replace NA with 0
for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

# Species with < 5 records can't be used in this analysis.
spLessThan5 <- sapply(spname, 
                      function(i){sum(d[,i]) < 5}
)
spname <- spname[!(spLessThan5)]

### Count number of 1km occurrence cells
occ <- lapply(spname, function(x){
  sum(d[,x] == 1) %>% return
})

reslist <- data.frame(spname, unlist(occ))
colnames(reslist)[2] <- "NumberOfGridCell"

#########################################################
## Count number of occurrence records
#########################################################

# Data import
chion <- read.csv(paste("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\", genus_name, "_bio_alt.csv", sep = ""))

chionl <- group_by(chion, taxa)

sp <- (attr(chionl, "labels")$taxa %>% as.character) 
sp <- gsub("novae-", "novae.", sp)

reslist <- mutate(reslist, record = attr(chionl, "group_sizes")[sp %in% spname])

# Add species name codes
reslist <- mutate(reslist, tag = codes$tag[codes$X %in% spname])

write.csv(reslist, file = paste(genus_name, "_species_list24sep.csv", sep = ""))

