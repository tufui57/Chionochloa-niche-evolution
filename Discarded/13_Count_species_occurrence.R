#########################################################
## Create list of species features
#########################################################

library(dplyr)

genus_name <- "Acaena"

#########################################################
## Count number of 1km occurrence cells
#########################################################

# Data import
alld <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bioclim_landcover_1km.csv", sep = ""))
d <- alld[is.na(alld$bioclim1) == F, ]

### Make species name codes
spname <- grepl(genus_name, colnames(d)) %>% colnames(d)[.]

source(".//Acaena niche evolution//BIOMOD//Create_Package_speciseNameCleaning.r")
codes <- makeTag_separate(spname, genus_name, "_")

# Replace NA with 0
for(i in spname){
  d[is.na(d[,i]),i] <- 0
}

# Species that doesn't have < 5 records can't be used in this analysis.
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

# data import
chion <- read.csv(paste("Y:\\Acaena project\\", genus_name, "_bio_alt.csv", sep = ""))

chionl <- group_by(chion, taxa)

sp <- (attr(chionl, "labels")$taxa %>% as.character) 
sp <- gsub("novae-", "novae.", sp)

reslist <- mutate(reslist, record = attr(chionl, "group_sizes")[sp %in% spname])

#########################################################
## Species name codes
#########################################################

reslist <- mutate(reslist, tag = codes$tag[codes$X %in% spname])

write.csv(reslist, file = paste(genus_name, "_species_list.csv", sep = ""))

