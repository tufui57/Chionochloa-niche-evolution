#########################################################
## Create list of species features
#########################################################

#########################################################
## Count number of 1km occurrence cells
#########################################################

# Data import
alld <- read.csv("Y:\\Acaena project\\chionochloa_bioclim_landcover_1km.csv")
d <- alld[is.na(alld$bioclim1) == F, ]

# sp names
sname <- colnames(d)[grepl("^Chion", colnames(d))]

# Replace NA with 0
for(i in sname){
  d[is.na(d[,i]),i] <- 0
}

# Species that doesn't have < 5 records can't be used in this analysis.
spLessThan5 <- sapply(sname, 
                      function(i){sum(d[,i]) < 5}
)
spname <- sname[!(spLessThan5)]

### Count number of 1km occurrence cells
occ <- lapply(spname, function(x){
  sum(d[,x] == 1) %>% return
})

reslist <- data.frame(spname, unlist(occ))


#########################################################
## Count number of occurrence records
#########################################################

# data import
chion <- read.csv("Y:\\Acaena project\\chionochloa_bio_alt.csv")

chionl <- group_by(chion, taxa)

sp <- (attr(chionl, "labels")$taxa %>% as.character) 

reslist <- mutate(reslist, record = attr(chionl, "group_sizes")[sp %in% spname])

#########################################################
## Species name codes
#########################################################

source(".//Acaena niche evolution//BIOMOD//Create_Package_speciseNameCleaning.r")
codes <- makeTag_separate(spname, "Chionochloa", "_")

reslist <- mutate(reslist, tag = codes$tag)


write.csv(reslist, file = "Chionochloa_species_list.csv")

