########################################################################################
### Mean elevation of occurrence records
########################################################################################

### Import data
alt <- read.csv(
  paste("Y:\\2nd chapter_phylogentic niche conservation\\raw data\\", genus_name, "_bio_alt.csv", sep=""
        )
)

# sp names
spname <- grepl(paste("^", genus_name, sep=""), colnames(alt)) %>% colnames(alt)[.]

ras <- project_and_convert_occurrencePoints_to_raster(alt, refWGS = pre, val = "NZL1_alt")

# Make raster stack
bio_land2 <- stack(c(bio_land, ras[[4]]))

