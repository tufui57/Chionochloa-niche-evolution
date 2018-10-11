########################################################
### Regression figures
########################################################

###################################################################################
### Reproducible data set for analyses (on construction at 3.July.2018) 
###################################################################################

genus_name = "Acaena"
genus_tag = "acaena"

# Import data
if(
  file.exists(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = "")) == FALSE
){
  source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_table_of_landcoverHistory_for_analyses.R")
  source(".//Chionochloa niche evolution//Chionochloa2ndary open habitat analysis//06_2_calculate_indices_for_analyses.R")
  }
d <- read.csv(paste("Y://Acaena project//", genus_tag, "_data_analyses.csv", sep = ""))

library(dplyr)
source(".//Acaena niche evolution/F_Create_Package_speciseNameCleaning.r")
d$tag <- makeTag_separate(d$spname, genus_name, "_")
d$tag <- d$tag$tag %>% toupper

library(ggplot2)
source(".//Acaena niche evolution//F_plotAnalysis_clade_niche.R")

########################################################
### Add species trait, elevation and availability data
########################################################

# Load niche overlap data
if(file.exists("Y://shoenerD.csv") == FALSE){
  source("Y:\\R scripts\\1 Acaena project\\Modified\\06_NicheOverlap.r")
}
# nihce overlap must be 0, not NA, for speices having no occurrence records in primary or secondary open habitat
overlap <- read.csv("Y://shoenerD.csv")

# Load trait data
trait <- read.csv("Y://traits.csv")

# Load availability
if(file.exists("Y://availability_from_forest_primary_10_100km.csv") == FALSE){
  source("Y:\\R scripts\\1 Acaena project\\Modified\\10_Availability_of_new_habitats_in_10km_neighbourhood.r")
}
availability <- read.csv("Y://availability_from_forest_primary_10_100km.csv")

# Claculate mean evelation of species occurrence records
# Elevation was extracted by ArcGIS. Thus, no scripts are left.
elev <- read.csv("Y://Acaena project//Acaena_elevation.csv")
elev <- elev[, c("mean_elevation", "niche_volume", "spname")]
  
# Niched breadths
if(file.exists(paste("Y://Acaena project//", genus_tag, "_nicheBreadths.csv", sep = ""))== FALSE){
  source("Y:\\R scripts\\1 Acaena project\\Modified\\08_2_Analyses_NicheBreadths.R")
}
breadths <- read.csv(paste("Y://Acaena project//", genus_tag, "_nicheBreadths.csv", sep = ""))

# Merge data
dat <- merge(trait, availability, by = "X") %>% 
  merge(., breadths[, c("spname", "median.of.temp", "median.of.prec")], by.x = "X", by.y = "spname") %>% 
  merge(., elev, by.x = "X", by.y = "spname") %>% 
  merge(., d[, -grep("x", colnames(d), ignore.case = T)], by.x = "X", by.y = "spname") %>% 
  merge(., overlap, by = "X") 

# Create types of dispersal ability (barb section - lifeform)
dat$type <- paste(dat$barbSection, dat$lifeForm, sep = "_")
dat$type2 <- sapply(dat$type, function(x){
  ifelse(x == "Ancistrum_Stoloniferous" | x == "Microphyllae_Rhizomatous", x, "Others")
})
       
########################################################
### Multivariate analyses
########################################################

### Tests
summary(lm(proportionSecondaryHabitat ~ median.of.temp + median.of.prec, data = dat))

summary(glm(proportionSecondaryHabitat ~ log10.total + 
              mean_elevation + X10km.. + PreferenceOpen + niche_volume + corrected.D + 
              type2 + median.of.temp + median.of.prec,
            data = dat)
)



#########################################################################
### Proportion of Secondary Open Habitat - Preference for Open Habitat
#########################################################################

myplot <- plotAnalysis(data = d,
                       genus_name = genus_name,
                       xv = "PreferenceOpen", yv = "proportionSecondaryHabitat", showStats = T,
                       xlabname = expression("closed" %<-% "Preference for open habitat" %->% "open"),
                       ylabname = "Proportion of secondary open habitat",
                       nodeNumbercol = "tag", label.point = T)

myplot2 <- myplot + 
  xlim(0, 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed")

# save
ggsave(paste("Y:\\preferenceOpen_proportionSecondary_nolegend.png", sep = ""), plot = myplot2,
       width = 300, height = 210, units = 'mm')

rm(myplot)

##################################################
### Proportion of secondary open habitat ~ dispersal type
##################################################
p <- ggplot(d, aes(x = type, y = proportionSecondaryHabitat, fill = type)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  # change xy labels
  labs(x = "Dispersal types", y = "Proportion of secondary open habitat") +
  # change text size
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 20))

# save
ggsave(paste("Y:\\barb_proportionSecondary.png", sep = ""), plot = p,
       width = 300, height = 210, units = 'mm')

### variance analysis
summary(aov(proportionSecondaryHabitat ~ type, d))

########################################################################
### Proportion of secondary open habitat - Niche overlap (D)
########################################################################

# prepare LM before drow plot
# Inf must be removed before lm()
dsc <- d[!is.na(d$shoenners_D),]
m <- lm(shoenners_D ~ proportionSecondaryHabitat, dsc)

myplot <- ggplot(dsc, aes_string(y = "proportionSecondaryHabitat", x = "shoenners_D", label = "tag")) +
  geom_point(aes(colour = type)) +
  # text label for points
  geom_text(size=5) +
  # 0 <= relative change <=1
  coord_cartesian(ylim = c(0,1), xlim = c(0,1)) +
  # change xy labels
  labs(y =  "Proportion of secondary open habitat", x = "Niche overlap") +
  # change text size
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 20)) +
  # drow LM line & confident intervals 
  stat_smooth(method = "lm", col = "red") +
  theme(panel.background = element_rect(fill = "gray95"), legend.position="none")

# save
ggsave(paste("Y:\\SchoenerD_proportionSecondary_noLegend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

myplot <- plotAnalysis(dsc, m, xv = "shoenners_D", yv ="proportionSecondaryHabitat",
                       ylabname = "Proportion of secondary open habitat", xlabname = "Niche overlap")
# save
ggsave(paste("Y:\\SchoenerD_proportionSecondary.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')



###############################################################
## Proportion of secondary open habitat - elevation
###############################################################

# prepare LM before drow plot
m <- lm(proportionSecondaryHabitat ~ mean_elevation, d)

myplot <- plotAnalysis(d, m, showStats = F,
                       xv = "mean_elevation", yv ="proportionSecondaryHabitat", 
                       ylabname = "Proportion of secondary open habitat", xlabname = "Mean elevation of species occurrences (m)")

# save
ggsave(paste("Y:\\proportionSecondary_elevation_noLegend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


########################################################
### Shrubland ratio - Preference for open habitat
########################################################

m <- lm(shrub ~ Preference_open_habitat, d)

myplot <- plotAnalysis(d, m, xv = "PreferenceOpen", yv = "shrub", showStats = F,
                       xlabname = expression("closed" %<-% "Preference for open habitat" %->% "open"), ylabname = "Proportion of current Shrubland habitat")

myplot2 <- myplot + xlim(0, 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed")

# save
ggsave(paste("Y:\\PreferenceOfOpenness_shrub_noLegend.png", sep = ""), plot = myplot2,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


#############################################################################################
###   Proportion of secondary open habitat - availability of secondary open habitat   ###
#############################################################################################

### Local(10km) availability
m <- lm(proportionSecondaryHabitat ~ X10km_average_abailability, d)

myplot <- plotAnalysis(d, m, xv = "X10km_average_abailability", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Availability of open habitat within 10 km neighbourhood", ylabname = "Proportion of secondary open habitat")

# save
ggsave(paste("Y:\\ProportionSecondary_10kmAvailability.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


### Regional (100km) availability
m <- lm(proportionSecondaryHabitat ~ X100km_average_abailability, d)

myplot <- plotAnalysis(d, m, xv = "X100km_average_abailability", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Availability of open habitat within 100 km neighbourhood", ylabname = "Proportion of secondary open habitat")

# save
ggsave(paste("Y:\\ProportionSecondary_100kmAvailability.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)




#########################################################################
### Proportion of Secondary Open Habitat - Niche volume
#########################################################################

m <- lm(proportionSecondaryHabitat ~ niche_volume, d)

myplot <- plotAnalysis(data=d, m=m, xv = "niche_volume", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Species niche volume", ylabname = "Proportion of secondary open habitat")

myplot2 <- myplot + 
  xlim(0, 1)

# save
ggsave(paste("Y:\\nicheVolume_proportionSecondary_nolegend.png", sep = ""), plot = myplot2,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)

#########################################################################
### Proportion of Secondary Open Habitat - temperature niche
#########################################################################

m <- lm(proportionSecondaryHabitat ~ unlist.med1., d)

myplot <- plotAnalysis(data=d, m=m, xv = "unlist.med1.", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Median of temperature niche", ylabname = "Proportion of secondary open habitat")


# save
ggsave(paste("Y:\\tempNiche_proportionSecondary_nolegend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)


#########################################################################
### Proportion of Secondary Open Habitat - precipitation niche
#########################################################################

m <- lm(proportionSecondaryHabitat ~ unlist.med2., d)

myplot <- plotAnalysis(data=d, m=m, xv = "unlist.med2.", yv = "proportionSecondaryHabitat", showStats = F,
                       xlabname = "Median of precipitation niche", ylabname = "Proportion of secondary open habitat")


# save
ggsave(paste("Y:\\precNiche_proportionSecondary_nolegend.png", sep = ""), plot = myplot,
       width = 300, height = 210, units = 'mm')

rm(myplot, m)
