# 1. Create dataframe of land cover change history, species records, climate data
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\Chionochloa2ndary open habitat analysis\\01_createData.R")
# 2. Format past climate raster
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\LGM climate niche\\00_PastDataPreparation.R")
# 3. Extract current cliamte data by polygon of past land area & Calculate PC values for past climate
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\LGM climate niche\\01_SpatialDataPreparation.R")
# 4. Count persistent climate
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\LGM climate niche\\02_PCAdataPreparation.R")
# 5. Analize persistent climate
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\LGM climate niche\\04_PastClimate_analysis.R")
# 6. Calculate climate variables from PC values
source("C:\\Users\\nomur\\Documents\\Chionochloa niche evolution\\LGM climate niche\\06_ReversePCvalues_to_originalClimateVlues.R"")

# 7. Linear regression between persistence and species age
source("C:\\Users\\nomur\\Documents\\report consequences of change in LGM neighbourhood cell size.Rmd")