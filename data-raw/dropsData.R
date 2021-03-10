## Load raw data.
dropsRaw <- read.csv("./data-raw/2a-GrainYield_components_Plot_level-1.csv",
                     stringsAsFactors = FALSE)
## Load genotype meta data.
genoMeta <- read.csv("./data-raw/8-Info_Maize_variety.csv",
                     stringsAsFactors = FALSE)
## Load environment meta data.
envMeta <- read.csv("./data-raw/3b-Indices_Env_level.csv",
                     stringsAsFactors = FALSE)

## Rename genetic_group geneticGroup for consistency.
colnames(genoMeta)[colnames(genoMeta) == "genetic_group"] <- "geneticGroup"

## Rename EC_water and EC_temp to scenarioWater and scenarioTemp for consistency.
colnames(envMeta)[colnames(envMeta) == "EC_water"] <- "scenarioWater"
colnames(envMeta)[colnames(envMeta) == "EC_temp"] <- "scenarioTemp"

## Restrict to 10 relevant environments.
exps <- c("Cam12R", "Cra12R", "Gai12W", "Kar12W", "Kar13R", "Kar13W",
          "Mar13R", "Mur13R", "Mur13W", "Ner12R")
dropsRaw <- dropsRaw[dropsRaw[["Experiment"]] %in% exps, ]

## Remove anthesis.silking.interval.
dropsRaw <- dropsRaw[colnames(dropsRaw) != "anthesis.silking.interval"]

## F546 has no family. Remove it from the data.
dropsRaw <- dropsRaw[dropsRaw$Variety_ID != "F546", ]

## Add scenario and latitude/longitude columns.
dropsRaw <- merge(dropsRaw, envMeta[, c("Experiment", "Lat", "Long",
                                        "scenarioWater", "scenarioTemp")])

dropsRaw[["scenarioFull"]] <- interaction(dropsRaw[c("scenarioWater",
                                                     "scenarioTemp")],
                                          drop = TRUE)

## Add genetic groups.
dropsRaw <- merge(dropsRaw, genoMeta[c("Variety_ID", "geneticGroup")])

dropsRaw <- droplevels(dropsRaw)

usethis::use_data(dropsRaw, overwrite = TRUE)
