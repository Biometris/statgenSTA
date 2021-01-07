## Load raw data.
dropsRaw <- read.csv("./data-raw/2a-GrainYield_components_Plot_level-1.csv",
                     stringsAsFactors = FALSE)
## Load genotype meta data.
genoMeta <- read.csv("./data-raw/8-Info_Maize_variety.csv",
                     stringsAsFactors = FALSE)
## Rename genetic_group geneticGroup for consistency.
colnames(genoMeta)[colnames(genoMeta) == "genetic_group"] <- "geneticGroup"

## Restrict to 10 relevant environments.
exps <- c("Cam12R", "Cra12R", "Gai12W", "Kar12W", "Kar13R", "Kar13W",
          "Mar13R", "Mur13R", "Mur13W", "Ner12R")
dropsRaw <- dropsRaw[dropsRaw[["Experiment"]] %in% exps, ]

## Remove anthesis.silking.interval.
dropsRaw <- dropsRaw[colnames(dropsRaw) != "anthesis.silking.interval"]

## Add location column.
dropsRaw[["loc"]] <- substring(dropsRaw[["Experiment"]],  first = 1, last = 3)

## Add scenario columns.
scenario <- data.frame(Experiment = exps,
                       scenarioWater = c("WD", "WD", "WW", "WW", "WW",
                                         "WW", "WD", "WW", "WW", "WD"),
                       scenarioTemp = c("Hot", "Hot", "Cool", "Cool",
                                        "Hot(Day)", "Hot(Day)", "Hot(Day)",
                                        "Hot", "Hot", "Hot(Day)"))
dropsRaw <- merge(dropsRaw, scenario)

dropsRaw[["scenarioFull"]] <- interaction(dropsRaw[c("scenarioWater",
                                                     "scenarioTemp")],
                                          drop = TRUE)

## Add genetic groups.
dropsRaw <- merge(dropsRaw, genoMeta[c("Variety_ID", "geneticGroup")])

dropsRaw <- droplevels(dropsRaw)

usethis::use_data(dropsRaw, overwrite = TRUE)
