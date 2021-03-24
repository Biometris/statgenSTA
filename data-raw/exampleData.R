## Create TDHeat05.

# Read raw data
SB_Yield <- read.csv("./data-raw/SB_yield.csv", stringsAsFactors = FALSE,
                     na.strings = c("NA", "*"))
# Restrict to HEAT05
Heat05 <- SB_Yield[SB_Yield[["Env"]] == "HEAT05", ]
# Create object of class TD
TDHeat05 <- createTD(data = Heat05, genotype = "Genotype", trial = "Env",
                     repId = "Rep", subBlock = "Subblock", rowCoord = "Row",
                     colCoord = "Column", trLong = 5.66667, trLat = 51.97,
                     trLocation = "Wageningen")
# Export to package
usethis::use_data(TDHeat05, overwrite = TRUE)

## Create TDMaize.

# Read raw data
F2Maize <- read.csv("./data-raw/F2maize_pheno.csv", stringsAsFactors = FALSE)
# Add column regime
F2Maize[["regime"]] <- factor(substr(F2Maize[["env."]], 1, 2),
                              levels = c("NS", "IS", "SS", "LN", "HN"))
# Reorder columns.
F2Maize <- F2Maize[, c("env.", "genotype.", "regime", "yld")]
# Create object of class TD
TDMaize <- createTD(data = F2Maize, genotype = "genotype.", trial = "env.")
# Export to package
usethis::use_data(TDMaize, overwrite = TRUE)

## Create a dataset for unit testing.
set.seed(1)
testData <- data.frame(seed = rep(x = paste0("G", rep(x = 1:15, times = 2)),
                                  times = 3),
                       family = paste0("F", 1:90),
                       field = rep(x = paste0("E", 1:3), each = 30),
                       rep = rep(x = c(1, 2), each = 15),
                       checkId = sample.int(n = 2, size = 90, replace = TRUE),
                       X = rep(x = rep(x = 1:3, each = 10), times = 3),
                       Y = as.numeric(replicate(n = 9,
                                                expr = sample.int(n = 10))),
                       block = rep(x = 1:5, times = 18),
                       t1 = rnorm(n = 90, mean = 80, sd = 25),
                       t2 = rnorm(n = 90, mean = 3, sd = 0.5),
                       t3 = rnorm(n = 90, mean = 60, sd = 10),
                       t4 = rnorm(n = 90, mean = 80, sd = 25)
)
## Add some random NAs to traits t3 and t4.
testData$t3[sample.int(n = 90, size = 15)] <- NA
testData$t4[sample.int(n = 90, size = 15)] <- NA

## Create TD object for unit testing.
testTD <- createTD(data = testData[testData[["field"]] == "E1", ],
                   trial = "field", genotype = "seed", repId = "rep",
                   subBlock = "block", rowCoord = "Y", colCoord = "X")

## Read extractOptions and save as data.frame.
# Column asDataFrame contains 1 for data.frame like outputs that can simply
# be bound together.
# It contains 2 for nested list like outputs that need conversion from nested
# list to data.frame with trial x trait.
extractOptions <- read.csv("data-raw/extractOptions.csv",
                           stringsAsFactors = FALSE)

## Export all internal data in one go to package.
usethis::use_data(testData, testTD, extractOptions,
                  overwrite = TRUE, internal = TRUE)

