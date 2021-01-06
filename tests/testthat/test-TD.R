context("Class TD")

### Create.

test_that("input checks for createTD work correctly", {
  expect_error(createTD(data = 1), "has to be a data.frame")
})

test_that("createTD creates objects of class TD", {
  expect_is(createTD(data = testData), "TD")
})

test_that("renaming columns 'one to one' works properly in createTD", {
  testData2 <- testData
  colnames(testData2)[colnames(testData2) == "block"] <- "trial"
  expect_error(createTD(data = testData, genotype = "a"), "has to be NULL or")
  expect_error(createTD(data = testData2, trial = "field"),
               "following columns already exist in the input data")

  expect_equal(colnames(createTD(data = testData, genotype = "seed")[[1]])[1:2],
               c("genotype", "family"))
  expect_equal(colnames(createTD(data = testData, genotype = "seed",
                                 trial = "field")[[1]])[1:3],
               c("genotype", "family", "trial"))
})

test_that("renaming columns 'one to many' works properly in createTD", {
  expect_equal(ncol(createTD(data = testData, rowId = "Y",
                             rowCoord = "Y")[[1]]), ncol(testData) + 1)
  expect_equal(colnames(createTD(data = testData, rowId = "Y",
                                 rowCoord = "Y")[[1]])[c(7, 13)],
  c("rowId", "rowCoord"))
})

test_that("class conversion works properly in createTD", {
  expect_is(createTD(data = testData)[[1]][, "Y"], "numeric")
  expect_is(createTD(data = testData, rowId = "Y")[[1]][, "rowId"], "factor")
  expect_is(createTD(data = testData,
                     rowCoord = "Y")[[1]][, "rowCoord"], "numeric")
  expect_is(createTD(data = testData)[[1]][, "checkId"], "factor")
})

test_that("attribute renamed is properly filled in createTD", {
  expect_null(attr(createTD(data = testData)[[1]], "renamed"))
  expect_is(attr(createTD(data = testData, genotype = "seed")[[1]], "renamed"),
            "data.frame")
  expect_equal(attr(createTD(data = testData, rowId = "Y",
                             rowCoord = "Y")[[1]], "renamed"),
               data.frame(orig = c("Y", "Y"),
                          new = c("rowId", "rowCoord"),
                          stringsAsFactors = FALSE))
})

test_that("attribute design is properly filled in createTD", {
  expect_null(attr(createTD(data = testData)[["testData"]], "design"))
  expect_equal(attr(createTD(data = testData, trDesign = "rcbd")[["testData"]],
                    "trDesign"), "rcbd")
  expect_error(createTD(data = testData, trDesign = "abc"), "should be one of")
})

test_that("meta data can be added from function arguments", {
  ## Single trial
  testMeta <- getMeta(createTD(data = testData, trPlWidth = 5,
                               trPlLength = 2, trLocation = "loc1"))
  expect_equal(testMeta[["trLocation"]], "loc1")
  expect_equal(testMeta[["trPlWidth"]], 5)
  expect_equal(testMeta[["trPlLength"]], 2)
  ## Multiple trials.
  testMeta2 <- getMeta(createTD(data = testData, trial = "field", trPlWidth = 5,
                               trPlLength = 2, trLocation = "loc1"))
  expect_equal(testMeta2[["trLocation"]], rep("loc1", times = 3))
  expect_equal(testMeta2[["trPlWidth"]], c(5, 5, 5))
  expect_equal(testMeta2[["trPlLength"]], c(2, 2, 2))
})

test_that("meta data can be added from columns in input data", {
  testData2 <- testData
  testData2[["lat"]] <- rep(c(10, 20 ,30), each = 30)
  testData2[["long"]] <- rep(c(10, 20 ,30), each = 30)
  testData2[["date"]] <- rep(as.Date("2000/1/1") + 0:2, each = 30)
  testData2[["design"]] <- rep(c("ibd", "rowcol" ,"res.rowcol"), each = 30)
  expect_error(createTD(data = testData2, trLat = "lat"),
               "trLat not unique for testData2")
  expect_error(createTD(data = testData2, trLong = "long"),
               "trLong not unique for testData2")
  expect_error(createTD(data = testData2, trDate = "date"),
               "trDate not unique for testData2")
  expect_error(createTD(data = testData2, trDesign = "design"),
               "trDesign not unique for testData2")
  testMeta <- getMeta(createTD(data = testData2, trial = "field", trLat = "lat",
                               trLong = "long", trDate = "date",
                               trDesign = "design"))
  expect_equal(testMeta[["trLat"]], c(10, 20 ,30))
  expect_equal(testMeta[["trLong"]], c(10, 20 ,30))
  expect_equal(testMeta[["trDate"]], as.Date("2000/1/1") + 0:2)
  expect_equal(testMeta[["trDesign"]], c("ibd", "rowcol" ,"res.rowcol"))
})

test_that("row and column are tested for uniqueness in createTD", {
  testData2 <- testData
  testData2[testData2[["Y"]] == 1 & testData2[["X"]] == 1, "Y"] <- 2
  expect_warning(createTD(data = testData2, rowCoord = "Y", colCoord = "X"),
                 "Combinations of row and column coordinates should be unique")
  expect_warning(createTD(data = testData2, trial = "field",
                        rowCoord = "Y", colCoord = "X"),
                 "row and column coordinates should be unique within trials")
})

test_that("createTD accepts tibbles as input", {
  ## Skip on cran since it needs package tibble as extra dependency.
  skip_if_not_installed("tibble")
  expect_is(createTD(data = tibble::as_tibble(testData)), "TD")
})

### Drop.

test_that("dropTD functions properly", {
  testTD <- createTD(data = testData, trial = "field")
  expect_warning(dropTD(TD = testTD, rmTrials = "E4"),
                "The following trials are not in TD")
  expect_warning(dropTD(TD = testTD, rmTrials = c("E1", "E2", "E3")),
                 "All trials have been removed from TD")
  testTDrm <- dropTD(TD = testTD, rmTrials = "E1")
  expect_is(testTDrm, "TD")
  expect_named(testTDrm, c("E2", "E3"))
})

### Add.

test_that("addTD functions properly", {
  testTD <- createTD(data = testData[testData[["field"]] %in% c("E1" ,"E2"), ],
                     trial = "field")
  testTD2 <- addTD(testTD, data = testData[testData[["field"]] == "E3", ],
                   trial = "field")
  expect_is(testTD2, "TD")
  expect_equal(testTD2, createTD(data = testData, trial = "field"))
  expect_warning(addTD(testTD, data = testData[testData[["field"]] == "E1", ],
                       trial = "field"),
                 "already existed in TD and will be added again: E1")
})

### getMeta.

test_that("getMeta functions properly", {
  expect_error(getMeta(), "TD should be an object of class TD")
  TD1 <- createTD(data = testData)
  meta1 <- getMeta(TD1)
  ## No trial defined, so only 1 row in meta
  expect_equal(nrow(meta1), 1)
  expect_equal(rownames(meta1), "testData")
  TD2 <- createTD(data = testData, trial = "field")
  meta2 <- getMeta(TD2)
  expect_equal(nrow(meta2), 3)
  expect_equal(rownames(meta2), c("E1", "E2", "E3"))
  expect_equal(meta2$trLocation, c("E1", "E2", "E3"))
})

### setMeta.

test_that("setMeta functions properly", {
  expect_error(setMeta(), "TD should be an object of class TD")
  TD1 <- createTD(data = testData, trial = "field")
  expect_error(setMeta(TD1), "meta should be a data.frame")
  meta1 <- getMeta(TD1)
  meta1$trDesign <- c("res.rowcol", "rowcol", "res.ibd")
  TD2 <- setMeta(TD = TD1, meta = meta1)
  expect_equal(attr(x = TD2$E1, which = "trDesign"), "res.rowcol")
  rownames(meta1)[1] <- "E4"
  expect_warning(setMeta(TD1, meta = meta1),
                 "The following trials in meta are not in TD: E4")
  meta1$trDesign[2] <- "trDes"
  expect_error(suppressWarnings(setMeta(TD1, meta = meta1)), "Error for E2:")
})

### checkTDMeta.

test_that("checkTDMeta functions properly", {
  expect_error(checkTDMeta(trDesign = "trDes"), "should be one of")
  expect_silent(checkTDMeta(trDesign = "rowcol"))
  expect_error(checkTDMeta(trLat = 145), "between -90 and 90")
  expect_silent(checkTDMeta(trLat = 45))
  expect_error(checkTDMeta(trLong = -200), "between -180 and 180")
  expect_silent(checkTDMeta(trLong = 120))
  expect_warning(checkTDMeta(trLat = 0, trLong = 0),
               "latitude and longitude should all match a known land location")
  expect_silent(checkTDMeta(trLat = 53, trLong = 0))
  expect_error(checkTDMeta(trPlWidth = c(-1, 1, 2)), "a positive numerical")
  expect_silent(checkTDMeta(trPlWidth = c(1, 1, 2)))
  expect_error(checkTDMeta(trPlLength = c(-1, 1, 2)), "a positive numerical")
  expect_silent(checkTDMeta(trPlLength = c(1, 1, 2)))
})

### Summary.

test_that("check in summary.TD function correctly", {
  expect_error(summary(testTD, trial = "E2"),
               "trial should be a single character string in testTD")
  expect_error(summary(testTD, trial = "E1", traits = "t5"),
               "All traits should be columns in trial")
  expect_error(summary(testTD, trial = "E1", traits = "t1", groupBy = "grp"),
               "groupBy should be a single character string indicating")
  expect_error(summary(testTD, trial = "E1", traits = "t1", what = "st1"),
               "At least one statistic should be chosen")
})

test_that("summary.TD produces correct output", {
  sumTD <- summary(createTD(data = testData), traits = c("t1", "t4"),
                   what = "all")
  expect_is(sumTD, "array")
  expect_equal(mean(testData$t1), sumTD["Mean", "t1", 1])
  expect_equal(max(testData$t4, na.rm = TRUE), sumTD["Max", "t4", 1])
})

test_that("option groupBy in summary.TD produces correct output", {
  sumTD <- summary(createTD(data = testData), traits = c("t1", "t4"),
                   groupBy = "field")
  expect_equal(dim(sumTD), c(3, 2, 3))
  expect_equivalent(as.numeric(by(data = testData$t1, INDICES = testData$field,
                                  FUN = mean)), sumTD["Mean", "t1", ])
  expect_equivalent(sumTD["Number of observations", "t4", ], c(24, 25, 26))
})

test_that("print.summary.TD produces correct output", {
  sumTD <- capture.output(print(summary(createTD(data = testData),
                                        traits = c("t1", "t4"), what = "all")))
  expect_true(all(c("Summary statistics for t1 in testData  ",
                    "Summary statistics for t4 in testData  ") %in% sumTD))
})

test_that("option groupBy in print.summary.TD produces correct output", {
  sumTD <- capture.output(print(summary(createTD(data = testData),
                                        traits = c("t1", "t4"),
                                        groupBy = "field")))
  expect_true(all(c("Summary statistics for t1 in testData grouped by field ",
                    "Summary statistics for t4 in testData grouped by field ") %in% sumTD))
})

