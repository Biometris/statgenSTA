context("layoutPlot")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

test_that("general checks in plot.TD function properly", {
  expect_error(plot(testTD, trials = 2),
               "trials has to be a character vector defining trials in testTD")
})

### TD layout plot.

p0 <- plot(testTD, plotType = "layout")
test_that("TD layout plot gives correct output types", {
  expect_is(p0, "list")
  expect_length(p0, 1)
  expect_is(p0[[1]], "ggplot")
  testTD$E1$colCoord <- NULL
  expect_warning(plot(testTD, plotType = "layout"), "colCoord should be")
  testTD$E1$rowCoord <- NULL
  expect_warning(plot(testTD, plotType = "layout"), "rowCoord should be")
})

test_that("TD layout plot skips plots with missing row or column coordinates", {
  testTD$E1$colCoord[1] <- NA
  expect_warning(plot(testTD),
                 "colCoord contains missing values")
  testTD$E1$rowCoord[1] <- NA
  expect_warning(plot(testTD),
                 "rowCoord contains missing values")
})

test_that("option showGeno functions properly in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", showGeno = TRUE, output = FALSE)
  ## Difference with default plot p0 should be the extra GeomText layer.
  geoms0 <- getGeoms(p0)
  geoms1 <- getGeoms(p1)
  expect_equal(setdiff(geoms1, geoms0), "GeomText")
})

test_that("option sizeGeno functions properly in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", showGeno = TRUE, sizeGeno = 1,
             output = FALSE)
  ## Size of text should be 1.
  geoms1 <- getGeoms(p1)
  expect_equal(p1$E1$layers[[which(geoms1 == "GeomText")]]$aes_params$size, 1)
})

test_that("option highlight functions properly in TD layout plot", {
  expect_error(plot(testTD, plotType = "layout", highlight = 1),
               "highlight should be a character vector")
  p1 <- plot(testTD, plotType = "layout", highlight = "G1", output = FALSE)
  geoms1 <- getGeoms(p1)
  ## Two plots should be highlighted as defined in variable highlight..
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~highlight.", "~color."))
  expect_equal(sum(!is.na(p1[[1]]$data$highlight.)), 2)
})

test_that("specifying custom colors for highlight functions properly in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", highlight = "G1",
             colHighlight = "green", output = FALSE)
  ## Fill color should be equal to specified colors.
  plottedCols <- unique(ggplot2::ggplot_build(p1[[1]])$data[[1]][["fill"]])
  expect_setequal(plottedCols, c("green", NA))
})

test_that("option colorSubBlock functions properly in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", colorSubBlock = TRUE,
             output = FALSE)
  geoms1 <- getGeoms(p1)
  ## Fill should be based on subBlocks.
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~subBlock", "~color."))
})

test_that("specifying custom colors for subBlock functions properly in TD layout plot", {
  cols <- c("green", "blue", "red", "orange", "yellow")
  p1 <- plot(testTD, plotType = "layout", colorSubBlock = TRUE,
             colSubBlock = cols, output = FALSE)
  ## Fill color should be equal to specified colors.
  plottedCols <- unique(ggplot2::ggplot_build(p1[[1]])$data[[1]][["fill"]])
  expect_setequal(cols, plottedCols)
})

test_that("option highlight overrides colorSubBlock in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", highlight = "G1",
             colorSubBlock = TRUE, output = FALSE)
  geoms1 <- getGeoms(p1)
  ## Two plots should be highlighted as defined in variable highlight..
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~highlight.", "~color."))
})

test_that("option title overrides default title in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", title = "test")
  expect_equal(p1$E1$labels$title, "test")
})
