context("Plots")

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
  geoms0 <- sapply(p0[[1]]$layers, function(x) class(x$geom)[1])
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  expect_equal(setdiff(geoms1, geoms0), "GeomText")
})

test_that("option highlight functions properly in TD layout plot", {
  expect_error(plot(testTD, plotType = "layout", highlight = 1),
               "highlight should be a character vector")
  p1 <- plot(testTD, plotType = "layout", highlight = "G1", output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Two plots should be highlighted as defined in variable highlight..
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~highlight.", "~color."))
  expect_equal(sum(!is.na(p1[[1]]$data$highlight.)), 2)
})

test_that("option colorSubBlock functions properly in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", colorSubBlock = TRUE,
             output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Fill should be based on subBlocks.
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~subBlock", "~color."))
})

test_that("option highlight overrides colorSubBlock in TD layout plot", {
  p1 <- plot(testTD, plotType = "layout", highlight = "G1",
             colorSubBlock = TRUE, output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Two plots should be highlighted as defined in variable highlight..
  expect_setequal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
                  c("~highlight.", "~color."))
})

### TD map plot.

test_that("TD map plot gives correct output types", {
  expect_error(plot(testTD, plotType = "map"),
               "should have latitude and longitude")
  p <- plot(TDHeat05, plotType = "map")
  expect_is(p, "ggplot")
})

test_that("options minLatRange and minLongRange function properly for TD map plot", {
  expect_error(plot(TDHeat05, plotType = "map", minLatRange = c(20, 20)),
               "minLatRange should be a single numerical value")
  expect_error(plot(TDHeat05, plotType = "map", minLongRange = c(20, 20)),
               "minLongRange should be a single numerical value")
  p <- plot(TDHeat05, plotType = "map", minLatRange = 20, minLongRange = 20,
            output = FALSE)
  expect_equal(p$coordinates$limits$x, c(-6.33333, 17.66667))
  expect_equal(p$coordinates$limits$y, c(39.97, 63.97))
})

### TD box plot.

test_that("TD box plot gives correct output types", {
  expect_error(plot(testTD, plotType = "box", traits = 1),
               "traits should be a character vector")
  expect_warning(plot(testTD, plotType = "box", traits = "trait"),
                 "trait isn't a column in any of the trials")
  p <- plot(testTD, plotType = "box", traits = "t1")
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "ggplot")
})

test_that("option groupBy functions properly for TD box plot", {
  expect_error(plot(testTD, plotType = "box", traits = "t1", groupBy = 1),
               "groupBy should be a character string")
  expect_error(plot(testTD, plotType = "box", traits = "t1", groupBy = "grp"),
               "groupBy should be a column in TD")
  p <- plot(testTD, plotType = "box", traits = "t1", groupBy = "repId",
            output = FALSE)
  expect_true("~repId" %in% as.character(p$t1$mapping))
})

test_that("option colorBy functions properly for TD box plot", {
  expect_error(plot(testTD, plotType = "box", traits = "t1", colorBy = 1),
               "colorBy should be a character string")
  expect_error(plot(testTD, plotType = "box", traits = "t1", colorBy = "grp"),
               "colorBy should be a column in TD")
  p <- plot(testTD, plotType = "box", traits = "t1", colorBy = "repId",
            output = FALSE)
  expect_true(all(c("~repId", "~trial") %in% as.character(p$t1$mapping)))
})

test_that("option orderBy functions properly for TD box plot", {
  p0 <- plot(testTD, plotType = "box", traits = "t1", output = FALSE)
  p1 <- plot(testTD, plotType = "box", traits = "t1",
             orderBy = "ascending", output = FALSE)
  p2 <- plot(testTD, plotType = "box", traits = "t1",
             orderBy = "descending", output = FALSE)
  ## This basically only checks that releveling took place.
  expect_equal(setdiff(names(p1$t1$plot_env), names(p0$t1$plot_env)),
               "levNw")
  expect_equal(setdiff(names(p2$t1$plot_env), names(p0$t1$plot_env)),
               "levNw")
})

### TD correlation plot.

test_that("TD correlation plot gives correct output types", {
  expect_error(plot(testTD, plotType = "cor", traits = "trait"),
               "At least two trials requiered for a correlation plot")
  expect_error(plot(TDMaize, plotType = "cor", traits = 1),
               "traits should be a character vector")
  expect_warning(plot(TDMaize, plotType = "cor", traits = "trait"),
                 "trait isn't a column in any of the trials")
  p <- plot(TDMaize, plotType = "cor", traits = "yld")
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "ggplot")
})

## melting data in the plot function caused an error when trials have a
## numerical value. This should not be the case.
test_that("TD correlation plot gives correct output types", {
  expect_warning(plot(TDMaize, plotType = "cor", traits = "trait"),
                 "trait isn't a column in any of the trials")
  TDMaize2 <- TDMaize
  for (trial in seq_along(TDMaize2)) {
    levels(TDMaize2[[trial]][["trial"]]) <- 1:8
  }
  expect_silent(p <- plot(TDMaize2, plotType = "cor", traits = "yld",
                          output = FALSE))
})
