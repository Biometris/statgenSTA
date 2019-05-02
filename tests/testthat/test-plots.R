context("Plots")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

SSA <- fitTD(TD = TDHeat05, design = "res.rowcol", traits = "yield")
test_that("SSA base plot gives correct output types", {
  p1 <- plot(SSA, traits = "yield", output = FALSE)
  expect_is(p1, "list")
  expect_length(p1, 1)
  expect_is(p1[[1]], "list")
  expect_length(p1[[1]], 1)
  expect_is(p1[[1]][[1]], "list")
  expect_length(p1[[1]][[1]], 4)
  lapply(X = p1[[1]][[1]], FUN = expect_is, "ggplot")
})

test_that("SSA spatial plot gives correct output types", {
  p1 <- plot(SSA, plotType = "spatial", traits = "yield", output = FALSE)
  expect_is(p1, "list")
  expect_length(p1, 1)
  expect_is(p1[[1]], "list")
  expect_length(p1[[1]], 1)
  expect_is(p1[[1]][[1]], "list")
  expect_length(p1[[1]][[1]], 6)
  lapply(X = p1[[1]][[1]], FUN = expect_is, "ggplot")
})

test_that("option what in SSA plot functions properly", {
  p1 <- plot(SSA, what = "random", output = FALSE)
  p2 <- plot(SSA, plotType = "spatial", what = "random", output = FALSE)
  expect_is(p1, "list")
  expect_equal(p2[[1]][[1]][[5]]$labels$title, "Genotypic BLUPs")
  expect_equal(p2[[1]][[1]][[6]]$labels$x, "Genotypic BLUPs")
})

p0 <- plot(TDHeat05, plotType = "layout", output = FALSE)
test_that("TD layout plot gives correct output types", {
  expect_warning(plot(TDMaize, plotType = "layout"), "Plot skipped")
  expect_is(p0, "list")
  expect_length(p0, 1)
  expect_is(p0[[1]], "ggplot")
})

test_that("option showGeno functions properly in TD layout plot", {
  p1 <- plot(TDHeat05, plotType = "layout", showGeno = TRUE, output = FALSE)
  ## Difference with default plot p0 should be the extra GeomText layer.
  geoms0 <- sapply(p0[[1]]$layers, function(x) class(x$geom)[1])
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  expect_equal(setdiff(geoms1, geoms0), "GeomText")
})

test_that("option highlight functions properly in TD layout plot", {
  p1 <- plot(TDHeat05, plotType = "layout", highlight = "SB001", output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Two plots should be highlighted as defined in variable highlight..
  expect_equal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
               "~highlight.")
  expect_equal(sum(!is.na(p1[[1]]$data$highlight.)), 2)
})

test_that("option colorSubBlock functions properly in TD layout plot", {
  p1 <- plot(TDHeat05, plotType = "layout", colorSubBlock = TRUE,
             output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Fill should be based on subBlocks.
  expect_equal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
               "~subBlock")
})

test_that("option highlight overrides colorSubBlock in TD layout plot", {
  p1 <- plot(TDHeat05, plotType = "layout", highlight = "SB001",
             colorSubBlock = TRUE, output = FALSE)
  geoms1 <- sapply(p1[[1]]$layers, function(x) class(x$geom)[1])
  ## Two plots should be highlighted as defined in variable highlight..
  expect_equal(as.character(p1[[1]]$layers[geoms1 == "GeomTile"][[1]]$mapping),
               "~highlight.")
})

test_that("TD map plot gives correct output types", {
  expect_error(plot(TDMaize, plotType = "map"),
               "should have latitude and longitude")
  p <- plot(TDHeat05, plotType = "map", output = FALSE)
  expect_is(p, "ggplot")
})

test_that("options minLatRange and minLongRange function properly for TD map plot", {
  p <- plot(TDHeat05, plotType = "map", minLatRange = 20, minLongRange = 20,
            output = FALSE)
  expect_equal(p$coordinates$limits$x, c(-6.33333, 17.66667))
  expect_equal(p$coordinates$limits$y, c(39.97, 63.97))
})

test_that("TD box plot gives correct output types", {
  expect_warning(plot(TDMaize, plotType = "box", traits = "trait"),
                 "trait isn't a column in any of the trials")
  p <- plot(TDMaize, plotType = "box", traits = "yld", output = FALSE)
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "ggplot")
})

test_that("option groupBy functions properly for TD box plot", {
  p <- plot(TDHeat05, plotType = "box", traits = "yield", groupBy = "repId",
            output = FALSE)
  expect_true("~repId" %in% as.character(p$yield$mapping))
})

test_that("option colorBy functions properly for TD box plot", {
  p <- plot(TDHeat05, plotType = "box", traits = "yield", colorBy = "repId",
            output = FALSE)
  expect_true(all(c("~repId", "~trial") %in% as.character(p$yield$mapping)))
})

test_that("option orderBy functions properly for TD box plot", {
  p0 <- plot(TDHeat05, plotType = "box", traits = "yield", output = FALSE)
  p1 <- plot(TDHeat05, plotType = "box", traits = "yield",
             orderBy = "ascending", output = FALSE)
  p2 <- plot(TDHeat05, plotType = "box", traits = "yield",
             orderBy = "descending", output = FALSE)
  ## This basically only checks that releveling took place.
  expect_equal(setdiff(names(p1$yield$plot_env), names(p0$yield$plot_env)),
               "levNw")
  expect_equal(setdiff(names(p2$yield$plot_env), names(p0$yield$plot_env)),
               "levNw")
})

test_that("TD correlation plot gives correct output types", {
  expect_warning(plot(TDMaize, plotType = "cor", traits = "trait"),
                 "trait isn't a column in any of the trials")
  p <- plot(TDMaize, plotType = "cor", traits = "yld", output = FALSE)
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
