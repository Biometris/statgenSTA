context("corPlot")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

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

test_that("option title overrides default title in TD correlation plot", {
  p1 <- plot(TDMaize, plotType = "cor", traits = "yld", title = "test")
  expect_equal(p1$yld$labels$title, "test")
})


