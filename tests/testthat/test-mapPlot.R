context("mapPlot")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

### TD map plot.

test_that("TD map plot gives correct output types", {
  expect_error(plot(testTD, plotType = "map"),
               "should have latitude and longitude")
  p <- plot(TDHeat05, plotType = "map")
  expect_is(p, "ggplot")
})

test_that(paste("options minLatRange and minLongRange function properly for",
                "TD map plot"), {
  expect_error(plot(TDHeat05, plotType = "map", minLatRange = c(20, 20)),
               "minLatRange should be NULL or a single numerical value")
  expect_error(plot(TDHeat05, plotType = "map", minLongRange = c(20, 20)),
               "minLongRange should be NULL or a single numerical value")
  p <- plot(TDHeat05, plotType = "map", minLatRange = 20, minLongRange = 20,
            output = FALSE)
  expect_equal(p$coordinates$limits$x, c(-6.33333, 17.66667))
  expect_equal(p$coordinates$limits$y, c(39.97, 63.97))
})

test_that("option colorTrialBy functions properly for TD map plot", {
  expect_error(plot(TDHeat05, plotType = "map", colorTrialBy = 1),
               "colorTrialBy should be a character string")
  expect_error(plot(TDHeat05, plotType = "map", colorTrialBy = "grp"),
               "colorTrialBy should be a column in TD")
  expect_error(plot(TDHeat05, plotType = "map", colorTrialBy = "Plot"),
               "colorTrialBy should be unique within each trial")
  expect_error(plot(TDHeat05, plotType = "map", colorTrialBy = "trial",
                    colTrial = c("orange", "red")),
               "Number of colors provided doesn't match number of trial groups")
  expect_silent(p1 <- plot(TDHeat05, plotType = "map", colorTrialBy = "trial"))
  expect_silent(p2 <- plot(TDHeat05, plotType = "map", colorTrialBy = "trial",
                           colTrial = "orange"))
  expect_equal(p1$plot_env$colTrial, "red")
  expect_equal(p2$plot_env$colTrial, "orange")
})

test_that("option title overrides default title in TD map plot", {
  p1 <- plot(TDHeat05, plotType = "map", title = "test")
  expect_equal(p1$labels$title, "test")
})
