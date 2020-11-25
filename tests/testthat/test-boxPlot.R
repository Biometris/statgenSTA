context("boxPlot")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

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

test_that("option colorTrialBy functions properly for TD box plot", {
  expect_error(plot(testTD, plotType = "box", traits = "t1", colorTrialBy = 1),
               "colorTrialBy should be a character string")
  expect_error(plot(testTD, plotType = "box", traits = "t1",
                    colorTrialBy = "grp"),
               "colorTrialBy should be a column in TD")
  expect_error(plot(testTD, plotType = "box", traits = "t1",
                    colorTrialBy = "repId", colTrial = "red"),
               "Number of colors provided doesn't match number of trial groups")
  p <- plot(testTD, plotType = "box", traits = "t1", colorTrialBy = "repId",
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

test_that("option title overrides default title in TD box plot", {
  p1 <- plot(testTD, plotType = "box", traits = "t1", title = "test")
  expect_equal(p1$t1$labels$title, "test")
})

