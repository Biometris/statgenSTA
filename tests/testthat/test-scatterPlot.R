context("scatterPlot")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

### TD scatter plot.

test_that("TD scatter plot gives correct output types", {
  expect_error(plot(testTD, plotType = "scatter", traits = "trait"),
               "At least two trials are requiered for a scatter plot")
  expect_error(plot(TDMaize, plotType = "scatter", traits = 1),
               "traits should be a character vector")
  expect_warning(plot(TDMaize, plotType = "scatter", traits = "trait"),
                 "has no valid observations for a least two trials")
  p <- plot(TDMaize, plotType = "scatter", traits = "yld")
  expect_is(p, "list")
  expect_length(p, 1)
  expect_is(p[[1]], "gtable")
})

test_that("option colorGenoBy functions properly for TD scatter plot", {
  expect_error(plot(TDMaize, plotType = "scatter", traits = "t1",
                    colorGenoBy = 1),
               "colorGenoBy should be a character string")
  expect_error(plot(TDMaize, plotType = "scatter", traits = "t1",
                    colorGenoBy = "grp"),
               "colorGenoBy should be a column in TD")
})

test_that("option colorTrialBy functions properly for TD scatter plot", {
  expect_error(plot(TDMaize, plotType = "scatter", traits = "yld",
                    colorTrialBy = 1),
               "colorTrialBy should be a character string")
  expect_error(plot(TDMaize, plotType = "scatter", traits = "yld",
                    colorTrialBy = "grp"),
               "colorTrialBy should be a column in TD")
  expect_error(plot(TDMaize, plotType = "scatter", traits = "yld",
                    colorTrialBy = "trial", colTrial = "red"),
               "Number of colors provided doesn't match number of trial groups")
  expect_silent(plot(TDMaize, plotType = "scatter", traits = "yld",
                     colorTrialBy = "trial"))
})

test_that("option addCorr functions properly for TD box plot", {
  expect_error(plot(TDMaize, plotType = "scatter", traits = "t1",
                    addCorr = TRUE),
               "must be NULL or a character vector")
  expect_silent(p <- plot(TDMaize, plotType = "scatter", traits = "yld",
                          addCorr = "tl"))
  ## melting data in the plot function caused an error when trials have a
  ## numerical value. This should not be the case.
  TDMaize2 <- TDMaize
  for (trial in seq_along(TDMaize2)) {
    levels(TDMaize2[[trial]][["trial"]]) <- 1:8
    names(TDMaize2) <- 1:8
  }
  expect_silent(plot(TDMaize2, plotType = "scatter", traits = "yld"))
})

test_that("option trialOrder functions properly for TD scatter plot", {
  expect_error(plot(TDMaize, plotType = "scatter", traits = "t1",
                    trialOrder = 1),
               "trials and trialOrder should contain exactly the same trials")
  expect_silent(p <- plot(TDMaize, plotType = "scatter", traits = "yld",
                          trialOrder = rev(names(TDMaize))))
})

test_that("scatterPlot works correctly for trials with non-syntactic names", {
  TDMaize2 <- TDMaize
  levels(TDMaize2$HN96b[["trial"]])[1] <- "HN-96b"
  names(TDMaize2)[1] <- "HN-96b"
  expect_silent(plot(TDMaize2, plotType = "scatter", traits = "yld"))
})

test_that("option title overrides default title in TD scatter plot", {
  p1 <- plot(TDMaize, plotType = "scatter", traits = "yld", title = "test")
  titleGrob <- which(p1$yld$layout$name == "title")
  expect_equal(p1$yld$grobs[[titleGrob]]$children[[1]]$label, "test")
})
