context("STA Plots")

## Testing the exact plot output is difficult but since also the ggplot
## objects on which the plots are based are invisibly returned at least some
## checking can be done.

### STA plots.

modelSp <- fitTD(TD = testTD, design = "rowcol", traits = "t1")
test_that("checks in plot.STA function properly", {
  expect_error(plot(modelSp, trials = 2),
               "trials has to be a character vector defining trials in modelSp")
  expect_error(plot(modelSp, traits = 1),
               "traits should be NULL or a character vector")
  expect_error(plot(modelSp, traits = "t2", outCols = 0),
               "a single numerical value greater than or equal to 1")
  expect_warning(plot(modelSp, traits = "myTr"),
                 "The following traits are not modeled for E1: myTr")
  modelSp$E1$mRand <- NULL
  expect_warning(plot(modelSp, what = "random"),
                 "No model with genotype random for trial E1 and trait t1")
  modelSp$E1$TD$E1$rowCoord <- NULL
  expect_warning(plot(modelSp, plotType = "spatial"),
                 "rowCoord should be a column in E1")
})

### STA base plot.

test_that("STA base plot gives correct output types", {
  p1 <- plot(modelSp, traits = "t1")
  expect_is(p1, "list")
  expect_length(p1, 1)
  expect_is(p1[[1]], "list")
  expect_length(p1[[1]], 1)
  expect_is(p1[[1]][[1]], "list")
  expect_length(p1[[1]][[1]], 4)
  lapply(X = p1[[1]][[1]], FUN = expect_is, "ggplot")
})

### STA spatial plot.

test_that("STA spatial plot gives correct output types", {
  p1 <- plot(modelSp, plotType = "spatial", traits = "t1")
  expect_is(p1, "list")
  expect_length(p1, 1)
  expect_is(p1[[1]], "list")
  expect_length(p1[[1]], 1)
  expect_is(p1[[1]][[1]], "list")
  expect_length(p1[[1]][[1]], 6)
  lapply(X = p1[[1]][[1]], FUN = expect_is, "ggplot")
})

test_that("option what in STA plot functions properly", {
  p1 <- plot(modelSp, what = "random", output = FALSE)
  p2 <- plot(modelSp, plotType = "spatial", what = "random", output = FALSE)
  expect_is(p1, "list")
  expect_equal(p2[[1]][[1]][[5]]$labels$title, "Genotypic BLUPs")
  expect_equal(p2[[1]][[1]][[6]]$labels$x, "Genotypic BLUPs")
})

test_that("option spaTrend in STA plot functions properly", {
  expect_silent(p <- plot(modelSp, plotType = "spatial",
                          spaTrend = "percentage", output = FALSE))
})
