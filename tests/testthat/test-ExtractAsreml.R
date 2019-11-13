context("Extract asreml")

if (requireNamespace("asreml", quietly = TRUE)) {
  modelAs <- fitTD(testTD, design = "res.ibd", traits = "t1", engine = "asreml")

  test_that("the output of extract is of the proper type - asreml", {
    expect_is(extract(modelAs, what = "BLUEs"), "list")
    expect_is(extract(modelAs), "list")
    expect_length(extract(modelAs)[["E1"]], 23)
    expect_is(extract(modelAs, what = c("BLUEs", "BLUPs")), "list")
    expect_length(extract(modelAs, what = c("BLUEs", "BLUPs"))[[1]], 2)
  })

  extAs <- extract(modelAs)[["E1"]]
  test_that("BLUEs are computed correctly", {
    expect_is(extAs$BLUEs, "data.frame")
    expect_equal(dim(extAs$BLUEs), c(15, 2))
    expect_named(extAs$BLUEs, c("genotype", "t1"))
    expect_equal(extAs$BLUEs$t1,
                 c(95.3095904797629, 71.490232749484, 67.3353174166737,
                   76.5265375574466, 65.1245631058874, 88.1527137958988,
                   69.6010609226447, 104.057191025005, 105.882210564877,
                   93.5549875917668, 120.862619188253, 79.0343746452041,
                   58.2720382647675, 90.7377103038097, 56.558637897391))
  })

  test_that("SE of BLUEs are computed correctly", {
    expect_is(extAs$seBLUEs, "data.frame")
    expect_equal(dim(extAs$seBLUEs), c(15, 2))
    expect_named(extAs$seBLUEs, c("genotype", "t1"))
    expect_equal(extAs$seBLUEs$t1, rep(x = 11.2959379047307, times = 15))
  })

  test_that("BLUPs are computed correctly", {
    expect_is(extAs$BLUPs, "data.frame")
    expect_equal(dim(extAs$BLUPs), c(15, 2))
    expect_named(extAs$BLUPs, c("genotype", "t1"))
    expect_equal(extAs$BLUPs$t1,
                 c(90.8474030156004, 75.5471294258374, 72.8782441613928,
                   78.7821875251628, 71.4581678779281, 86.2502140681783,
                   74.3336276993471, 96.4663891629683, 97.638674519251,
                   89.7203423461482, 107.261278098397, 80.3930852909488,
                   67.0564852395517, 87.9106672768472, 65.9558898013124))
  })

  test_that("SE of BLUPs are computed correctly", {
    expect_is(extAs$seBLUPs, "data.frame")
    expect_equal(dim(extAs$seBLUPs), c(15, 2))
    expect_named(extAs$seBLUPs, c("genotype", "t1"))
    expect_equal(extAs$seBLUPs$t1, rep(x = 9.21978979317836, times = 15))
  })

  test_that("unit errors are computed correctly", {
    expect_is(extAs$ue, "data.frame")
    expect_equal(dim(extAs$ue), c(15, 2))
    expect_named(extAs$ue, c("genotype", "t1"))
    expect_equal(extAs$ue$t1, rep(x = 127.59821314688, times = 15))
  })

  test_that("heritability is computed correctly", {
    expect_is(extAs$heritability, "numeric")
    expect_length(extAs$heritability, 1)
    expect_named(extAs$heritability, "t1")
    expect_equivalent(extAs$heritability, 0.642345563238147)
  })

  test_that("heritability can be coerced to data.frame correctly", {
    herit <- as.data.frame(extract(modelAs))
    expect_is(herit, "data.frame")
    expect_named(herit, c("trial", "t1"))
    expect_equal(herit[1, 2], 0.642345563238147)
  })

  test_that("varGen is computed correctly", {
    expect_is(extAs$varGen, "numeric")
    expect_length(extAs$varGen, 1)
    expect_named(extAs$varGen, "t1")
    expect_equivalent(extAs$varGen, 229.165595904912)
  })

  test_that("varErr is computed correctly", {
    expect_is(extAs$varErr, "numeric")
    expect_length(extAs$varErr, 1)
    expect_named(extAs$varErr, "t1")
    expect_equivalent(extAs$varErr, 255.195914107909)
  })

  test_that("fitted values are computed correctly", {
    expect_is(extAs$fitted, "data.frame")
    expect_equal(dim(extAs$fitted), c(30, 3))
    expect_named(extAs$fitted, c("genotype", "repId", "t1"))
    expect_equal(extAs$fitted$t1,
                 c(99.7867011260781, 108.327680923931, 75.7607401870932,
                   54.0015483658409, 99.5801080078541, 73.8715683602538,
                   91.0390729516717, 60.8540246982061, 95.0082487114911,
                   74.7638571171129, 72.25604765852, 60.8291404436402,
                   116.592111750644, 110.152748972559, 69.3951015135688,
                   101.611672157196, 125.133126625862, 71.6058349447649,
                   89.2844850455176, 63.0647998885825, 62.5425281636941,
                   86.4671718961283, 97.825490138016, 80.7970274563732,
                   67.2197253118749, 65.3305534850356, 92.423216342148,
                   52.2881353511417, 83.8822112496495, 83.3048921732953))
  })

  test_that("residuals are computed correctly", {
    expect_is(extAs$residF, "data.frame")
    expect_equal(dim(extAs$residF), c(30, 3))
    expect_named(extAs$residF, c("genotype", "repId", "t1"))
    expect_equal(extAs$residF$t1,
                 c(10.4099940235012, -10.4099940235012, -0.945258777142314,
                   -5.91635357729179, 24.6020737264621, 1.69516958300599,
                   -24.6020737264629, -3.62706591201722, 20.7988072520505,
                   19.1983035220197, 8.79684917008586, 2.90345072360062,
                   3.078724612877, -7.39839323517792, 3.62706591201722,
                   7.39839323517791, -3.07872461287698, -1.42603318081447,
                   8.2208561923574, 1.42603318081444, 5.91635357729178,
                   -20.7988072520505, -8.22085619235742, -8.79684917008588,
                   0.945258777142328, -1.69516958300599, -0.0685080604523449,
                   -2.90345072360063, 0.0685080604523307, -19.1983035220197))
  })

  test_that("standardized residuals are computed correctly", {
    expect_is(extAs$stdResF, "data.frame")
    expect_equal(dim(extAs$stdResF), c(30, 3))
    expect_named(extAs$stdResF, c("genotype", "repId", "t1"))
    expect_equal(extAs$stdResF$t1,
                 c(0.953916675431384, -0.953916675431386, -0.0866185041104054,
                   -0.542143282924636, 2.25440363605238, 0.155336355550531,
                   -2.25440363605246, -0.332365095364224, 1.90589245507956,
                   1.75923077653106, 0.806096631338637, 0.266056834930289,
                   0.282118005126064, -0.677949541805918, 0.332365095364224,
                   0.677949541805917, -0.282118005126063, -0.130674122177821,
                   0.753315687838944, 0.130674122177818, 0.542143282924635,
                   -1.90589245507956, -0.753315687838945, -0.806096631338638,
                   0.0866185041104067, -0.155336355550531, -0.00627771554137491,
                   -0.26605683493029, 0.0062777155413736, -1.75923077653106))
  })

  test_that("rMeans are computed correctly", {
    expect_is(extAs$rMeans, "data.frame")
    expect_equal(dim(extAs$rMeans), c(30, 3))
    expect_named(extAs$rMeans, c("genotype", "repId", "t1"))
    expect_equal(extAs$rMeans$t1,
                 c(92.1958935229209, 100.736873320774, 79.8176446812092,
                   62.7859900790058, 95.1179165099279, 78.6041429745039,
                   86.5768814537454, 67.1876372348313, 92.1812131808703,
                   76.1225638995418, 74.5116921734401, 70.2263867665892,
                   102.99077796148, 101.909220264668, 75.7287140501941,
                   93.3681434493051, 111.531792836698, 77.148757948691,
                   85.4498338314689, 68.6077228925085, 71.326969876859,
                   83.6401363655075, 93.9908389239674, 83.0526719712934,
                   71.2766298059908, 70.0631280992855, 90.5207107025748,
                   61.6853816740906, 81.9797056100763, 84.6635989557244))
  })

  test_that("random effects are computed correctly", {
    expect_is(extAs$ranEf, "data.frame")
    expect_equal(dim(extAs$ranEf), c(15, 2))
    expect_named(extAs$ranEf, c("genotype", "t1"))
    expect_equal(extAs$ranEf$t1,
                 c(8.01408398129642, -7.28618960774253, -9.95507487206082,
                   -4.05113150857021, -11.3751511554583, 3.41689503409189,
                   -8.4996913341754, 13.6330701283984, 14.8053554846257,
                   6.88702331189755, 24.427959063316, -2.44023374286049,
                   -15.7768337936265, 5.07734824268224, -16.8774292318137))
  })

  test_that("residuals are computed correctly for genotype random", {
    expect_is(extAs$residR, "data.frame")
    expect_equal(dim(extAs$residR), c(30, 3))
    expect_named(extAs$residR, c("genotype", "repId", "t1"))
    expect_equal(extAs$residR$t1,
                 c(18.0008014050288, -2.81918664197362, -5.00216315280836,
                   -14.7007950339765, 29.0642650941052, -3.03740489306652,
                   -20.1398823588198, -9.96067826371926, 23.6258427001301,
                   17.839596779261, 6.54120472102399, -6.49379532497607,
                   16.6800580049217, 0.845135232025754, -2.70654643968489,
                   15.6419217023817, 10.5226087791677, -6.96895602290334,
                   12.0555072944453, -4.11688966127437, -2.86808787939299,
                   -17.9717718039708, -4.38620509026956, -11.0524936191478,
                   -3.11164559852367, -6.42774405907845, 1.83399752357317,
                   -12.3006967721773, 1.97101364447791, -20.5570102647785))
  })

  test_that("standardized residuals are computed correctly for genotype random", {
    expect_is(extAs$stdResR, "data.frame")
    expect_equal(dim(extAs$stdResR), c(30, 3))
    expect_named(extAs$stdResR, c("genotype", "repId", "t1"))
    expect_equal(extAs$stdResR$t1,
                 c(1.41565449006919, -0.221712010224678, -0.393389974618067,
                   -1.15612889315454, 2.28572918133451, -0.238873587955739,
                   -1.58388027920206, -0.783347289882539, 1.85803005391855,
                   1.40297669788351, 0.514426297928484, -0.510697861599377,
                   1.31178600049098, 0.066464808150031, -0.212853569589573,
                   1.23014282870732, 0.82753975276263, -0.5480663760781,
                   0.94809295407525, -0.323768556761212, -0.225557835238294,
                   -1.41337146309429, -0.344948569552087, -0.869211973786119,
                   -0.244712170163276, -0.50550331716259, 0.144232849264527,
                   -0.967375640601468, 0.155008340867269, -1.61668488678822))
  })

  test_that("Waldtest is computed correctly", {
    expect_is(extAs$wald$t1$Ftest, "numeric")
    expect_length(extAs$wald$t1$Ftest, 4)
    expect_equivalent(unlist(extAs$wald$t1$Ftest),
                      c(2.796, 14, 14, 0.0320877415537768))
  })

  test_that("CV is computed correctly", {
    expect_is(extAs$CV, "numeric")
    expect_length(extAs$CV, 1)
    expect_named(extAs$CV, "t1")
    expect_equivalent(extAs$CV, 19.285543536136)
  })

  test_that("rDfF is computed correctly", {
    expect_is(extAs$rDfF, "integer")
    expect_length(extAs$rDfF, 1)
    expect_named(extAs$rDfF, "t1")
    expect_equivalent(extAs$rDfF, 14)
  })

  test_that("rDfR is computed correctly", {
    expect_is(extAs$rDfR, "integer")
    expect_length(extAs$rDfR, 1)
    expect_named(extAs$rDfR, "t1")
    expect_equivalent(extAs$rDfR, 28)
  })

  test_that("sed is computed correctly", {
    expect_is(extAs$sed$t1, "numeric")
    expect_length(extAs$sed$t1, 3)
    expect_equivalent(extAs$sed$t1,
                      c(15.974855804714, 15.9748667588973, 15.9748685845945))
  })

  test_that("lsd is computed correctly", {
    expect_is(extAs$lsd$t1, "numeric")
    expect_length(extAs$lsd$t1, 3)
    expect_equivalent(extAs$lsd$t1,
                      c(34.2626580713571, 34.2626815657436, 34.2626854814746))
  })

  test_that("correct attributes are added", {
    expect_equal(attr(x = extAs, which = "traits"), "t1")
    expect_equal(attr(x = extAs, which = "design"), "res.ibd")
    expect_equal(attr(x = extAs, which = "engine"), "asreml")
  })

  test_that("calculated values are logically correct", {
    ## Fitted + residuals should match raw data.
    expect_equal(testTD[["E1"]]$t1, extAs$fitted$t1 + extAs$residF$t1)
    expect_equal(testTD[["E1"]]$t1, extAs$rMeans$t1 + extAs$residR$t1)
  })
}

