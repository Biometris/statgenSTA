context("extractSTA asreml")

if (requireNamespace("asreml", quietly = TRUE)) {
  modelAs <- fitTD(testTD, design = "res.ibd", traits = "t1", engine = "asreml")

  test_that("the output of extractSTA is of the proper type - asreml", {
    expect_is(extractSTA(modelAs, what = "BLUEs"), "data.frame")
    expect_is(extractSTA(modelAs), "list")
    expect_length(extractSTA(modelAs)[["E1"]], 23)
    expect_is(extractSTA(modelAs, what = c("BLUEs", "BLUPs")), "list")
    expect_length(extractSTA(modelAs, what = c("BLUEs", "BLUPs"))[[1]], 2)
  })

  extAs <- extractSTA(modelAs)[["E1"]]
  test_that("BLUEs are computed correctly", {
    expect_is(extAs$BLUEs, "data.frame")
    expect_equal(dim(extAs$BLUEs), c(15, 2))
    expect_named(extAs$BLUEs, c("genotype", "t1"))
    expect_equal(extAs$BLUEs$t1,
                 c(86.9594324026272, 60.0587826374786, 94.1691705937487,
                   74.0099150750079, 90.3428175809651, 66.5671108868047,
                   67.0747150491462, 63.7011273329069, 87.0566508937095,
                   94.594647452768, 86.5691490691607, 83.6003540781321,
                   64.9321767034443, 58.9511717976486, 112.067789322994))
  })

  test_that("SE of BLUEs are computed correctly", {
    expect_is(extAs$seBLUEs, "data.frame")
    expect_equal(dim(extAs$seBLUEs), c(15, 2))
    expect_named(extAs$seBLUEs, c("genotype", "t1"))
    expect_equal(extAs$seBLUEs$t1, rep(x = 19.8671927642608, times = 15))
  })

  test_that("BLUPs are computed correctly", {
    expect_is(extAs$BLUPs, "data.frame")
    expect_equal(dim(extAs$BLUPs), c(15, 2))
    expect_named(extAs$BLUPs, c("genotype", "t1"))
    expect_equal(extAs$BLUPs$t1,
                 c(79.3770249887836, 79.3769389070209, 79.3770480598736,
                   79.3769835505153, 79.3770358156086, 79.3769597335509,
                   79.3769613579344, 79.3769505624977, 79.3770252999081,
                   79.3770494213874, 79.3770237399281, 79.3770142397666,
                   79.3769545018434, 79.3769353626563, 79.3771053352665))
  })

  test_that("SE of BLUPs are computed correctly", {
    expect_is(extAs$seBLUPs, "data.frame")
    expect_equal(dim(extAs$seBLUPs), c(15, 2))
    expect_named(extAs$seBLUPs, c("genotype", "t1"))
    expect_equal(extAs$seBLUPs$t1, rep(x = 4.61381582546008, times = 15))
  })

  test_that("unit errors are computed correctly", {
    expect_is(extAs$ue, "data.frame")
    expect_equal(dim(extAs$ue), c(15, 2))
    expect_named(extAs$ue, c("genotype", "t1"))
    expect_equal(extAs$ue$t1, rep(x = 394.705348332232, times = 15))
  })

  test_that("heritability is computed correctly", {
    expect_is(extAs$heritability, "numeric")
    expect_length(extAs$heritability, 1)
    expect_named(extAs$heritability, "t1")
    expect_equivalent(extAs$heritability, 0)
  })

  test_that("heritability can be coerced to data.frame correctly", {
    herit <- extractSTA(modelAs, what = "heritability")
    expect_is(herit, "data.frame")
    expect_named(herit, c("trial", "t1"))
    expect_equal(herit[1, 2], 0)
  })

  test_that("varGen is computed correctly", {
    expect_is(extAs$varGen, "numeric")
    expect_length(extAs$varGen, 1)
    expect_named(extAs$varGen, "t1")
    expect_equivalent(extAs$varGen, 0.00102174364157353)
  })

  test_that("varErr is computed correctly", {
    expect_is(extAs$varErr, "numeric")
    expect_length(extAs$varErr, 1)
    expect_named(extAs$varErr, "t1")
    expect_equivalent(extAs$varErr, 638.58972992586)
  })

  test_that("fitted values are computed correctly", {
    expect_is(extAs$fitted, "data.frame")
    expect_equal(dim(extAs$fitted), c(30, 3))
    expect_named(extAs$fitted, c("genotype", "repId", "t1"))
    expect_equal(extAs$fitted$t1,
                 c(113.978158345129, 96.0795526929543, 88.4324406657218,
                   60.8615487128919, 84.6587803048258, 81.6899719789264,
                   61.9691514018134, 61.7907414747871, 110.157420300858,
                   96.5050164749035, 85.1462739784662, 57.0407948824052,
                   88.4795178334955, 75.9203009331277, 58.1484138731437,
                   88.9670278089528, 68.4774799089402, 72.0995292168881,
                   65.6115131910267, 92.2531944962084, 63.0217908453245,
                   85.5107361773377, 85.0490503034216, 65.1643462848113,
                   88.8698145018329, 92.6842784306324, 64.6567418646691,
                   66.8425625615641, 68.9850838134811, 92.258788494543))
  })

  test_that("residuals are computed correctly", {
    expect_is(extAs$residF, "data.frame")
    expect_equal(dim(extAs$residF), c(30, 3))
    expect_named(extAs$residF, c("genotype", "repId", "t1"))
    expect_equal(extAs$residF$t1,
                 c(-7.63438676941396, -7.68912244373395, 8.80187677841506,
                   -22.8661167727902, 32.7866887211346, 4.87616448764264,
                   -9.95912901611794, -1.87107190890565, 7.63438676941401,
                   -26.8180136619635, -32.9904023864545, 22.8661167727903,
                   -32.7866887211345, 16.4495932447013, 9.95912901611796,
                   32.9904023864545, 8.55271944658909, -16.4495932447012,
                   1.87107190890566, -8.80187677841501, -13.8443208346061,
                   -4.87616448764261, -31.7663680847912, -15.9570227451638,
                   31.7663680847907, 26.8180136619635, -8.55271944658907,
                   13.8443208346061, 15.9570227451638, 7.68912244373398))
  })

  test_that("standardized residuals are computed correctly", {
    expect_is(extAs$stdResF, "data.frame")
    expect_equal(dim(extAs$stdResF), c(30, 3))
    expect_named(extAs$stdResF, c("genotype", "repId", "t1"))
    expect_equal(extAs$stdResF$t1,
                 c(-0.397758419410911, -0.400610197289146, 0.458585699280824,
                   -1.19134525670731, 1.70821598084126, 0.254052556933952,
                   -0.518879582055088, -0.0974845298737197, 0.397758419410913,
                   -1.39724264018937, -1.71882964608739, 1.19134525670731,
                   -1.70821598084126, 0.857038607891624, 0.518879582055088,
                   1.71882964608739, 0.445604378123664, -0.857038607891622,
                   0.0974845298737205, -0.458585699280821, -0.721301571339325,
                   -0.25405255693395, -1.65505635769639, -0.831375241695739,
                   1.65505635769636, 1.39724264018937, -0.445604378123662,
                   0.721301571339326, 0.83137524169574, 0.400610197289148))
  })

  test_that("rMeans are computed correctly", {
    expect_is(extAs$rMeans, "data.frame")
    expect_equal(dim(extAs$rMeans), c(30, 3))
    expect_named(extAs$rMeans, c("genotype", "repId", "t1"))
    expect_equal(extAs$rMeans$t1,
                 c(81.2874845342649, 81.2874378711863, 77.4666583841474,
                   81.2873117616817, 77.4666478925641, 77.466639852668,
                   81.2873005883265, 77.4665544146552, 77.4667464899939,
                   81.2874286203858, 77.4666478684469, 77.4665579311951,
                   81.2873854212338, 81.2873591189123, 77.4665630596569,
                   81.2874016989335, 81.2873389325493, 77.4665874026728,
                   81.2873261308947, 81.287412214634, 77.4665583540009,
                   81.2874040510792, 77.466650601685, 77.4665855105704,
                   81.2874148000962, 77.4666905761148, 77.4666008882784,
                   81.2873300702404, 81.28732303924, 77.4666736727751))
  })

  test_that("random effects are computed correctly", {
    expect_is(extAs$ranEf, "data.frame")
    expect_equal(dim(extAs$ranEf), c(15, 2))
    expect_named(extAs$ranEf, c("genotype", "t1"))
    expect_equal(extAs$ranEf$t1,
                 c(2.42636807955481e-05, -6.18180818554043e-05,
                   4.73347708436268e-05, -1.71745874332096e-05,
                   3.5090505831903e-05, -4.09915518320206e-05,
                   -3.93671683617431e-05, -5.01626050255014e-05,
                   2.45748053244931e-05, 4.86962846466234e-05,
                   2.3014825379157e-05, 1.35146637787577e-05,
                   -4.6223259361567e-05, -6.53624464703231e-05,
                   0.000104610163739661))
  })

  test_that("residuals are computed correctly for genotype random", {
    expect_is(extAs$residR, "data.frame")
    expect_equal(dim(extAs$residR), c(30, 3))
    expect_named(extAs$residR, c("genotype", "repId", "t1"))
    expect_equal(extAs$residR$t1,
                 c(25.0562870414505, 7.10299237803409, 19.7676590599895,
                   -43.29187982158, 39.9788211333962, 9.09949661390107,
                   -29.277278202631, -17.5468848487738, 40.3250605802784,
                   -11.6004258074458, -25.3107762764352, 2.4403537240004,
                   -25.5945563088727, 11.0825350589166, -9.35902017039521,
                   40.6700284964739, -4.25713957702004, -21.816651430486,
                   -13.8047410309624, 2.16390550315946, -28.2890883432825,
                   -0.652832361384085, -24.1839683830547, -28.2592619709228,
                   39.3487677865274, 42.0356015164811, -21.3625784701983,
                   -0.60044667407027, 3.65478351940486, 22.4812372655019))
  })

  test_that("standardized residuals are computed correctly for genotype random", {
    expect_is(extAs$stdResR, "data.frame")
    expect_equal(dim(extAs$stdResR), c(30, 3))
    expect_named(extAs$stdResR, c("genotype", "repId", "t1"))
    expect_equal(extAs$stdResR$t1,
                 c(1.02633174916988, 0.290946004075872, 0.809704010273758,
                   -1.77328072066828, 1.63757436828969, 0.372724907759114,
                   -1.19922796617595, -0.718738773607296, 1.65175669851932,
                   -0.475165585801895, -1.03675589466497, 0.0999594433925187,
                   -1.04837982188888, 0.453952238551462, -0.383355264333765,
                   1.66588694552083, -0.174376893963392, -0.893632882902356,
                   -0.565456645100944, 0.0886358348474369, -1.15875067498036,
                   -0.0267406969861743, -0.990600663673303, -1.15752895554265,
                   1.61176672358589, 1.72182224606734, -0.875033579070472,
                   -0.0245949243901264, 0.149703759224292, 0.920855014473447))
  })

  test_that("Waldtest is computed correctly", {
    expect_is(extAs$wald$t1$Ftest, "numeric")
    expect_length(extAs$wald$t1$Ftest, 4)
    expect_equivalent(unlist(extAs$wald$t1$Ftest),
                      c(0.6179, 14, 14, 0.81074002965176))
  })

  test_that("CV is computed correctly", {
    expect_is(extAs$CV, "numeric")
    expect_length(extAs$CV, 1)
    expect_named(extAs$CV, "t1")
    expect_equivalent(extAs$CV, 35.3962093849912)
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
                      c(28.0964493801834, 28.0964528715962, 28.0964534534983))
  })

  test_that("lsd is computed correctly", {
    expect_is(extAs$lsd$t1, "numeric")
    expect_length(extAs$lsd$t1, 3)
    expect_equivalent(extAs$lsd$t1,
                      c(60.2608906083738, 60.2608980967094, 60.2608993447654))
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

