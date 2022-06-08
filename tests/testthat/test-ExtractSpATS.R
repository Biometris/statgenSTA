context("extractSTA SpATS")

modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")

test_that("the output of extractSTA is of the proper type", {
  expect_is(extractSTA(modelSp, what = "BLUEs"), "data.frame")
  expect_is(extractSTA(modelSp), "list")
  expect_length(extractSTA(modelSp)[["E1"]], 22)
  expect_is(extractSTA(modelSp, what = c("BLUEs", "BLUPs")), "list")
  expect_length(extractSTA(modelSp, what = c("BLUEs", "BLUPs"))[["E1"]], 2)
})

extSp <- extractSTA(modelSp)[["E1"]]
test_that("BLUEs are computed correctly", {
  expect_is(extSp$BLUEs, "data.frame")
  expect_equal(dim(extSp$BLUEs), c(15, 2))
  expect_named(extSp$BLUEs, c("genotype", "t1"))
  expect_equal(extSp$BLUEs$t1,
               c(69.9202841139079, 73.011566816012, 106.61526288738,
                 80.5465563933279, 82.5251967659839, 75.021141783374,
                 66.9476240572365, 62.7262039251742, 90.0880562676267,
                 97.9178080741426, 94.6684033674549, 68.598458437976,
                 49.8026925088618, 73.1067643214725, 126.941193543844))
})

test_that("SE of BLUEs are computed correctly", {
  expect_is(extSp$seBLUEs, "data.frame")
  expect_equal(dim(extSp$seBLUEs), c(15, 2))
  expect_named(extSp$seBLUEs, c("genotype", "t1"))
  expect_equal(extSp$seBLUEs$t1,
               c(19.4698719546391, 18.4681177363461, 18.6193102974642,
                 18.5359185545503, 18.5412870698226, 18.7727808180917,
                 19.3707628075504, 18.35103764834, 18.6998129694293,
                 17.9214425827371, 18.5814934150145, 18.9360920285852,
                 18.8596344457161, 18.6043019324812, 19.0446820736492))
})

test_that("BLUPs are computed correctly", {
  expect_is(extSp$BLUPs, "data.frame")
  expect_equal(dim(extSp$BLUPs), c(15, 2))
  expect_named(extSp$BLUPs, c("genotype", "t1"))
  expect_equal(extSp$BLUPs$t1,
               c(84.2848383091137, 84.2182891061178, 84.3707050571758,
                 84.3013250849861, 84.2935946929651, 84.2631494765859,
                 84.2283453994588, 84.1928827139912, 84.349402581663,
                 84.3870872859797, 84.3340607654846, 84.2421481221215,
                 84.1654343671387, 84.1936112509318, 84.4689792525841))
})

test_that("SE of BLUPs are computed correctly", {
  expect_is(extSp$seBLUPs, "data.frame")
  expect_equal(dim(extSp$seBLUPs), c(15, 2))
  expect_named(extSp$seBLUPs, c("genotype", "t1"))
  expect_equal(extSp$seBLUPs$t1,
               c(5.61691281966534, 5.6253331197784, 5.60001319425303,
                 5.62935406968133, 5.61342370690578, 5.6208721197452,
                 5.61069798714695, 5.62405617961244, 5.62808230390181,
                 5.61956972040033, 5.62086325894509, 5.61689367812487,
                 5.6134087612672, 5.61959107242943, 5.61069140167746))
})

test_that("heritability is computed correctly", {
  expect_is(extSp$heritability, "numeric")
  expect_length(extSp$heritability, 1)
  expect_named(extSp$heritability, "t1")
  expect_equivalent(extSp$heritability, 0.01)
})

test_that("heritability can be coerced to data.frame correctly", {
  herit <- extractSTA(modelSp, what = "heritability")
  expect_is(herit, "data.frame")
  expect_named(herit, c("trial", "t1"))
  expect_equal(herit[1, 2], 0.01)
})

test_that("heritability can be coerced to data.frame correctly when one trait contains only NA", {
  testTD$E2 <- testTD$E1
  testTD$E1[["t3"]] <- NA
  testTD$E2[["trial"]] <- "E2"
  testTD$E2[["t4"]] <- NA
  modelSp <- suppressWarnings(fitTD(testTD, design = "rowcol", what = "random",
                                    traits = c("t1", "t2", "t3", "t4")))
  herit <- extractSTA(modelSp, what = "heritability")
  expect_is(herit, "data.frame")
  expect_named(herit, c("trial", "t1", "t2", "t4", "t3"))
  expect_equivalent(herit[1, 2:5], c(0.01, 0, 0.64, NA))
  expect_equivalent(herit[2, 2:5], c(0.01, 0, NA, 0))
})

test_that("varGen is computed correctly", {
  expect_is(extSp$varGen, "numeric")
  expect_length(extSp$varGen, 1)
  expect_named(extSp$varGen, "t1")
  expect_equivalent(extSp$varGen, 1.57464616738179)
})

test_that("varErr is computed correctly", {
  expect_is(extSp$varErr, "numeric")
  expect_length(extSp$varErr, 1)
  expect_named(extSp$varErr, "t1")
  expect_equivalent(extSp$varErr, 552.842100663149)
})

test_that("varSpat is computed correctly", {
  expect_is(extSp$varSpat, "matrix")
  expect_equal(dim(extSp$varSpat), c(5, 1))
  expect_equal(colnames(extSp$varSpat), "t1")
  expect_equal(as.numeric(extSp$varSpat),
               c(3.2629715613931e-15, 557.496152958815, 2.9783024662669e-62,
                 41.564871863117, 0.00666737411288173))
})

test_that("fitted values are computed correctly", {
  expect_is(extSp$fitted, "data.frame")
  expect_equal(dim(extSp$fitted), c(30, 2))
  expect_named(extSp$fitted, c("genotype", "t1"))
  expect_equal(extSp$fitted$t1,
               c(92.5704935353778, 97.1413078555221, 97.9480224715044,
                 44.9510756560669, 85.545800473024, 78.5097408150658,
                 51.2950935580125, 54.1791287307377, 131.56508511061,
                 83.1486518452886, 82.6246444151263, 72.9512679392303,
                 87.5924976652973, 74.9128374070536, 68.8224717169447,
                 91.4886573722926, 72.0005085433045, 73.1069927429623,
                 73.2231259350761, 82.7376126904259, 39.7324491122088,
                 88.6909673411984, 73.8651753103633, 54.7460478370784,
                 100.053689494891, 106.040643060247, 61.1337132303049,
                 90.1319042946797, 79.4033822612141, 91.1970333319753))
})

test_that("residuals are computed correctly", {
  expect_is(extSp$residF, "data.frame")
  expect_equal(dim(extSp$residF), c(30, 2))
  expect_named(extSp$residF, c("genotype", "t1"))
  expect_equal(extSp$residF$t1,
               c(13.7732780403377, -8.7508776063017, -0.713705027367581,
                 -6.95564371596522, 31.8996685529363, 8.05639565150327,
                 0.714928827682968, 5.74054083514376, -13.7732780403377,
                 -13.4616490323485, -30.4687728231146, 6.95564371596519,
                 -31.8996685529363, 17.4570567707754, -0.714928827683011,
                 30.4687728231147, 5.0296908122248, -17.4570567707754,
                 -5.74054083514376, 0.713705027367524, 9.44502089850954,
                 -8.05639565150327, -20.582493091733, -5.53872429743078,
                 20.5824930917325, 13.4616490323485, -5.02969081222481,
                 -9.44502089850953, 5.53872429743073, 8.7508776063017))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extSp$stdResF, "data.frame")
  expect_equal(dim(extSp$stdResF), c(30, 2))
  expect_named(extSp$stdResF, c("genotype", "t1"))
  expect_equal(extSp$stdResF$t1,
               c(0.89757912725894, -0.570278553994944, -0.0465108403176624,
                 -0.453286472386288, 2.07884256570864, 0.5250204458627,
                 0.0465905931271103, 0.374100458707964, -0.897579127258943,
                 -0.877270839558637, -1.9855937300567, 0.453286472386287,
                 -2.07884256570864, 1.13764419297515, -0.0465905931271131,
                 1.98559373005671, 0.327775673764614, -1.13764419297516,
                 -0.374100458707964, 0.0465108403176587, 0.615514592110769,
                 -0.5250204458627, -1.34132311363969, -0.360948447163848,
                 1.34132311363966, 0.877270839558635, -0.327775673764615,
                 -0.615514592110768, 0.360948447163845, 0.570278553994944))
})

test_that("rMeans are computed correctly", {
  expect_is(extSp$rMeans, "data.frame")
  expect_equal(dim(extSp$rMeans), c(30, 2))
  expect_named(extSp$rMeans, c("genotype", "t1"))
  expect_equal(extSp$rMeans$t1,
               c(74.0989013814905, 88.6980896828674, 103.318442072757,
                 71.875075406461, 83.07551156346, 94.0435865499007,
                 70.8525150012112, 78.217872553316, 85.8847347040855,
                 71.3728248678151, 75.0225664049186, 78.5542034883243,
                 73.2793394716864, 73.2287407030608, 73.1278440133701,
                 76.9209341511665, 73.1486534173819, 69.5008036319036,
                 81.9670992511179, 74.7909685358664, 67.3859673148854,
                 88.6432598150646, 77.9095377077799, 67.0766336343853,
                 96.5073790025201, 82.4159560730472, 68.0983466575741,
                 104.89793596677, 87.404928871074, 69.9913698578215))
})

test_that("random effects are computed correctly", {
  expect_is(extSp$ranEf, "data.frame")
  expect_equal(dim(extSp$ranEf), c(15, 2))
  expect_named(extSp$ranEf, c("genotype", "t1"))
  expect_equal(extSp$ranEf$t1,
               c(-0.00141858863949994, -0.0679677916353486, 0.0844481594225578,
                 0.0150681872329052, 0.00733779521189378, -0.0231074211672915,
                 -0.0579114982944327, -0.0933741837619972, 0.063145683909856,
                 0.1008303882265, 0.0478038677314341, -0.0441087756316412,
                 -0.120822530614508, -0.0926456468213558, 0.182722354830931))
})

test_that("residuals are computed correctly for genotype random", {
  expect_is(extSp$residR, "data.frame")
  expect_equal(dim(extSp$residR), c(30, 2))
  expect_named(extSp$residR, c("genotype", "t1"))
  expect_equal(extSp$residR$t1,
               c(32.2448701942249, -0.307659433647018, -6.0841246286202,
                 -33.8796434663593, 34.3699574625003, -7.47745008333166,
                 -18.8424926155157, -18.2982029874346, 31.9070723661868,
                 -1.68582205487502, -22.8666948129069, 1.3527081668712,
                 -17.5865103593253, 19.1411534747681, -5.0203011241084,
                 45.0364960442408, 3.88154593814744, -13.8508676597168,
                 -14.4845141511855, 8.660349181927, -18.2084973041671,
                 -8.00868812536947, -24.6268554891496, -17.8693100947378,
                 24.1288035841035, 37.0863360195487, -11.9943242394941,
                 -24.2110525705999, -2.46282231242915, 29.9565410804555))
})

test_that("standardized residuals are computed correctly for genotype random", {
  expect_is(extSp$stdResR, "data.frame")
  expect_equal(dim(extSp$stdResR), c(30, 2))
  expect_named(extSp$stdResR, c("genotype", "t1"))
  expect_equal(extSp$stdResR$t1,
               c(1.47650790266777, -0.014087871415636, -0.278594952960371,
                 -1.55136494631029, 1.57381665679106, -0.342395986503936,
                 -0.862806675455337, -0.83788339518281, 1.46103997987578,
                 -0.0771945916209764, -1.04707680309398, 0.0619411486651378,
                 -0.80529465212557, 0.876482497880659, -0.229881970027144,
                 2.06224251849204, 0.177737829853914, -0.63423780077862,
                 -0.663252774215198, 0.396561497377892, -0.833775729390985,
                 -0.366721628460065, -1.12767539545248, -0.818244186165982,
                 1.1048693624522, 1.69820091952683, -0.549225796850765,
                 -1.10863558256154, -0.112773802011442, 1.37172422699987))
})

test_that("CV is computed correctly", {
  expect_is(extSp$CV, "numeric")
  expect_length(extSp$CV, 1)
  expect_named(extSp$CV, "t1")
  expect_equivalent(extSp$CV, 1.58087235613645)
})


test_that("rDfF is computed correctly", {
  expect_is(extSp$rDfF, "numeric")
  expect_length(extSp$rDfF, 1)
  expect_named(extSp$rDfF, "t1")
  expect_equivalent(extSp$rDfF, 12)
})

test_that("rDfR is computed correctly", {
  expect_is(extSp$rDfR, "numeric")
  expect_length(extSp$rDfR, 1)
  expect_named(extSp$rDfR, "t1")
  expect_equivalent(extSp$rDfR, 25)
})

test_that("effective dimensions are computed correctly", {
  expect_is(extSp$effDim, "list")
  expect_is(extSp$effDim$t1, "data.frame")
  expect_equal(dim(extSp$effDim$t1), c(12, 1))
  expect_equal(extSp$effDim$t1[["effDim"]],
               c(1, 0.0704228914347658, 7.61552119726609e-07,
                 5.89381404331278e-19, 1, 1, 1, 6.13938962844153e-19,
                 0.869374386465353, 2.7126958541334e-66,
                 0.0507926646823833, 4.23079279024977e-07))
})

test_that("ratios of effective dimensions are computed correctly", {
  expect_is(extSp$ratEffDim, "list")
  expect_is(extSp$ratEffDim$t1, "data.frame")
  expect_equal(dim(extSp$ratEffDim$t1), c(12, 1))
  expect_equal(extSp$ratEffDim$t1[["ratEffDim"]],
               c(1, 0.01, 0, 0, 1, 1, 1, 0, 0.14, 0, 0.01, 0))
})

test_that("correct attributes are added", {
  expect_equal(attr(x = extSp, which = "traits"), "t1")
  expect_equal(attr(x = extSp, which = "design"), "rowcol")
  expect_equal(attr(x = extSp, which = "engine"), "SpATS")
})

test_that("calculated values are logically correct", {
  ## Fitted + residuals should match raw data.
  expect_equal(testTD[["E1"]]$t1, extSp$fitted$t1 + extSp$residF$t1)
  expect_equal(testTD[["E1"]]$t1, extSp$rMeans$t1 + extSp$residR$t1)
})
