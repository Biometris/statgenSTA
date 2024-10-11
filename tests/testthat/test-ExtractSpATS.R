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
               c(69.5167593813322, 72.6338122051147, 106.23106955308,
                 80.1666705529467, 82.1311686042837, 74.634496593809,
                 66.5553546465424, 62.341080985472, 89.7194425707504,
                 97.5423533003863, 94.2785143357814, 68.1905223798518,
                 49.4140519115095, 72.7293210845287, 126.557515427208))
})

test_that("SE of BLUEs are computed correctly", {
  expect_is(extSp$seBLUEs, "data.frame")
  expect_equal(dim(extSp$seBLUEs), c(15, 2))
  expect_named(extSp$seBLUEs, c("genotype", "t1"))
  expect_equal(extSp$seBLUEs$t1,
               c(19.3934536850038, 18.2955539756536, 18.7155028673541,
                 18.3401619156947, 18.4877807559459, 18.6809659000053,
                 19.3822853924568, 18.1764759919266, 18.4870281259355,
                 17.8070727580626, 18.4338646658729, 18.8442977690163,
                 18.8169672704717, 18.4869279723477, 19.0060195260883))
})

test_that("BLUPs are computed correctly", {
  expect_is(extSp$BLUPs, "data.frame")
  expect_equal(dim(extSp$BLUPs), c(15, 2))
  expect_named(extSp$BLUPs, c("genotype", "t1"))
  expect_equal(extSp$BLUPs$t1,
               c(83.586146495726, 83.5698971197698, 83.6063893648483,
                 83.6082212111448, 83.6002485191453, 83.5748689438451,
                 83.569351912874, 83.5232863758566, 83.6073486516667,
                 83.648261330801, 83.609327595995, 83.5435447120607,
                 83.562941183417, 83.5374368000269, 83.653341053207))
})

test_that("SE of BLUPs are computed correctly", {
  expect_is(extSp$seBLUPs, "data.frame")
  expect_equal(dim(extSp$seBLUPs), c(15, 2))
  expect_named(extSp$seBLUPs, c("genotype", "t1"))
  expect_equal(extSp$seBLUPs$t1,
               c(4.73167424952247, 4.73518886983081, 4.72435679710458,
                 4.73680899213873, 4.73008639794386, 4.73328323728827,
                 4.72895873743564, 4.73467700951607, 4.73630386982148,
                 4.73277774194677, 4.73328532600507, 4.73167560655675,
                 4.73011316601938, 4.73279211284365, 4.72898995259361))
})

test_that("heritability is computed correctly", {
  expect_is(extSp$heritability, "numeric")
  expect_length(extSp$heritability, 1)
  expect_named(extSp$heritability, "t1")
  expect_equivalent(extSp$heritability, 0)
})

test_that("heritability can be coerced to data.frame correctly", {
  herit <- extractSTA(modelSp, what = "heritability")
  expect_is(herit, "data.frame")
  expect_named(herit, c("trial", "t1"))
  expect_equal(herit[1, 2], 0)
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
  expect_equivalent(herit[1, 2:5], c(0, 0, 0.64, NA))
  expect_equivalent(herit[2, 2:5], c(0, 0, NA, 0))
})

test_that("varGen is computed correctly", {
  expect_is(extSp$varGen, "numeric")
  expect_length(extSp$varGen, 1)
  expect_named(extSp$varGen, "t1")
  expect_equivalent(extSp$varGen, 0.658635135446465)
})

test_that("varErr is computed correctly", {
  expect_is(extSp$varErr, "numeric")
  expect_length(extSp$varErr, 1)
  expect_named(extSp$varErr, "t1")
  expect_equivalent(extSp$varErr, 456.342843532687)
})

test_that("varSpat is computed correctly", {
  expect_is(extSp$varSpat, "matrix")
  expect_equal(dim(extSp$varSpat), c(5, 1))
  expect_equal(colnames(extSp$varSpat), "t1")
  expect_equal(as.numeric(extSp$varSpat),
               c(5.57916232730529e-17, 331.460313652896, 3.29766860170946e-72,
                 6350.91292605282, 5.25117002463805e-05))
})

test_that("fitted values are computed correctly", {
  expect_is(extSp$fitted, "data.frame")
  expect_equal(dim(extSp$fitted), c(30, 2))
  expect_named(extSp$fitted, c("genotype", "t1"))
  expect_equal(extSp$fitted$t1,
               c(92.569672460243, 97.1482982068825, 97.9532193097446,
                 44.9689277630993, 85.5579819962171, 78.51012939075,
                 51.2921346513194, 54.1740180800659, 131.565906185745,
                 83.1264261629245, 82.6128913192196, 72.933415832198,
                 87.5803161421042, 74.9126586268021, 68.8254306236378,
                 91.5004104681994, 71.9945450186387, 73.1071715232138,
                 73.228236585748, 82.7324158521857, 39.7304905278569,
                 88.6905787655143, 73.8660514964467, 54.7545636679638,
                 100.052813308808, 106.062868742612, 61.1396767549707,
                 90.1338628790315, 79.3948664303286, 91.1900429806148))
})

test_that("residuals are computed correctly", {
  expect_is(extSp$residF, "data.frame")
  expect_equal(dim(extSp$residF), c(30, 2))
  expect_named(extSp$residF, c("genotype", "t1"))
  expect_equal(extSp$residF$t1,
               c(13.7740991154725, -8.75786795766216, -0.718901865607734,
                 -6.97349582299759, 31.8874870297432, 8.05600707581911,
                 0.71788773437612, 5.74565148581561, -13.7740991154725,
                 -13.4394233499845, -30.4570197272079, 6.97349582299753,
                 -31.8874870297432, 17.4572355510269, -0.717887734376149,
                 30.4570197272079, 5.03565433689062, -17.4572355510269,
                 -5.74565148581557, 0.718901865607762, 9.44697948286147,
                 -8.05600707581915, -20.5833692778163, -5.54724012831624,
                 20.5833692778159, 13.4394233499844, -5.03565433689067,
                 -9.44697948286138, 5.54724012831628, 8.75786795766217))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extSp$stdResF, "data.frame")
  expect_equal(dim(extSp$stdResF), c(30, 2))
  expect_named(extSp$stdResF, c("genotype", "t1"))
  expect_equal(extSp$stdResF$t1,
               c(0.897816652996265, -0.570851104759294, -0.0468591129918384,
                 -0.454543025064647, 2.07847472546112, 0.525102748912378,
                 0.046793010381393, 0.374510270547087, -0.897816652996267,
                 -0.876001979449169, -1.98523469901611, 0.454543025064643,
                 -2.07847472546112, 1.13788906712486, -0.0467930103813949,
                 1.98523469901611, 0.328231580482437, -1.13788906712487,
                 -0.374510270547085, 0.0468591129918403, 0.61576843822037,
                 -0.52510274891238, -1.34165520063933, -0.361577517601638,
                 1.3416552006393, 0.876001979449163, -0.32823158048244,
                 -0.615768438220365, 0.361577517601641, 0.570851104759295))
})

test_that("rMeans are computed correctly", {
  expect_is(extSp$rMeans, "data.frame")
  expect_equal(dim(extSp$rMeans), c(30, 2))
  expect_named(extSp$rMeans, c("genotype", "t1"))
  expect_equal(extSp$rMeans$t1,
               c(81.9062041099569, 89.3958312680544, 96.9262692675285,
                 66.8068067515979, 83.3106872759732, 99.6768941271541,
                 60.3110478627319, 78.0979425379898, 96.0615026489855,
                 63.9038872922492, 74.5830656348479, 85.2332448250709,
                 74.1549633216532, 72.8229523649717, 71.4537237239392,
                 85.2287226941056, 72.7268461339402, 60.2908015695603,
                 93.2463024129486, 74.3772599954968, 55.3939481150779,
                 97.4295856992395, 77.7786521990214, 58.0683223412992,
                 96.5288540002577, 82.6370791613861, 68.6097971016609,
                 90.9371112078637, 88.1060668664894, 85.3056492420322))
})

test_that("random effects are computed correctly", {
  expect_is(extSp$ranEf, "data.frame")
  expect_equal(dim(extSp$ranEf), c(15, 2))
  expect_named(extSp$ranEf, c("genotype", "t1"))
  expect_equal(extSp$ranEf$t1,
               c(-0.000560922299585701, -0.0168102982558094, 0.019681946822702,
                 0.0215137931191361, 0.0135411011196361, -0.0118384741804787,
                 -0.0173555051515866, -0.0634210421690489, 0.0206412336411081,
                 0.0615539127753868, 0.0226201779693976, -0.0431627059648978,
                 -0.023766234608651, -0.0492706179987067, 0.0666336351813987))
})

test_that("residuals are computed correctly for genotype random", {
  expect_is(extSp$residR, "data.frame")
  expect_equal(dim(extSp$residR), c(30, 2))
  expect_named(extSp$residR, c("genotype", "t1"))
  expect_equal(extSp$residR$t1,
               c(24.4375674657585, -1.00540101883405, 0.308048176608324,
                 -28.8113748114962, 34.1347817499871, -13.110757660585,
                 -8.30102547703643, -18.1782729721083, 21.7303044212868,
                 5.78311552069083, -22.4271940428362, -5.32633316987538,
                 -18.4621342092922, 19.5469418128573, -3.34618083467753,
                 36.7287075013017, 4.30335322158911, -4.64086559737349,
                 -25.7637173130162, 9.07405772229662, -6.21647810435953,
                 -16.7950140095444, -24.495969980391, -8.8609988016516,
                 24.1073285863659, 36.8652129312098, -12.5057746835809,
                 -10.2502278116936, -3.16396030784452, 14.6422616962447))
})

test_that("standardized residuals are computed correctly for genotype random", {
  expect_is(extSp$stdResR, "data.frame")
  expect_equal(dim(extSp$stdResR), c(30, 2))
  expect_named(extSp$stdResR, c("genotype", "t1"))
  expect_equal(extSp$stdResR$t1,
               c(1.28450292500666, -0.0528465262062829, 0.0161918236932048,
                 -1.51440176157817, 1.79421405439633, -0.68913596198443,
                 -0.436323767525081, -0.955497916761161, 1.14220204730028,
                 0.303975786967642, -1.17883240171429, -0.279966103248671,
                 -0.970418411199475, 1.02743875669013, -0.175884079942369,
                 1.93055762539546, 0.22619558219996, -0.243936121823163,
                 -1.35420885462451, 0.476956378833571, -0.326754466027694,
                 -0.882790181592977, -1.28757271503295, -0.465757440676132,
                 1.26714469951531, 1.93773271040512, -0.657336462929138,
                 -0.538778977267423, -0.16630608900521, 0.769635848735469))
})

test_that("CV is computed correctly", {
  expect_is(extSp$CV, "numeric")
  expect_length(extSp$CV, 1)
  expect_named(extSp$CV, "t1")
  expect_equivalent(extSp$CV, 30.4643854331817)
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
  expect_equivalent(extSp$rDfR, 23)
})

test_that("effective dimensions are computed correctly", {
  expect_is(extSp$effDim, "list")
  expect_is(extSp$effDim$t1, "data.frame")
  expect_equal(dim(extSp$effDim$t1), c(12, 1))
  expect_equal(extSp$effDim$t1[["effDim"]],
               c(1, 0.0323042481186158, 7.58577712075052e-07,
                 1.22170074632629e-20, 1, 1, 1, 1.1312043947453e-20,
                 0.991094166031977, 3.67384638710293e-76,
                 1.95738948453598, 4.45894910907468e-09))
})

test_that("ratios of effective dimensions are computed correctly", {
  expect_is(extSp$ratEffDim, "list")
  expect_is(extSp$ratEffDim$t1, "data.frame")
  expect_equal(dim(extSp$ratEffDim$t1), c(12, 1))
  expect_equal(extSp$ratEffDim$t1[["ratEffDim"]],
               c(1, 0, 0, 0, 1, 1, 1, 0, 0.14, 0, 0.28, 0))
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
