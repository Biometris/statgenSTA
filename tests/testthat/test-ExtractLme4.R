context("extractSTA lme4")

modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")

test_that("the output of extractSTA is of the proper type", {
  expect_is(extractSTA(modelLm, what = "BLUEs"), "data.frame")
  expect_is(extractSTA(modelLm), "list")
  expect_length(extractSTA(modelLm)[["E1"]], 21)
  expect_is(extractSTA(modelLm, what = c("BLUEs", "BLUPs")), "list")
  expect_length(extractSTA(modelLm, what = c("BLUEs", "BLUPs"))[["E1"]], 2)
})

extLm <- extractSTA(modelLm)[["E1"]]
test_that("BLUEs are computed correctly", {
  expect_is(extLm$BLUEs, "data.frame"	)
  expect_equal(dim(extLm$BLUEs), c(15, 2))
  expect_named(extLm$BLUEs, c("genotype", "t1"))
  expect_equal(extLm$BLUEs$t1,
               c(86.959432402627, 60.0587826374786, 94.1691705937487,
                 74.0099150750079, 90.3428175809651, 66.5671108868047,
                 67.0747150491462, 63.701127332907, 87.0566508937095,
                 94.594647452768, 86.5691490691607, 83.6003540781321,
                 64.9321767034443, 58.9511717976486, 112.067789322994))
})

test_that("SE of BLUEs are computed correctly", {
  expect_is(extLm$seBLUEs, "data.frame"	)
  expect_equal(dim(extLm$seBLUEs), c(15, 2))
  expect_named(extLm$seBLUEs, c("genotype", "t1"))
  expect_equal(extLm$seBLUEs$t1, rep(x = 19.8671913400323, times = 15))
})

test_that("BLUPs are computed correctly", {
  expect_is(extLm$BLUPs, "data.frame"	)
  expect_equal(dim(extLm$BLUPs), c(15, 2))
  expect_named(extLm$BLUPs, c("genotype", "t1"))
  expect_equal(extLm$BLUPs$t1,
               c(79.3770007251028, 79.3770007251028, 79.3770007251028,
                 79.3770007251028, 79.3770007251028, 79.3770007251028,
                 79.3770007251028, 79.3770007251028, 79.3770007251028,
                 79.3770007251028, 79.3770007251028, 79.3770007251028,
                 79.3770007251028, 79.3770007251028, 79.3770007251028))
})

test_that("SE of BLUPs are computed correctly", {
  expect_is(extLm$seBLUPs, "data.frame"	)
  expect_equal(dim(extLm$seBLUPs), c(15, 2))
  expect_named(extLm$seBLUPs, c("genotype", "t1"))
  expect_equal(extLm$seBLUPs$t1, rep(x = 0, times = 15))
})

test_that("unit errors are computed correctly", {
  expect_is(extLm$ue, "data.frame"	)
  expect_equal(dim(extLm$ue), c(15, 2))
  expect_named(extLm$ue, c("genotype", "t1"))
  expect_equal(extLm$ue$t1, rep(x = 394.705291741457, times = 15))
})

test_that("heritability is computed correctly", {
  expect_is(extLm$heritability, "numeric")
  expect_length(extLm$heritability, 1)
  expect_named(extLm$heritability, "t1")
  expect_equivalent(extLm$heritability, 0)
})

test_that("heritability can be coerced to data.frame correctly", {
  herit <- extractSTA(modelLm, what= "heritability")
  expect_is(herit, "data.frame")
  expect_named(herit, c("trial", "t1"))
  expect_equal(herit[1, 2], 0)
})

test_that("varGen is computed correctly", {
  expect_is(extLm$varGen, "numeric")
  expect_length(extLm$varGen, 1)
  expect_named(extLm$varGen, "t1")
  expect_equivalent(extLm$varGen, 0)
})

test_that("varErr is computed correctly", {
  expect_is(extLm$varErr, "numeric")
  expect_length(extLm$varErr, 1)
  expect_named(extLm$varErr, "t1")
  expect_equivalent(extLm$varErr, 638.590646884335)
})

test_that("fitted values are computed correctly", {
  expect_is(extLm$fitted, "data.frame"	)
  expect_equal(dim(extLm$fitted), c(30, 3))
  expect_named(extLm$fitted, c("genotype", "repId", "t1"))
  expect_equal(extLm$fitted$t1,
               c(113.978165854802, 96.0795471255565, 88.4324410491573,
                 60.8615483294564, 84.6587725373528, 81.6899775463243,
                 61.9691591692864, 61.7907508010991, 110.157412791186,
                 96.5050239845758, 85.1462743619017, 57.0407952658408,
                 88.4795256009685, 75.9202916068157, 58.1484061056708,
                 88.9670274255173, 68.4774874186125, 72.0995385432001,
                 65.6115038647147, 92.2531941127729, 63.0218001716365,
                 85.5107306099399, 85.0490558708192, 65.1643385173384,
                 88.8698089344348, 92.6842709209602, 64.6567343549969,
                 66.8425532352521, 68.985091580954, 92.2587940619409))
})

test_that("residuals are computed correctly", {
  expect_is(extLm$residF, "data.frame"	)
  expect_equal(dim(extLm$residF), c(30, 3))
  expect_named(extLm$residF, c("genotype", "repId", "t1"))
  expect_equal(extLm$residF$t1,
               c(-7.63439427908629, -7.68911687633611, 8.80187639497948,
                 -22.8661163893547, 32.7866964886075, 4.87615892024477,
                 -9.95913678359087, -1.87108123521764, 7.63439427908628,
                 -26.8180211716357, -32.99040276989, 22.8661163893547,
                 -32.7866964886075, 16.4496025710133, 9.95913678359089,
                 32.99040276989, 8.55271193691682, -16.4496025710133,
                 1.87108123521765, -8.8018763949795, -13.8443301609181,
                 -4.87615892024477, -31.7663736521888, -15.9570149776908,
                 31.7663736521888, 26.8180211716357, -8.55271193691682,
                 13.8443301609181, 15.9570149776908, 7.6891168763361))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extLm$stdResF, "data.frame"	)
  expect_equal(dim(extLm$stdResF), c(30, 3))
  expect_named(extLm$stdResF, c("genotype", "repId", "t1"))
  expect_equal(extLm$stdResF$t1,
               c(-0.397758732092815, -0.400609828080684, 0.458585588708124,
                 -1.19134500137515, 1.70821604806888, 0.254052216678513,
                 -0.518879884240682, -0.0974849965244265, 0.397758732092815,
                 -1.39724275541933, -1.71882932650333, 1.19134500137515,
                 -1.70821604806888, 0.857038924489505, 0.518879884240683,
                 1.71882932650333, 0.445603898832203, -0.857038924489507,
                 0.0974849965244267, -0.458585588708124, -0.721301914752581,
                 -0.254052216678513, -1.65505632080001, -0.831374672762121,
                 1.65505632080002, 1.39724275541933, -0.445603898832203,
                 0.721301914752581, 0.831374672762121, 0.400609828080684))
})

test_that("rMeans are computed correctly", {
  expect_is(extLm$rMeans, "data.frame"	)
  expect_equal(dim(extLm$rMeans), c(30, 3))
  expect_named(extLm$rMeans, c("genotype", "repId", "t1"))
  expect_equal(extLm$rMeans$t1,
               c(81.2873772569106, 81.2873772569106, 77.466624193295,
                 81.2873772569106, 77.466624193295, 77.466624193295,
                 81.2873772569106, 77.466624193295, 77.466624193295,
                 81.2873772569106, 77.466624193295, 77.466624193295,
                 81.2873772569106, 81.2873772569106, 77.466624193295,
                 81.2873772569106, 81.2873772569106, 77.466624193295,
                 81.2873772569106, 81.2873772569106, 77.466624193295,
                 81.2873772569106, 77.466624193295, 77.466624193295,
                 81.2873772569106, 77.466624193295, 77.466624193295,
                 81.2873772569106, 81.2873772569106, 77.466624193295))
})

test_that("random effects are computed correctly", {
  expect_is(extLm$ranEf, "data.frame"	)
  expect_equal(dim(extLm$ranEf), c(15, 2))
  expect_named(extLm$ranEf, c("genotype", "t1"))
  expect_equal(extLm$ranEf$t1,
               c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
})

test_that("residuals are computed correctly for genotype random", {
  expect_is(extLm$residR, "data.frame"	)
  expect_equal(dim(extLm$residR), c(30, 3))
  expect_named(extLm$residR, c("genotype", "repId", "t1"))
  expect_equal(extLm$residR$t1,
               c(25.0563943188048, 7.10305299230976, 19.7676932508419,
                 -43.2919453168089, 39.9788448326654, 9.09951227327409,
                 -29.2773548712151, -17.5469546274135, 40.3251828769773,
                 -11.6003744439706, -25.3107526012833, 2.44028746190051,
                 -25.5945481445496, 11.0825169209183, -9.35908130403332,
                 40.6700529384967, -4.25717790138133, -21.8166882211082,
                 -13.8047921569782, 2.1639404608828, -28.2891541825766,
                 -0.652805567215481, -24.1839419746646, -28.2593006536474,
                 39.348805329713, 42.0356678993009, -21.3626017752149,
                 -0.600493860740485, 3.65472930173426, 22.481286744982))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extLm$stdResR, "data.frame"	)
  expect_equal(dim(extLm$stdResR), c(30, 3))
  expect_named(extLm$stdResR, c("genotype", "repId", "t1"))
  expect_equal(extLm$stdResR$t1,
               c(0.99153328646432, 0.281082480894285, 0.782248459432799,
                 -1.71315171174464, 1.58204548115008, 0.360086499068644,
                 -1.15856541548165, -0.694369244345031, 1.5957507930531,
                 -0.459050781620821, -1.0015987691733, 0.0965672161856089,
                 -1.01282914510523, 0.438558089606995, -0.370358181851825,
                 1.60939801384436, -0.168465324336469, -0.863331423364774,
                 -0.546284144566353, 0.0856315944581839, -1.11946027273744,
                 -0.0258328649065675, -0.957008544833016, -1.11827890692418,
                 1.55711351643815, 1.66343821902723, -0.845362284616925,
                 -0.0237627826121337, 0.144625188667564, 0.889630959929036))
})

test_that("Waldtest is computed correctly", {
  expect_is(extLm$wald$t1, "data.frame")
  expect_length(extLm$wald$t1, 4)
  expect_equivalent(unlist(extLm$wald$t1),
                    c(15, 14, 16.54, 2.22715086492192e-06))
})

test_that("CV is computed correctly", {
  expect_is(extLm$CV, "numeric")
  expect_length(extLm$CV, 1)
  expect_named(extLm$CV, "t1")
  expect_equivalent(extLm$CV, 0)
})

test_that("rDf is computed correctly", {
  expect_is(extLm$rDfF, "integer")
  expect_length(extLm$rDfF, 1)
  expect_named(extLm$rDfF, "t1")
  expect_equivalent(extLm$rDfF, 14)
})

test_that("rDfR is computed correctly", {
  expect_is(extLm$rDfR, "integer")
  expect_length(extLm$rDfR, 1)
  expect_named(extLm$rDfR, "t1")
  expect_equivalent(extLm$rDfR, 26)
})

test_that("correct attributes are added", {
  expect_equal(attr(x = extLm, which = "traits"), "t1")
  expect_equal(attr(x = extLm, which = "design"), "rcbd")
  expect_equal(attr(x = extLm, which = "engine"), "lme4")
})

test_that("calculated values are logically correct", {
  ## Fitted + residuals should match raw data.
  expect_equal(testTD[["E1"]]$t1, extLm$fitted$t1 + extLm$residF$t1)
  expect_equal(testTD[["E1"]]$t1, extLm$rMeans$t1 + extLm$residR$t1)
})

