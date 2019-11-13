context("Extract lme4")

modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")

test_that("the output of extract is of the proper type", {
  expect_is(extract(modelLm, what = "BLUEs"), "list")
  expect_is(extract(modelLm), "list")
  expect_length(extract(modelLm)[["E1"]], 21)
  expect_is(extract(modelLm, what = c("BLUEs", "BLUPs")), "list")
  expect_length(extract(modelLm, what = c("BLUEs", "BLUPs"))[["E1"]], 2)
})

extLm <- extract(modelLm)[["E1"]]
test_that("BLUEs are computed correctly", {
  expect_is(extLm$BLUEs, "data.frame"	)
  expect_equal(dim(extLm$BLUEs), c(15, 2))
  expect_named(extLm$BLUEs, c("genotype", "t1"))
  expect_equal(extLm$BLUEs$t1,
               c(95.3095904797624, 71.490232749484, 67.3353174166736,
                 76.5265375574466, 65.1245631058874, 88.1527137958987,
                 69.6010609226447, 104.057191025005, 105.882210564877,
                 93.5549875917668, 120.862619188253, 79.0343746452041,
                 58.2720382647674, 90.7377103038097, 56.558637897391))
})

test_that("SE of BLUEs are computed correctly", {
  expect_is(extLm$seBLUEs, "data.frame"	)
  expect_equal(dim(extLm$seBLUEs), c(15, 2))
  expect_named(extLm$seBLUEs, c("genotype", "t1"))
  expect_equal(extLm$seBLUEs$t1, rep(x = 11.2959314824773, times = 15))
})

test_that("BLUPs are computed correctly", {
  expect_is(extLm$BLUPs, "data.frame"	)
  expect_equal(dim(extLm$BLUPs), c(15, 2))
  expect_named(extLm$BLUPs, c("genotype", "t1"))
  expect_equal(extLm$BLUPs$t1,
               c(90.8473962101604, 75.547138279134, 72.8782469935908,
                 78.782186152954, 71.4581788473301, 86.250208473882,
                 74.3336372034972, 96.4663783059564, 97.6386714469136,
                 89.7203348906718, 107.261269942022, 80.3930840926121,
                 67.0564901563448, 87.9106694220613, 65.955895091741))
})

test_that("SE of BLUPs are computed correctly", {
  expect_is(extLm$seBLUPs, "data.frame"	)
  expect_equal(dim(extLm$seBLUPs), c(15, 2))
  expect_named(extLm$seBLUPs, c("genotype", "t1"))
  expect_equal(extLm$seBLUPs$t1, rep(x = 9.05328875230149, times = 15))
})

test_that("unit errors are computed correctly", {
  expect_is(extLm$ue, "data.frame"	)
  expect_equal(dim(extLm$ue), c(15, 2))
  expect_named(extLm$ue, c("genotype", "t1"))
  expect_equal(extLm$ue$t1, rep(x = 127.598068056822, times = 15))
})

test_that("heritability is computed correctly", {
  expect_is(extLm$heritability, "numeric")
  expect_length(extLm$heritability, 1)
  expect_named(extLm$heritability, "t1")
  expect_equivalent(extLm$heritability, 0.642345528552063)
})

test_that("heritability can be coerced to data.frame correctly", {
  herit <- as.data.frame(extract(modelLm))
  expect_is(herit, "data.frame")
  expect_named(herit, c("trial", "t1"))
  expect_equal(herit[1, 2], 0.642345528552063)
})

test_that("varGen is computed correctly", {
  expect_is(extLm$varGen, "numeric")
  expect_length(extLm$varGen, 1)
  expect_named(extLm$varGen, "t1")
  expect_equivalent(extLm$varGen, 229.165420190978)
})

test_that("varErr is computed correctly", {
  expect_is(extLm$varErr, "numeric")
  expect_length(extLm$varErr, 1)
  expect_named(extLm$varErr, "t1")
  expect_equivalent(extLm$varErr, 255.196101130501)
})

test_that("fitted values are computed correctly", {
  expect_is(extLm$fitted, "data.frame"	)
  expect_equal(dim(extLm$fitted), c(30, 3))
  expect_named(extLm$fitted, c("genotype", "repId", "t1"))
  expect_equal(extLm$fitted$t1,
               c(99.7866798612932, 108.327702188716, 75.7607439131956,
                 54.0015271010559, 99.580101643474, 73.8715720863562,
                 91.0390793160509, 60.8540519421759, 95.0082214675213,
                 74.7638634814925, 72.2560263937351, 60.8291490611025,
                 116.592108024542, 110.152721728589, 69.395074269599,
                 101.611699401166, 125.133130351965, 71.6058285803852,
                 89.2844764280552, 63.0648062529621, 62.542549428479,
                 86.4671991400982, 97.8254987554783, 80.7970487211581,
                 67.2197215857725, 65.3305497589332, 92.4232249596103,
                 52.2881267336795, 83.8822026321872, 83.3048858089156))
})

test_that("residuals are computed correctly", {
  expect_is(extLm$residF, "data.frame"	)
  expect_equal(dim(extLm$residF), c(30, 3))
  expect_named(extLm$residF, c("genotype", "repId", "t1"))
  expect_equal(extLm$residF$t1,
               c(10.4100152882861, -10.4100152882861, -0.945262503244722,
                 -5.91633231250682, 24.6020800908422, 1.6951658569036,
                 -24.6020800908422, -3.62709315598705, 20.7988344960203,
                 19.1982971576401, 8.79687043487081, 2.90344210613835,
                 3.07872833897939, -7.3983659912081, 3.62709315598705,
                 7.3983659912081, -3.07872833897939, -1.42602681643479,
                 8.22086480981972, 1.42602681643479, 5.91633231250682,
                 -20.7988344960203, -8.22086480981972, -8.79687043487083,
                 0.945262503244721, -1.69516585690361, -0.0685166779146422,
                 -2.90344210613835, 0.0685166779146469, -19.1982971576401))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extLm$stdResF, "data.frame"	)
  expect_equal(dim(extLm$stdResF), c(30, 3))
  expect_named(extLm$stdResF, c("genotype", "repId", "t1"))
  expect_equal(extLm$stdResF$t1,
               c(0.953917749122823, -0.953917749122824, -0.0866187661069102,
                 -0.542140837099416, 2.25440215158987, 0.155335871641509,
                 -2.25440215158987, -0.332367287021306, 1.90589320355248,
                 1.75922857982875, 0.806097840605445, 0.26605580125444,
                 0.282118087817275, -0.677946423523651, 0.332367287021306,
                 0.677946423523651, -0.282118087817275, -0.13067341913061,
                 0.753315786582037, 0.13067341913061, 0.542140837099416,
                 -1.90589320355248, -0.753315786582037, -0.806097840605447,
                 0.0866187661069101, -0.155335871641509, -0.00627849944151906,
                 -0.26605580125444, 0.00627849944151948, -1.75922857982875))
})

test_that("rMeans are computed correctly", {
  expect_is(extLm$rMeans, "data.frame"	)
  expect_equal(dim(extLm$rMeans), c(30, 3))
  expect_named(extLm$rMeans, c("genotype", "repId", "t1"))
  expect_equal(extLm$rMeans$t1,
               c(92.1958671422448, 100.736889469668, 79.8176494428456,
                 62.7859789926332, 95.1179073738719, 78.6041483672088,
                 86.5768850464488, 67.1876676836186, 92.1811805857729,
                 76.1225729289005, 74.5116749892425, 70.2264062554526,
                 102.99075877831, 101.909182610625, 75.7286900110417,
                 93.368160283202, 111.531781105734, 77.1487581573024,
                 85.4498237269603, 68.6077358298793, 71.3270013200564,
                 83.6401582583497, 93.9908460543834, 83.0526973166656,
                 71.2766271154225, 70.0631260397857, 90.5207196375936,
                 61.6853839280295, 81.9796973101705, 84.6635952563236))
})

test_that("random effects are computed correctly", {
  expect_is(extLm$ranEf, "data.frame"	)
  expect_equal(dim(extLm$ranEf), c(15, 2))
  expect_named(extLm$ranEf, c("genotype", "t1"))
  expect_equal(extLm$ranEf$t1,
               c(8.01407717623565, -7.28618075479071, -9.95507204033387,
                 -4.0511328809707, -11.3751401865946, 3.4168894399573,
                 -8.49968183042749, 13.6330592720316, 14.8053524129889,
                 6.88701585674711, 24.4279509080973, -2.44023494131264,
                 -15.7768288775799, 5.07735038813658, -16.8774239421837))
})

test_that("residuals are computed correctly for genotype random", {
  expect_is(extLm$residR, "data.frame"	)
  expect_equal(dim(extLm$residR), c(30, 3))
  expect_named(extLm$residR, c("genotype", "repId", "t1"))
  expect_equal(extLm$residR$t1,
               c(18.0008280073345, -2.81920256923777, -5.00216803289472,
                 -14.7007842040841, 29.0642743604442, -3.03741042394896,
                 -20.1398858212401, -9.96070889742971, 23.6258753777687,
                 17.8395877102321, 6.54122183936342, -6.49381508821175,
                 16.6800775852107, 0.845173126755526, -2.70652258545567,
                 15.6419051091718, 10.5226209072519, -6.968956393352,
                 12.0555175109147, -4.11690276048239, -2.86811957907051,
                 -17.9717936142719, -4.38621210872478, -11.0525190303783,
                 -3.11164302640525, -6.42774213775611, 1.83398864410209,
                 -12.3006993004884, 1.97102199993141, -20.5570066050481))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extLm$stdResR, "data.frame"	)
  expect_equal(dim(extLm$stdResR), c(30, 3))
  expect_named(extLm$stdResR, c("genotype", "repId", "t1"))
  expect_equal(extLm$stdResR$t1,
               c(1.12682238230086, -0.176477468367723, -0.313127534866673,
                 -0.920245039383041, 1.81937602322167, -0.190136923065153,
                 -1.2607239017622, -0.623524080371961, 1.47894114461141,
                 1.11672900350574, 0.409469785122474, -0.406502200062048,
                 1.04414556674126, 0.0529064549563655, -0.169423885737456,
                 0.979157668283882, 0.658698852848723, -0.436245268390323,
                 0.754655672282605, -0.257711377186513, -0.179539593150667,
                 -1.12500487679799, -0.274569701772401, -0.691869608395895,
                 -0.194783762527766, -0.402366141464758, 0.114804688551982,
                 -0.770003651170667, 0.123382752428119, -1.28683498038181))
})

test_that("Waldtest is computed correctly", {
  expect_is(extLm$wald$t1, "data.frame")
  expect_length(extLm$wald$t1, 4)
  expect_equivalent(unlist(extLm$wald$t1),
                    c(15, 14, 56.383, 6.77231470976713e-10))
})

test_that("CV is computed correctly", {
  expect_is(extLm$CV, "numeric")
  expect_length(extLm$CV, 1)
  expect_named(extLm$CV, "t1")
  expect_equivalent(extLm$CV, 19.2855479998502)
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

