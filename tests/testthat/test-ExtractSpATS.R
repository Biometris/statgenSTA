context("Extract SpATS")

modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")

test_that("the output of extract is of the proper type", {
  expect_is(extract(modelSp, what = "BLUEs"), "list")
  expect_is(extract(modelSp), "list")
  expect_length(extract(modelSp)[["E1"]], 20)
  expect_is(extract(modelSp, what = c("BLUEs", "BLUPs")), "list")
  expect_length(extract(modelSp, what = c("BLUEs", "BLUPs"))[["E1"]], 2)
})

extSp <- extract(modelSp)[["E1"]]
test_that("BLUEs are computed correctly", {
  expect_is(extSp$BLUEs, "data.frame")
  expect_equal(dim(extSp$BLUEs), c(15, 2))
  expect_named(extSp$BLUEs, c("genotype", "t1"))
  expect_equal(extSp$BLUEs$t1,
               c(96.9607560416685, 63.5836893851323, 64.1507981868,
                 77.8014040324403, 54.27317448703, 101.307901231223,
                 63.324581437287, 96.5487252524659, 111.319230922926,
                 101.004869992796, 128.318012419044, 99.8392557215096,
                 59.3509848665047, 82.1580227547161, 42.5354128067362))
})

test_that("SE of BLUEs are computed correctly", {
  expect_is(extSp$seBLUEs, "data.frame")
  expect_equal(dim(extSp$seBLUEs), c(15, 2))
  expect_named(extSp$seBLUEs, c("genotype", "t1"))
  expect_equal(extSp$seBLUEs$t1,
               c(11.4155595715121, 12.9760136104704, 10.2238284816119,
                 10.3192828649341, 10.5013965225556, 11.8546563682595,
                 10.651036427184, 13.9057287206705, 10.2134924520716,
                 10.6862412175175, 10.2566788922817, 13.3572544883173,
                 12.0431511418708, 11.6920803540639, 13.265723744995))
})

test_that("BLUPs are computed correctly", {
  expect_is(extSp$BLUPs, "data.frame")
  expect_equal(dim(extSp$BLUPs), c(15, 2))
  expect_named(extSp$BLUPs, c("genotype", "t1"))
  expect_equal(extSp$BLUPs$t1,
               c(87.6654652950817, 73.0399183733381, 71.5500331351354,
                 77.8085257342208, 66.4331201110017, 92.1223959073191,
                 72.0897789973347, 92.4812230530513, 98.2530432680006,
                 92.8110268938348, 108.843408137217, 82.7450710186186,
                 64.1228822756429, 86.9300093458638, 65.4836750575761))
})

test_that("SE of BLUPs are computed correctly", {
  expect_is(extSp$seBLUPs, "data.frame")
  expect_equal(dim(extSp$seBLUPs), c(15, 2))
  expect_named(extSp$seBLUPs, c("genotype", "t1"))
  expect_equal(extSp$seBLUPs$t1,
               c(9.95637211683753, 10.4422671993136, 9.76224577176412,
                 9.68937775491178, 9.66270069233701, 10.1427813001169,
                 9.62790074827744, 10.648430453699, 9.75535467774518,
                 9.76934718996732, 9.75746184789798, 10.0386116304429,
                 9.91276364350725, 9.89150302953437, 10.1340541208128))
})

test_that("heritability is computed correctly", {
  expect_is(extSp$heritability, "numeric")
  expect_length(extSp$heritability, 1)
  expect_named(extSp$heritability, "t1")
  expect_equivalent(extSp$heritability, 0.65)
})

test_that("varGen is computed correctly", {
  expect_is(extSp$varGen, "numeric")
  expect_length(extSp$varGen, 1)
  expect_named(extSp$varGen, "t1")
  expect_equivalent(extSp$varGen, 274.191950327797)
})

test_that("varSpat is computed correctly", {
  expect_is(extSp$varSpat, "matrix")
  expect_equal(dim(extSp$varSpat), c(5, 1))
  expect_equal(colnames(extSp$varSpat), "t1")
  expect_equal(as.numeric(extSp$varSpat),
               c(0.0422673961972523, 53.9578162922206, 0.000132642221207602,
                 0.00571888558225316, 8.53086903217822))
})

test_that("fitted values are computed correctly", {
  expect_is(extSp$fitted, "data.frame")
  expect_equal(dim(extSp$fitted), c(30, 2))
  expect_named(extSp$fitted, c("genotype", "t1"))
  expect_equal(extSp$fitted$t1,
               c(101.394436573511, 106.719945476499, 79.0804185103649,
                 54.7161122579793, 104.874123936337, 83.7861898237883,
                 85.7450570231875, 59.9247885813074, 104.676949941124,
                 85.9492811805233, 81.196466484715, 63.2155122240465,
                 114.533815800016, 112.452714201532, 70.3243376304674,
                 99.3117069282222, 127.191422576491, 73.9051424629386,
                 91.7572789250813, 60.7654923704086, 61.8279642715556,
                 76.798470666495, 95.3526962584523, 71.8566086301781,
                 63.9000469886032, 55.4159320215012, 85.1742447869031,
                 49.9017635707354, 91.1311828048945, 72.1194681098849))
})

test_that("residuals are computed correctly", {
  expect_is(extSp$resid, "data.frame")
  expect_equal(dim(extSp$resid), c(30, 2))
  expect_named(extSp$resid, c("genotype", "t1"))
  expect_equal(extSp$resid$t1,
               c(8.80225857606865, -8.80225857606867, -4.26493710041399,
                 -6.63091746943026, 19.3080577979796, -8.21945188052848,
                 -19.3080577979787, -2.69782979511849, 11.1301060224171,
                 8.01287945860933, -0.143569656109136, 0.517078943194306,
                 5.13702056350496, -9.69835846415172, 2.69782979511864,
                 9.69835846415162, -5.13702056350508, -3.72534069898822,
                 5.74806231279368, 3.72534069898829, 6.63091746943024,
                 -11.1301060224172, -5.74806231279369, 0.143569656109236,
                 4.264937100414, 8.2194518805284, 7.18046349479256,
                 -0.517078943194328, -7.18046349479263, -8.01287945860929))
})

test_that("standardized residuals are computed correctly", {
  expect_is(extSp$stdRes, "data.frame")
  expect_equal(dim(extSp$stdRes), c(30, 2))
  expect_named(extSp$stdRes, c("genotype", "t1"))
  expect_equal(extSp$stdRes$t1,
               c(1.06281892790892, -1.06281892790892, -0.51496508964015,
                 -0.800642759470036, 2.33132997758598, -0.992448581255369,
                 -2.33132997758588, -0.325746459928379, 1.34389228037666,
                 0.967506223779463, -0.0173351585468096, 0.0624341222540522,
                 0.620263838055629, -1.17101751286541, 0.325746459928396,
                 1.1710175128654, -0.620263838055643, -0.449812121920473,
                 0.694043394890311, 0.449812121920482, 0.800642759470034,
                 -1.34389228037667, -0.694043394890313, 0.0173351585468216,
                 0.514965089640152, 0.992448581255359, 0.866997083472755,
                 -0.0624341222540548, -0.866997083472763, -0.967506223779458))
})

test_that("rMeans are computed correctly", {
  expect_is(extSp$rMeans, "data.frame")
  expect_equal(dim(extSp$rMeans), c(30, 2))
  expect_named(extSp$rMeans, c("genotype", "t1"))
  expect_equal(extSp$rMeans$t1,
               c(97.7026310840205, 101.488545617164, 85.8325022293105,
                 68.245797612392, 95.2007406584737, 83.0350810091206,
                 90.6581454050138, 72.4644310840915, 95.9960147906498,
                 84.5234703256393, 82.2515833028451, 72.5862203593931,
                 109.213660953588, 100.91367695883, 71.3783864880633,
                 96.9381743415745, 109.444323569037, 74.0611897602377,
                 89.4999763830027, 69.7798967722086, 63.889021150218,
                 81.35486492112, 88.4016994072854, 74.5618822977662,
                 65.0305057458924, 64.8709172205356, 85.69292966158,
                 54.9454805070342, 81.9994753918212, 73.0383460098359))
})

test_that("random effects are computed correctly", {
  expect_is(extSp$ranEf, "data.frame")
  expect_equal(dim(extSp$ranEf), c(15, 2))
  expect_named(extSp$ranEf, c("genotype", "t1"))
  expect_equal(extSp$ranEf$t1,
               c(5.50682685486598, -9.11872006687767, -10.6086053050804,
                 -4.35011270599492, -15.725518329214, 9.9637574671034,
                 -10.068859442881, 10.3225846128355, 16.0944048277848,
                 10.6523884536191, 26.6847696970011, 0.586432578402862,
                 -18.0357561645729, 4.77137090564801, -16.6749633826396))
})

test_that("residuals are computed correctly for genotype random", {
  expect_is(extSp$residR, "data.frame")
  expect_equal(dim(extSp$residR), c(30, 2))
  expect_named(extSp$residR, c("genotype", "t1"))
  expect_equal(extSp$residR$t1,
               c(12.4940640655588, -3.57085871673348, -11.0170208193596,
                 -20.160602823843, 28.9814410758424, -7.46834306586075,
                 -24.221146179805, -15.2374722979027, 19.8110411728918,
                 9.43869031349327, -1.19868647423924, -8.85362919215226,
                 10.4571754099335, 1.84067877855088, 1.64378093752276,
                 12.0718910507993, 12.6100784439483, -3.88138799628724,
                 8.00536485487228, -5.28906370281173, 4.56986059076782,
                 -15.6865002770422, 1.20293453837321, -2.56170401147892,
                 3.1344783431248, -1.23553331850601, 6.66177862011567,
                 -5.56079587949308, 1.95124391828067, -8.93175735856036))
})

test_that("standardized residuals are computed correctly for genotype random", {
  expect_is(extSp$stdResR, "data.frame")
  expect_equal(dim(extSp$stdResR), c(30, 2))
  expect_named(extSp$stdResR, c("genotype", "t1"))
  expect_equal(extSp$stdResR$t1,
               c(1.07120501521698, -0.30615512662053, -0.944567587657882,
                 -1.72851193505804, 2.48478516403936, -0.640314192846519,
                 -2.07665121020414, -1.30641692399655, 1.69854152737791,
                 0.809246082606965, -0.102771920820978, -0.75908462960813,
                 0.896568057066663, 0.157814489235931, 0.140933036276511,
                 1.03500911864348, 1.08115175338928, -0.332779011361658,
                 0.686356892055635, -0.453469066152144, 0.391806665788372,
                 -1.34491528775577, 0.103136137586747, -0.219633112988599,
                 0.268741132078877, -0.105931062967711, 0.571161683715441,
                 -0.476766599199, 0.167294025403286, -0.765783113243793))
})

test_that("rDf is computed correctly", {
  expect_is(extSp$rDf, "numeric")
  expect_length(extSp$rDf, 1)
  expect_named(extSp$rDf, "t1")
  expect_equivalent(extSp$rDf, 11)
})

test_that("rDfR is computed correctly", {
  expect_is(extSp$rDfR, "numeric")
  expect_length(extSp$rDfR, 1)
  expect_named(extSp$rDfR, "t1")
  expect_equivalent(extSp$rDfR, 17)
})

test_that("effective dimensions are computed correctly", {
  expect_is(extSp$effDim, "matrix")
  expect_equal(dim(extSp$effDim), c(12, 1))
  expect_equal(colnames(extSp$effDim), "t1")
  expect_equal(as.numeric(extSp$effDim),
               c(1, 1, 1, 1, 9.14671486135065, 0.00016273208568656,
                 1.43834833591063e-05, 1.4982795165739e-05, 0.20697157790348,
                 2.15831939067247e-08, 9.66891224699188e-06, 0.00105396169741819
               ))
})

test_that("ratios of effective dimensions are computed correctly", {
  expect_is(extSp$ratEffDim, "matrix")
  expect_equal(dim(extSp$ratEffDim), c(12, 1))
  expect_equal(colnames(extSp$ratEffDim), "t1")
  expect_equal(as.numeric(extSp$ratEffDim),
               c(1, 0.65, 0, 0, 1, 1, 1, 0, 0.03, 0, 0, 0))
})

test_that("correct attributes are added", {
  expect_equal(attr(x = extSp, which = "traits"), "t1")
  expect_equal(attr(x = extSp, which = "design"), "rowcol")
  expect_equal(attr(x = extSp, which = "engine"), "SpATS")
})

test_that("calculated values are logically correct", {
  ## Fitted + residuals should match raw data.
  expect_equal(testTD[["E1"]]$t1, extSp$fitted$t1 + extSp$resid$t1)
  expect_equal(testTD[["E1"]]$t1, extSp$rMeans$t1 + extSp$residR$t1)
})
