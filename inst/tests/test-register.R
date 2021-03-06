context("register measurement")

openBrowser=getOption("epivizrTestSendRequest")

test_that("register measurement works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  dev <- epivizr:::register(gr)
  expect_true(validObject(dev))

  expect_is(dev, "EpivizBlockData")
  expect_is(dev$object, "GNCList")

  newgr <- as(dev$object, "GRanges")
  mcols(newgr) <- mcols(dev$object)
  expect_equal(newgr, gr)

  expect_true(is.null(dev$columns))
})

test_that("register works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),score=rnorm(10))
  dev <- epivizr::register(gr, columns="score", type="bp")
  expect_true(validObject(dev))

  expect_is(dev, "EpivizBpData")
  expect_is(dev$object, "GNCList")
  
  newgr <- as(dev$object, "GRanges")
  mcols(newgr) <- mcols(dev$object)
  
  expect_equal(newgr, unname(gr))
  expect_equal(dev$columns, "score") 
  rng=range(pretty(range(gr$score)))
  expect_equal(dev$ylim, unname(cbind(rng)))
})

test_that("register works for RangedSummarizedExperiment", {
  sset <- makeSExp()
  dev <- epivizr::register(sset, columns=c("A","B"), assay="counts2")
  expect_true(validObject(dev))

  order <- order(start(rowRanges(sset)))
  sset <- sset[order,]
  
  expect_is(dev, "EpivizFeatureData")
  expect_is(dev$object, "RangedSummarizedExperiment")
  
  gr <- as(rowRanges(dev$object), "GRanges")
  mcols(gr) <- mcols(rowRanges(dev$object))
  
  expect_false(is.null(gr$probeid))

  tmp <- rowRanges(sset)
  strand(tmp) <- "*"
  o <- order(tmp)
  
  expect_identical(gr, rowRanges(sset)[o,])
  
#  expect_identical(assays(dev$object), assays(sset)[o,])
  expect_identical(colData(dev$object), colData(sset))

  columns=c("A","B")
  expect_identical(dev$columns, columns)
  emat <- assay(sset,"counts2")[,c("A","B")]
  mat <- assay(dev$object,"counts2")[,c("A","B")]
  expect_equal(emat[o,], mat)

  rngs <- apply(emat, 2, function(x) range(pretty(range(x))))
  expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})

test_that("register works for ExpressionSet", {
  eset <- makeEset()
  dev <- epivizr::register(eset, columns=c("SAMP_1", "SAMP_2"))
  expect_true(validObject(dev))

  expect_is(dev, "EpivizFeatureData")
  expect_is(dev$object, "RangedSummarizedExperiment")

  obj <- dev$object
  gr <- rowRanges(obj)

  m <- match(gr$PROBEID, featureNames(eset))
  mat <- assay(obj)

  expect_equal(exprs(eset)[m,"SAMP_1"], mat[,"SAMP_1"], check.names=FALSE, check.attributes=FALSE)
  expect_equal(exprs(eset)[m,"SAMP_2"], mat[,"SAMP_2"], check.names=FALSE, check.attributes=FALSE)

  rngs <- apply(exprs(eset)[m,c("SAMP_1","SAMP_2")], 2, function(x) range(pretty(range(x))))
  expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})

test_that("register works for gene info granges", {
  gr <- makeGeneInfo()
  dev <- epivizr::register(gr, type="geneInfo")
  expect_true(validObject(dev))

  expect_is(dev, "EpivizGeneInfoData")
  expect_is(dev$object, "GNCList")
  expect_true(is.null(dev$columns))
})

test_that("register works for OrganismDb object", {
  library(Mus.musculus)
  dev <- epivizr::register(Mus.musculus, keepSeqlevels=paste0("chr",c(1:19,"X","Y")))
  expect_true(validObject(dev))

  expect_is(dev, "EpivizGeneInfoData")
  expect_is(dev$object, "GNCList")
  expect_true(is.null(dev$columns))
}) 
