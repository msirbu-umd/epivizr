context("row Filter")
#Do I need to have this? Should this load before or after
#other bioconductor classes? (Mention about overriding compare)

# sendRequest=getOption("epivizrTestSendRequest")
# 
# test_that("block data fetch works", {
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
#                  seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
#   msObj1 <- epivizr::register(gr1)
#   expect_is(msObj1, "EpivizBlockData")
#   query <- GRanges(seqnames="chr1", ranges=IRanges(start=2, end=6))
#   res <- msObj1$getRows(query,character())
#   out <- list(globalStartIndex=2,
#               useOffset=FALSE,
#               values=list(id=2:6,
#                           start=2:6,
#                           end=2:6,
#                           metadata=NULL))
#   expect_equal(res,out)
# # })


test_that("default initialization is correct", {
  
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  blocks_measure <- epivizr::register(gr)
  expect_is(blocks_measure, "EpivizBlockData")
  expect_equal(length(blocks_measure$object), 10)
  
  #No filters just yet!
  expect_equal(length(blocks_measure$filterList), 0)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr))
  expect_equal(blocks_measure$rowSelectCumSum, seq(1,10))
  
  #Clearing filter should not fail and still be 0
  blocks_measure$clearRowFilters()
  expect_equal(length(blocks_measure$filterList), 0)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr))
  expect_equal(blocks_measure$rowSelectCumSum, seq(1,10))

})

test_that("adding filters works", {
  
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=2:11),
                            seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  blocks_measure <- epivizr::register(gr)
  
  f1 <- function(x){
    keep <- width(x) > 2.5
    return(keep)
  }
    
  #Adding a filter!
  blocks_measure$addRowFilter(f1)
  expect_equal(length(blocks_measure$filterList), 1)
  expect_equal(blocks_measure$filterList[[1]], f1)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr[width(gr) > 2.5]))
  expect_equal(blocks_measure$rowSelect, width(gr) > 2.5)
  expect_equal(blocks_measure$rowSelectCumSum, seq(0,9))
  
  #Try to add a non-function to filterList
  #NOTE: the checking is not sophisticated enough to verify the function 
  #is appropriate (it just checks that) it is a function.

  blocks_measure$addRowFilter(5)
  expect_equal(length(blocks_measure$filterList), 1)
    
  f2 <- function(x){
    keep <- width(x) < 8
    return(keep)
  }
    
  #Adding a second filter!
  blocks_measure$addRowFilter(f2)
  expect_equal(length(blocks_measure$filterList), 2)
  expect_equal(blocks_measure$filterList[[2]], f2)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr[width(gr) > 2.5 & width(gr) < 8]))
  expect_equal(blocks_measure$rowSelect, (width(gr) > 2.5 & width(gr) < 8))
  expect_equal(blocks_measure$rowSelectCumSum, c(0, 1, 2, 3, 4, 5, 5, 5, 5, 5))
  
  #Adding a third filter (to make sure that it's logical or!)
  f3 <- function(x){
    keep <- width(x) %% 2 == 0
    return(keep)
  }
    
  blocks_measure$addRowFilter(f3)
  expect_equal(length(blocks_measure$filterList), 3)
  expect_equal(blocks_measure$filterList[[3]], f3)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr[width(gr) > 2.5 & width(gr) < 8 & width(gr) %% 2 == 0]))
  expect_equal(blocks_measure$rowSelect, (width(gr) > 2.5 & width(gr) < 8 & 
                                            width(gr) %% 2 == 0))

  expect_equal(blocks_measure$rowSelectCumSum, c(0, 0, 1,1, 2, 2, 2, 2, 2, 2))
  
  #Now let's clear the filters!
  blocks_measure$clearRowFilters()
  expect_equal(length(blocks_measure$filterList), 0)
  expect_equal(blocks_measure$rowSelectCumSum, seq(1,10))  
})

test_that("getData works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=2:11),
                seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  blocks_measure <- epivizr::register(gr)
  
  #Filter gr outside to have a check. 
  gr_updated <- gr[width(gr) > 3]
  
  f1 <- function(x){
    keep <- width(x) > 3
    return(keep)
  }
  
  #Add a filter and see what gets returned. Should be filtered
  #GNCList (converted to GRanges to compare with original)
  blocks_measure$addRowFilter(f1)
  updatedGNC <- blocks_measure$getData()
  expect_equal(as(updatedGNC, "GRanges"), unname(gr_updated))
  
  #But the underlying data should still be the same to original gr!
  expect_equal(as(blocks_measure$object, "GRanges"), unname(gr))
    
})

#This test should return the indices that satisfy (a) the filters and
#(b) the queries with respect to the indices present in the filter object
#NOT the original data. 
test_that("getHits works properly", {
  
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=rep((2:6),2)),
                          seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  
  #print(gr)
  blocks_measure <- epivizr::register(gr)
  
  query <- GRanges(seqnames="chr1", ranges=IRanges(start=7, end=8))
  res <- blocks_measure$getRows(query,character())
  #print(res)
  out <- list(globalStartIndex=4,
             useOffset=FALSE,
              values=list(id=4:8,
                          start=4:8,
                          end=c(8,10,7,9,11),
                          metadata=NULL))
  expect_equal(res,out)
  
  f1 <- function(x){
    keep <- width(x) > 3
    return(keep)
  }
  
  blocks_measure$addRowFilter(f1)
  res <- blocks_measure$getRows(query,character())
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=2:4,
                          start=c(4,5,8),
                          end=c(8,10,11),
                          metadata=NULL))
  expect_equal(res,out)
  #print(res)
  
  
  query1 <- GRanges(seqnames="chr11", ranges=IRanges(start=101161609, 
                                                  end=106321388))
  query2 <- GRanges(seqnames="chr11", ranges=IRanges(start=96001830, 
                                                  end=101161609))
  query3 <- GRanges(seqnames="chr11", ranges=IRanges(start=106321388, 
                                                  end= 111481167))
  blocks_colon <- epivizr::register(colon_blocks)
  
  res <- blocks_colon$getRows(query1,character())
  expect_equal(res$values$id, 94:101)
  
  res <- blocks_colon$getRows(query2,character())
  expect_equal(res$values$id, c(90:94))
  
  res <- blocks_colon$getRows(query3,character())
  expect_equal(res$values$id, c(101:107))
  
  f2<- function(x){
    keep <- width(x) > 250000
    return(keep)
  }
  
  blocks_colon$addRowFilter(f2)
  
  res <- blocks_colon$getRows(query1,character())
  expect_equal(res$values$id, 47:50)
  
  res <- blocks_colon$getRows(query2,character())
  expect_equal(res$values$id, c(45:46))
  
  res <- blocks_colon$getRows(query3,character())
  expect_equal(res$values$id, c(50:53))
  
})

test_that("getValues works properly", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=rep((2:6),2)),score1=seq(1,10), score2=-seq(1,10), seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  query <- GRanges(seqnames="chr1", ranges=IRanges(start=7, end=8))
  
  msObj1 <- epivizr::register(gr1, type="bp")
  
  f1 <- function(x){
    keep <- width(x) > 3
    return(keep)
  }
  
  res <- msObj1$getValues(query, c("score1"))
  out <- list(globalStartIndex=4,
              values=seq(4,8))
  expect_equal(res,out) 
  
  msObj1$addRowFilter(f1)
  res <- msObj1$getValues(query, c("score1"))
  out <- list(globalStartIndex=2,
              values=c(4,5,8))
  expect_equal(res,out)
  
})
