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
# })


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
  
  #Clearing filter should not fail and still be 0
  blocks_measure$clearRowFilters()
  expect_equal(length(blocks_measure$filterList), 0)
  expect_equal(length(blocks_measure$rowSelect[blocks_measure$rowSelect == TRUE]), 
               length(gr))
  
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
  
  #Now let's clear the filters!
  blocks_measure$clearRowFilters()
  expect_equal(length(blocks_measure$filterList), 0)
    
})
# 
# test_that("getData works", {
#   gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=2:11))
#   
#   mgr=startEpiviz()
#   
#   tryCatch({
#     blocks_measure <- mgr$addMeasurements(gr, 'gr_testing')
#     
#     #Filter gr outside to have a check. 
#     gr_updated <- gr[width(gr) > 3]
#     
#     f1 <- function(x){
#       keep <- width(x) > 3
#       return(keep)
#     }
#     
#     #Add a filter and see what gets returned. Should be filtered
#     #GNCList (converted to GRanges to compare with original)
#     blocks_measure$addRowFilter(f1)
#     updatedGNC <- blocks_measure$getData()
#     expect_equal(as(updatedGNC, "GRanges"), unname(gr_updated))
#     
#     #But the underlying data should still be the same to original gr!
#     expect_equal(as(blocks_measure$object, "GRanges"), unname(gr))
#     
#   }, finally=mgr$stopServer())
#   
# })
