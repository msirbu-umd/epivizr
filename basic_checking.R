
data(tcga_colon_example)
data(apColonData)

sendRequest = getOption("epivizrTestSendRequest")

mgr <- .startMGR()





#mgr=startEpiviz(workspace="qyOTB6vVnff", gists="2caf8e891201130c7daa")
blocks_dev <- mgr$addDevice(colon_blocks, "4505K colon_blocks")



#mgr <- startEpiviz(localURL= "http://localhost/index.php", openBrowser=TRUE, verbose=TRUE) #, port=7332L)
library(devtools)

install_github("epiviz/httpuv")
install_github("epiviz/epivizr@win32_daemon")


mgr <- startEpiviz(localURL= "http://localhost/index.php", openBrowser=TRUE, daemonize=TRUE)    #, verbose=TRUE)
mgr <- startEpiviz(localURL= "http://localhost/index.php", openBrowser=TRUE, port=7336L)

library(epivizr)
data(tcga_colon_example)

mgr <- startEpiviz(daemonize=TRUE, localURL= "http://localhost/index.php")

dev <- mgr$addDevice(colon_blocks, "test")



blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
mgr$service()

g <- function(x){
  keep <- width(x) > 250000
  return(keep)
}

x <- mgr$deviceList$epivizDevice_1$getMsObject()
keep <- width(colon_blocks) > 250000
mgr$updateDevice(blocks_dev, colon_blocks[keep,])

x$addRowFilter(g)

mgr$deviceList$epivizDevice_1$getMsObject()$addRowFilter(g)    mgr$deviceList$epivizDevice_1$getMsObject()$clearRowFilters()   
mgr$rmAllDevices()
  
blocks_dev2 <- mgr$addDevice(colon_blocks, "450k colon_blocks")

mgr$stopServer()

mgr <- .startMGR(openBrowser=sendRequest)
mgr$stopServer()

blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")




gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=rep((2:6),2)),
                                seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
               
msObj1 <- epivizr::register(gr1)

query <- GRanges(seqnames="chr1", ranges=IRanges(start=7, end=8))
res <- msObj1$getRows(query,character())

res <- msObj1$getValues(query,character())
f1 <- function(x){
  keep <- width(x) > 3
  return(keep)
}

msObj1$addRowFilter(f1)

gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5), seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))


gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=rep((2:6),2)),score1=seq(1,10), score2=-seq(1,10), seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))

keep <- width(gr1) > 3



msObj1 <- epivizr::register(gr1, type="bp")
res <- msObj1$getValues(query, c("score1"))

msObj1$addRowFilter(f1)

msObj1$getValues(query, c("score1"))


out <- list(globalStartIndex=2,
            values=list(6))

olaps <- suppressWarnings(GenomicRanges::findOverlaps(query, msObj1$object, select="all"))

curlHits <- subjectHits(olaps)

x <- which(keep == TRUE)

msObj1$getRows(query)

#A script I use to test commands directly. 

# source("http://bioconductor.org/biocLite.R")
# biocLite("antiProfilesData")
# library("antiProfilesData")
# 
# data(tcga_colon_example)
# data(apColonData)
# 
#mgr=startEpiviz() #workspace="qyOTB6vVnff", gists="2caf8e891201130c7daa")
mgr=startEpiviz(workspace="qyOTB6vVnff", gists="2caf8e891201130c7daa")

gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=2:11))
blocks_measure <- mgr$addMeasurements(gr, "gr_example")

#blocks_measure <- mgr$addMeasurements(colon_blocks, "450k colon_blocks")

blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
keep <- width(colon_blocks) > 250000
mgr$updateDevice(blocks_dev, colon_blocks[keep,])

#keep <- width(colon_blocks) > 250000
#mgr$updateDevice(blocks_dev, colon_blocks[keep,])


blocks_measure <- mgr$addMeasurements(colon_blocks, "450K colon_blocks")
# 
g <- function(x){
  keep <- width(x) > 250000
  return(keep)
}
# 
 blocks_measure$addRowFilter(g)
 blocks_measure$filterList
# 
# updateGNC <- blocks_measure$getData()
# 
# print(updateGNC)
# #Make sure original object is still the same
# print(blocks_measure$object)
# 
# 
# blocks_measure$clearRowFilters()
# 
# #Now there shouldn't be any difference
# blocks_measure$filterList
# updateGNC <- blocks_measure$getData()
# print(updateGNC)
# print(blocks_measure$object)