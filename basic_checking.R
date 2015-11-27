

#A script I use to test commands directly. 

source("http://bioconductor.org/biocLite.R")
biocLite("antiProfilesData")
library("antiProfilesData")

data(tcga_colon_example)
data(apColonData)

mgr=startEpiviz() #workspace="qyOTB6vVnff", gists="2caf8e891201130c7daa")


gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=2:11))
blocks_measure <- mgr$addMeasurements(gr, "gr_example")

#blocks_measure <- mgr$addMeasurements(colon_blocks, "450k colon_blocks")

blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
keep <- width(colon_blocks) > 250000
mgr$updateDevice(blocks_dev, colon_blocks[keep,])

#keep <- width(colon_blocks) > 250000
#mgr$updateDevice(blocks_dev, colon_blocks[keep,])

g <- function(x){
  keep <- width(x) > 250000
  return(keep)
}

blocks_measure$addRowFilter(g)
blocks_measure$filterList

updateGNC <- blocks_measure$getData()

print(updateGNC)
#Make sure original object is still the same
print(blocks_measure$object)


blocks_measure$clearRowFilters()

#Now there shouldn't be any difference
blocks_measure$filterList
updateGNC <- blocks_measure$getData()
print(updateGNC)
print(blocks_measure$object)