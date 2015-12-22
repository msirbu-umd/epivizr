EpivizFilterInfo <- setRefClass("EpivizFilterInfo",
    fields = list(
      container="ANY",
      filterList="list",
      rowSelect = "logical",
      rowSelectCumSum = "numeric"
     
    ),
    methods=list(
      initialize=function(container = NULL, ...){
        container <<- container
        filterList <<- list()
        rowSelect <<-  rep(TRUE, length(container$object))
        rowSelectCumSum <<- seq(1:length(container$object))
      },
      addRowFilter=function(function_filter){
        
        filterList <<- c(filterList, function_filter)
        rowSelect <<- rowSelect & function_filter(container$object)
        rowSelectCumSum <<- cumsum(rowSelect)
          
      },
      clearRowFilters=function(sendRequest=TRUE){
        
        filterList <<- list()
        rowSelect <<- rep(TRUE, length(container$object))
        rowSelectCumSum <<- seq(1:length(container$object))
        
      },
      getData=function(){
        return(container$object[rowSelect, ])
      },
      getTrueCurHits=function(curHits){
        return(curHits[rowSelect[curHits]])
      },
      getFilteredCurHits=function(curHitsTrueIndices){
        return(rowSelectCumSum[curHitsTrueIndices])
      }
    )
)