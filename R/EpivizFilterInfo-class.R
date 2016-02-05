EpivizFilterInfo <- setRefClass("EpivizFilterInfo",
    fields = list(
      length="numeric",
      filterList="list",
      #Any suggestions to solving this issue?
      rowSelect = "ANY",
      rowSelectCumSum = "ANY"
     
    ),
    methods=list(
      initialize=function(length = 0, ...){
        length <<- length
        filterList <<- list()
        rowSelect <- NULL
        rowSelectCumSum <<- NULL
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