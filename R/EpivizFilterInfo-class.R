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
        rowSelect <<-  NULL 
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
      getData=function(object){
        if(is.null(rowSelect)){
          object
        }else{
          object[rowSelect, ]
        }
      },
      getTrueCurHits=function(curHits){
        if(is.null(rowSelect)){
          curHits
        }else{
          curHits[rowSelect[curHits]]
        }
      },
      getFilteredCurHits=function(curHitsTrueIndices){
        if(is.null(rowSelectCumSum)){
          curHitsTrueIndices
        }else{
          rowSelectCumSum[curHitsTrueIndices]
        }
      }
    )
)