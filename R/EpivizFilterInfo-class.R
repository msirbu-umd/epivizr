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
      addRowFilter=function(object, function_filter){
        filterList <<- c(filterList, function_filter)
        
        if(is.null(rowSelect)){
          rowSelect <<- function_filter(object)
        }else{
          rowSelect <<- rowSelect & function_filter(object)
        }
        
        rowSelectCumSum <<- cumsum(rowSelect)
        
      },
      clearRowFilters=function(object, sendRequest=TRUE){
        
        filterList <<- list()
        rowSelect <<- NULL #rep(TRUE, length(container$object))
        rowSelectCumSum <<- NULL #seq(1:length(container$object))
        
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