EpivizFilterInfo <- setRefClass("EpivizFilterInfo",
    fields = list(
      object="EpivizData",
      filterList="list",
      rowSelect = "logical",
      rowSelectCumSum = "numeric"
    ),
    methods=list(
      initialize=function(object, ...) {
        object <<- object
        filterList <<- list()
        #At initialization you want to return everything!
        rowSelect <<- rep(TRUE, length(object))
        rowSelectCumSum <<- seq(1:length(object))
      }
    )
)