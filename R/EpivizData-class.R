EpivizData <- setRefClass("EpivizData",
  contains="VIRTUAL",
  fields=list(
    object="ANY",
    mgr="EpivizDeviceMgr",
    inDevice="logical",
    id="character",
    name="character",
    columns="ANY",
    ylim="ANY",
    curQuery="ANY",
    curHits="ANY",
      
    #Add Object
    filterInfo="EpivizFilterInfo"
    #filterList="list",
    #rowSelect = "logical",
    #rowSelectCumSum = "numeric"
  ),
  methods=list(
    initialize=function(object=GNCList(GRanges()), columns=NULL, ylim=NULL, ...) {
      object <<- object
      columns <<- columns

      if (!.self$.checkColumns(columns))
        stop("Invalid 'columns' argument")

      if (is.null(.self$columns))
        columns <<- .self$.getColumns()

      naIndex <- .self$.getNAs()
      if (length(naIndex)>0) {
        object <<- object[-naIndex,]
      }
      
      if (!is.null(ylim)) {
        if (!.self$.checkLimits(ylim))
          stop("invalid 'ylim' argument")
        ylim <<- ylim
      } else {
        ylim <<- .self$.getLimits()
      }

      curQuery <<- NULL
      curHits <<- NULL
      inDevice <<- FALSE
      
      
      #At initialization you want to return everything
      #filterList <<- list()
      #rowSelect <<- rep(TRUE, length(object))
      #rowSelectCumSum <<- seq(1:length(object))
      filterInfo <<- EpivizFilterInfo$new(length(object))
        
      callSuper(...)
    },
    .getNAs=function() {
      integer()
    },
    .checkColumns=function(columns) {
      is.null(columns)
    },
    .getColumns=function() {
      NULL
    },
    .checkLimits=function(ylim) {
      is.null(ylim)
    },
    .getLimits=function() {
      NULL
    },
    ###Do I need to sendRequest for these two functions? 
    addRowFilter=function(function_filter, sendRequest=TRUE){
      if(is.function(function_filter)){
        
        filterInfo$addRowFilter(object, function_filter)
        curQuery <<- NULL
        
        return(id)
        #if (sendRequest && !mgr$isClosed())
        #  mgr$.clearDatasourceGroupCache(.self, sendRequest=sendRequest)
      }
    },
    clearRowFilters=function(sendRequest=TRUE){
      
      filterInfo$clearRowFilters()
      curQuery <<- NULL
      
      return(id)
      #if (sendRequest && !mgr$isClosed())
      #  mgr$.clearDatasourceGroupCache(.self, sendRequest=sendRequest)
    },
    ###Same question as before, do I need sendRequest here? 
    getData=function(){
      return(filterInfo$getData(object))
    },
    update=function(newObject, sendRequest=TRUE) {
#      if(class(newObject) != class(object)) {
  #      stop("class of 'newObject' is not equal to class of current 'object'")
    #  }

      oldObject <- object
      object <<- newObject

      
      if (!is.null(columns)) {
        if (!.checkColumns(columns)) {
          object <<- oldObject
          stop("columns not found in 'newObject'")
        }

        ylim <<- .getLimits()
      }

      #Should this be .self version or filterInfo version? (The difference
      #being that the latter does not set curQuery to NULL. The original
      #update made no mention of that.)
      ###
      .self$clearRowFilters()
      
      #if(is(object,"RangedSummarizedExperiment") && !is(rowRanges(object),"GIntervalTree")) {
       # rowRanges(object) <<- as(rowRanges(object), "GIntervalTree")
      #}

      naIndex <- .self$.getNAs()
      if (length(naIndex) > 0) {
        object <<- object[-naIndex,]
      }
      
      if (sendRequest && !is.null(mgr))
      if (sendRequest && !mgr$isClosed())
        mgr$.clearDatasourceGroupCache(.self, sendRequest=sendRequest)

      invisible()
    },
    getId=function() {
      return(id)
    },
    setId=function(id) {
      id <<- id
      invisible()
    },
    getName=function() {return(name)},
    setName=function(name) {
      name <<- name
      invisible()
    },
    setLimits=function(ylim) {
      if (!.checkLimits(ylim))
          stop("'invalid' limits argument")
      ylim <<- ylim
    }, 
    getMeasurements=function() {
      stop("'getMeasurements' called on virtual class object")
    },
    parseMeasurement=function(msId=NULL) {
      stop("'parseMeasurement' called on virtual class object")
    },
    setMgr=function(mgr) {
      if (!is(mgr, "EpivizDeviceMgr"))
        stop("'mgr' must be of class 'EpivizDeviceMgr'")
      
      mgr <<- mgr
      invisible()
    },
    setInDevice=function(x) {
      if (!is.logical(x))
        stop("'x' must be 'logical'")
      inDevice <<- x
      invisible()
    },
    show=function() {
      cat(class(.self), "object", id, "\n")
      methods::show(object)
      cat("\n\tcolumns:", paste(columns,collapse=","),"\n")
      cat("\tlimits:\n")
      print(ylim)
    },
    plot=function() {
      stop("'plot' method called on virtual class object")
    }
  )
)

#####
# validity
.valid.EpivizData.columns <- function(x) {
  if(!x$.checkColumns(x$columns))
    return("invalid 'columns' slot")
  NULL
}

.valid.EpivizData <- function(x) {
  c(.valid.EpivizData.columns(x))
}

setValidity2("EpivizData", .valid.EpivizData)

#######
# get data
EpivizData$methods(
  getHits=function(query) {
    if (!is(query, "GRanges"))
      stop("'query' must be a GRanges object")
    if (length(query) != 1) {
      stop("'query' must be of length 1")
    }

    #print("query is:")
    #print(query)
    
    if (is.null(curQuery) || !identical(unname(query), unname(curQuery))) {
      curQuery <<- query
      olaps <- suppressWarnings(GenomicRanges::findOverlaps(query, object, select="all"))
      curHits <<- subjectHits(olaps)

      if (length(curHits) == 0) {
        return(invisible())
      }
      
      if (!S4Vectors::isSorted(start(object)[curHits])) {
        stop("these should be ordered by now...")
     }
      curHits <<- seq(min(curHits), max(curHits))
    }
    invisible()
  },
  getRows=function(query, metadata, useOffset=FALSE) {
    if (is.null(query)) {
      out <- list(globalStartIndex=NULL, useOffset=FALSE,
                  values=list(id=list(),
                              start=list(),
                              end=list(),
                              metadata=.self$.getMetadata(integer(), metadata)))
      return(out)
    }
    
    getHits(query)
    if (length(curHits) == 0) {
      out <- list(globalStartIndex=NULL, useOffset=FALSE,
                  values=list(id=list(),
                    start=list(),
                    end=list(),
                    metadata=.self$.getMetadata(curHits, metadata)))
    } else {
      
      curHitsTrueIndices <- filterInfo$getTrueCurHits(curHits)
      filteredCurHits <- filterInfo$getFilteredCurHits(curHitsTrueIndices)
      
      if (!useOffset) {
        out <- list(globalStartIndex=filteredCurHits[1],
                  useOffset=FALSE,
                  values=list(
                    id=filteredCurHits,
                    start=start(object)[curHitsTrueIndices],
                    end=end(object)[curHitsTrueIndices],
                    ###Which one should we use for metadata?
                    metadata=.self$.getMetadata(curHitsTrueIndices, metadata)
                   ))
      } else {
        st <- start(object)[curHitsTrueRowIndices]
        stDiff <- diff(st)
        end <- end(object)[curHitsTrueRowIndices]
        endDiff <- diff(end)
        
        out <- list(globalStartIndex=filteredCurHits[1],
                    useOffset=TRUE,
                    values=list(
                      id=filteredCurHits,
                      start=c(st[1], stDiff),
                      end=c(end[1],endDiff),
                      metadata=.self$.getMetadata(curHitsTrueIndices, metadata)
                     ))
        }
    }
    if (length(out$values)>0 && length(out$values$id) == 1) {
      for (slotName in names(out$values)) {
        # TODO: switch to simplejson
        if (slotName != "metadata")
          out$values[[slotName]] <- list(out$values[[slotName]])
      }
    }
    return(out)
  },
  .getValues=function(curHits, measurement, round) {
    numeric()
  },
  getValues=function(query, measurement, round=TRUE) {
    if (is.null(query)) {
      out <- list(globalstartIndex=NULL, values=list())
      return(out)
    }
    
    getHits(query)
    
    curHitsTrueIndices <- filterInfo$getTrueCurHits(curHits)
    #print(curHitsTrueIndices)
    filteredCurHits <- filterInfo$getFilteredCurHits(curHitsTrueIndices)
    #print(filteredCurHits)
    
    if (length(curHits) == 0) {
      out <- list(globalStartIndex=NULL, values=list())
    } else {
      out <- list(globalStartIndex=filteredCurHits[1],
                  values=.self$.getValues(curHitsTrueIndices, measurement, round=round))
      if (length(out$values) ==1) {
        out$values <- list(out$values)
      }
    }
    return(out)
    
#     if (length(curHits) == 0) {
#       out <- list(globalStartIndex=NULL, values=list())
#     } else {
#       out <- list(globalStartIndex=curHits[1],
#                   values=.self$.getValues(curHits, measurement, round=round))
#       if (length(out$values) ==1) {
#         out$values <- list(out$values)
#       }
#     }
#     return(out)
  }
)



