setGeneric("register", signature=c("object"), 
	function(object, columns=NULL, ...) standardGeneric("register"))

setGeneric("reorderIfNeeded", signature=c("object"),
           function(object, ...) standardGeneric("reorderIfNeeded"))

setGeneric("coerceIfNeeded", signature=c("object"), 
           function(object, ...) standardGeneric("coerceIfNeeded"))

# TODO: add a sort check
setMethod("reorderIfNeeded", "GenomicRanges",
          function(object, ...) {
            stranded <- any(strand(object) != "*")
            if (stranded) {
              oobj <- object
              strand(object) <- "*"
            }
            if (!S4Vectors::isSorted(object)) {
              order <- order(object)
              if (stranded) {
                object <- oobj[order,]
              } else {
                object <- object[order,]
              }
            }
            return(object)
})

setMethod("coerceIfNeeded", "GenomicRanges", 
          function(object, ...) {
            if (!is(object, "GNCList")) {
              newobject <- as(object, "GNCList")
              mcols(newobject) <- mcols(object)
              object <- newobject
            }
            object
          })

setMethod("reorderIfNeeded", "RangedSummarizedExperiment",
          function(object, ...) {
            gr <- rowRanges(object)
            stranded <- any(strand(gr) != "*")
            if (stranded) {
              ogr <- gr
              strand(gr) <- "*"
            }
            if (!S4Vectors::isSorted(gr)) {
              order <- order(gr)
              object <- object[order,]
            }
            return(object)
})

setMethod("register", "GenomicRanges",
	function(object, columns, type=c("block","bp","geneInfo"), ...) {
		type <- match.arg(type)
    object <- reorderIfNeeded(object)
    object <- coerceIfNeeded(object)
    
		dev <- switch(type,
					  block=EpivizBlockData$new(object=object, ...),
					  bp=EpivizBpData$new(object=object, columns=columns, ...),
            geneInfo=EpivizGeneInfoData$new(object=object, ...))
		return(dev)
})

setMethod("register", "RangedSummarizedExperiment",
	function(object, columns=NULL, assay=1, metadata=NULL) {
          object <- reorderIfNeeded(object)
		      rowRanges(object) <- coerceIfNeeded(rowRanges(object))
          
          mcolNames <- names(mcols(rowRanges(object)))
          if (is.null(metadata) && !is.null(mcolNames)) {
            metadata <- mcolNames
          }
          if (!is.null(metadata) && any(!metadata %in% mcolNames)) {
            stop("invalid metadata")
          }
          EpivizFeatureData$new(object=object, columns=columns, assay=assay, metadata=metadata)
})

setMethod("register", "ExpressionSet",
	function(object, columns, annotation=NULL, assay="exprs") {
		if (is.null(annotation) || missing(annotation)) 
			annotation <- annotation(object)

		if (annotation != 'hgu133plus2') {
			stop("only 'hgu133plus2' affy chips supported for now")
		}
		
		# make GRanges object with appropriate info
		probeids = featureNames(object)
		annoName = paste0(annotation, ".db")

		if (!require(annoName, character.only=TRUE)) {
			stop("package '", annoName, "' is required")
		}
		
		res = suppressWarnings(select(get(annoName), keys=probeids, columns=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
		dups = duplicated(res$PROBEID)
		res = res[!dups,]

		drop = is.na(res$CHR) | is.na(res$CHRLOC) | is.na(res$CHRLOCEND)
		res = res[!drop,]
		
		gr = GRanges(seqnames=paste0("chr",res$CHR),
				strand=ifelse(res$CHRLOC>0, "+","-"),
				ranges=IRanges(start=abs(res$CHRLOC), end=abs(res$CHRLOCEND)))
		
		mcols(gr)[,"SYMBOL"] = res$SYMBOL
		mcols(gr)[,"PROBEID"] = res$PROBEID

    mat <- assayDataElement(object, assay)[!drop,]
    if (missing(columns) || is.null(columns))
        columns <- colnames(mat)
    
		if (any(!(columns %in% colnames(mat))))
		  stop("'columns' not found is 'assayDataElement(object, assay)'")
		
    mat <- mat[,columns]
		colnames(mat) <- columns

    if (!all(columns %in% rownames(pData(object)))) {
      pd <- data.frame(dummy=character(length(columns)))
      rownames(pd) <- columns
    } else {
      pd <- pData(object)[columns,]
    }
		sumexp <- SummarizedExperiment(assays=SimpleList(mat),
									  rowRanges=gr,
									  colData=DataFrame(pd))

		register(sumexp, columns=columns, assay=1,metadata=c("PROBEID","SYMBOL"))
})


setMethod("register", "BigWigFile",
          function(object, ...) {
            dev <- EpivizWigData$new(file=object, ...)
            return(dev)
})
          
setMethod("register", "GAlignments",
          function(object, coverage.only=TRUE, ...) {
            if (!coverage.only) {
              stop("'coverage.only' must be 'TRUE'. Only coverage supported for GAlignments for now.")
            }
            cov <- coverage(object)
            register(as(cov,"GRanges"), columns="score", type="bp", ...)
})

setMethod("register", "BamFile",
          function(object, coverage.only=TRUE, ...) {
            if (!coverage.only) {
              stop("'coverage.only' muse be 'TRUE'. Only coverage supported for BamFiles for now.")
            }
            cov <- coverage(object)
            register(as(cov,"GRanges"), columns="score", type="bp", ...)
})
setMethod("register", "OrganismDb",
          function(object, kind=c("gene","tx"), keepSeqlevels=NULL, ...) {
            epivizrMsg("creating gene annotation")
            kind <- match.arg(kind)
            gr <- genes(object, columns=c("GENEID", "SYMBOL"))
            exons <- exonsBy(object, by=kind)
            
            ids <- as.character(gr$GENEID)
            exons <- reduce(ranges(exons)[ids])
            gr$Exons <- exons
            
            if (!is.null(keepSeqlevels)) {
              gr <- keepSeqlevels(gr, keepSeqlevels)
            }
            
            nms <- names(mcols(gr))
            geneNameIdx <- match("SYMBOL", nms)
            nms[geneNameIdx] <- "Gene"
            names(mcols(gr)) <- nms

            args <- list(...)
            if (!is.null(args$type)) {
              register(gr, ...)
            } else {
              register(gr, type="geneInfo", ...)
            }
})            
setMethod("register", "OrganismDb",
          function(object, kind=c("gene","tx"), keepSeqlevels=NULL, ...) {
            epivizrMsg("creating gene annotation:")
            kind <- match.arg(kind)
            gr <- makeGeneTrackAnnotation(object, kind, keepSeqlevels)
            args <- list(...)
            if (!is.null(args$type)) {
              register(gr, ...)
            } else {
              register(gr, type="geneInfo", ...)
            }
})            
