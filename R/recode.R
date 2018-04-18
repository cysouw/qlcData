# ======================
# recode data according to specifications in recoding
#=======================

recode <- function(data, recoding) {

  # expand the possible shortcuts in the formulation of a recoding
  recodings <- read.recoding(recoding)

  # prepare data when single column
  singleColumn <- FALSE
  if (is.null(dim(data))) {
    singleColumn <- TRUE
    recodings <- recodings[[1]]
  }
  
  # Make the recoding and return result
  if (singleColumn) {
    result <- .makeAttribute(recodings, data, singleColumn = TRUE)
  } else {
    result <- as.data.frame(sapply(recodings, .makeAttribute, data = data, simplify = F))
  }
  return(result)
}

# ======================
# recoding of a single new attribute
#=======================

.makeAttribute <- function(recoding, data, singleColumn = FALSE) {
  
  # when doNotRecode is specified, do not recode attributes
  if (!is.null(recoding$doNotRecode)) {
    newAttribute <- data[,recoding$doNotRecode, drop = FALSE]
  } else {
    
    recoding$link[recoding$link == 0]  <- NA
    
    # simple when it is based on a single old attribute
    if (length(recoding$recodingOf) == 1) {
      
      if (singleColumn) {
        newAttribute <- as.factor(data)
        levels(newAttribute) <- recoding$values[recoding$link]
      } else {
        newAttribute <- data[,recoding$recodingOf, drop = FALSE]
        
        linkNames <- names(recoding$link)
        if (!is.null(linkNames)) {
          # connect linkNames to levels newAttribite
          linkage <- match(levels(newAttribute[,1]),linkNames)
          levels(newAttribute[,1]) <- recoding$values[linkage][recoding$link]
        } else {
          # assume order of link matches order of levels newAttribute
          levels(newAttribute[,1]) <- recoding$values[recoding$link]
        }
        colnames(newAttribute) <- recoding$attribute
      }
      
    } else {
      
      # a bit more complex for combinations of attributes
      newAttribute <- data[,recoding$recodingOf, drop = FALSE]
      newAttribute <- apply(newAttribute,1,function(x){paste(x, collapse = " + ")})
      
      if(!is.null(names(recoding$link))){
        # when link has names are in profile, use these
        match <- names(recoding$link)
      } else if (!is.null(names(recoding$originalFrequency))) {
        # when originalFrequency has nems, use these
        match <- names(recoding$originalFrequency)
      } else {
        # recreate all possible interactions and use those
        match <- expand.grid(
          sapply(recoding$recodingOf, function(x){ 
            c(levels(data[,x]),NA) 
          }, simplify = FALSE )
        )
        match <- apply(match,1,function(x){paste(x, collapse = " + ")})
      }
      
      newAttribute <- factor(newAttribute, levels = match)
      levels(newAttribute) <- recoding$values[recoding$link]
      newAttribute <- as.data.frame(newAttribute)
      colnames(newAttribute) <- recoding$attribute
        
    }
  }
  return(newAttribute)
}

