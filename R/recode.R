# ======================
# recode data according to specifications in recoding
#=======================

recode <- function(recoding, data = NULL) {

  # expand the possible shortcuts in the formulation of a recoding
  recodings <- read.recoding(recoding)

  # try to read csv data from relative path in profile
  # when no explicit data is given here
  if (is.null(data)) {
    if (is.null(recodings$originalData)) {
      stop("Specify data, either in recoding or in function-call")
    } else {
      data <- read.csv(recodings$originalData)
    }
  }
  
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
    result <- sapply(recodings, .makeAttribute, data = data, simplify = FALSE)
    names <- unlist(sapply(result, colnames))
    result <- as.data.frame(result)
    colnames(result) <- names
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
    
    recoding$link <- unlist(recoding$link)
    recoding$values <- unlist(recoding$values)
    
    recoding$link[recoding$link == 0]  <- NA
    
    # simple when it is based on a single old attribute
    if (length(recoding$recodingOf) == 1) {
      
      if (singleColumn) {
        newAttribute <- as.factor(data)
        levels(newAttribute) <- recoding$values[recoding$link]
      } else {
        newAttribute <- as.factor(data[,recoding$recodingOf])
        
        linkNames <- names(recoding$link)
        if (!is.null(linkNames)) {
          # connect linkNames to levels newAttribite
          linkage <- match(levels(newAttribute),linkNames)
          levels(newAttribute) <- recoding$link[linkage]
        } else {
          # assume order of link matches order of levels newAttribute
          levels(newAttribute) <- recoding$values[recoding$link]
        }
        newAttribute <- as.data.frame(newAttribute)
        colnames(newAttribute) <- recoding$attribute
      }
      
    } else {
      
      # a bit more complex for combinations of attributes
      newAttribute <- data[,recoding$recodingOf, drop = FALSE]
      newAttribute <- apply(newAttribute, 1, function(x){paste(x, collapse = " + ")})
      
      if(!is.null(names(recoding$link))){
        # when link has names are in profile, use these
        match <- names(recoding$link)
      } else if (!is.null(names(recoding$originalFrequency))) {
        # when originalFrequency has names, use these
        match <- names(recoding$originalFrequency)
      } else {
        # recreate all possible interactions and use those
        match <- expand.grid(
          sapply(recoding$recodingOf, function(x){ 
            c(levels(data[,x]),NA) 
          }, simplify = FALSE )
        )
        match <- apply(match, 1, function(x){paste(x, collapse = " + ")})
      }
      
      newAttribute <- factor(newAttribute, levels = match)
      levels(newAttribute) <- recoding$values[recoding$link]
      newAttribute <- as.data.frame(newAttribute)
      colnames(newAttribute) <- recoding$attribute
        
    }
  }
  return(newAttribute)
}

