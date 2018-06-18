# ==========================================================
# help function: expand values for combination of attributes
# ==========================================================

.expandValues <- function(attributes, data, all) {
  
  alldata <- data[, attributes, drop = FALSE]
  alldata <- apply(alldata, 1, function(x){
    paste(x, collapse = " + ")
  })
  freq <- table(alldata)
  
  if (all) {
    combination <- expand.grid(
      sapply( attributes, function(x){ 
        c(levels(data[,x]),NA) 
        }, simplify = FALSE )
    )
    combination <- apply(combination, 1, function(x){
      paste(x, collapse = " + ")
    })
    freq <- freq[combination]
    names(freq) <- combination
    
  }
  return(as.list(freq))
}

# ===================
# write YAML-template
# ===================

write.recoding <- function(data, attributes = NULL, all.options = FALSE, file = NULL) {
  
  # prepare data when single column
  if (is.null(dim(data))) {
    data <- as.data.frame(data)
    attributes <- 1
    colnames(data) <- 1
  } else if (is.null(attributes)) {
    attributes <- 1:ncol(data)
  }
  
  # prepare the template for one attribute
  makeTemplate <- function(attribute, data) {
    if (length(attribute) > 1) {
      originalValues <- .expandValues(attribute, data, all = all.options)
    } else {
      originalValues <- as.list(table(data[,attribute]))
    }
    link <- sapply(originalValues, function(x){NULL})
    return(list(
      recodingOf = attribute,
      attribute = NULL,
      values = list(a = NULL,b = NULL),
      link = link,
      originalFrequency = originalValues,
      comments = NULL
    ))
  }
  
  # combine all templates
  attributes <- as.list(sapply(attributes, function(x){colnames(data)[x]}))
  result <- list(
    title = NULL,
    author = NULL,
    date = format(Sys.time(),"%Y-%m-%d"),
    original_data = NULL,
    recoding = sapply(attributes, function(x) { makeTemplate(x, data) }, simplify = FALSE)
  )
  
  # return the result, defaults to a yaml-file
  if (!is.null(file)) {
    yaml <- yaml::as.yaml(result)
    yaml <- gsub("\n- recodingOf:","\n# ==========\n- recodingOf:",yaml)
    cat(yaml, file = file)
  } else {
    return(result)
  }
}

# ========================================
# Read YAML files, and normalize shortcuts
# ========================================

read.recoding <- function(recoding, file = NULL, data = NULL) {
  
  # recodings can be a file as input
  # remember any metadata already included
  if (is.character(recoding)) {
    infile <- yaml::yaml.load_file(recoding)
    meta <- infile[-which(names(infile)=="recoding")]
    recoding <- infile$recoding  
  } else {
    if (!is.null(recoding$recoding)) {
      meta <- recoding[-which(names(recoding)=="recoding")]
      recoding <- recoding$recoding      
    } else {
      meta <- NULL
    }
  }
  
  # Allow for various shortcuts in the writing of recodings
  # The following lines normalise the input to the cannonical form
  reallabels <- c(
    "recodingOf", "attribute", "values", "link",
    "originalFrequency", "doNotRecode", "comments")
  remove <- c()

  # ===============
  
  # This loop has become a mess, should be cleaned up!
  for (i in 1:length(recoding)) {
    
    # write labels in full
    names(recoding[[i]]) <- reallabels[pmatch(names(recoding[[i]]),reallabels)]    
    
    # when doNotRecode is specified, you're ready to go
    if (!is.null(recoding[[i]]$doNotRecode)) {
      # but break on possible error
      if (!is.null(recoding[[i]]$link)) {
        stop(paste("Both doNotRecode and link specified in recoding number", i))
      }
    } else {
      # if no doNotRecode: then recodingOf is necessary, otherwise break
      if (is.null(recoding[[i]]$recodingOf)) {
        stop(paste("Specify **recodingOf** for recoding number", i))
      }
      # with no link, add doNotRecode
      if (length(unlist(recoding[[i]]$link)) == 0) { 
        recoding[[i]] <- list(doNotRecode = recoding[[i]]$recodingOf)
      } else {
        
        # make attribute names if necessary
        if (is.null(recoding[[i]]$attribute)) {
          recoding[[i]]$attribute <- paste0("Att", i)
        }
        
        # just for visual inspection
        recoding[[i]]$originalFrequency <- unlist(recoding[[i]]$originalFrequency)
        
        # prepare link
        recoding[[i]]$link <- unlist(recoding[[i]]$link)
        linkNumeric <- is.numeric(recoding[[i]]$link)
        
        # prepare values if not present
        if (is.null(unlist(recoding[[i]]$values))) {
          # make values from link
          newValues <- levels(factor(recoding[[i]]$link))
          recoding[[i]]$values <- newValues
          # make valuess when none specified
          if (is.numeric(recoding[[i]]$link)) {
            recoding[[i]]$values <- paste0("val", 1:length(recoding[[i]]$values))
          } else {
            # change link to indexes
            newLink <- as.numeric(factor(recoding[[i]]$link))
            names(newLink) <- names(recoding[[i]]$link)
            recoding[[i]]$link <- newLink
          }
          names(recoding[[i]]$values) <- 1:length(newValues)
        } else {
          # take available values
          recoding[[i]]$values <- unlist(recoding[[i]]$values)
        } 
      }
    }
    
    # when data is specified, add names of original attributes
    # this leads to nicer documentation of the recoding
    if (!is.null(data)) {
      if (is.numeric(recoding[[i]]$recodingOf)) {
        recoding[[i]]$recodingOf <- colnames(data)[recoding[[i]]$recodingOf]
      }
      if (is.numeric(recoding[[i]]$doNotRecode)) {
        recoding[[i]]$doNotRecode <- colnames(data)[recoding[[i]]$doNotRecode]
      }    
    }
    # put everything in the same order
    recoding[[i]] <- recoding[[i]][reallabels]
    recoding[[i]] <- recoding[[i]][na.omit(names(recoding[[i]]))]
    
    # merge sequences of doNotRecode
    if (i > 1) {
      if (!is.null(recoding[[i]]$doNotRecode) & 
          !is.null(recoding[[i-1]]$doNotRecode)) {
        recoding[[i]]$doNotRecode <- c( recoding[[i-1]]$doNotRecode
                                       , recoding[[i]]$doNotRecode)
        remove <- c(remove,(i-1))
      }
    }
  }
  
  # remove superflous recodings because of contractions of doNotRecode
  if (!is.null(remove)) {
    recoding <- recoding[-remove]
  }
  
  # return result
  if (is.null(file)) {
    return(recoding)
  } else {
    # add metadata and write out as yaml
    if (!("date" %in% names(meta))) {
      meta <- c(list(date = format(Sys.time(),"%Y-%m-%d")), meta)
    }
    if (!("author" %in% names(meta))) {
      meta <- c(list(author = NULL), meta)
    }
    if (!("title" %in% names(meta))) {
      meta <- c(list(title = NULL), meta)
    }
    outfile <- c(meta, list(recoding = recoding))
    yaml <- yaml::as.yaml(outfile)
    yaml <- gsub("\n- recodingOf:","\n# ==========\n- recodingOf:",yaml)
    yaml <- gsub("\n- doNotRecode:","\n# ==========\n- doNotRecode:",yaml)
    cat(yaml, file = file)
  }
}