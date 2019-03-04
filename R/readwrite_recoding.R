# ===================
# write YAML-template
# ===================

write.recoding <- function(data, attributes = NULL, all.options = FALSE, file = NULL) {
  
  # prepare data when vector
  if (is.null(dim(data))) {
    data <- as.data.frame(data)
    attributes <- 1
    colnames(data) <- 1
  }
  # take all columns when not attributes are specified
  if (is.null(attributes)) {
    attributes <- 1:ncol(data)
  }
  # turn attributes into a list if necessary, replace numbers with names
  attributes <- as.list(sapply(attributes, function(x) {
    colnames(data[,x,drop = FALSE])
  }))
  names(attributes) <- NULL
  
  # combine all templates
  result <- list(
    title = NULL,
    author = NULL,
    date = format(Sys.time(),"%Y-%m-%d"),
    originalData = NULL,
    selectRows = NULL,
    recoding = sapply(attributes, function(x) { .makeTemplate(x, data, all.options) }, simplify = FALSE)
  )
  
  # return the result
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
  
  # This is the central loop expanding all recodings
  for (i in 1:length(recoding)) {
    recoding[[i]] <- .doExpandRecoding(recoding[[i]], counter = i, data = data)
  }
  
  # return result
  if (is.null(file)) {
    # check for data-address in profile
    if (!is.null(meta$originalData)) {
      recoding$originalData <- meta$originalData
    }
    return(recoding)
  } else {
    # add metadata and write out as yaml
    if (!("selectRows" %in% names(meta))) {
      meta <- c(list(selectRows = NULL), meta)
    }    
    if (!("originalData" %in% names(meta))) {
      meta <- c(list(originalData = deparse(substitute(data))), meta)
    }
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

# ==========================================================
# help function: prepare the template for one attribute
# ==========================================================

.makeTemplate <- function(attribute, data, all.options) {
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
    newData = NULL,
    originalFrequency = originalValues,
    comments = NULL
  ))
}

# ==========================================================
# help function: expand values for combination of attributes
# ==========================================================

.expandValues <- function(attributes, data, all) {
  
  alldata <- data[, attributes, drop = FALSE]
  alldata <- apply(alldata, 1, function(x){
    paste(x, collapse = " + ") # This separator is currently hard coded!
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

# ======================================================
# help function: expand shortcuts and normalize recoding
# =======================================================

.doExpandRecoding <- function(recoding, counter, data) {
  
  # =============
  # preliminaries
  # =============  
  
  # Allow for various shortcuts in the writing of recodings
  # The following lines normalise the input to the cannonical form
  reallabels <- c(
    "recodingOf", "attribute", "values", "link", "newData",
    "originalFrequency", "doNotRecode", "comments")
  
  # write labels in full
  names(recoding) <- reallabels[pmatch(names(recoding),reallabels)]    
  
  # make template if data is available
  if (!is.null(data)) {
    template <- write.recoding(data = data, attributes = recoding$recodingOf)
    template <- template$recoding[[1]]
  }
  
  # when doNotRecode is specified, possibly add names
  if (!is.null(recoding$doNotRecode)) {
    if (!is.null(data)) {
      recoding$doNotRecode <- colnames(data[,recoding$doNotRecode, drop = FALSE])
    }
    # but break on possible error
    if (!is.null(recoding$link)) {
      stop(paste("Both **doNotRecode** and **link** specified in recoding number"
                 , counter))
    }
  } else {
    # if no doNotRecode: then recodingOf is necessary, otherwise break
    if (is.null(recoding$recodingOf)) {
      stop(paste("Specify **recodingOf** for recoding number", counter))
    }
    
    # =============================================     
    # here starts the real expanding of information
    # =============================================
    
    # with no link, add doNotRecode
    if (length(unlist(recoding$link)) == 0) { 
      recoding <- list(doNotRecode = recoding$recodingOf)
      if (!is.null(data)) {
        recoding$doNotRecode <- template$recodingOf
      }
    } else {
      
      # if recodingOf is number and data is available, look up names
      if (is.numeric(recoding$recodingOf) & !is.null(data)) {
        recoding$recodingOf <- template$recodingOf
      }
      
      # make new attribute name if left blanc
      if (is.null(recoding$attribute)) {
        recoding$attribute <- paste0("Att", counter)
      }    
      
      # prepare link
      recoding$link <- unlist(recoding$link)
      # numeric link: turn into integer
      if (is.numeric(recoding$link)) {
        recoding$link <- as.integer(recoding$link)
      }
      # when no names for link given, assume levels of data
      if (!is.null(data) & is.null(names(recoding$link))) {
        names(recoding$link) <- names(template$link)
      }
      
      # prepare values if not present
      if (is.null(unlist(recoding$values))) {
        # make new values when none specified and link is numeric
        if (is.numeric(recoding$link)) {
          recoding$values <- paste0("val", 1:length(unique(recoding$link)))
        } else {
          # make values from link
          recoding$values <- levels(factor(recoding$link))
        } 
      }
      
      # add frequencies from data if specified
      # or copy data from profile
      # will be left empty if neither is available
      if (!is.null(data)) {
        recoding$originalFrequency <- template$originalFrequency
      } else {
        recoding$originalFrequency <- unlist(recoding$originalFrequency)
      }      
    }
  }
  
  # =======
  # cleanup
  # =======  
  
  # for yaml output: turn into lists
  recoding$link <- as.list(recoding$link)
  recoding$values <- as.list(recoding$values)
  # put everything in the same order
  recoding <- recoding[reallabels]
  # remove superfluous
  recoding <- recoding[sapply(recoding, length) > 0]
  
}
