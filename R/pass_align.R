# pass alignments from one string to another, vectorized

pass_align <- function(originals, alignment, sep = " ", in.gap = "-", out.gap = "-") {
  
  # split strings by separator
  parts_ori <- strsplit(originals, split = sep)
  parts_ali <- strsplit(alignment, split = sep)
  
  # get indices of matching parts between originals and alignment
  match <- sapply(parts_ali, "!=", in.gap, simplify = FALSE)
  
  # test whether strings match between originals and alignment
  test <- sapply(match, sum) != sapply(parts_ori, length)
  if (sum(test) > 0 & !anyNA(originals) & !anyNA(alignment)) {
    warning("Number of non-gaps does not match everywhere between strings\n
              NA inserted at non-matching strings")
  }
  
  # change gap symbol
  # this is useful when the gap symbol from the alignments 
  # occurs as character in the originals
  if (in.gap != out.gap) {
    sapply(seq_along(parts_ali)[!test], function(x) {
          parts_ali[[x]] [parts_ali[[x]] == in.gap] <<- out.gap
    }, simplify = FALSE)
  }
  
  # insert originals at non-gap sites
  sapply(seq_along(parts_ali)[!test], function(x) {
            parts_ali[[x]] [match[[x]]] <<- parts_ori[[x]]
  }, simplify = FALSE)
  
  # return result, inserting NA at non-matching strings
  result <- sapply(parts_ali, paste, collapse = sep)
  result[test] <- NA
  return(result)
}