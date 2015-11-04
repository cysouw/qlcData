# pass alignments from one string to another, vectorized

pass_align <- function(originals, alignment, gap = "-", sep = " ") {
  
  parts_ori <- strsplit(originals, split = sep)
  parts_ali <- strsplit(alignment, split = sep)
  match <- sapply(parts_ali, "!=", gap, simplify = FALSE)
  
  # test whether strings match between originals and alignment
  test <- sapply(match, sum) != sapply(parts_ori, length)
  if (sum(test) > 0) {
    warning("Number of non-gaps does not match everywhere between strings\n
              NA inserted at non-matching strings")
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