# helper function to add an alias for executables
# dangerous for non-terminal people!

add_execs <- function(execs, package = "qlcData", profile = "~/.bash_profile") {
  
  for (e in execs) {
    path <- file.path(find.package(package), "exec", e)
    system(paste0("echo 'alias "
                  , e
                  , "="
                  , path
                  , "' >> "
                  , profile
    ))		
  }
  
  system(paste("source", profile))

}