# helper function to add an alias for executables
# dangerous for non-terminal people!

add_alias <- function(execs, package = "qlcData", shellprofile = "auto") {
  
  # which profile to choose
  if (shellprofile == "auto") {
    if (file.exists("~/.bash_profile")) {
      shellprofile = "~/.bash_profile"
    } else if (file.exists("~/.profile")) {
      shellprofile = "~/.profile"
    } else {
      shellprofile = "~/.bashrc"
    }
  }
  
  if (!file.exists(shellprofile)) {
    system2("touch", shellprofile)
  }
  
  for (e in execs) {
    path <- file.path(find.package(package), "exec", e)
    # check whether alias already exists
    if (system(paste0("grep 'alias\ ", e, "=' ", shellprofile)
               , ignore.stdout = TRUE) == 1 ) {
      system(paste0("echo 'alias "
                    , e
                    , "="
                    , path
                    , "' >> "
                    , shellprofile
      ))
      system(paste("source", shellprofile))
    }
  }
}