\name{add_execs}
\alias{add_execs}
\title{
Add Alias to Bash Profile
}
\description{
Adding an alias for R functions to a bash profile (on UNIX-alikes only) to invoke functions from the bash command line.
}
\usage{
add_execs(execs, package = "qlcData", profile = "~/.bash_profile")
}
\arguments{
  \item{execs}{
Character vector of names of executables. Currently \code{tokenize} and \code{pass_align} are available
}
  \item{package}{
In which package to search. Currently only sensible for this package
}
  \item{profile}{
To which profile should be alias be added?
}
}
\details{
Sometimes it is useful to invoke R functionality from a bash terminal command line. The package \code{docopt} makes this much nicer than using bare \code{Rscript}. This package contains a few specifically prepared bash-interfaces to functionality of the current package. These interfaces are a bit hidden away in the \code{exec} folder of this package, the location of which you can find using \code{find.package("qlcData")}. From there you can invoke the simple bash commands \code{tokenize --help} or \code{pass_align --help}.

To make this easier, this R-function adds an alias to your bash profile for any chosen functionality. Anybody 
}
\value{
Nothing is returned
}
\author{
Michael Cysouw <cysouw@mac.com
}
\note{
This is experimental functionality! 
}
\examples{
# try something like this
add_execs("tokenize")

# then open a bash terminal and try this to see the help
# >>> tokenize -h

}
