\name{launch_shiny}
\alias{launch_shiny}
\title{
Launch Shiny app
}
\description{
Shiny apps allow for an easy web-based user interface for R functionality. This function is a simple helper to launch shiny apps included in this package.
}
\usage{
launch_shiny(shiny_app)
}
\arguments{
  \item{shiny_app}{
String with the name of the shiny app to launch. Currently only \code{"tokenize"} is available.
}
}
\value{
A web app is opened in a browser.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
\dontrun{
launch_shiny("tokenize")
}
}
