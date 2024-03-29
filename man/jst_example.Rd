% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{jst_example}
\alias{jst_example}
\title{Get path to jstor example}
\usage{
jst_example(path = NULL)
}
\arguments{
\item{path}{Name of the example file. If \code{NULL}, the example files will be
listed.}
}
\value{
Either a character vector with the names of example files (if
\code{jst_example()} is called without supplying an argument), or a character
vector indicating the path to the example file.
}
\description{
jstor includes several sample files for demonstration purposes. This helper
makes them easy to access.
}
\details{
The code for this function was adapted from the package \code{readr}.
}
\examples{
jst_example()
jst_example("article_with_references.xml") 
}
