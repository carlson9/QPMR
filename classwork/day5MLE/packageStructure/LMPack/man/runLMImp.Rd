\name{runLMImp}
\alias{runLMImp}
\alias{runLMImp,ANY-method}
\title{Run linear models on combinations of covariates of matrix of observations}
\usage{
runLMImp(X, Y, ...)
}
\arguments{
  \item{X}{A matrix object of observations of potential
  covariates}

  \item{Y}{A numeric object with the same number of rows as
  \code{X} representing output variable.}
}
\value{
An object of class LMOutput with the elements \item{coef}{A
matrix of the estimated coefficients for each combination
of the potential covariates} \item{rSq}{A vector of the
R-squared values for each combination of potential
covariates} \item{imp}{A vector of the coefficients in
descending order of importance}
}
\description{
Finds the estimated coefficients and R-squared values of
all combinations of potential covariates and the importance
of coefficients
}
\examples{
myX <- matrix(c(1,2,3,4,5,6,4,3,7,8,3,6,7,8,9),nrow=5)
myY <- c(3,5,6,5,7)
runLMImp(myX, myY)
}
\author{
David G. Carlson
}
\seealso{
\code{\link{runLM}}
}

