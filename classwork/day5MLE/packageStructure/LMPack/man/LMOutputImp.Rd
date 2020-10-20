\docType{class}
\name{LMOutputImp-class}
\alias{LMOutputImp-class}
\alias{getcoef,LMOutputImp-method}
\alias{getimp,LMOutputImp-method}
\alias{getrSq,LMOutputImp-method}
\alias{initialize,runLMImp-method}
\title{A subclass containing linear model output object and a vector of coefficients in order of importance}
\description{
Objects of class \code{LMOutputImp} are created by the
\code{runLMImp} function Print function displays output
Plot function plots the coefficients one at a time,
displaying their values for each test. Show function runs
the print and plot functions
}
\details{
An object of the class `LMOutputImp' has the following
slots: \itemize{ \item \code{coef} Matrix of estimated
coefficients of the linear models \item \code{rSq} A
numeric vector of R-squared values for the linear models
\item \code{imp} A character vector of coefficients sorted
by importance }
}
\author{
David G. Carlson \email{carlson.david@wustl.edu}
}

