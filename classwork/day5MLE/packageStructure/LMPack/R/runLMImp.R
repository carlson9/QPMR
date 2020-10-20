#' Run linear models on combinations of covariates of matrix of observations
#'
#' Finds the estimated coefficients and R-squared values of all combinations of potential covariates and the importance of coefficients
#'
#' @param X A matrix object of observations of potential covariates
#' @param Y A numeric object with the same number of rows as \code{X} representing output variable.
#'
#' @return An object of class LMOutput with the elements
#'  \item{coef}{A matrix of the estimated coefficients for each combination of the potential covariates}
#'  \item{rSq}{A vector of the R-squared values for each combination of potential covariates} 
#'  \item{imp}{A vector of the coefficients in descending order of importance}
#' @author David G. Carlson
#' @examples
#' 
#' myX <- matrix(c(1,2,3,4,5,6,4,3,7,8,3,6,7,8,9),nrow=5) 
#' myY <- c(3,5,6,5,7) 
#' runLMImp(myX, myY)
#' @seealso \code{\link{runLM}}
#' @rdname runLMImp
#' @aliases runLMImp,ANY-method
#' @export
setGeneric(name="runLMImp",
           def=function(X, Y, ...)
           {standardGeneric("runLMImp")}
)

#' @export
setMethod(f="runLMImp",
          definition=function(X, Y, ...){
            outLMOutput<-runLM(X,Y)
            PrT<-summary(lm(Y~X))$coefficients[-1,4]
            PrT<-sort(PrT,decreasing=TRUE)
            outputimp<-paste("Beta",substr(names(PrT),start=2,stop=2),sep="")
            return(new("LMOutputImp", coef=outLMOutput@coef, rSq=outLMOutput@rSq, imp=outputimp))
          }
)
