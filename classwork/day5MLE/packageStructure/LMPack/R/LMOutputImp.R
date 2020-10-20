#' A subclass containing linear model output object and a vector of coefficients in order of importance
#' 
#' Objects of class \code{LMOutputImp} are created by the \code{runLMImp} function
#' Print function displays output
#' Plot function plots the coefficients one at a time, displaying their values for each test.
#' Show function runs the print and plot functions
#'
#' 
#' An object of the class `LMOutputImp' has the following slots:
#' \itemize{
#' \item \code{coef} Matrix of estimated coefficients of the linear models
#' \item \code{rSq} A numeric vector of R-squared values for the linear models
#' \item \code{imp} A character vector of coefficients sorted by importance
#' }
#'
#' @author David G. Carlson \email{carlson.david@@wustl.edu}
#' @aliases LMOutputImp-class initialize,runLMImp-method getrSq,LMOutputImp-method getcoef,LMOutputImp-method getimp,LMOutputImp-method
#' @rdname LMOutputImp
#' @export
setClass(Class="LMOutputImp",
         contains="LMOutput",
         representation = representation(
           imp = "character"
         ),
         prototype = prototype(
           coef = matrix(),
           rSq=numeric(),
           imp=character()
         )
)

#' @export
setMethod("initialize", "LMOutputImp", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export 
setGeneric("getimp",
           function(object="LMOutputImp")  {
             standardGeneric("getimp")
           }
)

#' @export
setMethod("getimp", "LMOutputImp",
          function(object){
            return(object@imp)
          }
)

#' @export
setMethod(f="print", "LMOutputImp",
          definition=function(x){ 
            cat("The coefficient table\n")
            print(x@coef)
            cat("\nThe vector of R-squareds\n")
            print(x@rSq)
            cat("\nThe vector of coefficients sorted by importance in descending order")
            print(x@imp)
          }
)

#' @export
setMethod(f="plot", "LMOutput",
          definition=function(x){ 
            devAskNewPage(TRUE)
            for(i in 1:dim(x@coef)[1]){
              plot(1:dim(x@coef)[2],x@coef[i,],xlab="Test Number",ylab=paste("Beta",i-1,sep=""))
            }
          }          
)

#' @export
setMethod(f="show", "LMOutput",
          definition=function(object){ 
            print(object)
            plot(object)
          }          
)
