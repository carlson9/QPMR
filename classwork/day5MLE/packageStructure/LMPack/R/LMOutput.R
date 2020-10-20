#' A linear model output object 
#' 
#' Objects of class \code{LMOutput} are created by the \code{runLM} function
#' Print function displays output
#' Plot function plots the coefficients one at a time, displaying their values for each test.
#' Show function runs the print and plot functions
#'
#' 
#' An object of the class `LMOutput' has the following slots:
#' \itemize{
#' \item \code{coef} Matrix of estimated coefficients of the linear models
#' \item \code{rSq} A vector of R-squared values for the linear models
#' }
#'
#' @author David G. Carlson \email{carlson.david@@wustl.edu}
#' @aliases LMOutput-class initialize,runLM-method getrSq,LMOutput-method getcoef,LMOutput-method
#' @rdname LMOutput
#' @export
setClass(Class="LMOutput", 
         representation = representation(
           coef = "matrix",
           rSq = "numeric"
         ),
         prototype = prototype(
           coef=matrix(),
           rSq=numeric()
         )
)

#' @export
setMethod("initialize", "LMOutput", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export 
setGeneric("getrSq",
           function(object="LMOutput")  {
             standardGeneric("getrSq")
           }
)

#' @export
setMethod("getrSq", "LMOutput",
          function(object){ 
            return(object@rSq)
          }
)

#' @export 
setGeneric("getcoef",
           function(object="LMOutput")  {
             standardGeneric("getcoef")
           }
)

#' @export
setMethod("getcoef", "LMOutput",
          function(object){ 
            return(object@coef)
          }
)

#' @export
setMethod(f="print", "LMOutput",
          definition=function(x){ 
            cat("The coefficient table\n")
            print(x@coef)
            cat("\nThe vector of R-squareds\n")
            print(x@rSq)
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
