#' Run linear models on combinations of covariates of matrix of observations
#'
#' Finds the estimated coefficients and R-squared values of all combinations of potential covariates
#'
#' @param X A matrix object of observations of potential covariates
#' @param Y A numeric object with the same number of rows as \code{X} representing output variable.
#'
#' @return An object of class LMOutput with the elements
#'  \item{coef}{A matrix of the estimated coefficients for each combination of the potential covariates}
#'  \item{rSq}{A vector of the R-squared values for each combination of potential covariates} 
#' @author David G. Carlson
#' @examples
#' 
#' myX <- matrix(c(1,2,3,4,5,6,4,3,7,8,3,6,7,8,9),nrow=5) 
#' myY <- c(3,5,6,5,7) 
#' runLM(myX, myY)
#' @seealso \code{\link{runLMImp}}
#' @rdname runLM
#' @aliases runLM,ANY-method
#' @export
setGeneric(name="runLM",
           def=function(X, Y, ...)
           {standardGeneric("runLM")}
)

#' @export
setMethod(f="runLM",
          definition=function(X, Y, ...){
            coef.matrix<-matrix(nrow=(dim(X)[2]+1))
            rSq.vec<-numeric()
            for(i in 1:dim(X)[2]){
              combination<-combn(1:dim(X)[2],i)
              for(k in 1:dim(combination)[2]){
                temp.model<-lm(Y~X[,combination[,k]])
                templm<-numeric(dim(X)[2]+1)
                templm[c(1,combination[,k]+1)]<-coefficients(temp.model)
                coef.matrix<-cbind(coef.matrix,templm)
                rSq.vec<-c(rSq.vec,summary(temp.model)$r.squared)
                templm<-numeric()
              }
            }
            coef.matrix<-coef.matrix[,-1]
            rownames(coef.matrix)<-paste("Beta",0:(dim(coef.matrix)[1]-1),sep="")
            colnames(coef.matrix)<-paste("Test",1:dim(coef.matrix)[2])
            return(new("LMOutput", coef=coef.matrix, rSq=rSq.vec))
          }
)



