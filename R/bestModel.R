#' The best linear model for a multi-variable problem
#'
#' @param data.frame and name of dependant variable
#' @return the object of class 'lm' which is the best fit
#' @export bestModel
#' @examples
#' lm_best <- bestModel(mtcars,"mpg")
#' summary(lm_best)
#'
bestModel <- function(df,dv){
    require(stats)
    require(MuMIn)
    #fits a best model to a multivariable regression problem
    myformula <- paste(dv,"~.",collapse = "")
    full.model <- lm(formula=as.formula(myformula), data=df, na.action = "na.fail")
    result <- dredge(full.model)
    return(get.models(result,subset=1)[[1]])
}

