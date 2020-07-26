#' MSE error
#' @description Compute cost function for clusterwise lmm that is the mean squared error between
#' the prediction and the target variable
#'
#' @param prediction vector containing the response of the clusterwise lmm to the data.
#' @param target vector containing the target variable.
#' @param residuals logical; if TRUE, returns the squared of residuals instead of the MSE
#'
#' @return mean squared error between prediction and target
#'

mse <- function(prediction, target, residuals=FALSE){
  if(length(prediction) != length(target)){
    stop("prediction and target must have the same length")
  }
  if(residuals){
    errors = (prediction-target)^2
  }
  else{
    errors = ((prediction-target)^2)/length(target)
  }
  return(errors)
}
