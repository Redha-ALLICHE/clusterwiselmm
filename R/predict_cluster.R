#' Predict an optimal clustering
#'
#' @description Compute and return an optimal clustering that minimize the MSE error given K lmm models.
#'
#' @param data data frame containing the variables in which the clmm model has been trained.
#' @param target vector containing the target variable.
#' @param models object of class clmm that contains a list of k lmm models.
#'
#' @return vector containing the optimal clustering.
#'
#' @importFrom ramify argmin
#' @importFrom stats predict

predict_cluster <- function(data, target, models){
  if(class(models)!="clmm") stop("models must be of class clmm")

  predictions = mse(predict(models[[1]],
                            newdata=data,
                            allow.new.levels = TRUE), target, residuals=TRUE)

  for(i in 2:length(models)){
    predictions = cbind(predictions,
                        mse(predict(models[[i]],
                                    newdata=data,
                                    allow.new.levels = TRUE), target, residuals=TRUE))
    clustering = argmin(predictions, rows=FALSE)
  }
  return(clustering)
}
