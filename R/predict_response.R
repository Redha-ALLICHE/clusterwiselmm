#' Returns the predicted response of the clusterwise lmm to the data given a clustering
#'
#' @param data data frame containing the variables in which the clmm model has been trained.
#' @param clusters vector that indicates the clustering. Must contain a number of clusters equal to
#' the number of models.
#' @param models object of class clmm that contains a list of k lmm models.
#'
#' @return vector containing the prediction.
#'
#' @importFrom stats predict

predict_response <- function(data, clusters, models){
  indices = c()
  prediction = c()
  if(class(models) != "clmm"){return(Inf)}
  for(i in 1:length(models)){
    temp = which(clusters==i)
    indices = c(indices, temp)
    data.sample = data[temp,]
    prediction = c(prediction, predict(models[[i]], newdata=data.sample, allow.new.levels = TRUE))
  }
  prediction = prediction[indices]
  return(prediction)
}
