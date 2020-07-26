#' Fitting clusterwise linear mixed effects model on a given data
#'
#' @description run clusterwise linear mixed effects model with K clusters on the given data. This
#' function uses a batch approach where the cost are calculated along all rows and the affectation is
#' made to minimize this cost, after that the k models are adjusted.
#'
#' @param data data frame containing the variables named in \code{formula}.
#' @param target vector containing the target variable.
#' @param K number of clusters.
#' @param formula a two sided linear formula object corresponding to \code{formula} parameter
#' in \code{lmer} from \pkg{lme4} package.
#' @param nb_iterations maximum number of iterations.
#' @param initialization if vector of the same length of data, use this vector as clustering initialization.
#'
#' @return list containing the models as a class of clmm, the final clustering and the final cost.
#'

optimizer_batch <- function(data, target, K, formula, nb_iterations=100, initialization=NULL){
  # initialization
  if(is.vector(initialization)){
    if(length(initialization)==nrow(data)){
      clusters = initialization
    }
  }
  else{
    clusters = sample(1:K, nrow(data), replace=TRUE)
  }
  # fitting the model
  models = adjust_models(data, formula, K, clusters)
  if(is.null(models)) return(NULL)
  # computing the cost
  cost = mse(predict_response(data, clusters, models), target)
  clusters_f = clusters
  # main loop
  for(j in 1:nb_iterations){
    clusters = predict_cluster(data, target, models)
    models_t = adjust_models(data, formula, K, clusters)
    temp = mse(predict_response(data, clusters, models), target)
    if(temp >= cost){
      break
    }
    cost = temp
    models = models_t
    clusters_f = clusters
  }
  return(list("models"=models, "clusters"=clusters_f, "cost"=cost))
}
