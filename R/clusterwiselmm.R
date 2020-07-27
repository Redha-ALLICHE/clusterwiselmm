#' Geometrical clusterwise method for linear multi level models
#'
#' @description This function fits a clusterwise multi level model with k clusters to a given data frame
#' using a geometrical approach. It runs the optimizer for many trials to get rid of the initialization
#' bias and reach a global minima. You can choose between a batch or sequential approach and also fix the
#' initial clustering.
#'
#' @param data data frame containing the variables named in \code{formula}.
#' @param target vector containing the target variable.
#' @param K number of clusters.
#' @param formula formula a two sided linear formula object corresponding to \code{formula} parameter
#' in \code{lmer} from \pkg{lme4} package.
#' @param nb_trials number of trials.
#' @param nb_iterations maximum number of iterations.
#' @param batch factor; if TRUE, runs a batch approach, otherwise runs a sequential one.
#' @param verbose factor; if TRUE, prints the final cost at each trial.
#' @param initialization if vector of the same length of data, use this vector as clustering initialization.
#' Otherwise, do a random initialization.
#'
#' @return list containing models as a class of clmm, the final clustering and the final cost.
#'
#' @export
#'
#' @examples
#' require(stats)
#' ## define correlation matrices for random effects slopes and intercepts
#' lvl2_corr_mat = matrix(c(1, 0,
#'                          0, 1), 2, 2)
#' lvl3_corr_mat = matrix(c(10, 0,
#'                          0, 10), 2, 2)
#'
#' ## generate 3 levels data with 3 groups that contains each 10 subgroups,
#' ## and each subgroup contains 30 individuals
#' data = simulate_data(n1 = 30,
#'                      n2 = 10,
#'                      n3 = 3,
#'                      fixed_slope = -2,
#'                      fixed_intercept = 2,
#'                      lvl2_corr_mat = lvl2_corr_mat,
#'                      lvl3_corr_mat = lvl3_corr_mat,
#'                      sigma2 = 1
#'                      )
#' ## fit a clusterwise lmm with 3 classes that tries to find level 3 clustering.
#'

clusterwiselmm <- function(data,
                           target,
                           K,
                           formula,
                           nb_trials = 50,
                           nb_iterations = 100,
                           batch = TRUE,
                           verbose = TRUE,
                           initialization = NULL) {
  cost = Inf
  if (batch) {
    trial = optimizer_batch(data,
                            target,
                            K,
                            formula,
                            nb_iterations = nb_iterations,
                            initialization = initialization)
    if (!is.null(trial)) {
      cost = trial$cost
    }
    if (verbose) {
      print(toString(c(
        "Trial : ", 1, " Final mse : ", cost
      )))
    }

    for (i in 2:nb_trials) {
      temp = optimizer_batch(data,
                             target,
                             K,
                             formula,
                             nb_iterations = nb_iterations,
                             initialization = initialization)
      if (is.null(temp)) {
        temp_cost = Inf
      }
      else {
        temp_cost = temp$cost
      }
      if (temp_cost < cost) {
        trial = temp
      }

      if (verbose) {
        print(toString(c(
          "Trial : ", i, " Final mse : ", temp_cost
        )))
      }
    }
  }

  return(trial)
}
# else{
#   trial = combination_seq(data, formule, K, target,initialization)
#   if(!is.null(trial)) cost = trial$cost
#
#   for(i in 1:nb_trials){
#     temp = combination_seq(data, formule, K, target,initialization)
#     if(is.null(temp)){
#       temp_cost = Inf
#     }
#     else{
#       temp_cost = temp$cost
#     }
#     if(temp_cost < cost){
#       trial = temp
#     }
#   }
# }
