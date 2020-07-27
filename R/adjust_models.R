#' Adjust K linear mixed effects models
#' @description Define and adjust a linear mixed effects model for each cluster given a clustering.
#' Fitting the models is done using \pkg{lme4} package with default parameters.
#'
#' @param data data frame containing the variables named in \code{formula}.
#' @param formula a two sided linear formula object corresponding to \code{formula} parameter
#' in \code{lmer} from \pkg{lme4} package.
#' @param K number of clusters.
#' @param clusters vector corresponding to the clustering.
#'
#' @return a class "clmm" object containing the list of the adjusted K linear mixed effects models.
#'
#' @importFrom lme4 lmer
#'

adjust_models <- function(data, formula, K, clusters) {
  models = list()
  for (i in 1:K) {
    sample = data[which(clusters == i), ]
    model = tryCatch(
      suppressMessages(suppressWarnings(lmer(
        formula, sample, verbose = FALSE
      ))),
      error = function(e) {
        print(e)
      }
    )

    if (typeof(model) != "S4")
      return(NULL)
    models = append(models, model)
  }
  class(models) = "clmm"
  return(models)
}
