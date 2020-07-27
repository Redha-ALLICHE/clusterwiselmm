#' @title  Simulate 3 levels data
#' @description Generate univariate (for now) linear 3 level data with \code{n3} groups,
#'  \code{n2} subgroups per group and \code{n1} individuals per subgroups.
#'
#' @param n1 number of individuals at level 1. Must be strictly positive.
#' @param n2 number of subgroups at level 2. Must be strictly positive.
#' @param n3 number of groups at level 3. Must be strictly positive.
#' @param fixed_slope Fixed effect slope. Must be finite.
#' @param fixed_intercept Fixed effect intercept. Must be finite.
#' @param lvl2_corr_mat Level 2 random effect correlation matrix. Must be of type
#' \pkg{matrix} with size of 2x2
#' @param lvl3_corr_mat Level 3 random effect correlation matrix. Must be of type
#' \pkg{matrix} with size of 2x2
#' @param sigma2 variance of the residuals. Must be strictly positive.
#' @param separable_in_x logical; if TRUE, the data will be separable in x axis.
#' @param distance_factor numeric; When separable_in_x == TRUE, indicates the distances in
#' x between level 3 groups. Must be strictly positive.
#'
#' @return data frame containing the generated data containing the following list of columns :\cr
#' - x : explicative variable\cr
#' - y : target variable\cr
#' - lvl2 : factors indicating level 2 groups\cr
#' - lvl3 : factors indicating level 3 groups\cr
#' - lvl3_intercepts : numeric indicating level 3 random intercepts\cr
#' - lvl3_slopes : numeric indicating level 3 random slopes\cr
#' - lvl2_intercepts : numeric indicating level 2 random intercepts\cr
#' - lvl2_slopes : numeric indicating level 2 random slopes\cr
#'
#' @details The generated data follows the 3 levels lmm equations given below :
#'  \deqn{Y_{ijk} = \beta_{0jk} + \beta_{1jk} X_{ijk} + \epsilon_{ijk}}
#'  Level 2 :
#'  \deqn{\beta_{0jk} = \gamma_{00k} + u_{0jk}}
#'  \deqn{\beta_{1jk} = \gamma_{10k} + u_{1jk}}
#'  Level 3 :
#'  \deqn{\gamma_{00k} = \delta_{000} + u_{00k}}
#'  \deqn{\gamma_{10k} = \delta_{100} + u_{10k}}
#'  With : \cr
#'  - \eqn{\delta_{000}} and \eqn{\delta_{100}} : fixed effects intercept and slope.\cr
#'  - \eqn{u_{00k}} and \eqn{u_{10k}} : level 3 random effects intercept and slope for each level 3 group.\cr
#'  - \eqn{u_{0jk}} and \eqn{u_{0jk}} : level 2 random effects intercept and slope for each level 2 group within level 3 group.\cr
#'  - \eqn{\epsilon_{ijk}} : residual noise.
#'
#' @export
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats runif
#'
#' @examples
#' ## define correlation matrices for random effects slopes and intercepts
#' lvl2_corr_mat = matrix(c(1, 0,
#'                          0, 1), 2, 2)
#' lvl3_corr_mat = matrix(c(10, 0,
#'                          0, 10), 2, 2)
#'
#' ## to generate 3 levels data with 3 groups that contains each 10 subgroups,
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
#'
simulate_data <-
  function(n1,
           n2,
           n3,
           fixed_slope,
           fixed_intercept,
           lvl2_corr_mat,
           lvl3_corr_mat ,
           sigma2,
           separable_in_x = TRUE,
           distance_factor = 2) {
    # generate level 3 coefficients
    u_lvl3 = mvrnorm(n3, c(0, 0), lvl3_corr_mat)
    gamma00k = u_lvl3[, 1] + fixed_intercept
    gamma10k = u_lvl3[, 2] + fixed_slope
    # generate level 2 coefficients
    u_lvl2 = mvrnorm(n2 * n3, c(0, 0), lvl2_corr_mat)
    beta0jk = u_lvl2[, 1] + rep(gamma00k, each = n2)
    beta1jk = u_lvl2[, 2] + rep(gamma10k, each = n2)
    # generate x (level1)
    if (separable_in_x) {
      X = runif(n1 * n2 * n3, -1, 1) + rep((0:(n3 - 1)) * distance_factor, each =
                                             n1 * n2)
    }
    else{
      X = rep(runif(n1, 0, 10), n2 * n3)
    }
    # compute the target variable y
    Y = rep(beta0jk, each = n1) + rep(beta1jk, each = n1) * X + mvrnorm(n1 *
                                                                          n2 * n3, 0, sigma2)
    # generate level 2 and level 3 classes
    lvl3 = rep(1:n3, each = n1 * n2)
    lvl2 = rep(1:n2, each = n1)
    # generate level 2 and level 3 coefficients
    lvl3_intercepts = rep(u_lvl3[, 1], each = n1 * n2)
    lvl3_slopes = rep(u_lvl3[, 2], each = n1 * n2)
    lvl2_intercepts = rep(u_lvl2[, 1], each = n1)
    lvl2_slopes = rep(u_lvl2[, 2], each = n1)
    # return the resulting data frame
    return(data.frame(
      list(
        "x" = X,
        "y" = Y,
        "lvl2" = as.factor(lvl2),
        "lvl3" = as.factor(lvl3),
        "lvl3_intercepts" = lvl3_intercepts,
        "lvl3_slopes" = lvl3_slopes,
        "lvl2_intercepts" = lvl2_intercepts,
        "lvl2_slopes" = lvl2_slopes
      )
    ))
  }
