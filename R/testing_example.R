#' Testing clusterwise lmm
#' @description This function runs \code{clusterwiselmm} function on a simulated data set
#' and illustrate the resulting clustering. you will find this example in the vignette.
#' You can also change the parameters of the simulation.
#' @return shows plots for the level 3 clustering and the resulting one
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_point ggtitle facet_wrap aes
#'
#' @examples
#' testing_example()

testing_example <- function() {
  # generate data
  lvl2_corr_mat = matrix(c(2, 0,
                           0, 2), 2, 2)
  lvl3_corr_mat = matrix(c(1000, 0,
                           0, 50), 2, 2)
  set.seed(50)
  data = simulate_data(
    n1 = 25,
    n2 = 8,
    n3 = 3,
    fixed_slope = -2,
    fixed_intercept = 2,
    lvl2_corr_mat = lvl2_corr_mat,
    lvl3_corr_mat = lvl3_corr_mat,
    sigma2 = 1
  )
  # visualize data
  print("visualizing the intial clustering")
  print(ggplot2::ggplot(data = data) + geom_point(aes(
    x = x, y = y, col = lvl3
  )) + ggtitle("level 3 groups"))
  print(
    ggplot2::ggplot(data = data) + geom_point(aes(
      x = x, y = y, col = lvl3
    )) +
      facet_wrap(data$lvl2) + ggtitle("level 3 groups for each level 2 group")
  )
  # fit the model
  print("training the model :")
  formula = formula(y ~ x + (0 + x || lvl2))
  final_model = clusterwiselmm(
    data = data,
    target = data$y,
    K = 3,
    formula = formula,
    nb_trials = 5
  )
  # print and visualize results
  print("visualizing the resulting clustering")
  print(toString(c("Final mse :", final_model$cost)))
  print(ggplot2::ggplot(data = data) + geom_point(aes(
    x = x,
    y = y,
    col = as.factor(final_model$clusters)
  )) + ggtitle("level 3 groups"))
  print(
    ggplot2::ggplot(data = data) + geom_point(aes(
      x = x,
      y = y,
      col = as.factor(final_model$clusters)
    )) + facet_wrap(data$lvl2) + ggtitle("level 3 groups for each level 2 group")
  )
}
