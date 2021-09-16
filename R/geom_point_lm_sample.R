#' A geom that shows the linear model of a sample of observations
#'
#' @inheritParams geom_point_sample
#'
#' @return a geom layer
#' @export
#'
#' @examples
#' my_seed <- sample(1:100, 1)
#' library(ggplot2)
#' ggplot(data = cars) +
#'   aes(x = speed, y = dist) +
#'   geom_point() +
#'   geom_point_lm_sample() +
#'   geom_point_lm_sample(color = "darkred")
geom_point_lm_sample <- function(sample_size = 10, seed = sample(3000:4000, 1), color = "blue" ) {

list(
  geom_point_sample(sample_size = sample_size,
                    seed = seed, color = color),
  geom_lm_sample(sample_size = sample_size,
                 seed = seed, color = color)

)

}
