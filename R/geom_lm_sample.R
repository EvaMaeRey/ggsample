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
#'   geom_smooth(method = lm,
#'              se = FALSE) +
#'    geom_point_sample(color = "red",
#'                      seed = my_seed) +
#'    geom_lm_sample(color = "red",
#'                   seed = my_seed)
#'
geom_lm_sample <- function(mapping = NULL,
                             data = NULL,
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {


  message("sample size is 10 by default")

  ggplot2::layer(
    stat = StatSamplelm,
    geom = ggplot2::GeomLine,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

StatSamplelm <- ggplot2::ggproto(
  "StatSamplelm",
  ggplot2::Stat,

  compute_group = function(data, scales, sample_size = 10, seed = NULL) {

    if(is.null(seed)){ seed <- sample(3000:4000, 1) }

    set.seed(seed)

    data[sample(1:nrow(data),
                size = sample_size,
                replace = F),
    ] ->
      data_sample

    lm(y ~ x, data = data_sample) ->
      model

    data.frame(x = data_sample$x,
               y = model$fitted.values)



  },

  required_aes = c("x", "y")
)

