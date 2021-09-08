#' A geom that shows a sampling of points
#'
#' @param ... all the geom_point arguments
#' @param mapping aesthetic mappings
#' @param data global data
#' @param position ggplot2 positions
#' @param na.rm logical remove na
#' @param show.legend logical, should legend be displayed for layer
#' @param inherit.aes logical, should global aes be used for this layer
#'
#' @return ggplot2 layer
#' @export
#'
#' @examples
#' # using the function
#' set.seed(92389)
#' library(ggplot2)
#' ggplot(data = cars) +
#'  aes(x = speed, y = dist) +
#'  geom_point() +
#'  geom_point_sample(color = "red") +
#'  geom_point_sample(color = "green",
#'                    sample_size = 5) +
#'  geom_point_sample(sample_size = 2,
#'                    color = "blue")
#'
geom_point_sample <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                ...) {

  message("Default sample_size is 10")

  ggplot2::layer(
    stat = StatSamplepoint,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatSamplepoint <- ggplot2::ggproto(
  "StatSamplepoint",
  ggplot2::Stat,

  compute_group = function(data, scales, sample_size = 10, seed = NULL) {

    if(is.null(seed)){ seed <- sample(3000:4000, 1) }

    set.seed(seed)

    data[sample(1:nrow(data),
                size = sample_size,
                replace = F),
         ]

  },

  required_aes = c("x", "y")
)

