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
#'  geom_rug() +
#'  geom_point_scramble()
#'
geom_point_scramble <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                ...) {

  ggplot2::layer(
    stat = StatPointscramble,
    geom = ggplot2::GeomPoint,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Title
#'
#' @param data
#' @param scales
#' @param sample_size
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#' cars %>%
#' rename(x = speed, y = dist) %>%
#' compute_group_scramble()
compute_group_scramble <- function(data, scales, seed = NULL) {

  if(is.null(seed)){ seed <- sample(3000:4000, 1) }

  set.seed(seed)

  data$x <- sample(data$x)
  data$y <- sample(data$y)

  data

}

StatPointscramble <- ggplot2::ggproto(
  "StatPointscramble",
  ggplot2::Stat,
  compute_group = compute_group_scramble,
  required_aes = c("x", "y")
)

