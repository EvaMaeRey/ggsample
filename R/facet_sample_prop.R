#' Title
#'
#' @inheritParams facet_sample
#' @param prop numeric proportion to sample, defaults to .2
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1323)
#' library(ggplot2)
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point(color = "olivedrab4") +
#'   geom_smooth(method = lm, se = FALSE) +
#'   facet_sample_prop() +
#'   labs(title = "Random Sample, 20% of population")
facet_sample_prop <- function(n = 9, prop = 0.2, nrow = NULL, ncol = NULL,
                            scales = "fixed", shrink = TRUE, strip.position = "top",
                         seed = sample(2000:3000, 1)) {

  facet <- ggplot2::facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                      shrink = shrink, strip.position = strip.position)
  facet$params$n <- n
  facet$params$prop <- prop
  facet$params$seed <- seed

  ggplot2::ggproto(NULL, FacetSampleProp,
          shrink = shrink,
          params = facet$params
  )
}

FacetSampleProp <- ggplot2::ggproto("FacetSampleProp", ggplot2::FacetWrap,
                          compute_layout = function(data, params) {
                            id <- seq_len(params$n)

                            dims <- wrap_dims(params$n, params$nrow, params$ncol)
                            layout <- data.frame(PANEL = factor(id))

                            if (params$as.table) {
                              layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
                            } else {
                              layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2])
                            }
                            layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)

                            layout <- layout[order(layout$PANEL), , drop = FALSE]
                            rownames(layout) <- NULL

                            # Add scale identification
                            layout$SCALE_X <- if (params$free$x) id else 1L
                            layout$SCALE_Y <- if (params$free$y) id else 1L

                            cbind(layout, .bootstrap = id)
                          },
                          map_data = function(data, layout, params) {
                            if (is.null(data) || nrow(data) == 0) {
                              return(cbind(data, PANEL = integer(0)))
                            }
                            set.seed(params$seed)
                            n_samples <- round(nrow(data) * params$prop)
                            new_data <- lapply(seq_len(params$n), function(i) {
                              cbind(data[sample(nrow(data), n_samples), , drop = FALSE], PANEL = i)
                            })
                            do.call(rbind, new_data)
                          }
)