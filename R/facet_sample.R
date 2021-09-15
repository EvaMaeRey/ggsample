#' Title
#'
#' @param n number of facets
#' @param nrow how many rows for facet grid
#' @param ncol how many columns for facet grid
#' @param scales should x and y scales be determined independently for facets
#' @param shrink not sure what shrink does
#' @param strip.position where facet label should go
#' @param n_sampled sample size
#' @param seed randomization start point
#'
#' @return a ggplot2 facet specification
#' @export
#'
#' @examples
#' set.seed(1323)
#' library(ggplot2)
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_point(color = "olivedrab4") +
#'   geom_smooth(method = lm,
#'               se = FALSE) +
#'   labs(caption = "Population are observations from cars correlation study dataset") +
#'   facet_sample(n_sampled = 8) +
#'   labs(title = "16 draws, random sample of 8 observations from the population") +
#'   labs(subtitle = "Depending on our particular random sample, slopes are flatter or steeper\nand intercepts vary")
facet_sample <- function(n = 16, n_sampled = 5, nrow = NULL, ncol = NULL,
                         scales = "fixed", shrink = TRUE, strip.position = "top",
                         seed = sample(2000:3000, 1)) {

  facet <- ggplot2::facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                               shrink = shrink, strip.position = strip.position)
  facet$params$n <- n
  facet$params$n_sampled <- n_sampled
  facet$params$seed <- seed

  ggplot2::ggproto(NULL, FacetSample,
                   shrink = shrink,
                   params = facet$params
  )
}

FacetSample <- ggplot2::ggproto("FacetSample", ggplot2::FacetWrap,
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
                                  # n_samples <- round(nrow(data) * params$prop)
                                  n_sampled <- params$n_sampled
                                  new_data <- lapply(seq_len(params$n), function(i) {
                                    cbind(data[sample(nrow(data), n_sampled), , drop = FALSE], PANEL = i)
                                  })
                                  do.call(rbind, new_data)
                                }
)
