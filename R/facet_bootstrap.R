#' Resample with replacement
#'
#' @inheritParams facet_sample_prop
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(1323)
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_count(alpha = .5) +
#'   facet_bootstrap(n_facets = 1) +
#'   facet_bootstrap(n_facets = 2) +
#'   facet_bootstrap(n_facets = 3) +
#'   ggxmean::geom_lm() +
#'   ggxmean::geom_lm_formula() +
#'   facet_bootstrap(n_facets = 15)
facet_bootstrap <- function(n_facets = 9, prop = 1, nrow = NULL, ncol = NULL,
                            scales = "fixed", shrink = TRUE, strip.position = "top",
                            seed = sample(2000:3000, 1)) {

  facet <- ggplot2::facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                      shrink = shrink, strip.position = strip.position)
  facet$params$n <- n_facets
  facet$params$prop <- prop
  facet$params$seed <- seed
  ggplot2::ggproto(NULL, FacetBootstrap,
          shrink = shrink,
          params = facet$params
  )
}

map_data_bootstrap <- function(data, layout, params) {
                            if (is.null(data) || nrow(data) == 0) {
                              return(cbind(data, PANEL = integer(0)))
                            }
                            n_samples <- round(nrow(data) * params$prop)
                            set.seed(params$seed)
                            new_data <- lapply(seq_len(params$n), function(i) {
                              cbind(data[sample(nrow(data), n_samples, replace = T), , drop = FALSE], PANEL = i)
                            })
                            do.call(rbind, new_data)
                          }

FacetBootstrap <- ggplot2::ggproto("FacetBootstrap", ggplot2::FacetWrap,
                          compute_layout = compute_layout_sample,
                          map_data = map_data_bootstrap
)



