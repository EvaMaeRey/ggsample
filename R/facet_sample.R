

map_data_sample <- function(data, layout, params) {
  
      if (is.null(data) || nrow(data) == 0) {return(cbind(data, PANEL = integer(0)))}
                                  
      set.seed(params$seed) # adding this
                                  
      n_sampled <- params$n_sampled
      
      new_data <- lapply(seq_len(params$n), function(i) {
        cbind(data[sample(nrow(data), n_sampled), , drop = FALSE], PANEL = i)
                                  })
      do.call(rbind, new_data)
                                
      }

FacetSample <- ggplot2::ggproto(
  `_class`   = "FacetSample", 
  `_inherit` = ggplot2::FacetWrap,
  compute_layout = compute_layout_sample,
  map_data = map_data_sample
)

#' Title
#'
#' @param n_facets number of facets
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
#' # mean from samples
#' library(ggplot2)
#' ggplot(data = mtcars) +
#'    aes(x = wt) +
#'    aes(y = mpg) +
#'    facet_sample(n_sampled = 10, 
#'                 n_facets = 9,
#'                 nrow = 3) +
#'    geom_point(alpha = .5) +
#'    geom_smooth(method = lm, se = F)
#'   labs(subtitle = "Depending on our particular random sample...") 
facet_sample <- function(n_facets = 16, n_sampled = 5, nrow = NULL, ncol = NULL,
                         scales = "fixed", shrink = TRUE, strip.position = "top",
                         seed = sample(2000:3000, 1)) {

  facet <- ggplot2::facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                               shrink = shrink, strip.position = strip.position)
  
  facet$params$n <- n_facets
  facet$params$n_sampled <- n_sampled
  facet$params$seed <- seed

  ggplot2::ggproto(`_class` = NULL, 
                   `_inherit` = FacetSample,
                   shrink = shrink,
                   params = facet$params)
  
}



