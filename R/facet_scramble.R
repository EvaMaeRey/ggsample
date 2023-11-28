#' Title
#'
#' @inheritParams facet_sample_prop
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(232)
#' ggplot(data = cars) +
#'   aes(x = speed) +
#'   aes(y = dist) +
#'   geom_rug() +
#'   geom_point(color = "springgreen4",
#'            alpha = .75) +
#'   ggxmean:::geom_corrlabel() +
#'   labs(subtitle = "Pearson Correlation Coefficient(s)") +
#'   facet_scramble(n_facets = 1) +
#'   labs(title = "Disassociating variables") +
#'   facet_scramble(n_facets = 2) +
#'   facet_scramble(n_facets = 3) +
#'   facet_scramble(n_facets = 12) +
#'   ggxmean::geom_lm()
#'
facet_scramble <- function(n_facets = 9, prop = 1, nrow = NULL, ncol = NULL,
                            scales = "fixed", shrink = TRUE, strip.position = "top",
                           seed = sample(2000:3000, 1)) {

  facet <- ggplot2::facet_wrap(~.bootstrap, nrow = nrow, ncol = ncol, scales = scales,
                      shrink = shrink, strip.position = strip.position)
  facet$params$n <- n_facets
  facet$params$seed <- seed
  facet$params$prop <- prop
  ggplot2::ggproto(NULL, FacetScramble,
          shrink = shrink,
          params = facet$params
  )
}


shuffle <- function(dat){

  for(i in 1:ncol(dat)){
    dat[,i] = sample(dat[,i])
  }

  dat

}

map_data_shuffle <- function(data, layout, params) {
                            if (is.null(data) || nrow(data) == 0) {
                              return(cbind(data, PANEL = integer(0)))
                            }
                            set.seed(params$seed)
                            # n_samples <- round(nrow(data) * params$prop)
                            new_data <- lapply(seq_len(params$n), function(i) {
                              # data$y <- sample(data$y)

                              cbind(shuffle(data), PANEL = i)
                            })
                            do.call(rbind, new_data)
                          }

FacetScramble <- ggplot2::ggproto("FacetScramble", ggplot2::FacetWrap,
                          compute_layout = compute_layout_sample,
                          map_data = map_data_shuffle
)

