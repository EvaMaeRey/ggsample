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

FacetScramble <- ggplot2::ggproto("FacetScramble", ggplot2::FacetWrap,
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
                            new_data <- lapply(seq_len(params$n), function(i) {
                              # data$y <- sample(data$y)

                              cbind(shuffle(data), PANEL = i)
                            })
                            do.call(rbind, new_data)
                          }
)


shuffle <- function(dat){

  for(i in 1:ncol(dat)){
    dat[,i] = sample(dat[,i])
  }

  dat

}

