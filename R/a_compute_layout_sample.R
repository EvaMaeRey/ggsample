compute_layout_sample <- function(data, params) {
  
      id     <- seq_len(params$n)
      dims   <- wrap_dims(params$n, params$nrow, params$ncol)
      layout <- data.frame(PANEL = factor(id))

      if (params$as.table) { layout$ROW <- as.integer((id - 1L) %/% dims[2] + 1L)
      } else {               layout$ROW <- as.integer(dims[1] - (id - 1L) %/% dims[2]) }
      
                             layout$COL <- as.integer((id - 1L) %% dims[2] + 1L)

      layout <- layout[order(layout$PANEL), , drop = FALSE]
                                  
      rownames(layout) <- NULL

      # Add scale identification
      layout$SCALE_X <- if (params$free$x) id else 1L
      layout$SCALE_Y <- if (params$free$y) id else 1L

      cbind(layout, .bootstrap = id)
      
      }
