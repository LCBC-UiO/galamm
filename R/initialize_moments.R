initialize_moments <- function(glmod){
  lapply(seq_along(glmod$reTrms$cnms), function(i){
    moments <- NULL
    nm <- names(glmod$reTrms$cnms)[[i]]
    eval(parse(
      text = paste0("moments <- data.frame(", nm,
                    " = levels(glmod$reTrms$flist[[1]]))"
      )))

    # First order moments
    mom1 <- glmod$reTrms$cnms[[i]]
    # Second order moments
    mom2 <- unique(apply(
      expand.grid(glmod$reTrms$cnms[[i]], glmod$reTrms$cnms[[i]]),
      1, function(x) {
        x <- factor(x, levels = intersect(mom1, x))
        paste(sort(x), collapse = ":")
      }))

    cross_moments <- which(vapply(mom2, function(x) {
      length(unique(strsplit(x, "\\:")[[1]])) == 2},
      logical(1)))

    for(x in c(mom1, mom2)){
      if(!grepl("\\:", x) || x %in% names(cross_moments)){
        a <- 0
      } else {
        a <- 1
      }

      eval(parse(text = paste0("moments$`", x,
                               "` <- rep(", a, ", nrow(moments))")))
    }

    eval(parse(text = paste0("moments$", nm, "<- NULL")))

    as.matrix(moments)

  })
}
