#' Extract Random Effects from galamm Object
#'
#' @param object An object
#' @param ... Other parameters
#'
#' @return Random effects
#'
#' @aliases ranef ranef.galamm
#'
#' @importFrom nlme ranef
#' @export ranef
#' @method ranef galamm
#' @export
#'
#' @author This function is derived from \code{lme4::ranef.merMod}, written by
#' Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker.
ranef.galamm <- function(object, ...) {
  ans <- object$b ## not always == c(matrix(unlist(getME(object,"b"))))
  if (!is.null(fl <- object$lmod$reTrms$flist)) {
    ## evaluate the list of matrices
    levs <- lapply(fl, levels)
    asgn <- attr(fl, "assign")
    cnms <- object$lmod$reTrms$cnms
    nc <- lengths(cnms) ## number of terms
    ## nb <- nc * lengths(levs)[asgn] ## number of cond modes per term
    nb <- diff(object$lmod$reTrms$Gp)  ## differencing group index is more robust
    nbseq <- rep.int(seq_along(nb), nb)
    ml <- split(ans, nbseq)
    for (i in seq_along(ml))
      ml[[i]] <- matrix(ml[[i]], ncol = nc[i], byrow = TRUE,
                        dimnames = list(NULL, cnms[[i]]))
    ## create a list of data frames corresponding to factors
    ans <- lapply(seq_along(fl),
                  function(i) {
                    m <- ml[asgn == i]
                    b2 <- vapply(m,nrow,numeric(1))
                    ub2 <- unique(b2)
                    if (length(ub2)>1)
                      stop("differing numbers of b per group")
                    ## if number of sets of modes != number of levels (e.g. Gaussian process/phyloglmm),
                    ##   generate numeric sequence for names

                    rnms <- if (ub2==length(levs[[i]])) levs[[i]] else seq(ub2)
                    data.frame(do.call(cbind, m),
                               row.names = rnms,
                               check.names = FALSE)
                  })
    names(ans) <- names(fl)

    # Have to implement covariance matrix for random effects later

    class(ans) <- "ranef.galamm"
  }
  ans
}
