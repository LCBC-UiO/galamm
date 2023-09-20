#' Function for setting up GAM terms
#'
#' This function is derived from \code{mgcv:::gam.setup}.
#'
#' @param formula Formula including smooth terms, but not \code{lme4} style
#' random effect terms.
#' @param pterms Parametric terms.
#' @param mf Model frame.
#'
#' @return A list containing all the data necessary to fit a GAMM.
#' @author Simon N Wood and Oystein Sorensen.
#'
#' @noRd
#'
#' @seealso [gamm4.setup()] and [gamm4()].
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
gam.setup <- function(formula, pterms, mf) {
  m <- length(formula$smooth.spec)

  G <- list(
    m = m, min.sp = NULL, H = NULL, pearson.extra = 0,
    n.true = -1, pterms = pterms
  )

  G$intercept <- attr(attr(mf, "terms"), "intercept") > 0

  X <- stats::model.matrix(pterms, mf)
  rownames(X) <- NULL

  G$nsdf <- ncol(X)
  G$contrasts <- attr(X, "contrasts")
  G$xlevels <- stats::.getXlevels(pterms, mf)
  G$assign <- attr(X, "assign")

  G$smooth <- list()
  G$S <- list()
  id.list <- list()
  for (i in seq_len(m)) {
    attr(formula$smooth.spec[[i]], "gamm") <- TRUE
  }

  G$off <- array(0, 0)
  first.para <- G$nsdf + 1
  sm <- list()
  newm <- 0
  id.list <- list()


  for (i in seq_len(m)) {
    id <- formula$smooth.spec[[i]]$id
    sml <- mgcv::smoothCon(formula$smooth.spec[[i]], mf, absorb.cons = TRUE)
    for (k in seq_along(sml)) {
      sml[[k]]$label <- paste(sml[[k]]$label,
        attr(formula$smooth.spec[[i]], "load.var")[[k]],
        sep = ":"
      )
    }
    ind <- seq_along(sml)
    sm[ind + newm] <- sml[ind]
    newm <- newm + length(sml)
  }

  G$m <- m <- newm ## number of actual smooths
  sm <- gam.side(sm)

  idx <- list()
  L <- matrix(0, 0, 0)
  lsp.names <- sp.names <- rep("", 0)

  for (i in seq_len(m)) {
    id <- sm[[i]]$id
    ## get the L matrix for this smooth...
    length.S <- if (is.null(sm[[i]]$updateS)) {
      length(sm[[i]]$S)
    } else {
      sm[[i]]$n.sp
    }
    Li <- if (is.null(sm[[i]]$L)) diag(length.S) else sm[[i]]$L

    if (length.S > 0) { ## there are smoothing parameters to name
      if (length.S == 1) {
        lspn <- sm[[i]]$label
      } else {
        Sname <- names(sm[[i]]$S)
        lspn <- if (is.null(Sname)) {
          paste(sm[[i]]$label, 1:length.S, sep = "")
        } else {
          paste(sm[[i]]$label, Sname, sep = "")
        }
      }
      spn <- lspn[seq_len(ncol(Li))] ## names for actual working sps
    }

    ## extend the global L matrix...
    if (is.null(id) || is.null(idx[[id]])) { ## new `id'
      if (!is.null(id)) { ## create record in `idx'
        idx[[id]]$c <- ncol(L) + 1 ## starting column in L for this `id'
        idx[[id]]$nc <- ncol(Li) ## number of columns relating to this `id'
      }
      L <- rbind(
        cbind(L, matrix(0, nrow(L), ncol(Li))),
        cbind(matrix(0, nrow(Li), ncol(L)), Li)
      )
      if (length.S > 0) { ## there are smoothing parameters to name
        sp.names <- c(sp.names, spn) ## extend the sp name vector
        lsp.names <- c(lsp.names, lspn) ## extend full.sp name vector
      }
    }
  }

  Xp <- NULL
  for (i in seq_len(m)) {
    n.para <- ncol(sm[[i]]$X)
    sm[[i]]$first.para <- first.para
    first.para <- first.para + n.para
    sm[[i]]$last.para <- first.para - 1

    if (is.null(sm[[i]]$Xp)) {
      if (!is.null(Xp)) Xp <- methods::cbind2(Xp, sm[[i]]$X)
    } else {
      if (is.null(Xp)) Xp <- X
      Xp <- methods::cbind2(Xp, sm[[i]]$Xp)
      sm[[i]]$Xp <- NULL
    }
    X <- methods::cbind2(X, sm[[i]]$X)
    sm[[i]]$X <- NULL

    G$smooth[[i]] <- sm[[i]]
  }

  if (is.null(Xp)) {
    G$cmX <- colMeans(X) ## useful for componentwise CI construction
  } else {
    G$cmX <- colMeans(Xp)

    qrx <- qr(Xp, LAPACK = TRUE)
    R <- qr.R(qrx)
    p <- ncol(R)
    rank <- mgcv::Rrank(R) ## rank of Xp/R
    QtX <- qr.qty(qrx, X)[1:rank, ]
    if (rank < p) { ## rank deficient
      R <- R[seq_len(rank), ]
      qrr <- qr(t(R), tol = 0)
      R <- qr.R(qrr)
      G$P <- forwardsolve(t(R), QtX)
    } else {
      G$P <- backsolve(R, QtX)
    }
    if (rank < p) {
      G$P <- qr.qy(qrr, rbind(G$P, matrix(0, p - rank, p)))
    }
    G$P[qrx$pivot, ] <- G$P
  }
  G$X <- X
  rm(X)
  n.p <- ncol(G$X)
  # deal with penalties

  G$sp <- rep(-1, ncol(L))

  names(G$sp) <- sp.names

  k <- 1
  if (length(idx)) for (i in seq_along(idx)) idx[[i]]$sp.done <- FALSE

  k.sp <- 0 # count through sp and S
  G$rank <- array(0, 0)

  for (i in seq_len(m)) { ## work through all smooths
    id <- sm[[i]]$id
    if (is.null(sm[[i]]$L)) nc <- length(sm[[i]]$S) else nc <- ncol(sm[[i]]$L)
    if (nc > 0) G$smooth[[i]]$sp <- G$sp[k:(k + nc - 1)]
    k <- k + nc
    sm <- G$smooth[[i]]

    for (j in seq_along(sm$S)) { # work through penalty matrices
      k.sp <- k.sp + 1
      G$off[k.sp] <- sm$first.para
      G$S[[k.sp]] <- sm$S[[j]]
      G$rank[k.sp] <- sm$rank[j]
    }
  }

  G$n.paraPen <- 0
  G$lsp0 <- rep(0, nrow(L))
  if (ncol(L) == nrow(L) && !sum(L != diag(ncol(L)))) L <- NULL

  G$L <- L
  names(G$lsp0) <- lsp.names

  G$y <- drop(mf[[formula$response]])
  ydim <- dim(G$y)
  if (!is.null(ydim) && length(ydim) < 2) dim(G$y) <- NULL

  G$n <- nrow(mf)


  if (G$nsdf > 0) {
    term.names <- colnames(G$X)[seq_len(G$nsdf)]
  } else {
    term.names <- array("", 0)
  }
  n.smooth <- length(G$smooth)
  n.sp0 <- 0

  for (i in seq_len(n.smooth)) {
    k <- 1
    jj <- seq(from = G$smooth[[i]]$first.para, to = G$smooth[[i]]$last.para, by = 1)
    if (G$smooth[[i]]$df > 0) {
      for (j in jj) {
        term.names[j] <- paste(G$smooth[[i]]$label, ".", as.character(k), sep = "")
        k <- k + 1
      }
    }
    n.sp <- length(G$smooth[[i]]$S)
    if (n.sp) { ## record sp this relates to in full sp vector
      G$smooth[[i]]$first.sp <- n.sp0 + 1
      n.sp0 <- G$smooth[[i]]$last.sp <- n.sp0 + n.sp
    }
    if (!is.null(G$smooth[[i]]$g.index)) {
      if (is.null(G$g.index)) G$g.index <- rep(FALSE, n.p)
      G$g.index[jj] <- G$smooth[[i]]$g.index
    }
  }

  G$term.names <- term.names
  G
} ## gam.setup


#' Create variable summary
#'
#' This function is derived from \code{mgcv:::variable.summary}.
#'
#' @param pf Formula for the parametric part of the model.
#' @param dl List containing all the explanatory variables in the model.
#' @param n Integer specifying the number of observations.
#'
#' @return A list containing summary statistics for each variable in the model.
#' @author Simon Wood
#' @noRd
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}

variable.summary <- function(pf, dl, n) {
  v.n <- length(dl)

  v.name <- v.name1 <- names(dl)

  k <- 0 ## counter for retained variables
  for (i in seq_len(v.n)) {
    if (length(dl[[i]]) >= n) {
      k <- k + 1
      v.name[k] <- v.name1[i] ## save names of variables of correct length
    }
  }
  if (k > 0) v.name <- v.name[seq_len(k)] else v.name <- rep("", k)

  p.name <- all.vars(pf[-2]) ## variables in parametric part (not response)
  vs <- list()
  v.n <- length(v.name)

  for (i in seq_len(v.n)) {
    if (v.name[i] %in% p.name) para <- TRUE else para <- FALSE ## is variable in the parametric part?

    if (para && is.matrix(dl[[v.name[i]]]) && ncol(dl[[v.name[i]]]) > 1) { ## parametric matrix --- a special case
      x <- matrix(apply(dl[[v.name[i]]], 2, stats::quantile, probs = 0.5, type = 3, na.rm = TRUE), 1, ncol(dl[[v.name[i]]])) ## nearest to median entries
    } else { ## anything else
      x <- dl[[v.name[i]]]
      if (is.character(x)) x <- as.factor(x)
      if (is.factor(x)) {
        x <- x[!is.na(x)]
        lx <- levels(x)
        freq <- tabulate(x)
        ii <- min((seq_along(lx))[freq == max(freq)])
        x <- factor(lx[ii], levels = lx)
      } else {
        x <- as.numeric(x)
        x <- c(min(x, na.rm = TRUE), as.numeric(stats::quantile(x, probs = .5, type = 3, na.rm = TRUE)), max(x, na.rm = TRUE)) ## 3 figure summary
      }
    }
    vs[[v.name[i]]] <- x
  }

  vs
}

#' Internal function from mgcv
#'
#' This function is derived \code{mgcv:::gam.side}.
#'
#' @param sm List of smooth terms.
#'
#' @return A list of smooth terms, with identifiability constraints imposed.
#' @author Simon Wood
#'
#' @noRd
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
gam.side <- function(sm) {
  for (i in seq_along(sm)) {
    vn <- sm[[i]]$term
    if (sm[[i]]$by != "NA") vn <- paste0(vn, sm[[i]]$by)
    if (!is.null(sm[[i]]$by.level)) vn <- paste0(vn, sm[[i]]$by.level)
    sm[[i]]$vn <- vn
  }
  sm
}


#' Internal function for interpreting GAM formulas
#'
#' This formula is based on \code{mgcv:::interpret.gam0}. It takes a formula
#' and returns a model formula for the parametric and a list of
#' descriptors for the smooths.
#'
#' @param gf A formula object.
#'
#' @return An object of class \code{split.gam.formula}, containing a
#'     specification of the smooth terms and a specification of the
#'     parametric components.
#' @noRd
#' @author Simon Wood, with modifications by Oystein Sorensen.
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
interpret.gam0 <- function(gf) {
  p.env <- environment(gf)
  tf <- stats::terms.formula(gf, specials = c("s", "t2"))

  terms <- attr(tf, "term.labels") # labels of the model terms
  nt <- length(terms) # how many terms?

  if (attr(tf, "response") > 0) { # start the replacement formulae
    response <- as.character(attr(tf, "variables")[2])
  } else {
    response <- NULL
  }
  sp <- attr(tf, "specials")$s # array of indices of smooth terms
  t2p <- attr(tf, "specials")$t2 # indices of type 2 tensor product terms

  vtab <- attr(tf, "factors") # cross tabulation of vars to terms

  for (i in seq_along(sp)) {
    ind <- (seq_len(nt))[as.logical(vtab[sp[i], ])]
    sp[i] <- ind # the term that smooth relates to
  }

  for (i in seq_along(t2p)) {
    ind <- seq_len(nt)[as.logical(vtab[t2p[i], ])]
    t2p[i] <- ind # the term that smooth relates to
  }

  k <- kt2 <- ks <- kp <- 1 # counters for terms in the 2 formulae
  len.sp <- length(sp)
  len.t2p <- length(t2p)

  ns <- len.sp + len.t2p # number of smooths
  pav <- av <- rep("", 0)
  smooth.spec <- list()

  for (i in seq_len(nt)) { # work through all terms
    if (k <= ns && ((ks <= len.sp && sp[ks] == i) ||
      (kt2 <= len.t2p && t2p[kt2] == i))) { # it's a smooth

      smooth.spec[[k]] <- eval(parse(text = paste0("galamm::", terms[[i]])), envir = p.env)

      if (ks <= len.sp && sp[ks] == i) {
        ks <- ks + 1
      } else {
        kt2 <- kt2 + 1
      }
      k <- k + 1 # counts smooth terms
    } else { # parametric
      av[kp] <- terms[i] ## element kp on rhs of parametric
      kp <- kp + 1 # counts parametric terms
    }
  }

  pf <- paste(response, "~", paste(av, collapse = " + "))
  if (attr(tf, "intercept") == 0) {
    pf <- paste(pf, "-1", sep = "")
    if (kp > 1) pfok <- 1 else pfok <- 0
  } else {
    pfok <- 1
    if (kp == 1) {
      pf <- paste(pf, "1")
    }
  }

  fake.formula <- pf

  if (length(smooth.spec) > 0) {
    for (i in seq_along(smooth.spec)) {
      nt <- length(smooth.spec[[i]]$term)
      ff1 <- paste(smooth.spec[[i]]$term[seq_len(nt)], collapse = "+")
      fake.formula <- paste(fake.formula, "+", ff1)
      if (smooth.spec[[i]]$by != "NA") {
        fake.formula <- paste(fake.formula, "+", smooth.spec[[i]]$by)
        av <- c(av, smooth.spec[[i]]$term, smooth.spec[[i]]$by)
      } else {
        av <- c(av, smooth.spec[[i]]$term)
      }
    }
  }
  fake.formula <- stats::as.formula(fake.formula, p.env)
  if (length(av)) {
    pred.formula <- stats::as.formula(paste("~", paste(av, collapse = "+")))
    pav <- all.vars(pred.formula)
    pred.formula <- stats::reformulate(pav, env = p.env)
  } else {
    pred.formula <- ~1
  }
  ret <- list(
    pf = stats::as.formula(pf, p.env), pfok = pfok, smooth.spec = smooth.spec,
    fake.formula = fake.formula, response = response, fake.names = av,
    pred.names = pav, pred.formula = pred.formula
  )
  class(ret) <- "split.gam.formula"
  ret
}
