#' Function for setting up GAM terms
#'
#' This function is derived from \code{mgcv:::gam.setup}.
#'
#' @param formula Formula including smooth terms, but not \code{lme4} style
#' random effect terms.
#' @param pterms Parametric terms.
#' @param mf Model data.
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
  H <- NULL
  m <- length(formula$smooth.spec)
  G <- list(
    m = m, min.sp = NULL, H = NULL, pearson.extra = 0,
    dev.extra = 0, n.true = -1, pterms = pterms
  ) ## dev.extra gets added to deviance if REML/ML used in gam.fit3

  G$intercept <- attr(attr(mf, "terms"), "intercept") > 0

  ## get any model offset. Complicated by possibility of offsets in multiple formulae...
  G$offset <- stats::model.offset(mf) # get any model offset including from offset argument

  if (!is.null(G$offset)) G$offset <- as.numeric(G$offset)

  # construct strictly parametric model matrix....

  X <- stats::model.matrix(pterms, mf)

  rownames(X) <- NULL ## save memory

  G$nsdf <- ncol(X)
  G$contrasts <- attr(X, "contrasts")
  G$xlevels <- stats::.getXlevels(pterms, mf)
  G$assign <- attr(X, "assign") # used to tell which coeffs relate to which pterms

  PP <- NULL

  # next work through smooth terms (if any) extending model matrix.....

  G$smooth <- list()
  G$S <- list()




  if (m > 0) { ## search smooth.spec[[]] for terms linked by common id's
    id.list <- list() ## id information list
    for (i in 1:m) {
      if (!is.null(formula$smooth.spec[[i]]$id)) {
        id <- as.character(formula$smooth.spec[[i]]$id)
        if (length(id.list) && id %in% names(id.list)) { ## it's an existing id
          ni <- length(id.list[[id]]$sm.i) ## number of terms so far with this id
          id.list[[id]]$sm.i[ni + 1] <- i ## adding smooth.spec index to this id's list
          ## clone smooth.spec from base smooth spec....
          base.i <- id.list[[id]]$sm.i[1]

          formula$smooth.spec[[i]] <- clone.smooth.spec(
            formula$smooth.spec[[base.i]],
            formula$smooth.spec[[i]]
          )

          ## add data for this term to the data list for basis setup...
          temp.term <- formula$smooth.spec[[i]]$term

          ## note cbind deliberate in next line, as construction will handle matrix argument
          ## correctly...
          for (j in 1:length(temp.term)) {
            id.list[[id]]$data[[j]] <- cbind(
              id.list[[id]]$data[[j]],
              get.var(temp.term[j], mf, vecMat = FALSE)
            )
          }
        } else { ## new id
          id.list[[id]] <- list(sm.i = i) ## start the array of indices of smooths with this id
          id.list[[id]]$data <- list()
          ## need to collect together all data for which this basis will be used,
          ## for basis setup...
          term <- formula$smooth.spec[[i]]$term
          for (j in 1:length(term)) id.list[[id]]$data[[j]] <- get.var(term[j], mf, vecMat = FALSE)
        } ## new id finished
      }
    }
  } ## id.list complete

  G$off <- array(0, 0)
  first.para <- G$nsdf + 1
  sm <- list()
  newm <- 0
  if (m > 0) {
    for (i in 1:m) {
      # idea here is that terms are set up in accordance with information given in formula$smooth.spec
      # appropriate basis constructor is called depending on the class of the smooth
      # constructor returns penalty matrices model matrix and basis specific information

      id <- formula$smooth.spec[[i]]$id
      if (is.null(id)) { ## regular evaluation
        sml <- mgcv::smoothCon(formula$smooth.spec[[i]], mf, NULL, TRUE,
          scale.penalty = TRUE,
          null.space.penalty = FALSE, sparse.cons = 0,
          diagonal.penalty = FALSE, apply.by = TRUE, modCon = 0
        )
      } else { ## it's a smooth with an id, so basis setup data differs from model matrix data
        names(id.list[[id]]$data) <- formula$smooth.spec[[i]]$term ## give basis data suitable names
        sml <- mgcv::smoothCon(formula$smooth.spec[[i]], id.list[[id]]$data, NULL,
          TRUE,
          n = nrow(mf), dataX = mf, scale.penalty = TRUE,
          null.space.penalty = FALSE, sparse.cons = 0,
          diagonal.penalty = FALSE, apply.by = TRUE, modCon = 0
        )
      }
      for (k in seq_along(sml)) {
        load_label <- attr(formula$smooth.spec[[i]], "load.var")[[k]]
        if (!is.null(load_label)) {
          sml[[k]]$label <- paste(sml[[k]]$label, load_label, sep = ":")
        }
      }
      ind <- 1:length(sml)
      sm[ind + newm] <- sml[ind]
      newm <- newm + length(sml)
    }
  }

  G$m <- m <- newm ## number of actual smooths

  ## at this stage, it is neccessary to impose any side conditions required
  ## for identifiability
  if (m > 0) {
    sm <- gam.side(sm, X, tol = .Machine$double.eps^.5)
  }

  ## The matrix, L, mapping the underlying log smoothing parameters to the
  ## log of the smoothing parameter multiplying the S[[i]] must be
  ## worked out...
  idx <- list() ## idx[[id]]$c contains index of first col in L relating to id
  L <- matrix(0, 0, 0)
  lsp.names <- sp.names <- rep("", 0) ## need a list of names to identify sps in global sp array
  if (m > 0) {
    for (i in 1:m) {
      id <- sm[[i]]$id
      ## get the L matrix for this smooth...
      length.S <- if (is.null(sm[[i]]$updateS)) length(sm[[i]]$S) else sm[[i]]$n.sp ## deals with possibility of non-linear penalty
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
          } ## names for all sp's
        }
        spn <- lspn[1:ncol(Li)] ## names for actual working sps
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
      } else { ## it's a repeat id => shares existing sp's
        L0 <- matrix(0, nrow(Li), ncol(L))
        if (ncol(Li) > idx[[id]]$nc) {
          stop("Later terms sharing an `id' can not have more smoothing parameters than the first such term")
        }
        L0[, idx[[id]]$c:(idx[[id]]$c + ncol(Li) - 1)] <- Li
        L <- rbind(L, L0)
        if (length.S > 0) { ## there are smoothing parameters to name
          lsp.names <- c(lsp.names, lspn) ## extend full.sp name vector
        }
      }
    }
  }

  ## create the model matrix...

  Xp <- NULL ## model matrix under prediction constraints, if given
  if (m > 0) {
    for (i in 1:m) {
      n.para <- ncol(sm[[i]]$X)
      # define which elements in the parameter vector this smooth relates to....
      sm[[i]]$first.para <- first.para
      first.para <- first.para + n.para
      sm[[i]]$last.para <- first.para - 1
      ## termwise offset handling ...
      Xoff <- attr(sm[[i]]$X, "offset")
      if (!is.null(Xoff)) {
        if (is.null(G$offset)) {
          G$offset <- Xoff
        } else {
          G$offset <- G$offset + Xoff
        }
      }
      ## model matrix accumulation ...

      ## alternative version under alternative constraint first (prediction only)
      if (is.null(sm[[i]]$Xp)) {
        if (!is.null(Xp)) Xp <- methods::cbind2(Xp, sm[[i]]$X)
      } else {
        if (is.null(Xp)) Xp <- X
        Xp <- methods::cbind2(Xp, sm[[i]]$Xp)
        sm[[i]]$Xp <- NULL
      }
      ## now version to use for fitting ...
      X <- methods::cbind2(X, sm[[i]]$X)
      sm[[i]]$X <- NULL

      G$smooth[[i]] <- sm[[i]]
    }
  }

  if (is.null(Xp)) {
    G$cmX <- colMeans(X) ## useful for componentwise CI construction
  } else {
    G$cmX <- colMeans(Xp)
    ## transform from fit params to prediction params...
    ## G$P <- qr.coef(qr(Xp),X) ## old code assumes always full rank!!

    qrx <- qr(Xp, LAPACK = TRUE)
    R <- qr.R(qrx)
    p <- ncol(R)
    rank <- mgcv::Rrank(R) ## rank of Xp/R
    QtX <- qr.qty(qrx, X)[1:rank, ]
    if (rank < p) { ## rank deficient
      R <- R[1:rank, ]
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
  ## cmX relates to computation of CIs incorportating uncertainty about the mean
  ## It may make more sense to incorporate all uncertainty about the mean,
  ## rather than just the uncertainty in the fixed effects mean. This means
  ## incorporating the mean of random effects and unconstrained smooths. Hence
  ## comment out the following.
  # if (G$nsdf>0) G$cmX[-(1:G$nsdf)] <- 0 ## zero the smooth parts here
  # else G$cmX <- G$cmX * 0
  G$X <- X
  rm(X)
  n.p <- ncol(G$X)
  # deal with penalties


  ## min.sp must be length nrow(L) to make sense
  ## sp must be length ncol(L) --- need to partition
  ## L into columns relating to free log smoothing parameters,
  ## and columns, L0, corresponding to values supplied in sp.
  ## lsp0 = L0%*%log(sp[sp>=0]) [need to fudge sp==0 case by
  ## setting log(0) to log(effective zero) computed case-by-case]

  ## following deals with supplied and estimated smoothing parameters...
  ## first process the `sp' array supplied to `gam'...


  G$sp <- rep(-1, ncol(L))

  names(G$sp) <- sp.names

  ## now work through the smooths searching for any `sp' elements
  ## supplied in `s' or `te' terms.... This relies on `idx' created
  ## above...

  k <- 1 ## current location in `sp' array
  if (m > 0) {
    for (i in 1:m) {
      id <- sm[[i]]$id
      if (is.null(sm[[i]]$L)) Li <- diag(length(sm[[i]]$S)) else Li <- sm[[i]]$L
      if (is.null(id)) { ## it's a smooth without an id
        spi <- sm[[i]]$sp
        if (!is.null(spi)) { ## sp supplied in `s' or `te'
          if (length(spi) != ncol(Li)) stop("incorrect number of smoothing parameters supplied for a smooth term")
          G$sp[k:(k + ncol(Li) - 1)] <- spi
        }
        k <- k + ncol(Li)
      } else { ## smooth has an id
        spi <- sm[[i]]$sp
        if (is.null(idx[[id]]$sp.done)) { ## not already dealt with these sp's
          if (!is.null(spi)) { ## sp supplied in `s' or `te'
            if (length(spi) != ncol(Li)) stop("incorrect number of smoothing parameters supplied for a smooth term")
            G$sp[idx[[id]]$c:(idx[[id]]$c + idx[[id]]$nc - 1)] <- spi
          }
          idx[[id]]$sp.done <- TRUE ## only makes sense to use supplied `sp' from defining term
          k <- k + idx[[id]]$nc
        }
      }
    }
  } ## finished processing `sp' vectors supplied in `s' or `te' terms

  ## copy initial sp's back into smooth objects, so there is a record of
  ## fixed and free...
  k <- 1
  if (length(idx)) for (i in 1:length(idx)) idx[[i]]$sp.done <- FALSE
  if (m > 0) {
    for (i in 1:m) { ## work through all smooths
      id <- sm[[i]]$id
      if (!is.null(id)) { ## smooth with id
        if (idx[[id]]$nc > 0) { ## only copy if there are sp's
          G$smooth[[i]]$sp <- G$sp[idx[[id]]$c:(idx[[id]]$c + idx[[id]]$nc - 1)]
        }
        if (!idx[[id]]$sp.done) { ## only update k on first encounter with this smooth
          idx[[id]]$sp.done <- TRUE
          k <- k + idx[[id]]$nc
        }
      } else { ## no id, just work through sp
        if (is.null(sm[[i]]$L)) nc <- length(sm[[i]]$S) else nc <- ncol(sm[[i]]$L)
        if (nc > 0) G$smooth[[i]]$sp <- G$sp[k:(k + nc - 1)]
        k <- k + nc
      }
    }
  } ## now all elements of G$smooth have a record of initial sp.



  k.sp <- 0 # count through sp and S
  G$rank <- array(0, 0)
  if (m > 0) {
    for (i in 1:m) {
      sm <- G$smooth[[i]]
      if (length(sm$S) > 0) {
        for (j in 1:length(sm$S)) { # work through penalty matrices
          k.sp <- k.sp + 1
          G$off[k.sp] <- sm$first.para
          G$S[[k.sp]] <- sm$S[[j]]
          G$rank[k.sp] <- sm$rank[j]
        }
      }
    }
  }

  ## need to modify L, lsp.names, G$S, G$sp, G$rank and G$off to include any penalties
  ## on parametric stuff, at this point....
  if (!is.null(PP)) { ## deal with penalties on parametric terms
    L <- rbind(
      cbind(L, matrix(0, nrow(L), ncol(PP$L))),
      cbind(matrix(0, nrow(PP$L), ncol(L)), PP$L)
    )
    G$off <- c(PP$off, G$off)
    G$S <- c(PP$S, G$S)
    G$rank <- c(PP$rank, G$rank)
    G$sp <- c(PP$sp, G$sp)
    lsp.names <- c(PP$full.sp.names, lsp.names)
    G$n.paraPen <- length(PP$off)
    if (!is.null(PP$min.sp)) { ## deal with minimum sps
      H <- matrix(0, n.p, n.p)
      for (i in 1:length(PP$S)) {
        ind <- PP$off[i]:(PP$off[i] + ncol(PP$S[[i]]) - 1)
        H[ind, ind] <- H[ind, ind] + PP$min.sp[i] * PP$S[[i]]
      }
    } ## min.sp stuff finished
  } else {
    G$n.paraPen <- 0
  }


  ## Now remove columns of L and rows of sp relating to fixed
  ## smoothing parameters, and use removed elements to create lsp0

  fix.ind <- G$sp >= 0

  if (sum(fix.ind)) {
    lsp0 <- G$sp[fix.ind]
    ind <- lsp0 == 0 ## find the zero s.p.s
    ef0 <- indi <- (1:length(ind))[ind]
    if (length(indi) > 0) {
      for (i in 1:length(indi)) {
        ## find "effective zero" to replace each zero s.p. with
        ii <- G$off[i]:(G$off[i] + ncol(G$S[[i]]) - 1)
        ef0[i] <- norm(G$X[, ii], type = "F")^2 / norm(G$S[[i]], type = "F") * .Machine$double.eps * .1
      }
    }
    lsp0[!ind] <- log(lsp0[!ind])
    lsp0[ind] <- log(ef0) ## log(.Machine$double.xmin)*1000 ## zero fudge
    lsp0 <- as.numeric(L[, fix.ind, drop = FALSE] %*% lsp0)

    L <- L[, !fix.ind, drop = FALSE]
    G$sp <- G$sp[!fix.ind]
  } else {
    lsp0 <- rep(0, nrow(L))
  }

  G$H <- H

  if (ncol(L) == nrow(L) && !sum(L != diag(ncol(L)))) L <- NULL ## it's just the identity

  G$L <- L
  G$lsp0 <- lsp0
  names(G$lsp0) <- lsp.names ## names of all smoothing parameters (not just underlying)


  G$y <- drop(mf[[formula$response]])
  ydim <- dim(G$y)
  if (!is.null(ydim) && length(ydim) < 2) dim(G$y) <- NULL



  G$n <- nrow(mf)

  if (is.null(mf$"(weights)")) {
    G$w <- rep(1, G$n)
  } else {
    G$w <- mf$"(weights)"
  }

  ## Create names for model coefficients...

  if (G$nsdf > 0) term.names <- colnames(G$X)[1:G$nsdf] else term.names <- array("", 0)
  n.smooth <- length(G$smooth)
  ## create coef names, if smooth has any coefs, and create a global indicator of non-linear parameters
  ## g.index, if needed
  n.sp0 <- 0
  if (n.smooth) {
    for (i in 1:n.smooth) {
      k <- 1
      jj <- G$smooth[[i]]$first.para:G$smooth[[i]]$last.para
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
  }
  G$term.names <- term.names

  ## Deal with non-linear parameterizations...


  G$pP <- PP ## return paraPen object, if present

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
  if (v.n) {
    k <- 0 ## counter for retained variables
    for (i in 1:v.n) {
      if (length(dl[[i]]) >= n) {
        k <- k + 1
        v.name[k] <- v.name1[i] ## save names of variables of correct length
      }
    }
    if (k > 0) v.name <- v.name[1:k] else v.name <- rep("", k)
  }


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
#' @param Xp Parametric model matrix.
#' @param tol Numerical tolerance.
#'
#' @return A list of smooth terms, with identifiability constraints imposed.
#' @author Simon Wood
#'
#' @noRd
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
gam.side <- function(sm, Xp, tol = .Machine$double.eps^.5) {
  with.pen <- nrow(Xp) < ncol(Xp) + sum(unlist(lapply(sm, function(x) ncol(x$X))))

  m <- length(sm)
  if (m == 0) {
    return(sm)
  }
  v.names <- array("", 0)
  maxDim <- 1
  for (i in seq_len(m)) { ## collect all term names and max smooth `dim'
    vn <- sm[[i]]$term
    ## need to include by variables in names
    if (sm[[i]]$by != "NA") vn <- paste(vn, sm[[i]]$by, sep = "")
    ## need to distinguish levels of factor by variables...
    if (!is.null(sm[[i]]$by.level)) vn <- paste(vn, sm[[i]]$by.level, sep = "")
    sm[[i]]$vn <- vn ## use this record to identify variables from now
    v.names <- c(v.names, vn)
    if (sm[[i]]$dim > maxDim) maxDim <- sm[[i]]$dim
  }
  lv <- length(v.names)
  v.names <- unique(v.names)
  if (lv == length(v.names)) {
    return(sm)
  } ## no repeats => no nesting

  ## Only get this far if there is nesting.
  ## Need to test for intercept or equivalent in Xp
  intercept <- FALSE
  if (ncol(Xp)) {
    ## first check columns directly...
    if (sum(apply(Xp, 2, stats::sd) < .Machine$double.eps^.75) > 0) {
      intercept <- TRUE
    } else {
      ## no constant column, so need to check span of Xp...
      f <- rep(1, nrow(Xp))
      ff <- qr.fitted(qr(Xp), f)
      if (max(abs(ff - f)) < .Machine$double.eps^.75) intercept <- TRUE
    }
  }

  sm.id <- as.list(v.names)
  names(sm.id) <- v.names
  for (i in seq_along(sm.id)) sm.id[[i]] <- array(0, 0)
  sm.dim <- sm.id
  for (d in 1:maxDim) {
    for (i in 1:m) {
      if (sm[[i]]$dim == d && sm[[i]]$side.constrain) {
        for (j in 1:d) { ## work through terms
          term <- sm[[i]]$vn[j]
          a <- sm.id[[term]]
          la <- length(a) + 1
          sm.id[[term]][la] <- i ## record smooth i.d. for this variable
          sm.dim[[term]][la] <- d ## ... and smooth dim.
        }
      }
    }
  }
  ## so now each unique variable name has an associated array of
  ## the smooths of which it is an argument, arranged in ascending
  ## order of dimension. Smooths for which side.constrain==FALSE are excluded.
  if (maxDim == 1) warning("model has repeated 1-d smooths of same variable.")

  ## Now set things up to enable term specific model matrices to be
  ## augmented with square root penalties, on the fly...
  if (with.pen) {
    k <- 1
    for (i in 1:m) { ## create parameter indices for each term
      k1 <- k + ncol(sm[[i]]$X) - 1
      sm[[i]]$p.ind <- k:k1
      k <- k1 + 1
    }
    np <- k - 1 ## number of penalized parameters
  }
  nobs <- nrow(sm[[1]]$X) ## number of observations

  for (d in seq_len(maxDim)) { ## work up through dimensions
    for (i in seq_len(m)) { ## work through smooths
      if (sm[[i]]$dim == d && sm[[i]]$side.constrain) { ## check for nesting
        if (with.pen) {
          X1 <- matrix(c(rep(1, nobs), rep(0, np)), nobs + np, as.integer(intercept))
        } else {
          X1 <- matrix(1, nobs, as.integer(intercept))
        }
        X1comp <- rep(0, 0) ## list of components of X1 to avoid duplication
        for (j in seq_len(d)) { ## work through variables
          b <- sm.id[[sm[[i]]$vn[j]]] # list of smooths dependent on this variable
          k <- (seq_along(b))[b == i] ## locate current smooth in list

          for (l in seq_len(k - 1)) {
            if (!b[l] %in% X1comp) { ## collect X columns
              X1comp <- c(X1comp, b[l]) ## keep track of components to avoid adding same one twice
              if (with.pen) { ## need to use augmented model matrix in testing
                if (is.null(sm[[b[l]]]$Xa)) sm[[b[l]]]$Xa <- augment.smX(sm[[b[l]]], nobs, np)
                X1 <- cbind(X1, sm[[b[l]]]$Xa)
              } else {
                X1 <- cbind(X1, sm[[b[l]]]$X)
              } ## penalties not considered
            }
          }
        } ## Now X1 contains columns for all lower dimensional terms
        if (ncol(X1) == as.integer(intercept)) {
          ind <- NULL
        } else {
          if (with.pen) {
            if (is.null(sm[[i]]$Xa)) sm[[i]]$Xa <- augment.smX(sm[[i]], nobs, np)
            ind <- mgcv::fixDependence(X1, sm[[i]]$Xa, tol = tol)
          } else {
            ind <- mgcv::fixDependence(X1, sm[[i]]$X, tol = tol)
          }
        }
        ## ... the columns to zero to ensure independence
        if (!is.null(ind)) {
          sm[[i]]$X <- sm[[i]]$X[, -ind]
          ## work through list of penalty matrices, applying constraints...
          nsmS <- length(sm[[i]]$S)
          if (nsmS > 0) {
            for (j in seq(from = nsmS, to = 1, by = -1)) { ## working down so that dropping is painless
              sm[[i]]$S[[j]] <- sm[[i]]$S[[j]][-ind, -ind]
              if (sum(sm[[i]]$S[[j]] != 0) == 0) {
                rank <- 0
              } else {
                rank <- qr(sm[[i]]$S[[j]], tol = tol, LAPACK = FALSE)$rank
              }
              sm[[i]]$rank[j] <- rank ## replace previous rank with new rank
              if (rank == 0) { ## drop the penalty
                sm[[i]]$rank <- sm[[i]]$rank[-j]
                sm[[i]]$S[[j]] <- NULL
                sm[[i]]$S.scale <- sm[[i]]$S.scale[-j]
                if (!is.null(sm[[i]]$L)) sm[[i]]$L <- sm[[i]]$L[-j, , drop = FALSE]
              }
            }
          } ## penalty matrices finished
          ## Now we need to establish null space rank for the term
          mi <- length(sm[[i]]$S)
          if (mi > 0) {
            St <- sm[[i]]$S[[1]] / norm(sm[[i]]$S[[1]], type = "F")
            if (mi > 1) {
              for (j in 1:mi) {
                St <- St +
                  sm[[i]]$S[[j]] / norm(sm[[i]]$S[[j]], type = "F")
              }
            }
            es <- eigen(St, symmetric = TRUE, only.values = TRUE)
            sm[[i]]$null.space.dim <- sum(es$values < max(es$values) * .Machine$double.eps^.75)
          } ## rank found

          if (!is.null(sm[[i]]$L)) {
            ind <- as.numeric(colSums(sm[[i]]$L != 0)) != 0
            sm[[i]]$L <- sm[[i]]$L[, ind, drop = FALSE] ## retain only those sps that influence something!
          }

          sm[[i]]$df <- ncol(sm[[i]]$X)
          attr(sm[[i]], "del.index") <- ind
          ## Now deal with case in which prediction constraints differ from fit constraints
          if (!is.null(sm[[i]]$Xp)) { ## need to get deletion indices under prediction parameterization
            ## Note that: i) it doesn't matter what the identifiability con on X1 is
            ##            ii) the degree of rank deficiency can't be changed by an identifiability con
            if (with.pen) {
              smi <- sm[[i]] ## clone smooth
              smi$X <- smi$Xp ## copy prediction Xp to X slot.
              smi$S <- smi$Sp ## and make sure penalty parameterization matches.
              Xpa <- augment.smX(smi, nobs, np)
              ind <- mgcv::fixDependence(X1, Xpa, rank.def = length(ind))
            } else {
              ind <- mgcv::fixDependence(X1, sm[[i]]$Xp, rank.def = length(ind))
            }
            sm[[i]]$Xp <- sm[[i]]$Xp[, -ind, drop = FALSE]
            attr(sm[[i]], "del.index") <- ind ## over-writes original
          }
        }
        sm[[i]]$vn <- NULL
      } ## end if (sm[[i]]$dim == d)
    } ## end i in 1:m loop
  }
  if (with.pen) {
    for (i in 1:m) { ## remove working variables that were added
      sm[[i]]$p.ind <- NULL
      if (!is.null(sm[[i]]$Xa)) sm[[i]]$Xa <- NULL
    }
  }
  sm
} ## gam.side

#' Clone smooth specification for ID-linked bases
#'
#' This is an internal function from \code{mgcv} which clones the specifications
#' for bases linked with an `id` argument.
#'
#' @param specb Specification to be cloned.
#' @param spec Original specification which will be modified.
#'
#' @return A cloned smooth specification.
#' @author Simon Wood
#'
#' @noRd
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
clone.smooth.spec <- function(specb, spec) {
  if (specb$dim != spec$dim) stop("`id' linked smooths must have same number of arguments")

  if (inherits(specb, c("tensor.smooth.spec", "t2.smooth.spec"))) {
    specb$term <- spec$term
    specb$label <- spec$label
    specb$by <- spec$by
    k <- 1
    for (i in seq_along(specb$margin)) {
      if (is.null(spec$margin)) { ## sloppy user -- have to construct margin info...
        for (j in seq_along(specb$margin[[i]]$term)) {
          specb$margin[[i]]$term[j] <- spec$term[k]
          k <- k + 1
        }
        specb$margin[[i]]$label <- ""
      } else { ## second term was at least `te'/`t2', so margin cloning is easy
        specb$margin[[i]]$term <- spec$margin[[i]]$term
        specb$margin[[i]]$label <- spec$margin[[i]]$label
        specb$margin[[i]]$xt <- spec$margin[[i]]$xt
      }
    }
  } else { ## `s' generated case
    specb$term <- spec$term
    specb$label <- spec$label
    specb$by <- spec$by
    specb$xt <- spec$xt ## don't generally know what's in here => don't clone
  }
  specb
}


#' Function for creating augmented model matrix.
#'
#' This is an internal function taken from \code{mgcv}.
#'
#' @param sm List of smooth objects, each of which returned from
#' \code{mgcv::smoothCon}.
#' @param nobs Number of observations.
#' @param np Number of penalized parameters.
#'
#' @return A scaled and augmented model matrix.
#' @author Simon Wood
#'
#' @noRd
#'
#' @references
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
augment.smX <- function(sm, nobs, np) {
  ns <- length(sm$S) ## number of penalty matrices
  if (ns == 0) { ## nothing to do
    return(rbind(sm$X, matrix(0, np, ncol(sm$X))))
  }
  ind <- colMeans(abs(sm$S[[1]])) != 0
  sqrmaX <- mean(abs(sm$X[, ind]))^2
  alpha <- sqrmaX / mean(abs(sm$S[[1]][ind, ind]))
  St <- sm$S[[1]] * alpha
  if (ns > 1) {
    for (i in 2:ns) {
      ind <- colMeans(abs(sm$S[[i]])) != 0
      alpha <- sqrmaX / mean(abs(sm$S[[i]][ind, ind]))
      St <- St + sm$S[[i]] * alpha
    }
  }
  rS <- mgcv::mroot(St, rank = ncol(St)) ## get sqrt of penalty
  X <- rbind(sm$X, matrix(0, np, ncol(sm$X))) ## create augmented model matrix
  X[nobs + sm$p.ind, ] <- t(rS) ## add in
  X ## scaled augmented model matrix
} ## augment.smX


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

  for (i in seq_along(smooth.spec)) {
    attr(smooth.spec[[i]], "gamm") <- TRUE
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

  fake.formula <- stats::as.formula(fake.formula, p.env)
  if (length(av)) {
    pred.formula <- stats::as.formula(paste("~", paste(av, collapse = "+")))
    pav <- all.vars(pred.formula) ## trick to strip out 'offset(x)' etc...
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
} ## interpret.gam0
