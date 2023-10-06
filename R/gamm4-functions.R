#' Internal function for fitting GAMMs using lme4
#'
#' This function is derived from an internal function in the \code{gamm4}
#' package.
#'
#' @param formula A formula excluding lme4 style random effects but including
#'   smooths.
#' @param pterms Parametric terms in the model.
#' @param mf Model frame.
#'
#' @return model
#' @author Simon N Wood, with some modifications by Oystein Sorensen.
#' @noRd
#'
#' @seealso [gamm4()] and [gam.setup()].
#'
#' @references \insertRef{woodStraightforwardIntermediateRank2013}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
#'
gamm4.setup <- function(formula, pterms, mf) {
  G <- gam.setup(formula, pterms, mf)
  first.f.para <- G$nsdf + 1
  random <- list()
  ind <- seq_len(G$nsdf)
  X <- G$X[, ind, drop = FALSE]
  xlab <- rep("", 0)
  G$Xf <- methods::as(X, "dgCMatrix")
  first.para <- G$nsdf + 1
  used.names <- names(mf)

  for (i in seq_len(G$m)) {
    sm <- G$smooth[[i]]
    sm$X <- G$X[, seq(sm$first.para, sm$last.para, by = 1), drop = FALSE]
    rasm <- mgcv::smooth2random(sm, used.names, type = 2)
    used.names <- c(used.names, names(rasm$rand))
    sm$fixed <- rasm$fixed
    n.lev <- 1
    G$Xf <- methods::cbind2(G$Xf, methods::as(sm$X, "dgCMatrix"))


    n.para <- 0

    if (!sm$fixed) {
      for (k in seq_along(rasm$rand)) n.para <- n.para + ncol(rasm$rand[[k]])
      sm$lmer.name <- names(rasm$rand)
      random <- c(random, rasm$rand)
      sm$trans.D <- rasm$trans.D
      sm$trans.U <- rasm$trans.U
    }

    sm$last.para <- first.para + ncol(rasm$Xf) + n.para - 1
    sm$first.para <- first.para
    first.para <- sm$last.para + 1

    if (ncol(rasm$Xf)) {
      Xfnames <- rep("", ncol(rasm$Xf))
      k <- length(xlab) + 1
      for (j in seq_len(ncol(rasm$Xf))) {
        xlab[k] <- Xfnames[j] <-
          mgcv::new.name(paste(sm$label, "Fx", j, sep = ""), xlab)
        k <- k + 1
      }
      colnames(rasm$Xf) <- Xfnames
    }

    X <- cbind(X, rasm$Xf) # add fixed model matrix to overall fixed X

    sm$first.f.para <- first.f.para
    first.f.para <- first.f.para + ncol(rasm$Xf)
    sm$last.f.para <- first.f.para - 1

    sm$rind <- rasm$rind
    sm$rinc <- rasm$rinc
    sm$pen.ind <- rasm$pen.ind
    sm$n.para <- n.para
    sm$X <- NULL
    G$smooth[[i]] <- sm
  }


  G$random <- random
  G$X <- X

  G
}


#' Convert a GAMM to its mixed model representation
#'
#' This function is derived from \code{gamm4::gamm4}.
#'
#' @param fixed A formula excluding lme4 style random effects but including
#'   smooths.
#' @param random A formula containing an optional random effect part in
#'   \code{lme4} style. See the documentation of the \code{random} argument to
#'   \code{gamm4::gamm4}.
#' @param data Data frame.
#'
#' @return An object of class \code{lmod}, returned from \code{lme4::lFormula}.
#'
#' @author Simon Wood and Fabian Scheipl.
#' @noRd
#'
#' @seealso [gamm4.setup()] and [gam.setup()].
#'
#' @references
#' \insertRef{woodStraightforwardIntermediateRank2013}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
#' @examples
#' # To run the match.call(expand.dots = FALSE) interactively, do:
#' mf <- match.call(gamm4, call("gamm4", fixed, random, data),
#'   expand.dots = FALSE
#' )
#'
gamm4 <- function(fixed, random = NULL, data) {
  random.vars <- all.vars(random)
  gp <- interpret.gam0(fixed)
  mf <- match.call(expand.dots = FALSE)

  mf$formula <- gp$fake.formula
  mf$fixed <- NULL
  mf$control <- mf$random <- NULL
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  pmf <- mf
  gmf <- eval(mf, parent.frame())
  gam.terms <- attr(gmf, "terms")

  if (length(random.vars)) {
    mf$formula <- stats::as.formula(paste(paste(deparse(gp$fake.formula,
      backtick = TRUE
    ), collapse = ""), "+", paste(random.vars,
      collapse = "+"
    )))
    mf <- eval(mf, parent.frame())
  } else {
    mf <- gmf
  }
  rm(gmf)

  vars <- all.vars(gp$fake.formula[-2])
  inp <- parse(text = paste("list(", paste(vars, collapse = ","), ")"))
  dl <- eval(inp, data, parent.frame())
  names(dl) <- vars
  var.summary <- variable.summary(gp$pf, dl, nrow(mf))

  mvars <- vars[!vars %in% names(mf)]
  for (i in seq_along(mvars)) mf[[mvars[i]]] <- dl[[mvars[i]]]

  pmf$formula <- gp$pf
  pmf <- eval(pmf, parent.frame())
  pTerms <- attr(pmf, "terms")

  G <- gamm4.setup(formula = gp, pterms = pTerms, mf = mf)
  G$var.summary <- var.summary
  n.sr <- length(G$random)

  yname <- mgcv::new.name("y", names(mf))
  eval(parse(text = paste("mf$", yname, "<-G$y", sep = "")))
  Xname <- mgcv::new.name("X", names(mf))
  eval(parse(text = paste("mf$", Xname, "<-G$X", sep = "")))

  lme4.formula <- paste(yname, "~", Xname, "-1")

  ## Add the random effect dummy variables for the smooth
  r.name <- names(G$random)

  for (i in seq_len(n.sr)) {
    mf[[r.name[i]]] <- factor(rep(seq_len(ncol(G$random[[i]])),
      length = nrow(G$random[[i]])
    ))
    lme4.formula <- paste(lme4.formula, "+ (1|", r.name[i], ")")
  }


  if (!is.null(random)) {
    lme4.formula <- paste(
      lme4.formula, "+",
      substring(paste(deparse(random, backtick = TRUE), collapse = ""),
        first = 2
      )
    )
  }

  lme4.formula <- stats::as.formula(lme4.formula)
  b <- lme4::lFormula(lme4.formula, data = mf, REML = FALSE)

  tn <- names(b$reTrms$cnms)
  ind <- seq_along(tn)
  sn <- names(G$random)
  for (i in seq_len(n.sr)) {
    k <- ind[sn[i] == tn]
    ii <- (b$reTrms$Gp[k] + 1):b$reTrms$Gp[k + 1]
    b$reTrms$Ztlist[[k]] <- b$reTrms$Zt[ii, ] <-
      methods::as(t(G$random[[i]]), "dgCMatrix")
    b$reTrms$cnms[[k]] <- attr(G$random[[i]], "s.label")
  }

  list(
    lmod = b,
    G = G,
    gam.terms = gam.terms,
    fake.formula = gp$fake.formula,
    n.sr = n.sr,
    mf = mf
  )
}


gamm4.wrapup <- function(gobj, ret, final_model) {
  if (length(gobj$G$smooth) == 0) {
    return(list())
  }

  object <- list(
    model = gobj$mf,
    smooth = gobj$G$smooth,
    nsdf = gobj$G$nsdf,
    family = ret$model$family[[1]],
    df.null = nrow(gobj$G$X),
    y = ret$model$response,
    terms = gobj$gam.terms,
    pterms = gobj$G$pterms,
    xlevels = gobj$G$xlevels,
    contrasts = gobj$G$contrasts,
    assign = gobj$G$assign,
    na.action = attr(gobj$mf, "na.action"),
    cmX = gobj$G$cmX,
    var.summary = gobj$G$var.summary
  )
  pvars <- all.vars(stats::delete.response(object$terms))
  object$pred.formula <-
    if (length(pvars) > 0) stats::reformulate(pvars) else NULL
  sn <- names(gobj$G$random)

  if (object$family$family == "gaussian" && object$family$link == "identity") {
    linear <- TRUE
  } else {
    linear <- FALSE
  }

  B <- Matrix::Matrix(0, ncol(gobj$G$Xf), ncol(gobj$G$Xf))
  diag(B) <- 1
  Xfp <- gobj$G$Xf
  ## Transform  parameters back to the original space....
  bf <- fixef(ret)
  br <- ranef(ret) ## a named list
  p <- bf[seq_len(gobj$G$nsdf)]

  for (i in seq_len(gobj$G$m)) {
    fx <- gobj$G$smooth[[i]]$fixed
    first <- gobj$G$smooth[[i]]$first.f.para
    last <- gobj$G$smooth[[i]]$last.f.para
    beta <- bf[seq(from = first, to = last, by = 1)]
    if (fx) {
      b <- beta
    } else { ## not fixed so need to undo transform of random effects etc.
      b <- rep(0, 0)
      for (k in seq_along(gobj$G$smooth[[i]]$lmer.name)) {
        b <- c(b, as.numeric(br[[gobj$G$smooth[[i]]$lmer.name[k]]][[1]]))
      }
      b <- b[gobj$G$smooth[[i]]$rind]
      b <- c(b, beta)
      b <- gobj$G$smooth[[i]]$trans.D * b
      if (!is.null(gobj$G$smooth[[i]]$trans.U)) {
        b <- gobj$G$smooth[[i]]$trans.U %*% b
      }
    }
    p <- c(p, b)

    ## now fill in B...
    ind <- seq(
      from = gobj$G$smooth[[i]]$first.para,
      to = gobj$G$smooth[[i]]$last.para, by = 1
    )
    if (!fx) {
      D <- gobj$G$smooth[[i]]$trans.D
      if (is.null(gobj$G$smooth[[i]]$trans.U)) {
        B[ind, ind] <- Matrix::Diagonal(length(D), D)
      } else {
        B[ind, ind] <- t(D * t(gobj$G$smooth[[i]]$trans.U))
      }
    }

    Xfp[, ind] <- gobj$G$Xf[, ind, drop = FALSE] %*% B[ind, ind, drop = FALSE]
  }


  object$coefficients <- p

  vr <- VarCorr(ret)

  scale <- as.numeric(attr(vr, "sc"))^2
  if (!is.finite(scale) || scale == 1) {
    scale <- 1
    object$scale.estimated <- FALSE
  } else {
    object$scale.estimated <- TRUE
  }

  sp <- rep(-1, gobj$n.sr)

  Zt <- Matrix::Matrix(0, 0, ncol(gobj$lmod$reTrms$Zt))
  if (gobj$n.sr == 0) sn <- NULL
  rn <- names(vr)
  ind <- rep(0, 0)
  for (i in seq_along(vr)) {
    if (is.null(sn) || !rn[i] %in% sn) {
      Gp <- ret$model$lmod$reTrms$Gp
      ind <- c(ind, seq(from = (Gp[i] + 1), to = Gp[i + 1], by = 1))
    } else if (!is.null(sn)) {
      k <- (1:gobj$n.sr)[rn[i] == sn]
      if (as.numeric(vr[[i]] > 0)) {
        sp[k] <- scale / as.numeric(vr[[i]])
      } else {
        sp[k] <- 1e10
      }
    }
  }

  if (length(ind)) {
    Zt <- gobj$lmod$reTrms$Zt[ind, ]
    root.phi <- gobj$lmod$reTrms$Lambdat[ind, ind]
  }

  object$prior.weights <- gobj$G$w

  if (linear) {
    object$weights <- object$prior.weights
    V <- Matrix::Diagonal(
      n = length(object$weights),
      x = scale / object$weights
    )
  } else {
    V <- Matrix::Diagonal(length(final_model$V), scale / final_model$V)
  }


  if (nrow(Zt) > 0) V <- V + Matrix::crossprod(root.phi %*% Zt) * scale

  R <- Matrix::chol(V, pivot = FALSE)
  piv <- attr(R, "pivot")

  gobj$G$Xf <- methods::as(gobj$G$Xf, "dgCMatrix")
  Xfp <- methods::as(Xfp, "dgCMatrix")


  WX <- methods::as(Matrix::solve(Matrix::t(R), Xfp), "matrix")
  XVX <- methods::as(Matrix::solve(Matrix::t(R), gobj$G$Xf), "matrix")

  qrz <- qr(XVX, LAPACK = TRUE)
  object$R <- qr.R(qrz)
  object$R[, qrz$pivot] <- object$R

  XVX <- crossprod(object$R) ## X'V^{-1}X original parameterization

  object$sp <- sp

  colx <- ncol(gobj$G$Xf)
  Sp <- matrix(0, colx, colx) # penalty matrix - fit param
  first <- gobj$G$nsdf + 1
  k <- 1

  for (i in seq_len(gobj$G$m)) { # Accumulate the total penalty matrix
    if (!object$smooth[[i]]$fixed) {
      ii <- seq(
        from = object$smooth[[i]]$first.para,
        to = object$smooth[[i]]$last.para, by = 1
      ) ## index this smooth's params
      for (j in seq_along(object$smooth[[i]]$S)) {
        ind <- ii[object$smooth[[i]]$pen.ind == j]
        diag(Sp)[ind] <- sqrt(object$sp[k])
        k <- k + 1
      }
    }
    first <- last + 1
  }

  qrx <- qr(rbind(WX, Sp / sqrt(scale)), LAPACK = TRUE)
  Ri <- backsolve(qr.R(qrx), diag(ncol(WX)))
  ind <- qrx$pivot
  ind[ind] <- seq_along(ind)
  Ri <- Ri[ind, ]
  Vb <- methods::as(B %*% Ri, "matrix")
  Vb <- Vb %*% Matrix::t(Vb)

  object$edf <- rowSums(Vb * t(XVX))

  object$df.residual <- length(object$y) - sum(object$edf)

  object$sig2 <- scale
  if (linear) {
    object$method <- "lmer.REML"
  } else {
    object$method <- "glmer.ML"
  }

  object$Vp <- methods::as(Vb, "matrix")

  object$Ve <- methods::as(Vb %*% XVX %*% Vb, "matrix")

  class(object) <- "gam"

  if (!is.null(gobj$G$P)) {
    object$coefficients <- gobj$G$P %*% object$coefficients
    object$Vp <- gobj$G$P %*% object$Vp %*% t(gobj$G$P)
    object$Ve <- gobj$G$P %*% object$Ve %*% t(gobj$G$P)
  }

  object$linear.predictors <- object$family$linkfun(fitted(ret))
  object$fitted.values <- fitted(ret)

  object$residuals <- residuals(ret$mer)

  term.names <- colnames(gobj$G$X)[seq_len(gobj$G$nsdf)]
  n.smooth <- length(gobj$G$smooth)

  for (i in seq_len(n.smooth)) {
    k <- 1
    for (j in seq(
      from = object$smooth[[i]]$first.para,
      to = object$smooth[[i]]$last.para, by = 1
    )) {
      term.names[j] <-
        paste(object$smooth[[i]]$label, ".", as.character(k), sep = "")
      k <- k + 1
    }
  }

  names(object$coefficients) <- term.names
  names(object$edf) <- term.names
  names(object$sp) <- names(gobj$G$sp)
  object$gcv.ubre <- deviance(ret$mer)

  object
}
