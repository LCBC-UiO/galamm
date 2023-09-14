#' Internal function for fitting GAMMs using lme4
#'
#' This function is derived from an internal function in the \code{gamm4} package.
#'
#' @param fixed A formula excluding lme4 style random effects but including
#'   smooths.
#' @param pterms Parametric terms in the model.
#' @param data Model frame.
#'
#' @return model
#' @author Simon N Wood, with some modifications by Oystein Sorensen.
#' @keywords internal
#'
#' @seealso [gamm4()] and [gam.setup()].
#'
#' @references
#' \insertRef{woodStraightforwardIntermediateRank2013}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
#'
gamm4.setup <- function(formula, pterms,
                        data = stop("No data supplied to gamm.setup")) {
  G <- gam.setup(formula, pterms, data = data)

  if (!is.null(G$L)) stop("gamm can not handle linked smoothing parameters (probably from use of `id' or adaptive smooths)")
  # now perform re-parameterization...

  first.f.para <- G$nsdf + 1

  random <- list()

  if (G$nsdf > 0) ind <- 1:G$nsdf else ind <- rep(0, 0)
  X <- G$X[, ind, drop = FALSE] # accumulate fixed effects into here

  xlab <- rep("", 0)

  G$Xf <- methods::as(X, "dgCMatrix") ## sparse version of full matrix, treating smooths as fixed

  first.para <- G$nsdf + 1

  used.names <- names(data) ## keep track of all variable names already used

  if (G$m) {
    for (i in 1:G$m) { ## work through the smooths

      sm <- G$smooth[[i]]
      sm$X <- G$X[, sm$first.para:sm$last.para, drop = FALSE]
      rasm <- mgcv::smooth2random(sm, used.names, type = 2) ## convert smooth to random effect and fixed effects
      used.names <- c(used.names, names(rasm$rand))

      sm$fixed <- rasm$fixed

      ## deal with creation of sparse full model matrix
      if (!is.null(sm$fac)) {
        flev <- levels(sm$fac) ## grouping factor for smooth
        n.lev <- length(flev)
        for (k in 1:n.lev) {
          G$Xf <- methods::cbind2(G$Xf, methods::as(sm$X * as.numeric(sm$fac == flev[k]), "dgCMatrix"))
        }
      } else {
        n.lev <- 1
        G$Xf <- methods::cbind2(G$Xf, methods::as(sm$X, "dgCMatrix"))
      }

      ## now append random effects to main list
      n.para <- 0 ## count random coefficients

      if (!sm$fixed) {
        for (k in seq_along(rasm$rand)) n.para <- n.para + ncol(rasm$rand[[k]])
        sm$lmer.name <- names(rasm$rand)
        random <- c(random, rasm$rand)
        sm$trans.D <- rasm$trans.D
        sm$trans.U <- rasm$trans.U ## matrix mapping fit coefs back to original
      }

      ## ensure stored first and last para relate to G$Xf in expanded version

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
      sm$last.f.para <- first.f.para - 1 ## note less than sm$first.f.para => no fixed

      ## store indices of random parameters in smooth specific array
      sm$rind <- rasm$rind
      sm$rinc <- rasm$rinc

      sm$pen.ind <- rasm$pen.ind ## pen.ind==i TRUE for coef penalized by ith penalty

      sm$n.para <- n.para

      sm$X <- NULL ## delete model matrix

      G$smooth[[i]] <- sm ## replace smooth object with extended version
    }
  }

  G$random <- random ## named list of random effect matrices
  G$X <- X ## fixed effects model matrix

  G
} ## end of gamm4.setup


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
#' @keywords internal
#'
#' @seealso [gamm4.setup()] and [gam.setup()].
#'
#' @references
#' \insertRef{woodStraightforwardIntermediateRank2013}{galamm}
#'
#' \insertRef{woodGeneralizedAdditiveModels2017a}{galamm}
#'
gamm4 <- function(fixed, random = NULL, data = list()) {
  if (!is.null(random)) {
    if (!inherits(random, "formula")) stop("gamm4 requires `random' to be a formula")
    random.vars <- all.vars(random)
  } else {
    random.vars <- NULL
  }

  # create model frame.....
  gp <- mgcv::interpret.gam(fixed) # interpret the formula

  mf <- match.call(expand.dots = FALSE)

  mf$formula <- gp$fake.formula
  mf$fixed <- NULL
  mf$control <- mf$random <- NULL

  mf[[1]] <- as.name("model.frame")
  pmf <- mf
  gmf <- eval(mf, parent.frame()) # the model frame now contains all the data, for the gam part only
  gam.terms <- attr(gmf, "terms") # terms object for `gam' part of fit -- need this for prediction to work properly

  if (length(random.vars)) {
    mf$formula <- as.formula(paste(paste(deparse(gp$fake.formula,
      backtick = TRUE
    ), collapse = ""), "+", paste(random.vars,
      collapse = "+"
    )))
    mf <- eval(mf, parent.frame())
  } else {
    mf <- gmf
  }
  rm(gmf)

  if (nrow(mf) < 2) stop("Not enough (non-NA) data to do anything meaningful")

  ## summarize the *raw* input variables
  ## note can't use get_all_vars here -- buggy with matrices
  vars <- all.vars(gp$fake.formula[-2]) ## drop response here
  inp <- parse(text = paste("list(", paste(vars, collapse = ","), ")"))
  dl <- eval(inp, data, parent.frame())
  names(dl) <- vars ## list of all variables needed
  var.summary <- variable.summary(gp$pf, dl, nrow(mf)) ## summarize the input data

  ## lmer offset handling work around...
  mvars <- vars[!vars %in% names(mf)] ## variables not in mf raw -- can cause lmer problem
  if (length(mvars) > 0) for (i in seq_along(mvars)) mf[[mvars[i]]] <- dl[[mvars[i]]] ## append raw versions to mf

  rm(dl) ## save space

  pmf$formula <- gp$pf
  pmf <- eval(pmf, parent.frame()) # pmf contains all data for non-smooth part
  pTerms <- attr(pmf, "terms")


  G <- gamm4.setup(gp, pterms = pTerms, data = mf)

  G$var.summary <- var.summary

  n.sr <- length(G$random) # number of random smooths (i.e. s(...,fx=FALSE,...) terms)

  if (is.null(random) && n.sr == 0) {
    stop("gamm4 models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect")
  }

  offset.name <- attr(mf, "names")[attr(attr(mf, "terms"), "offset")]

  yname <- mgcv::new.name("y", names(mf))
  eval(parse(text = paste("mf$", yname, "<-G$y", sep = "")))
  Xname <- mgcv::new.name("X", names(mf))
  eval(parse(text = paste("mf$", Xname, "<-G$X", sep = "")))

  lme4.formula <- paste(yname, "~", Xname, "-1")
  if (length(offset.name)) {
    lme4.formula <- paste(lme4.formula, "+", offset.name)
  }

  ## Basic trick is to call (g)lFormula to set up model, with simple i.i.d. dummy random effects for the
  ## penalized component of each smooth. This results in columns of Z being produced for these dummy's,
  ## which can be over-written with the right thing. NOTE: that lambdat could also be modified, I think!!

  ## Add the random effect dummy variables for the smooth
  r.name <- names(G$random)
  if (n.sr) {
    for (i in 1:n.sr) {
      mf[[r.name[i]]] <- factor(rep(seq_len(ncol(G$random[[i]])), length = nrow(G$random[[i]])))
      lme4.formula <- paste(lme4.formula, "+ (1|", r.name[i], ")")
    }
  }

  if (!is.null(random)) { ## append the regular random effects
    lme4.formula <- paste(
      lme4.formula, "+",
      substring(paste(deparse(random, backtick = TRUE), collapse = ""), first = 2)
    )
  }

  lme4.formula <- as.formula(lme4.formula)

  ## NOTE: further arguments should be passed here...
  b <- lme4::lFormula(lme4.formula, data = mf, REML = FALSE)

  if (n.sr) { ## Fabian Scheipl's trick of overwriting dummy slots revised for new structure
    tn <- names(b$reTrms$cnms) ## names associated with columns of Z (same order as Gp)
    ind <- seq_along(tn)
    sn <- names(G$random) ## names of smooth random components
    for (i in 1:n.sr) { ## loop through random effect smooths
      k <- ind[sn[i] == tn] ## which term should contain G$random[[i]]
      ii <- (b$reTrms$Gp[k] + 1):b$reTrms$Gp[k + 1]
      b$reTrms$Ztlist[[k]] <- b$reTrms$Zt[ii, ] <- methods::as(t(G$random[[i]]), "dgCMatrix")
      b$reTrms$cnms[[k]] <- attr(G$random[[i]], "s.label")
    }
  }
  list(
    lmod = b,
    G = G,
    gam.terms = gam.terms,
    fake.formula = gp$fake.formula,
    n.sr = n.sr,
    mf = mf
  )
} ## end of gamm4


gamm4.wrapup <- function(gobj, ret, final_model) {
  object <- list(
    model = gobj$mf,
    smooth = gobj$G$smooth,
    nsdf = gobj$G$nsdf,
    df.null = nrow(gobj$G$X),
    y = ret$response,
    terms = gobj$gam.terms,
    pterms = gobj$G$pterms,
    xlevels = gobj$G$xlevels,
    contrasts = gobj$G$contrasts,
    assign = gobj$G$assign,
    cmX = gobj$G$cmX,
    var.summary = gobj$G$var.summary,
    method = "ML"
  )

  pvars <- all.vars(delete.response(object$terms))
  object$pred.formula <- if (length(pvars) > 0) {
    reformulate(pvars)
  } else {
    NULL
  }

  B <- Matrix::Matrix(0, ncol(gobj$G$Xf), ncol(gobj$G$Xf))
  diag(B) <- 1
  Xfp <- gobj$G$Xf

  ## Transform  parameters back to the original space....
  bf <- as.numeric(fixef(ret)) ## the fixed effects
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
      for (k in seq_along(gobj$G$smooth[[i]]$lmer.name)) { ## collect all coefs associated with this smooth
        b <- c(b, as.numeric(br[[gobj$G$smooth[[i]]$lmer.name[k]]][[1]]))
      }
      b <- b[gobj$G$smooth[[i]]$rind] ## make sure coefs are in order expected by smooth
      b <- c(b, beta)
      b <- gobj$G$smooth[[i]]$trans.D * b
      if (!is.null(gobj$G$smooth[[i]]$trans.U)) b <- gobj$G$smooth[[i]]$trans.U %*% b ## transform back to original
    }
    p <- c(p, b)

    ## now fill in B...
    ind <- with(gobj$G$smooth[[i]], seq(from = first.para, to = last.para, by = 1))

    if (!fx) {
      D <- gobj$G$smooth[[i]]$trans.D
      if (is.null(gobj$G$smooth[[i]]$trans.U)) {
        B[ind, ind] <- Matrix::Diagonal(length(D), D)
      } else {
        B[ind, ind] <- t(D * t(gobj$G$smooth[[i]]$trans.U))
      }
    }
    ## and finally transform G$Xf into fitting parameterization...
    Xfp[, ind] <- gobj$G$Xf[, ind, drop = FALSE] %*% B[ind, ind, drop = FALSE]
  }

  object$coefficients <- p

  vr <- lme4::VarCorr(ret)

  scale <- as.numeric(attr(vr, "sc"))^2
  if (!is.finite(scale) || scale == 1) {
    scale <- 1
    object$scale.estimated <- FALSE
  } else {
    object$scale.estimated <- TRUE
  }

  sp <- rep(-1, gobj$n.sr)

  Zt <- Matrix::Matrix(0, 0, ncol(gobj$lmod$reTrms$Zt))
  sn <- names(gobj$G$random)
  if (gobj$n.sr == 0) sn <- NULL
  rn <- names(vr)
  ind <- rep(0, 0) ## index the non-smooth random effects among the random effects
  for (i in seq_along(vr)) {
    if (is.null(sn) || !rn[[i]] %in% sn) { ## append non smooth r.e.s to Zt
      Gp <- gobj$lmod$reTrms$Gp ## group index ends
      ind <- c(ind, seq(from = (Gp[i] + 1), to = Gp[i + 1], by = 1))
    } else if (!is.null(sn)) { ## extract smoothing parameters for smooth r.e.s
      k <- seq(gobj$n.sr)[rn[i] == sn] ## where in original smooth ordering is current smoothing param
      if (as.numeric(vr[[i]] > 0)) {
        sp[k] <- scale / as.numeric(vr[[i]])
      } else {
        sp[k] <- 1e10
      }
    }
  }

  if (length(ind)) { ## extract columns corresponding to non-smooth r.e.s
    Zt <- gobj$lmod$reTrms$Zt[ind, ] ## extracting random effects model matrix
    root.phi <- gobj$lmod$reTrms$Lambdat[ind, ind] ## and corresponding sqrt of cov matrix (phi)
  }

  V <- Matrix::Diagonal(length(final_model$V), scale / final_model$V)

  # This one is invoked if there are classical random effect, not smooths.
  if (nrow(Zt) > 0) V <- V + Matrix::crossprod(root.phi %*% Zt) * scale

  R <- Matrix::chol(V, pivot = TRUE)
  piv <- attr(R, "pivot")

  gobj$G$Xf <- methods::as(gobj$G$Xf, "dgCMatrix")
  Xfp <- methods::as(Xfp, "dgCMatrix")

  if (is.null(piv)) {
    WX <- methods::as(Matrix::solve(Matrix::t(R), Xfp), "matrix") ## V^{-.5}Xp -- fit parameterization
    XVX <- methods::as(Matrix::solve(Matrix::t(R), gobj$G$Xf), "matrix") ## same in original parameterization
  } else {
    WX <- methods::as(Matrix::solve(Matrix::t(R), Xfp[piv, ]), "matrix") ## V^{-.5}Xp -- fit parameterization
    XVX <- methods::as(Matrix::solve(Matrix::t(R), gobj$G$Xf[piv, ]), "matrix") ## same in original parameterization
  }

  qrz <- qr(XVX, LAPACK = TRUE)
  object$R <- qr.R(qrz)
  object$R[, qrz$pivot] <- object$R

  XVX <- crossprod(object$R) ## X'V^{-1}X original parameterization

  object$sp <- sp

  colx <- ncol(gobj$G$Xf)
  Sp <- matrix(0, colx, colx) # penalty matrix - fit param
  first <- gobj$G$nsdf + 1
  k <- 1
  for (i in seq_along(gobj$G$m)) { # Accumulate the total penalty matrix
    if (!object$smooth[[i]]$fixed) {
      ii <- with(object$smooth[[i]], seq(from = first.para, to = last.para)) ## index this smooth's params
      for (j in seq_along(object$smooth[[i]]$S)) { ## work through penalty list
        ind <- ii[object$smooth[[i]]$pen.ind == j] ## index of currently penalized
        diag(Sp)[ind] <- sqrt(object$sp[k]) ## diagonal penalty
        k <- k + 1
      }
    }
    first <- last + 1
  }

  ## Alternative cov matrix calculation. Basic
  ## idea is that cov matrix is computed stably in
  ## fitting parameterization, and then transformed to
  ## original parameterization.
  qrx <- qr(rbind(WX, Sp / sqrt(scale)), LAPACK = TRUE)
  Ri <- backsolve(qr.R(qrx), diag(ncol(WX)))
  ind <- qrx$pivot
  ind[ind] <- seq_along(ind) ## qrx$pivot
  Ri <- Ri[ind, ] ## unpivoted square root of cov matrix in fitting parameterization Ri Ri' = cov
  Vb <- methods::as(B, "matrix") %*% Ri
  Vb <- Vb %*% t(Vb)

  object$edf <- rowSums(Vb * t(XVX))
  object$df.residual <- length(object$y) - sum(object$edf)
  object$sig2 <- scale
  object$Vp <- methods::as(Vb, "matrix")
  object$Ve <- methods::as(Vb %*% XVX %*% Vb, "matrix")

  class(object) <- "gam"

  if (!is.null(gobj$G$original.smooth)) {
    object$smooth <- gobj$G$smooth <- gobj$G$original.smooth
  }

  ## If prediction parameterization differs from fit parameterization, transform now...
  ## (important for t2 smooths, where fit constraint is not good for component wise
  ##  prediction s.e.s)

  if (!is.null(gobj$G$P)) {
    object$coefficients <- gobj$G$P %*% object$coefficients
    object$Vp <- gobj$G$P %*% object$Vp %*% t(gobj$G$P)
    object$Ve <- gobj$G$P %*% object$Ve %*% t(gobj$G$P)
  }

  object$linear.predictors <- predict(object, type = "link")
  object$fitted.values <- object$linear.predictors # should eventually add family here

  object$residuals <- residuals(ret)

  term.names <- colnames(gobj$G$X)[seq(length.out = gobj$G$nsdf)]
  n.smooth <- length(gobj$G$smooth)

  for (i in seq_along(n.smooth)) {
    k <- 1
    for (j in with(object$smooth[[i]], seq(from = first.para, to = last.para))) {
      term.names[j] <- paste(object$smooth[[i]]$label, ".", as.character(k), sep = "")
      k <- k + 1
    }
  }
  names(object$coefficients) <- term.names # note - won't work on matrices!!
  names(object$edf) <- term.names
  names(object$sp) <- names(gobj$G$sp)

  object$gcv.ubre <- deviance(ret)

  if (!is.null(gobj$G$Xcentre)) object$Xcentre <- gobj$G$Xcentre ## any column centering applied to smooths
  object
}