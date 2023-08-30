#' Function to set up gamm4 model
#'
#' This function is derived from an internal function in the gamm4 package.
#'
#' @param formula formula
#' @param pterms parametric terms
#' @param data data
#' @param knots knots
#'
#' @return model
#' @author Simon N Wood
#' @keywords internal
gamm4.setup <- function(formula, pterms,
                        data = stop("No data supplied to gamm.setup"), knots = NULL) {
  G <- gam.setup(formula, pterms,
    data = data, knots = knots, sp = NULL,
    min.sp = NULL, H = NULL, absorb.cons = TRUE, sparse.cons = 0, gamm.call = TRUE
  )

  if (!is.null(G$L)) stop("gamm can not handle linked smoothing parameters (probably from use of `id' or adaptive smooths)")
  # now perform re-parameterization...

  first.f.para <- G$nsdf + 1

  random <- list()

  if (G$nsdf > 0) ind <- 1:G$nsdf else ind <- rep(0, 0)
  X <- G$X[, ind, drop = FALSE] # accumulate fixed effects into here

  xlab <- rep("", 0)

  G$Xf <- as(X, "dgCMatrix") ## sparse version of full matrix, treating smooths as fixed

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
          G$Xf <- cbind2(G$Xf, as(sm$X * as.numeric(sm$fac == flev[k]), "dgCMatrix"))
        }
      } else {
        n.lev <- 1
        G$Xf <- cbind2(G$Xf, as(sm$X, "dgCMatrix"))
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


#' Internal function for setting up model
#'
#' @param fixed formula excluding lme4 style random effects but including smooths
#' @param random random part
#' @param data data
#'
#' @return lmod object
#'
#' @author Simon Wood and Fabian Scheipl
#'
gamm4 <- function(fixed, random = NULL, data = list()) {
  # Routine to fit a GAMM to some data. Fixed and smooth terms are defined in the formula, but the wiggly
  # parts of the smooth terms are treated as random effects. The onesided formula random defines additional
  # random terms.

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
  b <- lFormula(lme4.formula, data = mf, REML = FALSE)

  if (n.sr) { ## Fabian Scheipl's trick of overwriting dummy slots revised for new structure
    tn <- names(b$reTrms$cnms) ## names associated with columns of Z (same order as Gp)
    ind <- seq_along(tn)
    sn <- names(G$random) ## names of smooth random components
    for (i in 1:n.sr) { ## loop through random effect smooths
      k <- ind[sn[i] == tn] ## which term should contain G$random[[i]]
      ii <- (b$reTrms$Gp[k] + 1):b$reTrms$Gp[k + 1]
      b$reTrms$Zt[ii, ] <- as(t(G$random[[i]]), "dgCMatrix")
      b$reTrms$cnms[[k]] <- attr(G$random[[i]], "s.label")
    }
  }
  list(
    lmod = b,
    G = G,
    gam.terms = gam.terms,
    fake.formula = gp$fake.formula
  )
} ## end of gamm4
