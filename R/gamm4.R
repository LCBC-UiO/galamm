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
                        data = stop("No data supplied to gamm.setup"), knots = NULL)
                        ## set up the model matrix, penalty matrices and auxilliary information about the smoothing bases
                        ## needed for a gamm4 fit.
                        ## There is an implicit assumption that any rank deficient penalty does not penalize
                        ## the constant term in a basis.
                        ## 1. Calls gam.setup, as for a gam to produce object G suitable for estimating a gam.
                        ## 2. Works through smooth list, G$smooth, modifying so that...
                        ##    i) Smooths are reparameterized to have a sequence of (portion of) identity matrix
                        ##       penalties.
                        ##    ii) 'random' list is accumulated containing random effect model matrices for terms.
                        ##    iii) Sparse version of full model matrix in original parameterization is also accumulated
##    iv) Various indices are created for moving between the parameterizations.
{
  ## first simply call `gam.setup'....

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
      # rinc <- rind <- rep(0,0)
      if (!sm$fixed) {
        for (k in 1:length(rasm$rand)) n.para <- n.para + ncol(rasm$rand[[k]])
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
        for (j in 1:ncol(rasm$Xf)) {
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
#' @param formula formula
#' @param random random part
#' @param family model family
#' @param data data
#' @param weights weights
#' @param subset subset
#' @param na.action what to do
#' @param knots know
#' @param drop.unused.levels logical
#' @param REML for Gaussian model
#' @param control control
#' @param start starting point
#' @param verbose whether to blabla
#' @param ... other arguments
#'
#' @return lmod object
#'
#' @author Simon Wood and Fabian Scheipl
#'
gamm4 <- function(formula, random = NULL, family = gaussian(), data = list(), weights = NULL,
                  subset = NULL, na.action, knots = NULL, drop.unused.levels = TRUE, REML = FALSE,
                  control = NULL, start = NULL, verbose = 0L, ...) {
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
  gp <- mgcv::interpret.gam(formula) # interpret the formula

  mf <- match.call(expand.dots = FALSE)

  mf$formula <- gp$fake.formula
  mf$REML <- mf$verbose <- mf$control <- mf$start <- mf$family <- mf$scale <-
    mf$knots <- mf$random <- mf$... <- NULL ## mf$weights?
  mf$drop.unused.levels <- drop.unused.levels
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
  if (length(mvars) > 0) for (i in 1:length(mvars)) mf[[mvars[i]]] <- dl[[mvars[i]]] ## append raw versions to mf

  rm(dl) ## save space

  pmf$formula <- gp$pf
  pmf <- eval(pmf, parent.frame()) # pmf contains all data for non-smooth part
  pTerms <- attr(pmf, "terms")

  if (is.character(family)) family <- eval(parse(text = family))
  if (is.function(family)) family <- family()
  if (is.null(family$family)) stop("family not recognized")
  if (family$family == "gaussian" && family$link == "identity") linear <- TRUE else linear <- FALSE
  # now call gamm4.setup

  G <- gamm4.setup(gp, pterms = pTerms, data = mf, knots = knots)

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
    for (i in 1:n.sr) # adding the constructed variables to the model frame avoiding name duplication
    {
      mf[[r.name[i]]] <- factor(rep(1:ncol(G$random[[i]]), length = nrow(G$random[[i]])))
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

  if (is.null(control)) control <- if (linear) lmerControl() else glmerControl()

  ## NOTE: further arguments should be passed here...
  b <- if (linear) {
    lFormula(lme4.formula, data = mf, weights = G$w, REML = REML, control = control, ...)
  } else {
    glFormula(lme4.formula, data = mf, family = family, weights = G$w, control = control, ...)
  }


  if (n.sr) { ## Fabian Scheipl's trick of overwriting dummy slots revised for new structure
    tn <- names(b$reTrms$cnms) ## names associated with columns of Z (same order as Gp)
    ind <- 1:length(tn)
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
    fake.formula = gp$fake.formula
  )
} ## end of gamm4
