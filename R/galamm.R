#' Fitting Generalized Additive Latent and Mixed Models
#'
#' @param formula a two-sided formula object using \code{lme4} style to specify
#' random effects.
#' @param data data frame containing the variables in \code{formula}.
#' @param family a GLM family, see \code{\link{glm}} and
#' \code{\link{family}}.
#' @param latent Formula for latent variables, of the form
#' \code{~(factor | load_var)}.
#' @param lambda Initial matrix for factor loadings.
#'
#' @return Object of class \code{galamm}.
#' @export
#'
#' @examples
#' # empty example
galamm <- function(formula, data, family = gaussian,
                   latent = NULL, lambda = NULL){

  latent_barlist <- lme4::findbars(latent)
  factors <- lapply(latent_barlist, function(x) as.character(x)[[2]])

  if(any(factors %in% names(data))){
    stop("Factors in argument 'latent' cannot be columns of 'data'.")
  }

  load_vars <- lapply(latent_barlist, function(x) as.character(x)[[3]])
  if(!all(load_vars %in% names(data))){
    stop("All loading variables in 'latent' must be columns of 'data'.")
  }
  if(!all(load_vars %in% names(lambda))){
    stop("All loading variables specified in 'latent' must be in 'lambda'.")
  }

  lambda_init <- lapply(lambda, function(x) {
    x[is.na(x)] <- runif(sum(is.na(x)), .9, 1.1)
    x
  })

  for(i in seq_along(latent_barlist)){
    ff <- factors[[i]]
    lv <- load_vars[[i]]
    eval(parse(text = paste0("data$", ff, " <- lambda_init$", lv,
                             "[data$item]")))
  }

  X <- model.matrix(lme4::nobars(formula), data = data)
  # Which columns of X correspond to which latent variable
  fixed_mappings <- Map(function(ff, lv){
    col_inds <- grep(paste0("(?<![a-zA-Z])", ff, "(?![a-zA-Z])"),
                     colnames(X), perl = TRUE)
    list(
      col_inds = col_inds,
      lambda_ind = data[[lv]]
    )
  }, ff = factors, lv = load_vars)


}
