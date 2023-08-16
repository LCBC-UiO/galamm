factor_loadings <- function(object){
  UseMethod("factor_loadings")
}

#' Extract Factor Loadings from galamm Object
#'
#' @param object An object
#'
#' @return Vector
#' @aliases factor_loadings factor_loadings.galamm
#' @export factor_loadings
#' @export
#'
#' @details
#' This function has been named \code{factor_loadings} rather than just
#' \code{loadings} to avoid conflict with \code{stats::loadings}.
#'
#'
factor_loadings.galamm <- function(object){
  if(!exists("lambda", object)) return(NULL)

  lambda_tmp_est <- lambda_tmp_se <- object$lambda[[1]]
  lambda_tmp_se[lambda_tmp_se %in% c(0, 1)] <- NA_real_

  lambda_tmp_est[lambda_tmp_est > 1] <- object$par[object$lambda_inds]
  lambda_tmp_se[!is.na(lambda_tmp_se)] <-
    sqrt(diag(vcov(object, parm = object$lambda_inds)))

  matrix(rbind(lambda_tmp_est, lambda_tmp_se),
         nrow = nrow(lambda_tmp_est),
         dimnames = list(paste0("lambda", seq_len(nrow(object$lambda[[1]]))),
                         as.character(rbind(colnames(lambda_tmp_est), "SE"))))


}
