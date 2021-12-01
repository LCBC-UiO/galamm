

linkinv <- function(x) (1 + exp(-x))^(-1)

log_response_prob <- function(mean, y, trials = 1){
  y * (log(pmax(mean, 1e-10))) + (trials - y) * log(pmax(1 - mean, 1e-10))
}



define_crossedvars <- function(mainvars){
  crossedvars <- expand.grid(mainvars, mainvars)
  crossedvars <- crossedvars[crossedvars$Var1 != crossedvars$Var2, , drop = FALSE]

  if(nrow(crossedvars) > 0) {
    crossedvars <- unique(t(apply(crossedvars, 1, function(x){
      x <- factor(x, levels = mainvars)
      sort(x)
    })))
  }
  crossedvars <- cbind(crossedvars, apply(crossedvars, 1, paste, collapse = ":"),
                       paste(crossedvars[, 1], crossedvars[, 1], sep = ":"))
  dimnames(crossedvars) <- list(NULL, c("var1", "var2", "var1var2", "var1var1"))
  crossedvars
}
