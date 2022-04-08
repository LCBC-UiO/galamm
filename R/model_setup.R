
find_factors <- function(latent_barlist, data){
  factors <- lapply(latent_barlist, function(x) as.character(x)[[2]])

  if(any(factors %in% names(data))){
    stop("Factors in argument 'latent' cannot be columns of 'data'.")
  }

  factors
}


find_load_vars <- function(latent_barlist, data, lambda){
  load_vars <- lapply(latent_barlist, function(x) as.character(x)[[3]])
  if(!all(load_vars %in% names(data))){
    stop("All loading variables in 'latent' must be columns of 'data'.")
  }
  if(!all(load_vars %in% names(lambda))){
    stop("All loading variables specified in 'latent' must be in 'lambda'.")
  }
  load_vars
}

initialize_lambda <- function(lambda){
  lapply(lambda, function(x) {
    free_inds <- is.na(x)
    x[free_inds] <- runif(sum(free_inds), .9, 1.1)
    attr(x, "free_inds") <- free_inds
    x
  })
}

add_latent_to_data <- function(latent_barlist, factors, load_vars, data,
                               lambda_init){
  for(i in seq_along(latent_barlist)){
    ff <- factors[[i]]
    lv <- load_vars[[i]]
    eval(parse(text = paste0("data$", ff, " <- lambda_init$", lv,
                             "[data$item]")))
  }
  data
}

find_load_cols <- function(factors, load_vars, cnms){
  Map(function(ff, lv){
    grep(paste0("(?<![a-zA-Z])", ff, "(?![a-zA-Z])"), cnms, perl = TRUE)
  }, ff = factors, lv = load_vars)
}
