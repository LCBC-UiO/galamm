#' Convencience function for replication quadrature points
#'
#' @param quadrature_rules Object returned by \code{\link{generate_cubature_rules}}.
#' @param nrep Number of repetitions.
#'
#' @return List of quadrature rules.
#' @export
#'
replicate_quadrature_points <- function(quadrature_rules, nrep){
  quadrature_list <- list()
  quadrature_list$quadrature_points <- do.call("rbind", rep(list(t(quadrature_rules$quadrature_points)), nrep))
  quadrature_list$quadrature_weights <- do.call("rbind", rep(list(quadrature_rules$quadrature_weights), nrep))
  quadrature_list$num_quadrature_points <- quadrature_rules$num_quadrature_points
  quadrature_list
}


#' Generate cubature rules for integration
#'
#' @param dimension Dimension of the integral.
#' @param num_quadrature_points Vector containing the number of
#' quadrature points per dimension. Only used when \code{type = "gauss-hermite"}.
#' @param type Type of rule.
#' @param weight_multiplier Number which the weights are multiplied by. Defaults
#' to yielding standard normal distribution.
#' @param location_multiplier Number which each location is multiplied with.
#'
#' @export
#' @return List of rules and weights.
#'
generate_cubature_rules <- function(
  dimension,
  num_quadrature_points = NULL,
  type = "lu_darmofal_4.2",
  weight_multiplier = pi^(-dimension / 2),
  location_multiplier = sqrt(2)
  ){
  stopifnot(dimension >= 2)

  if(type == "lu_darmofal_4.1") {
    stopifnot(dimension >= 4)

    quadrature_weights <- c(
      2 * pi^(dimension / 2) / (dimension + 2),
      rep((dimension^2 * (7 - dimension) * pi^(dimension / 2)) /
        (2 * (dimension + 1)^2 * (dimension + 2)^2), 2 * (dimension + 1)),
      rep((2 * (dimension - 1)^2 * pi^(dimension / 2)) /
        ((dimension + 1)^2 * (dimension + 2)^2), dimension^2 + dimension)
    )

    part1 <- rep(0, dimension)

    ar <- matrix(nrow = dimension + 1, ncol = dimension)
    for(i in seq_len(ncol(ar))){
      for(r in seq_len(nrow(ar))){
        if(i < r){
          ar[r, i] <- -sqrt(
            (dimension + 1) /
              (dimension * (dimension - i + 2) * (dimension - i + 1))
            )
        } else if(i == r){
          ar[r, i] <- sqrt(
            (dimension + 1) * (dimension - r + 1) /
              (dimension * (dimension - r + 2))
              )
        } else {
          ar[r, i] <- 0
        }
      }
    }
    part2 <- rbind(ar, -ar) * sqrt(dimension / 2 + 1)

    bj <- matrix(nrow = (dimension^2 + dimension) / 2, ncol = dimension)
    b_rowind <- 1
    for(l in seq_len(dimension + 1)){
      for(k in seq_len(l - 1)){
        bj[b_rowind, ] <- sqrt(dimension / (2 * (dimension - 1))) *
          (ar[k, ] + ar[l, ])
        b_rowind <- b_rowind + 1
      }
    }
    part3 <- rbind(bj, -bj) * sqrt(dimension / 2 + 1)

    quadrature_points <- rbind(part1, part2, part3)
    stopifnot(nrow(quadrature_points) == (dimension^2 + 3 * dimension + 3))
    stopifnot(length(quadrature_weights) == (dimension^2 + 3 * dimension + 3))

    list(
      quadrature_points = as.matrix(quadrature_points) * location_multiplier,
      quadrature_weights = as.numeric(quadrature_weights) * weight_multiplier,
      num_quadrature_points = prod(num_quadrature_points)
    )


    }
  else if(type == "lu_darmofal_4.2"){
    # Formula 4.2 in Lu and Darmofal
    all_permuted_indices <- combinat::permn(seq_len(dimension))

    quadrature_weights <- c(
      2 / (dimension + 2) * pi^(dimension / 2),
      rep((4 - dimension) / 2 / (dimension + 2)^2 * pi^(dimension / 2), 2 * dimension),
      rep(pi^(dimension / 2) / (dimension + 2)^2, 2 * dimension * (dimension - 1))
    )

    part1 <- rep(0, dimension)

    part2 <- unique(do.call(rbind, lapply(all_permuted_indices, function(ind){
      rbind(c(sqrt(dimension / 2 + 1), rep(0, dimension - 1))[ind],
            c(-sqrt(dimension / 2 + 1), rep(0, dimension - 1))[ind])
    })))

    part3 <- unique(do.call(rbind, lapply(all_permuted_indices, function(ind){
      rbind(c(sqrt(dimension / 4 + 1 / 2), sqrt(dimension / 4 + 1 / 2), rep(0, dimension - 2))[ind],
            c(-sqrt(dimension / 4 + 1 / 2), sqrt(dimension / 4 + 1 / 2), rep(0, dimension - 2))[ind],
            c(sqrt(dimension / 4 + 1 / 2), -sqrt(dimension / 4 + 1 / 2), rep(0, dimension - 2))[ind],
            c(-sqrt(dimension / 4 + 1 / 2), -sqrt(dimension / 4 + 1 / 2), rep(0, dimension - 2))[ind]
      )
    })))

    quadrature_points <- rbind(part1, part2, part3)
    stopifnot(nrow(quadrature_points) == (2 * dimension^2 + 1))

    return(list(quadrature_points = quadrature_points * location_multiplier,
                quadrature_weights = quadrature_weights * weight_multiplier,
                num_quadrature_points = nrow(quadrature_points)))
  } else if(type == "lu_darmofal_4.3"){

    # Formula 4.3 in Lu and Darmofal
    all_permuted_indices <- combinat::permn(seq_len(dimension))

    quadrature_weights <- c(
      (dimension^2 - 7 * dimension + 18) / 18 * pi^(dimension / 2),
      rep((4 - dimension) / 18 * pi^(dimension / 2), 2 * dimension),
      rep(pi^(dimension / 2) / 36, 2 * dimension * (dimension - 1))
    )

    part1 <- rep(0, dimension)

    part2 <- unique(do.call(rbind, lapply(all_permuted_indices, function(ind){
      rbind(c(sqrt(3 / 2), rep(0, dimension - 1))[ind],
            c(-sqrt(3 / 2), rep(0, dimension - 1))[ind])
    })))

    part3 <- unique(do.call(rbind, lapply(all_permuted_indices, function(ind){
      rbind(c(sqrt(3 / 2), sqrt(3 / 2), rep(0, dimension - 2))[ind],
            c(-sqrt(3 / 2), sqrt(3 / 2), rep(0, dimension - 2))[ind],
            c(sqrt(3 / 2), -sqrt(3 / 2), rep(0, dimension - 2))[ind],
            c(-sqrt(3 / 2), -sqrt(3 / 2), rep(0, dimension - 2))[ind]
      )
    })))

    quadrature_points <- rbind(part1, part2, part3)
    stopifnot(nrow(quadrature_points) == (2 * dimension^2 + 1))

    return(list(quadrature_points = quadrature_points * location_multiplier,
                quadrature_weights = quadrature_weights * weight_multiplier,
                num_quadrature_points = nrow(quadrature_points)))


  } else if(type == "gauss-hermite"){
    stopifnot(is.numeric(num_quadrature_points) &
                length(num_quadrature_points) == dimension)

    hq <- lapply(num_quadrature_points, function(dimension){
      gauss_hermite_quadrature_rules[[dimension]]
    })
    quadrature_points <- do.call(expand.grid, lapply(hq, function(x) x$x))
    quadrature_weights <- apply(do.call(expand.grid, lapply(hq, function(x) x$w)),
               1, prod)
    stopifnot(prod(num_quadrature_points) == nrow(quadrature_points))
    stopifnot(prod(num_quadrature_points) == length(quadrature_weights))

    list(
      quadrature_points = as.matrix(quadrature_points) * location_multiplier,
      quadrature_weights = as.numeric(quadrature_weights) * weight_multiplier,
      num_quadrature_points = prod(num_quadrature_points)
    )
  }

}


