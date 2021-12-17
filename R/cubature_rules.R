#' Generate cubature rules for integration
#'
#' @param dimension Dimension of the integral.
#' @param num_quadrature_points Vector containing the number of
#' quadrature points per dimension. Only used when \code{type = "gauss-hermite"}.
#' @param type Type of rule.
#'
#' @export
#' @return List of rules and weights.
#'
generate_cubature_rules <- function(dimension,
                                    num_quadrature_points = NULL,
                                    type = "lu_darmofal_4.2"){
  stopifnot(dimension >= 2)

  if(type == "lu_darmofal_4.2"){
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

    return(list(quadrature_points = quadrature_points,
                quadrature_weights = quadrature_weights,
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
      quadrature_points = as.matrix(quadrature_points),
      quadrature_weights = as.numeric(quadrature_weights),
      num_quadrature_points = prod(num_quadrature_points)
    )
  }

}


