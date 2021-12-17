#' Generate cubature rules for integration
#'
#' @param dimension Dimension of the integral.
#' @param type Type of rule.
#'
#' @return List of rules and weights.
#'
generate_cubature_rules <- function(dimension, type = "lu_darmofal_4.2"){
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
  }

}


