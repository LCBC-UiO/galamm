#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

dual1st deviance(
    GALAMM::Model& mod,
    Eigen::SimplicialLLT<Eigen::SparseMatrix<autodiff::dual1st> >& solver){
  mod.get_conditional_modes(solver);
  return -2 * (mod.exponent_g() - log(solver.determinant()) / 2);
}

Rcpp::List compute(
    GALAMM::Model& mod,
    Eigen::SimplicialLLT<Eigen::SparseMatrix<autodiff::dual1st> >& solver){
  dual1st dev{};
  Eigen::VectorXd g;

  g = gradient(deviance, wrt(mod.theta, mod.beta, mod.lambda),
               at(mod, solver), dev);

  return Rcpp::List::create(
    Rcpp::Named("deviance") = static_cast<double>(dev),
    Rcpp::Named("gradient") = g
  );
}

//' Compute the marginal likelihood of a GLLAMM or GALAMM
//'
//' This function computes the Laplace approximate marginal
//' likelihood.
//'
//' @param y A \code{numeric} vector of responses.
//' @param trials A \code{numeric} vector representing the number
//' of trials. Only used for binomial models, but should be set to
//' some arbtirary value otherwise.
//' @param X A dense \code{numeric} matrix.
//' @param Zt A sparse matrix of class \code{dgCMatrix}.
//' @param Lambdat A sparse matrix of class \code{dgCMatrix}.
//' @param beta A \code{numeric} vector of fixed effects.
//' @param theta A \code{numeric} vector of variance components,
//' parametrized identically to \code{lme4}.
//' @param theta_mapping An \code{integer} vector corresponding to
//' \code{Lind} used by \code{lme4}, but with base zero indexing.
//' @param lambda A \code{numeric} vector of factor loadings.
//' @param lambda_mapping_X An \code{integer} vector of mappings between
//' \code{X} and \code{lambda}, columnwise. Should be set to
//' \code{integer()} if not used. An entry \code{-1} indicates that the
//' corresponding value of \code{X} does not depend on \code{lambda},
//' as in the case where the first element of \code{lambda} is fixed to 1.
//' @param lambda_mapping_Zt An \code{integer} vector of mappings between
//' \code{Zt} and \code{lambda}, along the nonzero elements of \code{Zt}
//' as can be found by \code{Zt@x}. Should be set to
//' \code{integer()} if not used. An entry \code{-1} indicates that the
//' corresponding value of \code{X} does not depend on \code{lambda},
//' as in the case where the first element of \code{lambda} is fixed to 1.
//' @param family A length one \code{character} denoting the family.
//'
//' @return A \code{list} with elements \code{deviance} and \code{gradient}.
//'
// [[Rcpp::export]]
Rcpp::List marginal_likelihood(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::VectorXd> trials,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_mapping,
    const Eigen::Map<Eigen::VectorXd> lambda,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_X,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_Zt,
    const std::string family
){

  Eigen::SimplicialLLT<Eigen::SparseMatrix<autodiff::dual1st> > solver;
  solver.setShift(1);

  if(family == "gaussian"){
    GALAMM::Gaussian mod{y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
                         lambda, lambda_mapping_X, lambda_mapping_Zt, 1};
    solver.analyzePattern(mod.get_inner_hessian());
    return compute(mod, solver);
  } else if(family == "binomial"){
    GALAMM::Binomial mod{y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
                         lambda, lambda_mapping_X, lambda_mapping_Zt, 50};
    solver.analyzePattern(mod.get_inner_hessian());
    return compute(mod, solver);
  } else if(family == "poisson"){
    GALAMM::Poisson mod{y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
                        lambda, lambda_mapping_X, lambda_mapping_Zt, 50};
    solver.analyzePattern(mod.get_inner_hessian());
    return compute(mod, solver);
  } else {
    Rcpp::stop("Unknown family.");
  }


}
