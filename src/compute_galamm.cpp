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

// [[Rcpp::export]]
Rcpp::List compute_galamm(
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
