#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

using llt = Eigen::SimplicialLLT<Eigen::SparseMatrix<dual1st> >;

dual1st deviance(GALAMM::Model& mod, llt& solver){
  mod.Lambdat_needs_update = true;
  mod.get_conditional_modes(solver);
  solver.factorize(mod.get_inner_hessian());
  return -2 * (mod.exponent_g() - log(solver.determinant()) / 2);
}

Rcpp::List compute(GALAMM::Model& mod, llt& solver){
  dual1st deviance{};
  Eigen::VectorXd g;

  g = gradient(deviance, wrt(mod.beta, mod.theta, mod.phi),
               at(mod, solver), deviance);

  return Rcpp::List::create(
    Rcpp::Named("deviance") = static_cast<double>(deviance),
    Rcpp::Named("gradient") = g.cast<double>()
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
    const Eigen::Map<Eigen::VectorXd> lambda_mapping_X,
    const Eigen::Map<Eigen::VectorXd> lambda_mapping_Zt,
    const std::string family
){

  llt solver;
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
