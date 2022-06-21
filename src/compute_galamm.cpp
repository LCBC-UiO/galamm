#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

using dscl = dual2nd;
using ldlt = Eigen::SimplicialLLT<Eigen::SparseMatrix<dscl> >;
using dvec = VectorXdual2nd;
using dmat = MatrixXdual2nd;
using dspmat = Eigen::SparseMatrix<dscl>;
using ddiag = Eigen::DiagonalMatrix<dscl, Eigen::Dynamic>;
using ivec = Eigen::VectorXi;






dscl get_deviance(GALAMM::Model& mod, ldlt& solver){

  mod.get_conditional_modes(solver);
  solver.factorize(mod.get_inner_hessian());
  dscl logdet = log(solver.determinant()) / 2;
  dscl loglik = mod.exponent_g() - logdet;

  return -2 * loglik;
}




// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const int maxit_outer,
    const std::string family,
    const Eigen::Map<Eigen::VectorXd> trials
  ){

  GALAMM::Gaussian mod{y, X, Zt, Lambdat, Lind, theta, trials};

  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_inner_hessian());

  dscl deviance = get_deviance(mod, solver);
  mod.b = mod.get_Lambdat().transpose() * mod.u;


  return Rcpp::List::create(
    Rcpp::Named("Lambdat") = mod.get_Lambdat().cast<double>(),
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("u") = mod.u.cast<double>(),
    Rcpp::Named("b") = mod.b.cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.get_phi()),
    Rcpp::Named("deviance") = static_cast<double>(deviance)
  );
}
