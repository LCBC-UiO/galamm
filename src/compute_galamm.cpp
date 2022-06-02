#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "family.h"
#include "model.h"
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > ldlt;


dual2nd negloglik(Model::Model& mod, ldlt& solver, int maxit = 50){
  mod.update_Lambda();
  VectorXdual2nd delta{};

  for(int i{}; i < maxit; ++i){
    Eigen::SparseMatrix<dual2nd> A = mod.get_hessian();
    solver.factorize(A);
    VectorXdual2nd b = mod.get_rhs();
    delta = solver.solve(b);
    mod.u += delta;
    if(delta.squaredNorm() < 1e-10) {
      break;
    }
  }

  dual2nd logdet = (solver.vectorD()).array().log().sum() / 2;

  dual2nd ll = -logdet +
    (mod.y.dot(mod.get_eta()) - mod.family.d(mod.get_eta())) /
    mod.family.a(mod.phi) +
      mod.family.c(mod.y, mod.phi) - mod.u.squaredNorm() / 2;

  return -ll;
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Z,
    const Eigen::MappedSparseMatrix<double> Lambda,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_inds,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXi> beta_inds,
    const double phi = 1
  ){

  Gaussian family;
  Model::Model mod{y, X, Z, Lambda, Lind, theta, beta, phi, family};
  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_hessian());


  Eigen::MatrixXd H{};
  Eigen::VectorXd g{};
  dual2nd nll{};
  VectorXdual2nd delta{};

  for(int i{}; i < 100; i++){
    H = hessian(negloglik, wrt(mod.theta, mod.beta, mod.phi),
                at(mod, solver, 10), nll, g);

    delta = H.colPivHouseholderQr().solve(-g);
    mod.theta += delta.segment(theta_inds.minCoeff(), theta_inds.rows());
    mod.beta += delta.segment(beta_inds.minCoeff(), beta_inds.rows());
    mod.phi += delta(delta.rows() - 1);

    if(delta.squaredNorm() < 1e-10){
      break;
    }
  }


  return Rcpp::List::create(
    Rcpp::Named("nll") = static_cast<double>(nll),
    Rcpp::Named("u") = mod.u.cast<double>(),
    Rcpp::Named("Lambda") = mod.Lambda.cast<double>(),
    Rcpp::Named("H") = H,
    Rcpp::Named("g") = g,
    Rcpp::Named("delta") = delta.cast<double>(),
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.phi)
  );
}
