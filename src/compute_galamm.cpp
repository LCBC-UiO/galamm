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
  mod.Lambdat_needs_update = true;
  mod.get_conditional_modes(solver);
  solver.factorize(mod.get_inner_hessian());
  return -2 * (mod.exponent_g() - log(solver.determinant()) / 2);
}

Rcpp::List compute(GALAMM::Model& mod, ldlt& solver, int maxit_outer,
                   double delta_tol){
  dscl deviance{};
  dscl deviance_new{};
  Eigen::VectorXd g;
  Eigen::MatrixXd H;
  dvec delta_param;
  int max_backtracking_steps = 20;

  // parameters for backtracking line search
  double alpha = 1;
  double c = 1e-4;
  double rho = .9;

  solver.analyzePattern(mod.get_inner_hessian());

  for(int i{}; i < maxit_outer; i++){
    dvec param_old;
    double alpha_bar = alpha;

    H = hessian(get_deviance, wrt(mod.theta), at(mod, solver), deviance, g);
    param_old.resize(mod.theta.size());
    param_old << mod.theta;

    delta_param = -H.colPivHouseholderQr().solve(g);

    if(delta_param.squaredNorm() < delta_tol){
      Rcpp::Rcout << "Stopping at iteration " << i << std::endl;
      break;
    }

    for(int j{}; j < max_backtracking_steps; j++){
      mod.theta += alpha_bar * delta_param;

      deviance_new = get_deviance(mod, solver);
      dscl deviance_armijo = deviance + c * alpha_bar * g.dot(delta_param);

      if(deviance_new <= deviance_armijo) {
        break;
      }

      mod.theta = param_old;
      alpha_bar = rho * alpha_bar;
    }
  }

  deviance = get_deviance(mod, solver);

  return Rcpp::List::create(
    Rcpp::Named("Lambdat") = mod.get_Lambdat().cast<double>(),
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("u") = mod.u.cast<double>(),
    Rcpp::Named("b") = (mod.get_Lambdat().transpose() * mod.u).cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.get_phi()),
    Rcpp::Named("deviance") = static_cast<double>(deviance),
    Rcpp::Named("RXtRX") = mod.RXtRX.cast<double>()
  );
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_log,
    const int maxit_outer,
    const std::string family,
    const Eigen::Map<Eigen::VectorXd> trials,
    const double delta_tol
  ){

  ldlt solver;
  solver.setShift(1);


  if(family == "gaussian"){
    GALAMM::Gaussian mod{y, X, Zt, Lambdat, Lind, theta, theta_log,
                         trials};
    return compute(mod, solver, maxit_outer, delta_tol);
  } else if(family == "binomial"){
    GALAMM::Binomial mod{y, X, Zt, Lambdat, Lind, theta, theta_log,
                         trials, 50};
    return compute(mod, solver, maxit_outer, delta_tol);
  } else if(family == "poisson"){
    GALAMM::Poisson mod{y, X, Zt, Lambdat, Lind, theta, theta_log,
                        trials, 50};
    return compute(mod, solver, maxit_outer, delta_tol);
  } else {
    Rcpp::stop("Unknown family.");
  }


}
