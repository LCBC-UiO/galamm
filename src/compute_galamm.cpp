#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>


using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

using dscl = dual2nd;
using ldlt = Eigen::SimplicialLLT<Eigen::SparseMatrix<dscl> >;
using dvec = VectorXdual2nd;
using dmat = MatrixXdual2nd;
using dspmat = Eigen::SparseMatrix<dscl>;
using ddiag = Eigen::DiagonalMatrix<dscl, Eigen::Dynamic>;
using ivec = Eigen::VectorXi;


struct Model{
  Model(
    const Eigen::VectorXd y0,
    const Eigen::MatrixXd X0,
    const Eigen::MappedSparseMatrix<double> Zt0,
    const Eigen::MappedSparseMatrix<double> Lambdat0,
    const ivec Lind0,
    const Eigen::VectorXd theta0,
    const std::string family0
  ) : y { y0.cast<dscl>() },
  X { X0.cast<dscl>() },
  Zt { Zt0.cast<dscl>() },
  Lambdat { Lambdat0.cast<dscl>() },
  Lind { Lind0 },
  theta { theta0.cast<dscl>() },
  family { family0 }
  {
    n = X.rows();
    p = X.cols();
    q = Zt.rows();
    u = dvec::Zero(q);
    beta = dvec::Zero(p);
  }

  dscl dsum(const dvec& linpred){
    if(family == "gaussian"){
      return linpred.squaredNorm() / 2;
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  dscl csum(){
    if(family == "gaussian"){
      return -.5 * (y.squaredNorm() / phi + n * log(2 * M_PI * phi));
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  dvec meanfun(const dvec& beta, const dvec& u){
    if(family == "gaussian"){
      return X * beta + Zt.transpose() * Lambdat.transpose() * u;
    } else {
      Rcpp::stop("Unknown family.");
    }
  }


  ddiag V(){
    ddiag V(n);
    if(family == "gaussian"){
      V.diagonal().array() = phi;
      return V;
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  dspmat inner_hessian(){
    return (1 / phi) * Lambdat * Zt * V() * Zt.transpose() * Lambdat.transpose();
  }

  void update_phi(){
    if(family == "gaussian"){
      phi = ((y - meanfun(beta, u)).squaredNorm() + u.squaredNorm()) / n;
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  void update_Lambdat(){
    int lind_counter{};
    for (int k{}; k < Lambdat.outerSize(); ++k)
      for (dspmat::InnerIterator it(Lambdat, k); it; ++it)
      {
        it.valueRef() = theta(Lind(lind_counter));
        lind_counter++;
      }
  }

  const dvec y;
  dmat X;
  dspmat Zt;
  dspmat Lambdat;
  const ivec Lind;
  dvec theta;
  dvec theta_old;
  dvec beta{};
  dvec b{};
  dvec u{};
  dscl phi{1};
  std::string family;

  int n;
  int p;
  int q;
};

void conditional_modes(Model& mod, ldlt& solver){
  dvec delta_beta{};
  dvec delta_u{};
  dvec beta_new = mod.beta;
  dvec u_new = mod.u;

  solver.factorize(mod.inner_hessian());
  dvec b1 = solver.permutationP() *
    (mod.Lambdat * mod.Zt * (mod.y - mod.meanfun(mod.beta, mod.u)) - mod.u );
  dvec cu = solver.matrixL().solve(b1);

  dmat b2 = solver.permutationP() * mod.Lambdat * mod.Zt * mod.V() * mod.X / mod.phi;
  dmat RZX = solver.matrixL().solve(b2);

  dmat RXtRX = (1/mod.phi) * mod.X.transpose() * mod.V() * mod.X - RZX.transpose() * RZX;
  delta_beta = RXtRX.colPivHouseholderQr().solve(mod.X.transpose() * (mod.y - mod.meanfun(mod.beta, mod.u)) -
    RZX.transpose() * cu);

  delta_u = solver.permutationPinv() * solver.matrixU().solve(cu - RZX * delta_beta);

  mod.beta = delta_beta;
  mod.u = delta_u;


}

dscl get_deviance(Model& mod, ldlt& solver){
  mod.update_Lambdat();
  mod.update_phi();
  conditional_modes(mod, solver);

  mod.update_phi();
  dscl logdet = log(solver.determinant()) / 2;
  dvec linpred = mod.X * mod.beta + mod.Zt.transpose() * mod.Lambdat.transpose() * mod.u;

  dscl loglik = (mod.y.dot(linpred) - mod.dsum(linpred)) / mod.phi + mod.csum() -
    mod.u.squaredNorm() / 2 / mod.phi - logdet;

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
    const std::string family
  ){

  Model mod{y, X, Zt, Lambdat, Lind, theta, family};

  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.inner_hessian());

  dscl deviance = get_deviance(mod, solver);
  mod.b = mod.Lambdat.transpose() * mod.u;


  return Rcpp::List::create(
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("u") = mod.u.cast<double>(),
    Rcpp::Named("b") = mod.b.cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.phi),
    Rcpp::Named("deviance") = static_cast<double>(deviance)
  );
}
