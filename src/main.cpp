#include <RcppEigen.h>
#include <autodiff/forward/real.hpp>
#include <autodiff/forward/real/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

enum class Family{
  Gaussian
};

struct ModelData{
  ModelData(
    Eigen::VectorXd y0, Eigen::MatrixXd Xt0, Eigen::SparseMatrix<double> Zt0,
    Eigen::SparseMatrix<double> Lambdat0
  ) : y { y0.cast<real>() }, Xt { Xt0.cast<real>() }, Zt{ Zt0.cast<real>()},
          Lambdat { Lambdat0.cast<real>() }{}

  const VectorXreal y;
  const MatrixXreal Xt;
  const Eigen::SparseMatrix<real> Zt;
  const Eigen::SparseMatrix<real> Lambdat;
};

struct ModelParameters{
  ModelParameters(
    Eigen::VectorXi Lind0,
    Eigen::VectorXd theta0,
    Eigen::RowVectorXd beta0
  ) : Lind { Lind0 },
  theta { theta0.cast<real>() }, beta { beta0.cast<real>() },
  u { VectorXreal::Zero(Lind.rows(), 1) } {}

  VectorXreal const cumulant (const VectorXreal&) const;
  real const varfun (const real&) const;
  real const c (const VectorXreal&) const;

  const Eigen::VectorXi Lind;
  VectorXreal theta;
  RowVectorXreal beta;
  real phi{1};
  RowVectorXreal u;
  Family family{Family::Gaussian};
};

real const ModelParameters::c (const VectorXreal& y) const {
  switch(family){
  case Family::Gaussian: {
    return y.squaredNorm() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

real loglik(const ModelParameters& mpar, const ModelData& mdat) {
  Eigen::SparseMatrix<real> LambdatUpd = mdat.Lambdat;
  real ll{};
  VectorXreal linpred = mpar.u * LambdatUpd * mdat.Zt + mpar.beta * mdat.Xt;

  ll = (mdat.y.dot(linpred) - mpar.cumulant(linpred).sum()) / mpar.varfun(mpar.phi) +
    mpar.c(mdat.y) - mpar.u.squaredNorm() / 2;
  return ll;
}

VectorXreal const ModelParameters::cumulant(const VectorXreal& a) const {
  switch(family){
  case Family::Gaussian: {
    return a.array().square() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

real const ModelParameters::varfun(const real& phi) const {
  switch(family){
  case Family::Gaussian: {
    return pow(phi, 2.0);
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}


// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> Xt,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::RowVectorXd> beta
  ){

  ModelData mdat{y, Xt, Zt, Lambdat};
  ModelParameters mpar{Lind, theta, beta};

  real res = loglik(mpar, mdat);
  Eigen::VectorXd g = gradient(loglik, wrt(mpar.beta), at(mpar, mdat));

  return Rcpp::List::create(
    Rcpp::Named("loglik") = static_cast<double>(res),
    Rcpp::Named("g") = g
  );
}
