#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "family.h"
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > ldlt;

struct Model{

  Model(
    Eigen::VectorXd y0,
    Eigen::MatrixXd X0,
    Eigen::SparseMatrix<double> Z0,
    Eigen::SparseMatrix<double> Lambda0,
    Eigen::VectorXi Lind0,
    Eigen::VectorXd theta0,
    Eigen::VectorXd beta0,
    Family& family0
  ) :
  y { y0.cast<dual2nd>() },
  X { X0.cast<dual2nd>() },
  Z { Z0.cast<dual2nd>() },
  Lambda { Lambda0.cast<dual2nd>() },
  Lind { Lind0 },
  theta { theta0.cast<dual2nd>() },
  beta { beta0.cast<dual2nd>() },
  family { family0 }
  {
    q = Z.cols();
    p = X.cols();
    u = VectorXdual2nd::Zero(q, 1);
  }

  VectorXdual2nd get_eta(){
    VectorXdual2nd eta = X * beta + Z * Lambda * u;
    Rcpp::Rcout << "eta = " << eta << std::endl;
    return eta;
  }
  VectorXdual2nd get_mu(){
    return family.inv_link(get_eta());
  }
  Eigen::SparseMatrix<dual2nd> get_hessian(){

    Eigen::SparseMatrix<dual2nd> A = Lambda.transpose() * Z.transpose() *
      family.V(get_eta(), phi) * Z * Lambda /
      pow(family.a(phi), 2.0);
    return A;
  }
  VectorXdual2nd get_rhs(){
    return Lambda.transpose() * Z.transpose() * (y - get_mu()) / family.a(phi) - u;
  }
  void update_Lambda(){
    int lind_counter{};
    for (int k=0; k<Lambda.outerSize(); ++k)
      for (Eigen::SparseMatrix<dual2nd>::InnerIterator it(Lambda, k); it; ++it)
      {
        it.valueRef() = theta(Lind(lind_counter));
        lind_counter++;
      }
  }

  VectorXdual2nd y;
  MatrixXdual2nd X;
  Eigen::SparseMatrix<dual2nd> Z;
  Eigen::VectorXi Lind;
  const Family& family;
  Eigen::SparseMatrix<dual2nd> Lambda;
  VectorXdual2nd theta;
  VectorXdual2nd beta;
  VectorXdual2nd u;
  dual2nd phi{954.5278};

private:
  int q{};
  int p{};
};

dual2nd loglik(Model& mod, ldlt& solver){
  mod.update_Lambda();
  VectorXdual2nd delta{};

  for(int i{}; i < 10; ++i){
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

  dual2nd rss = (mod.y.dot(mod.get_eta()) - mod.family.d(mod.get_eta()) / 2 -
    mod.y.squaredNorm() / 2) / mod.phi;
  Rcpp::Rcout << "rss = " << rss << std::endl;

  dual2nd ll = -logdet +
    (mod.y.dot(mod.get_eta()) - mod.family.d(mod.get_eta())) /
    mod.family.a(mod.phi) +
      mod.family.c(mod.y, mod.phi) - mod.u.squaredNorm() / 2;

  return ll;
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Z,
    const Eigen::MappedSparseMatrix<double> Lambda,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXd> beta
  ){

  Gaussian family;
  Model mod{y, X, Z, Lambda, Lind, theta, beta, family};
  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_hessian());


  dual2nd ll = loglik(mod, solver);



  return Rcpp::List::create(
    Rcpp::Named("ll") = static_cast<double>(ll),
    Rcpp::Named("u") = mod.u.cast<double>()
  );
}
