#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

using ldlt = Eigen::SimplicialLLT<Eigen::SparseMatrix<dual2nd> >;
using dvec = VectorXdual2nd;
using dmat = MatrixXdual2nd;
using dspmat = Eigen::SparseMatrix<dual2nd>;
using ivec = Eigen::VectorXi;

struct Model{
  Model(
    const Eigen::VectorXd y0,
    const Eigen::MatrixXd X0,
    const Eigen::MappedSparseMatrix<double> Zt0,
    const Eigen::MappedSparseMatrix<double> Lambdat0,
    const ivec Lind0,
    const Eigen::VectorXd theta0
  ) : y { y0.cast<dual2nd>() },
  X { X0.cast<dual2nd>() },
  Zt { Zt0.cast<dual2nd>() },
  Lambdat { Lambdat0.cast<dual2nd>() },
  Lind { Lind0 },
  theta { theta0.cast<dual2nd>() }
  {
    n = X.rows();
    p = X.cols();
    q = Zt.rows();
  }


  dspmat ZtZ(){
    dmat V = dmat::Identity(n, n);
    V.diagonal().array() = phi;

    return Lambdat * Zt * Zt.transpose() * Lambdat.transpose();
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
  dual2nd phi{2};


  int n;
  int p;
  int q;
};



// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta
  ){

  Model mod{y, X, Zt, Lambdat, Lind, theta};

  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.ZtZ());
  mod.update_Lambdat();
  solver.factorize(mod.ZtZ());

  dvec b1 = solver.permutationP() * mod.Lambdat * mod.Zt * mod.y;
  dvec cu = solver.matrixL().solve(b1);
  dmat b2 = solver.permutationP() * mod.Lambdat * mod.Zt * mod.X;
  dmat RZX = solver.matrixL().solve(b2);
  dmat RXtRX = X.transpose() * X - RZX.transpose() * RZX;
  dvec beta = RXtRX.ldlt().solve(mod.X.transpose() * y - RZX.transpose() * cu);
  dvec u = solver.permutationPinv() * solver.matrixU().solve(cu - RZX * beta);
  dvec b = mod.Lambdat.transpose() * u;


  return Rcpp::List::create(
    Rcpp::Named("cu") = cu.cast<double>(),
    Rcpp::Named("RZX") = RZX.cast<double>(),
    Rcpp::Named("RXtRX") = RXtRX.cast<double>(),
    Rcpp::Named("beta") = beta.cast<double>(),
    Rcpp::Named("u") = u.cast<double>(),
    Rcpp::Named("b") = b.cast<double>(),
    Rcpp::Named("Lambdat") = mod.Lambdat.cast<double>()
  );
}
