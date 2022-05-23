#include <RcppEigen.h>
#include <autodiff/forward/real.hpp>
#include <autodiff/forward/real/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

Eigen::SparseMatrix<real> update_lambda(
    Eigen::SparseMatrix<real> Lambdat,
    VectorXreal theta,
    Eigen::VectorXi Lind
){
  int iteration_counter{};
  for (int k=0; k<Lambdat.outerSize(); ++k)
    for (Eigen::SparseMatrix<real>::InnerIterator it(Lambdat,k); it; ++it)
    {
      it.valueRef() = theta(Lind(iteration_counter) - 1);
      ++iteration_counter;
    }
    return Lambdat;
}


// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y0,
    const Eigen::Map<Eigen::MatrixXd> X0,
    const Eigen::MappedSparseMatrix<double> Zt0,
    const Eigen::MappedSparseMatrix<double> Lambdat0,
    const Eigen::Map<Eigen::VectorXi> Lind
){

  VectorXreal y = y0.cast<real>();
  MatrixXreal X = X0.cast<real>();
  Eigen::SparseMatrix<real> Zt = Zt0.cast<real>();
  Eigen::SparseMatrix<real> Lambdat = Lambdat0.cast<real>();

  VectorXreal beta(2);
  beta << 251.405, 10.4673;
  real phi{30.895};
  VectorXreal theta(1);
  theta << 36.012;

  Lambdat = update_lambda(Lambdat, theta, Lind);


  Eigen::SimplicialLDLT<Eigen::SparseMatrix<real> > solver;



  return Rcpp::List::create(
    Rcpp::Named("beta") = 0,
    Rcpp::Named("Lambdat") = Lambdat.cast<double>()
  );
}
