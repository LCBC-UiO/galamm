#include <RcppEigen.h>
#include <autodiff/forward/real.hpp>
#include <autodiff/forward/real/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

Eigen::SparseMatrix<real> update_Lambdat(
    Eigen::SparseMatrix<real> Lambdat,
    VectorXreal theta,
    Eigen::VectorXi Lind
){
  int iteration_counter{};
  for (int k=0; k<Lambdat.outerSize(); ++k){
    for (Eigen::SparseMatrix<real>::InnerIterator it(Lambdat,k); it; ++it)
    {
      it.valueRef() = theta(Lind(iteration_counter));
      ++iteration_counter;
    }
  }
  return Lambdat;
}

Eigen::SparseMatrix<real> update_V(Eigen::SparseMatrix<real> V, real phi){
  for(int i = 0; i < V.rows(); ++i){
    V.coeffRef(i, i) = phi;
  }
  return V;
}

VectorXreal update_u(VectorXreal u_prev, VectorXreal b0,
                     Eigen::SparseMatrix<real> A,
                     Eigen::SimplicialLDLT<Eigen::SparseMatrix<real> >& solver){
  VectorXreal u;
  for(int iter = 0; iter < 10; ++iter){
    VectorXreal b = b0 - u_prev;
    solver.factorize(A);
    u = solver.solve(b);
    real delta = (u - u_prev).squaredNorm();

    if(delta < 1e-10){
      Rcpp::Rcout << "Breaking at " << iter << std::endl;
      break;
    }
    u_prev = u;
  }
  return u;
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y0,
    const Eigen::Map<Eigen::MatrixXd> X0,
    const Eigen::MappedSparseMatrix<double> Zt0,
    const Eigen::MappedSparseMatrix<double> Lambdat0,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const int n_obs,
    const int n_ranef
){

  VectorXreal y = y0.cast<real>();
  MatrixXreal X = X0.cast<real>();
  Eigen::SparseMatrix<real> Zt = Zt0.cast<real>();
  Eigen::SparseMatrix<real> Lambdat = Lambdat0.cast<real>();
  MatrixXreal Iq;
  Iq.setIdentity(n_ranef, n_ranef);
  Eigen::SparseMatrix<real> V(n_obs, n_obs);
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<real> > solver;
  V = update_V(V, 1);

  VectorXreal beta(2);
  beta << 251.405, 10.4673;
  VectorXreal theta(1);
  theta << 36.012;
  V = update_V(V, std::pow(30.895, -2));
  VectorXreal u_prev = VectorXreal::Random(n_ranef, 1);
  VectorXreal mu = X * beta;

  Eigen::SparseMatrix<real> A = Lambdat * Zt * V * Zt.transpose() * Lambdat.transpose() + Iq;
  solver.analyzePattern(A);

  Lambdat = update_Lambdat(Lambdat, theta, Lind);

  A = Lambdat * Zt * V * Zt.transpose() * Lambdat.transpose() + Iq;

  VectorXreal b0 = Lambdat * Zt * (y - mu) / std::pow(30.895, 2);
  VectorXreal u = update_u(u_prev, b0, A, solver);



  return Rcpp::List::create(
    Rcpp::Named("beta") = 0,
    Rcpp::Named("Lambdat") = Lambdat.cast<double>(),
    Rcpp::Named("V") = V.cast<double>(),
    Rcpp::Named("u_prev") = u_prev.cast<double>(),
    Rcpp::Named("u") = u.cast<double>()
  );
}
