#include <RcppEigen.h>
#include <autodiff/forward/real.hpp>
#include <autodiff/forward/real/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<real> > ldlt;

enum class Family{
  Gaussian
};

struct Model{
  Model(
    Eigen::VectorXd y0,
    Eigen::MatrixXd Xt0,
    Eigen::SparseMatrix<double> Zt0,
    Eigen::VectorXd theta0
  ) : y { y0.cast<real>() }, Xt { Xt0.cast<real>() },
  Zt{ Zt0.cast<real>()},
  theta { theta0 } {
    beta = RowVectorXreal::Zero(1, Xt.rows());

  }

  VectorXreal const cumulant (const VectorXreal&) const;
  real const varfun () const;
  real const c_phi () const;

  const VectorXreal y;
  const MatrixXreal Xt;
  const Eigen::SparseMatrix<real> Zt;
  VectorXreal theta;
  RowVectorXreal beta;
  real phi{1};
  Family family{Family::Gaussian};
};

struct RandomEffects{
  RandomEffects(
    Eigen::VectorXi Lind0,
    Eigen::SparseMatrix<double> Lambdat0
  ) : Lind { Lind0 }, Lambdat { Lambdat0.cast<real>() } {
    u = RowVectorXreal::Zero(1, Lambdat0.rows());
    if(Lind.minCoeff() != 0){
      Rcpp::stop("Wrong index in Lind\n");
    }
  }

  void update_lambda(const VectorXreal& theta){
    int lind_counter{};
    for (int k=0; k<Lambdat.outerSize(); ++k)
      for (Eigen::SparseMatrix<real>::InnerIterator it(Lambdat,k); it; ++it)
      {
        it.valueRef() = theta(Lind(lind_counter));
        lind_counter++;
      }
  }
  void update_u(ldlt& solver, const Model& mod){
    Eigen::SparseMatrix<real> A = Lambdat * mod.Zt * mod.Zt.transpose() *
      Lambdat.transpose() / mod.varfun();
    solver.factorize(A);

    for(int i{}; i < 10; ++i){
      VectorXreal linpred = mod.beta * mod.Xt + u * Lambdat * mod.Zt;
      VectorXreal b = Lambdat * mod.Zt * (mod.y - linpred) / mod.varfun() - u.transpose();

      VectorXreal delta = solver.solve(b);
      u += delta;
      if(delta.squaredNorm() < 1e-5){
        Rcpp::Rcout << "Stopping at iteration " << i << std::endl;
        break;
      }
    }

  }

  const Eigen::VectorXi Lind;
  Eigen::SparseMatrix<real> Lambdat;
  RowVectorXreal u;
};

real const Model::c_phi () const {
  switch(family){
  case Family::Gaussian: {
    return y.squaredNorm() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

VectorXreal const Model::cumulant(const VectorXreal& a) const {
  switch(family){
  case Family::Gaussian: {
    return a.array().square() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

real const Model::varfun() const {
  switch(family){
  case Family::Gaussian: {
    return pow(phi, 2.0);
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}


real loglik(const Model& mod, RandomEffects& re, ldlt& solver){
  re.update_lambda(mod.theta);
  re.update_u(solver, mod);
  RowVectorXreal linpred = mod.beta * mod.Xt + re.u * re.Lambdat * mod.Zt;
  real ll = (linpred.dot(mod.y) - mod.cumulant(linpred).sum()) / mod.varfun() +
    mod.c_phi() - re.u.squaredNorm() / 2;

  return ll;
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::MatrixXd> Xt,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta
  ){

  Model mod{y, Xt, Zt, theta};
  RandomEffects re{Lind, Lambdat};
  ldlt solver;
  solver.setShift(1);

  Eigen::SparseMatrix<real> A = re.Lambdat * mod.Zt * mod.Zt.transpose() *
    re.Lambdat.transpose();
  solver.analyzePattern(A);

  real ll{};
  Eigen::VectorXd g = gradient(loglik, wrt(mod.beta), at(mod, re, solver), ll);


  return Rcpp::List::create(
    Rcpp::Named("loglik") = static_cast<double>(ll),
    Rcpp::Named("grad") = g
  );
}
