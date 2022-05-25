#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > ldlt;

enum class Family{
  Gaussian
};

struct Model{
  Model(
    Eigen::VectorXd y0,
    Eigen::MatrixXd Xt0,
    Eigen::SparseMatrix<double> Zt0,
    Eigen::VectorXd theta0
  ) : y { y0.cast<dual2nd>() }, Xt { Xt0.cast<dual2nd>() },
  Zt{ Zt0.cast<dual2nd>()},
  theta { theta0 } {
    beta = RowVectorXdual2nd::Zero(1, Xt.rows());
  }

  VectorXdual2nd const cumulant (const VectorXdual2nd&) const;
  dual2nd const varfun () const;
  dual2nd const c_phi () const;

  const VectorXdual2nd y;
  const MatrixXdual2nd Xt;
  const Eigen::SparseMatrix<dual2nd> Zt;
  Family family{Family::Gaussian};
  VectorXdual2nd theta;
  RowVectorXdual2nd beta;
  dual2nd phi{1};
};

struct RandomEffects{
  RandomEffects(
    Eigen::VectorXi Lind0,
    Eigen::SparseMatrix<double> Lambdat0
  ) : Lind { Lind0 }, Lambdat { Lambdat0.cast<dual2nd>() } {
    u = RowVectorXdual2nd::Zero(1, Lambdat0.rows());
    if(Lind.minCoeff() != 0){
      Rcpp::stop("Wrong index in Lind\n");
    }
  }

  void update_lambda(const VectorXdual2nd& theta){
    int lind_counter{};
    for (int k=0; k<Lambdat.outerSize(); ++k)
      for (Eigen::SparseMatrix<dual2nd>::InnerIterator it(Lambdat,k); it; ++it)
      {
        it.valueRef() = theta(Lind(lind_counter));
        lind_counter++;
      }
  }
  void update_u(ldlt& solver, const Model& mod){
    Eigen::SparseMatrix<dual2nd> A = Lambdat * mod.Zt * mod.Zt.transpose() *
      Lambdat.transpose() / mod.varfun();
    solver.factorize(A);

    for(int i{}; i < 10; ++i){
      VectorXdual2nd linpred = mod.beta * mod.Xt + u * Lambdat * mod.Zt;
      VectorXdual2nd b = Lambdat * mod.Zt * (mod.y - linpred) / mod.varfun() - u.transpose();

      VectorXdual2nd delta = solver.solve(b);
      u += delta;
      if(delta.squaredNorm() < 1e-10) break;
    }

  }

  const Eigen::VectorXi Lind;
  Eigen::SparseMatrix<dual2nd> Lambdat;
  RowVectorXdual2nd u;
};

dual2nd const Model::c_phi () const {
  switch(family){
  case Family::Gaussian: {
    return -y.squaredNorm() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

VectorXdual2nd const Model::cumulant(const VectorXdual2nd& a) const {
  switch(family){
  case Family::Gaussian: {
    return a.array().square() / 2;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}

dual2nd const Model::varfun() const {
  switch(family){
  case Family::Gaussian: {
    return phi;
  } break;
  default: {
    Rcpp::stop("Unknown family\n");
  }
  }
}


dual2nd loglik(const Model& mod, RandomEffects& re, ldlt& solver){
  re.update_lambda(mod.theta);
  re.update_u(solver, mod);
  RowVectorXdual2nd linpred = mod.beta * mod.Xt + re.u * re.Lambdat * mod.Zt;

  dual2nd logdet = solver.vectorD().array().log().sum() / 2;

  dual2nd rss = (linpred.dot(mod.y) - mod.cumulant(linpred).sum()) / mod.varfun();
    + mod.c_phi();

  dual2nd ll = -logdet + rss - re.u.squaredNorm() / 2;

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


  dual2nd ll{};
  Eigen::VectorXd g{};
  Eigen::MatrixXd H{};

  Eigen::SparseMatrix<dual2nd> A = re.Lambdat * mod.Zt * mod.Zt.transpose() *
    re.Lambdat.transpose();
  solver.analyzePattern(A);

  mod.phi = 954.5278;
  for(int i = 0; i < 10; ++i){
    H = hessian(loglik, wrt(mod.theta, mod.beta),
                at(mod, re, solver), ll, g);
    Eigen::VectorXd delta = H.colPivHouseholderQr().solve(-g);
    mod.theta += delta.segment(0, 1);
    mod.beta += delta.segment(1, 2);
  }





  return Rcpp::List::create(
    Rcpp::Named("loglik") = static_cast<double>(ll),
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("u") = re.u.cast<double>(),
    Rcpp::Named("Lambdat") = re.Lambdat.cast<double>(),
    Rcpp::Named("grad") = g,
    Rcpp::Named("H") = H
  );
}
