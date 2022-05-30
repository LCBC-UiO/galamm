#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > ldlt;

struct Family {
  virtual ~Family() = default;
  virtual dual2nd c_phi(const VectorXdual2nd& y, const dual2nd& phi) const = 0;
  virtual VectorXdual2nd cumulant(const VectorXdual2nd& a) const = 0;
  virtual dual2nd varfun(const dual2nd& phi) const = 0;
};


struct Gaussian : Family {
  dual2nd c_phi(const VectorXdual2nd& y, const dual2nd& phi) const override {
    return -y.squaredNorm() / 2 / phi;
  }
  VectorXdual2nd cumulant(const VectorXdual2nd& a) const override {
    return a.array().square() / 2;
  }
  dual2nd varfun(const dual2nd& phi) const override {
    return phi;
  }
};

struct Model{
  Model(
    const Family& family,
    const Eigen::VectorXd& y0,
    const Eigen::MatrixXd& Xt0,
    const Eigen::SparseMatrix<double>& Zt0,
    const Eigen::SparseMatrix<double>& Lambdat0,
    const Eigen::VectorXi& Lind0,
    const Eigen::VectorXd& theta0
  ) : family { family }, y{ y0.cast<dual2nd>() },
  Xt{ Xt0.cast<dual2nd>() }, Zt{ Zt0.cast<dual2nd>() },
  Lambdat{ Lambdat0.cast<dual2nd>() }, Lind { Lind0 },
  theta{ theta0.cast<dual2nd>() }
  {
    phi = 954.5278;
    betat = RowVectorXdual2nd::Zero(1, Xt.rows());
    ut = RowVectorXdual2nd::Random(1, Zt.rows());
    solver.setShift(1);
    A = Lambdat * Zt * Zt.transpose() * Lambdat.transpose();
    solver.analyzePattern(A);
  }

  void update_Lambdat();
  void update_ut();

  const Family& family;
  const VectorXdual2nd y;
  MatrixXdual2nd Xt;
  Eigen::SparseMatrix<dual2nd> Zt;
  Eigen::SparseMatrix<dual2nd> Lambdat;
  const Eigen::VectorXi Lind;
  VectorXdual2nd theta;
  ldlt solver;
  dual2nd phi;
  RowVectorXdual2nd betat;
  RowVectorXdual2nd ut;

private:
  Eigen::SparseMatrix<dual2nd> A;
};

void Model::update_Lambdat(){
  int lind_counter{};
  for (int k=0; k < Lambdat.outerSize(); ++k)
    for (Eigen::SparseMatrix<dual2nd>::InnerIterator it(Lambdat, k); it; ++it)
    {
      it.valueRef() = theta(Lind(lind_counter));
      lind_counter++;
    }
}

void Model::update_ut(){
  A = (Lambdat * Zt * Zt.transpose() * Lambdat.transpose()) / family.varfun(phi);
  solver.factorize(A);

  for(int i{}; i < 10; ++i){
    VectorXdual2nd linpred = (betat * Xt + ut * Lambdat * Zt);
    VectorXdual2nd b = Lambdat * Zt * (y - linpred) / family.varfun(phi) - ut.transpose();

    VectorXdual2nd delta = solver.solve(b);
    ut += delta;
    if(delta.squaredNorm() < 1e-10) break;
  }
}

dual2nd loglik(Model& mod){
  mod.update_Lambdat();
  mod.update_ut();
  VectorXdual2nd linpred = (mod.betat * mod.Xt + mod.ut * mod.Lambdat * mod.Zt).transpose();
  dual2nd logdet = mod.solver.vectorD().array().log().sum() / 2;

  dual2nd rss = (linpred.dot(mod.y) - mod.family.cumulant(linpred).sum()) / mod.family.varfun(mod.phi)
    + mod.family.c_phi(mod.y, mod.phi);

  dual2nd ll = -logdet + rss - mod.ut.squaredNorm() / 2;

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

  Gaussian family;
  Model mod{family, y, Xt, Zt, Lambdat, Lind, theta};
  dual2nd ll{};
  Eigen::VectorXd g{};
  Eigen::MatrixXd H{};


  for(int i = 0; i < 10; ++i){
  H = hessian(loglik, wrt(mod.theta, mod.betat, mod.theta), at(mod), ll, g);
    Eigen::VectorXd delta = H.colPivHouseholderQr().solve(-g);
    mod.theta += delta.segment(0, 1);
    mod.betat += delta.segment(1, 2);
    mod.phi += delta(3);
  }

  return Rcpp::List::create(
    Rcpp::Named("loglik") = static_cast<double>(ll),
    Rcpp::Named("theta") = mod.theta.cast<double>().transpose(),
    Rcpp::Named("beta") = mod.betat.cast<double>().transpose(),
    Rcpp::Named("phi") = static_cast<double>(mod.phi),
    Rcpp::Named("u") = mod.ut.cast<double>().transpose(),
    Rcpp::Named("Lambdat") = mod.Lambdat.cast<double>(),
    Rcpp::Named("grad") = g,
    Rcpp::Named("H") = H
  );
}
