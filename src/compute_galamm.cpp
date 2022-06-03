#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

typedef Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > ldlt;

struct Model {

  Model(
    Eigen::VectorXd y0,
    Eigen::VectorXi trials0,
    Eigen::MatrixXd X0,
    Eigen::SparseMatrix<double> Z0,
    Eigen::SparseMatrix<double> Lambda0,
    Eigen::VectorXi Lind0,
    Eigen::VectorXd theta0,
    Eigen::VectorXd beta0,
    double phi0,
    std::string family0
  ) :
  y { y0.cast<dual2nd>() },
  trials { trials0.cast<dual2nd>() },
  X { X0.cast<dual2nd>() },
  Z { Z0.cast<dual2nd>() },
  Lambda { Lambda0.cast<dual2nd>() },
  Lind { Lind0 },
  theta { theta0.cast<dual2nd>() },
  beta { beta0.cast<dual2nd>() },
  phi { static_cast<dual2nd>(phi0) },
  family { family0 }
  {
    q = Z.cols();
    p = X.cols();
    n = X.rows();
    u = VectorXdual2nd::Zero(q, 1);
  }

  VectorXdual2nd get_eta(){
    return X * beta + Z * Lambda * u;
  }
  VectorXdual2nd get_mu(){
    return inv_link();
  }
  Eigen::SparseMatrix<dual2nd> get_hessian(){

    Eigen::SparseMatrix<dual2nd> A = Lambda.transpose() * Z.transpose() *
      V() * Z * Lambda / pow(a(), 2.0);
    return A;
  }
  VectorXdual2nd get_rhs(){
    return Lambda.transpose() * Z.transpose() * (y - get_mu()) / a() - u;
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
  dual2nd a(){
    if(family == "gaussian") {
      return phi;
    } else if(family == "binomial") {
      return 1;
    } else {
      Rcpp::stop("Unknown family.\n");
    }
  }
  dual2nd c(){
    if(family == "gaussian"){
      return -y.squaredNorm() / 2 / phi - n / 2 * log(2 * M_PI * phi);
    } else if(family == "binomial") {
      return 1;
    } else {
      Rcpp::stop("Unknown family.\n");
    }
  }
  dual2nd d(){
    if(family == "gaussian"){
      return get_eta().squaredNorm() / 2;
    } else if(family == "binomial") {
      return ((1 + get_eta().array().exp()).log() * trials.array()).sum();
    } else {
      Rcpp::stop("Unknown family.\n");
    }
  }
  Eigen::DiagonalMatrix<dual2nd, Eigen::Dynamic> V() {
    Eigen::DiagonalMatrix<dual2nd, Eigen::Dynamic> V(get_eta().rows());
    V.setIdentity();
    if(family == "gaussian"){
      V.diagonal().array() = a();
      return V;
    } else if(family == "binomial") {
      ArrayXdual2nd dd = inv_link().array();
      V.diagonal().array() = trials.array() * dd * (1 - dd);
      return V;
    } else {
      Rcpp::stop("Unknown family.\n");
    }
  }
  VectorXdual2nd inv_link(){
    if(family == "gaussian"){
      return get_eta();
    } else if(family == "binomial") {
      return get_eta().array().exp() / (1 + get_eta().array().exp());
    } else {
      Rcpp::stop("Unknown family.\n");
    }
  }

  VectorXdual2nd y;
  MatrixXdual2nd X;
  Eigen::SparseMatrix<dual2nd> Z;
  Eigen::VectorXi Lind;
  std::string family;
  Eigen::SparseMatrix<dual2nd> Lambda;
  VectorXdual2nd theta;
  VectorXdual2nd beta;
  VectorXdual2nd u;
  dual2nd phi;
  VectorXdual2nd trials;

private:
  int q{};
  int p{};
  int n{};
};

dual2nd negloglik(Model& mod, ldlt& solver, int maxit = 50){
  mod.update_Lambda();
  VectorXdual2nd delta{};

  for(int i{}; i < maxit; ++i){
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

  dual2nd ll = -logdet +
    (mod.y.dot(mod.get_eta()) - mod.d()) / mod.a() +
      mod.c() - mod.u.squaredNorm() / 2;

  return -ll;
}

// [[Rcpp::export]]
Rcpp::List compute_galamm(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::VectorXi> trials,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Z,
    const Eigen::MappedSparseMatrix<double> Lambda,
    const Eigen::Map<Eigen::VectorXi> Lind,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_inds,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXi> beta_inds,
    const double phi = 1,
    std::string family = "gaussian"
  ){

  Model mod{y, trials, X, Z, Lambda, Lind, theta, beta, phi, family};
  ldlt solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_hessian());


  Eigen::MatrixXd H{};
  Eigen::VectorXd g{};
  dual2nd nll{};
  VectorXdual2nd delta{};

  for(int i{}; i < 100; i++){
    H = hessian(negloglik, wrt(mod.theta, mod.beta, mod.phi),
                at(mod, solver, 10), nll, g);

    delta = H.colPivHouseholderQr().solve(-g);
    mod.theta += delta.segment(theta_inds.minCoeff(), theta_inds.rows());
    mod.beta += delta.segment(beta_inds.minCoeff(), beta_inds.rows());
    mod.phi += delta(delta.rows() - 1);

    if(delta.squaredNorm() < 1e-10){
      break;
    }
  }


  return Rcpp::List::create(
    Rcpp::Named("nll") = static_cast<double>(nll),
    Rcpp::Named("u") = mod.u.cast<double>(),
    Rcpp::Named("Lambda") = mod.Lambda.cast<double>(),
    Rcpp::Named("H") = H,
    Rcpp::Named("g") = g,
    Rcpp::Named("delta") = delta.cast<double>(),
    Rcpp::Named("theta") = mod.theta.cast<double>(),
    Rcpp::Named("beta") = mod.beta.cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.phi)
  );
}
