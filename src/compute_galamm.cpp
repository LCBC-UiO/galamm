#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
Eigen::Matrix<T, Eigen::Dynamic, 1> linpred(
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    Model<T>& mod
  ){
  return mod.X * mod.beta + mod.Zt.transpose() * Lambdat.transpose() * mod.u;
};

template <typename T>
T exponent_g(
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(X, Zt, Lambdat, mod);
  T phi = mod.get_phi(lp);
  return (mod.y.dot(lp) - mod.cumulant(lp)) / phi + mod.constfun(lp) -
    mod.u.squaredNorm() / 2 / phi;
};

template <typename T>
T loss(
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    Model<T>& mod,
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  return exponent_g(X, Zt, Lambdat, mod) - solver.vectorD().array().log().sum() / 2;
}

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
Eigen::SparseMatrix<T> inner_hessian(
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(X, Zt, Lambdat, mod);
  return (1 / mod.get_phi(lp)) *
    Lambdat * mod.Zt * mod.get_V(lp) *
    mod.Zt.transpose() * Lambdat.transpose();
};

template <typename T>
void update_Lambdat(Eigen::SparseMatrix<T>& Lambdat, Model<T>& mod){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (typename Eigen::SparseMatrix<T>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      int ind = mod.theta_mapping(lind_counter);
      if(ind != -1){
        it.valueRef() = mod.theta(ind);
      }
      lind_counter++;
    }
  }
};

template <typename T>
void update_X(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X, Model<T>& mod){
  if(mod.lambda_mapping_X.size() == 0) return;
  for(int i = 0; i < X.size(); i++){
    int newind = mod.lambda_mapping_X(i);
    if(newind != -1){
      *(X.data() + i) *= mod.lambda(newind);
    }
  }
};

template <typename T>
void update_Zt(Eigen::SparseMatrix<T>& Zt, Model<T>& mod){
  if(mod.lambda_mapping_Zt.size() == 0) return;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(typename Model<T>::SpMdual::InnerIterator it(Zt, k); it; ++it){
      int newind = mod.lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = mod.lambda(newind) * it.value();
      }
      counter++;
    }
  }
};


template <typename T>
T logLik(
    Eigen::MatrixXd X_init,
    Eigen::SparseMatrix<double> Zt_init,
    Eigen::SparseMatrix<double> Lambdat_init,
    Model<T>& mod
  ){
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  Mdual X = X_init.cast<T>();
  SpMdual Zt = Zt_init.cast<T>();
  SpMdual Lambdat = Lambdat_init.cast<T>();

  update_Lambdat(Lambdat, mod);
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> > solver;
  solver.setShift(1);
  SpMdual H = inner_hessian(X, Zt, Lambdat, mod);
  solver.analyzePattern(H);

  Vdual delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(X, Zt, Lambdat, mod, solver);
  T deviance_new;

  for(int i{}; i < mod.maxit_conditional_modes; i++){
    delta_u = solver.solve((Lambdat * mod.Zt * (mod.y - mod.meanfun(linpred(X, Zt, Lambdat, mod))) - mod.u));
    if(delta_u.squaredNorm() < 1e-10) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      mod.u += step * delta_u;
      H = inner_hessian(X, Zt, Lambdat, mod);
      solver.factorize(H);
      deviance_new = -2 * loss(X, Zt, Lambdat, mod, solver);
      if(deviance_new < deviance_prev){
        break;
      }
      mod.u -= step * delta_u;
      step /= 2;
      if(j == 9){
        Rcpp::Rcout << "Could not find reducing step: i = " << i << ", j = " << j << std::endl;
        Rcpp::stop("Error");
      }
    }

    deviance_prev = deviance_new;
  }

  return - deviance_new / 2;
}



//' Compute the marginal likelihood of a GLLAMM or GALAMM
//'
//' This function computes the Laplace approximate marginal
//' likelihood.
//'
//' @param y A \code{numeric} vector of responses.
//' @param trials A \code{numeric} vector representing the number
//' of trials. Only used for binomial models, but should be set to
//' some arbitrary value otherwise.
//' @param X A dense \code{numeric} matrix.
//' @param Zt A sparse matrix of class \code{dgCMatrix}.
//' @param Lambdat A sparse matrix of class \code{dgCMatrix}.
//' @param beta A \code{numeric} vector of fixed effects.
//' @param theta A \code{numeric} vector of variance components,
//' parametrized identically to \code{lme4}.
//' @param theta_mapping An \code{integer} vector corresponding to
//' \code{Lind} used by \code{lme4}, but with base zero indexing.
//' @param lambda A \code{numeric} vector of factor loadings.
//' @param lambda_mapping_X An \code{integer} vector of mappings between
//' \code{X} and \code{lambda}, columnwise. Should be set to
//' \code{integer()} if not used. An entry \code{-1} indicates that the
//' corresponding value of \code{X} does not depend on \code{lambda},
//' as in the case where the first element of \code{lambda} is fixed to 1.
//' @param lambda_mapping_Zt An \code{integer} vector of mappings between
//' \code{Zt} and \code{lambda}, along the nonzero elements of \code{Zt}
//' as can be found by \code{Zt@x}. Should be set to
//' \code{integer()} if not used. An entry \code{-1} indicates that the
//' corresponding value of \code{X} does not depend on \code{lambda},
//' as in the case where the first element of \code{lambda} is fixed to 1.
//' @param family A length one \code{character} denoting the family.
//' @param maxit_conditional_modes Maximum number of iterations for
//' conditional models. Can be 1 when \code{family = "gaussian"}.
//'
//' @return A \code{list} with elements \code{logLik} and \code{gradient}.
//' @export
//'
//' @details For examples, see the vignette on maximum likelihood estimation.
// [[Rcpp::export]]
Rcpp::List marginal_likelihood(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::VectorXd> trials,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_mapping,
    const Eigen::Map<Eigen::VectorXd> lambda,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_X,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_Zt,
    const std::string family,
    const int maxit_conditional_modes
){

  if(family == "gaussian"){
    Gaussian<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    dual1st ll = logLik(X, Zt, Lambdat, mod);
    return Rcpp::List::create(Rcpp::Named("ll") = static_cast<double>(ll));
  } else if(family == "binomial"){
    Binomial<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    dual1st ll = logLik(X, Zt, Lambdat, mod);
    return Rcpp::List::create(Rcpp::Named("ll") = static_cast<double>(ll));
  } else if(family == "poisson"){
    Poisson<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    dual1st ll = logLik(X, Zt, Lambdat, mod);
    return Rcpp::List::create(Rcpp::Named("ll") = static_cast<double>(ll));
  } else {
    Rcpp::stop("Unknown family.");
  }

}
