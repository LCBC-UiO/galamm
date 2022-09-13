#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
Eigen::Matrix<T, Eigen::Dynamic, 1> linpred(
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& beta,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& u,
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    Model<T>& mod
  ){
  return X * beta + Zt.transpose() * Lambdat.transpose() * u;
};

template <typename T>
T exponent_g(
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& beta,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& u,
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& y,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& trials,
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(beta, u, X, Zt, Lambdat, mod);
  T phi = mod.get_phi(lp, u, y);
  return (y.dot(lp) - mod.cumulant(lp, trials)) / phi + mod.constfun(lp, u, y, trials) -
    u.squaredNorm() / 2 / phi;
};

template <typename T>
T loss(
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& beta,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& u,
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& y,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& trials,
    Model<T>& mod,
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  return exponent_g(beta, u, X, Zt, Lambdat, y, trials, mod) - solver.vectorD().array().log().sum() / 2;
}

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
Eigen::SparseMatrix<T> inner_hessian(
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& beta,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& u,
    const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
    const Eigen::SparseMatrix<T>& Zt,
    const Eigen::SparseMatrix<T>& Lambdat,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& y,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& trials,
    Eigen::DiagonalMatrix<T, Eigen::Dynamic> V,
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(beta, u, X, Zt, Lambdat, mod);
  return (1 / mod.get_phi(lp, u, y)) *
    Lambdat * Zt * mod.get_V(V, lp, u, y, trials) *
    Zt.transpose() * Lambdat.transpose();
};

template <typename T>
void update_Lambdat(Eigen::SparseMatrix<T>& Lambdat,
                    Eigen::Matrix<T, Eigen::Dynamic, 1> theta,
                    const Eigen::VectorXi& theta_mapping
                      ){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (typename Eigen::SparseMatrix<T>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      int ind = theta_mapping(lind_counter);
      if(ind != -1){
        it.valueRef() = theta(ind);
      }
      lind_counter++;
    }
  }
};

template <typename T>
void update_X(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
              Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
              const Eigen::VectorXi& lambda_mapping_X){
  if(lambda_mapping_X.size() == 0) return;
  for(int i = 0; i < X.size(); i++){
    int newind = lambda_mapping_X(i);
    if(newind != -1){
      *(X.data() + i) *= lambda(newind);
    }
  }
};

template <typename T>
void update_Zt(Eigen::SparseMatrix<T>& Zt,
               Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
               const Eigen::VectorXi& lambda_mapping_Zt){
  if(lambda_mapping_Zt.size() == 0) return;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(typename Model<T>::SpMdual::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind) * it.value();
      }
      counter++;
    }
  }
};


template <typename T>
T logLik(
    Eigen::Matrix<T, Eigen::Dynamic, 1> theta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> beta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
    Eigen::Matrix<T, Eigen::Dynamic, 1> u,
    const Eigen::VectorXi& theta_mapping,
    const Eigen::VectorXi& lambda_mapping_X,
    const Eigen::VectorXi& lambda_mapping_Zt,
    Eigen::Matrix<T, Eigen::Dynamic, 1> y,
    Eigen::Matrix<T, Eigen::Dynamic, 1> trials,
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> X,
    Eigen::SparseMatrix<T> Zt,
    Eigen::SparseMatrix<T> Lambdat,
    Model<T>& mod,
    const int maxit_conditional_modes
  ){
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  int n = X.rows();
  Ddual V(n);

  update_Lambdat(Lambdat, theta, theta_mapping);
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> > solver;
  solver.setShift(1);
  SpMdual H = inner_hessian(beta, u, X, Zt, Lambdat, y, trials, V, mod);
  solver.analyzePattern(H);

  Vdual delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(beta, u, X, Zt, Lambdat, y, trials, mod, solver);
  T deviance_new;

  for(int i{}; i < maxit_conditional_modes; i++){
    delta_u = solver.solve((Lambdat * Zt * (y - mod.meanfun(linpred(beta, u, X, Zt, Lambdat, mod), trials)) - u));
    if(delta_u.squaredNorm() < 1e-10) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      u += step * delta_u;
      H = inner_hessian(beta, u, X, Zt, Lambdat, y, trials, V, mod);
      solver.factorize(H);
      deviance_new = -2 * loss(beta, u, X, Zt, Lambdat, y, trials, mod, solver);
      if(deviance_new < deviance_prev){
        break;
      }
      u -= step * delta_u;
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

template <typename T>
Rcpp::List wrapper(
    Eigen::Matrix<T, Eigen::Dynamic, 1> theta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> beta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
    Eigen::Matrix<T, Eigen::Dynamic, 1> u,
    const Eigen::VectorXi& theta_mapping,
    const Eigen::VectorXi& lambda_mapping_X,
    const Eigen::VectorXi& lambda_mapping_Zt,
    Eigen::Matrix<T, Eigen::Dynamic, 1> y,
    Eigen::Matrix<T, Eigen::Dynamic, 1> trials,
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> X,
    Eigen::SparseMatrix<T> Zt,
    Eigen::SparseMatrix<T> Lambdat,
    Model<T>& mod,
    const int maxit_conditional_modes
  ){

  T ll = logLik(theta, beta, lambda, u, theta_mapping, lambda_mapping_X,
                lambda_mapping_Zt, y, trials, X, Zt, Lambdat, mod,
                maxit_conditional_modes);

  return Rcpp::List::create(
    Rcpp::Named("ll") = static_cast<double>(ll)//,
    //Rcpp::Named("g") = g.cast<double>()
  );
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
    const Eigen::Map<Eigen::VectorXd> u,
    const std::string family,
    const int maxit_conditional_modes
){

  if(family == "gaussian"){
    Gaussian<dual1st> mod{};

    return wrapper<dual1st>(
      theta.cast<dual1st>(), beta.cast<dual1st>(), lambda.cast<dual1st>(),
      u.cast<dual1st>(),
      theta_mapping, lambda_mapping_X, lambda_mapping_Zt,
      y.cast<dual1st>(), trials.cast<dual1st>(),
      X.cast<dual1st>(), Zt.cast<dual1st>(), Lambdat.cast<dual1st>(), mod,
      maxit_conditional_modes
    );
  } else if(family == "binomial"){
    Binomial<dual1st> mod{};

    return wrapper<dual1st>(
      theta.cast<dual1st>(), beta.cast<dual1st>(), lambda.cast<dual1st>(),
      u.cast<dual1st>(),
      theta_mapping, lambda_mapping_X, lambda_mapping_Zt,
      y.cast<dual1st>(), trials.cast<dual1st>(),
      X.cast<dual1st>(), Zt.cast<dual1st>(), Lambdat.cast<dual1st>(), mod,
      maxit_conditional_modes
    );

  } else if(family == "poisson"){
    Poisson<dual1st> mod{};

    return wrapper<dual1st>(
      theta.cast<dual1st>(), beta.cast<dual1st>(), lambda.cast<dual1st>(),
      u.cast<dual1st>(),
      theta_mapping, lambda_mapping_X, lambda_mapping_Zt,
      y.cast<dual1st>(), trials.cast<dual1st>(),
      X.cast<dual1st>(), Zt.cast<dual1st>(), Lambdat.cast<dual1st>(), mod,
      maxit_conditional_modes
    );
  } else {
    Rcpp::stop("Unknown family.");
  }

}
