#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
T loss(Model<T>& mod, Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  return mod.exponent_g() - solver.vectorD().array().log().sum() / 2;
}


template <typename T>
T logLik(Model<T>& mod){
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  mod.update_Zt();
  mod.update_X();
  mod.update_Lambdat();
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> > solver;
  solver.setShift(1);
  SpMdual H = mod.get_inner_hessian();
  solver.analyzePattern(H);

  Vdual delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(mod, solver);
  T deviance_new;

  for(int i{}; i < mod.maxit_conditional_modes; i++){
    delta_u = solver.solve((mod.Lambdat * mod.Zt * (mod.y - mod.meanfun()) - mod.u));
    if(delta_u.squaredNorm() < 1e-10) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      mod.u += step * delta_u;
      H = mod.get_inner_hessian();
      solver.factorize(H);
      deviance_new = -2 * loss(mod, solver);
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

    Rcpp::checkUserInterrupt();
    deviance_prev = deviance_new;
  }

  return loss(mod, solver);
}

template <typename T>
Rcpp::List compute(Model<T>& mod){

  T dev{};
  Eigen::VectorXd g;

  gradient(logLik<T>, wrt(mod.theta, mod.beta, mod.lambda), at(mod), dev, g);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(dev),
    Rcpp::Named("gradient") = g.cast<double>(),
    Rcpp::Named("u") = mod.u.template cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(mod.phi),
    Rcpp::Named("V") = mod.V.diagonal().array().template cast<double>()
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
    const std::string family,
    const int maxit_conditional_modes
){

  if(family == "gaussian"){
    Gaussian<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    return compute(mod);
  } else if(family == "binomial"){
    Binomial<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    return compute(mod);
  } else if(family == "poisson"){
    Poisson<dual1st> mod{
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
      lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
    return compute(mod);
  } else {
    Rcpp::stop("Unknown family.");
  }

}
