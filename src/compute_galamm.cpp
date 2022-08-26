#include "model.h"

using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
T deviance(
    Model<T>& mod,
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  mod.get_conditional_modes(solver);
  return -2 * (mod.exponent_g() - solver.vectorD().array().log().sum() / 2);
}

Rcpp::List compute(Model<dual1st>& mod){

  Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual1st> > solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_inner_hessian());

  dual1st dev{};
  Eigen::VectorXd g;

  gradient(deviance<dual1st>, wrt(mod.theta, mod.beta, mod.lambda),
           at(mod, solver), dev, g);

  return Rcpp::List::create(
    Rcpp::Named("deviance") = static_cast<double>(dev),
    Rcpp::Named("gradient") = g.cast<double>()
  );
}

Rcpp::List compute(Model<dual2nd>& mod){

  Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual2nd> > solver;
  solver.setShift(1);
  solver.analyzePattern(mod.get_inner_hessian());

  dual2nd dev{};
  Eigen::VectorXd g;
  Eigen::MatrixXd H;

  hessian(deviance<dual2nd>, wrt(mod.theta, mod.beta, mod.lambda),
          at(mod, solver), dev, g, H);

  return Rcpp::List::create(
    Rcpp::Named("deviance") = static_cast<double>(dev),
    Rcpp::Named("gradient") = g.cast<double>(),
    Rcpp::Named("hessian") = H.cast<double>()
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
//' @param compute_hessian Boolean specifying whether or not to compute the Hessian
//' matrix. If \code{TRUE}, the Hessian at the given parameters are computed to
//' machine precision using algorithmic differentiation. Defaults to
//' \code{FALSE}.
//'
//' @return A \code{list} with elements \code{deviance} and \code{gradient}.
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
    const int maxit_conditional_modes,
    const bool compute_hessian = false
){

  if(family == "gaussian"){
    if(compute_hessian){
      Gaussian<dual2nd> mod{
        y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
        lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
      return compute(mod);
    } else {
      Gaussian<dual1st> mod{
        y, trials, X, Zt, Lambdat, beta, theta, theta_mapping,
        lambda, lambda_mapping_X, lambda_mapping_Zt, maxit_conditional_modes};
      return compute(mod);
    }


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
