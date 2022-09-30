#include "data.h"
#include "parameters.h"
#include "model.h"
#include "misc.h"
#include "update_funs.h"
#include <unsupported/Eigen/SpecialFunctions>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
T loss(const parameters<T>& parlist, const data<T>& datlist, const Vdual<T>& lp,
       const T k, Model<T>* mod, ldlt<T>& solver){
  T phi = mod->get_phi(lp, parlist.u, datlist.y, parlist.WSqrt);

  T yDotLinpred = (parlist.WSqrt * datlist.y).dot(parlist.WSqrt * lp);
  T bSquared = mod->cumulant(lp, datlist.trials, parlist.WSqrt);
  T c_phi = mod->constfun(datlist.y, phi, k, parlist.WSqrt);
  T exponent_g = (yDotLinpred - bSquared) / phi +
    c_phi - parlist.u.squaredNorm() / 2 / phi;

  T hessian_determinant = solver.vectorD().array().log().sum();
  T res = exponent_g - hessian_determinant / 2;

  return res;
}

template <typename T>
logLikObject<T> logLik(
    parameters<T> parlist, data<T> datlist, const T k, std::vector<Model<T>*> mod){

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);
  update_WSqrt(parlist.WSqrt, parlist.weights, parlist.weights_mapping);

  Vdual<T> lp = linpred(parlist, datlist);
  Ddual<T> V;
  V.diagonal() = mod[0]->get_V(lp, datlist.trials, parlist.WSqrt);

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  ldlt<T> solver;
  solver.setShift(1);
  SpMdual<T> H = inner_hessian(parlist, datlist, V);
  solver.analyzePattern(H);

  Vdual<T> delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(parlist, datlist, lp, k, mod[0], solver);
  T deviance_new;

  for(int i{}; i < parlist.maxit_conditional_modes; i++){
    Vdual<T> residual =
      (datlist.y - mod[0]->meanfun(linpred(parlist, datlist), datlist.trials)).array();
    Vdual<T> weighted_residual = parlist.WSqrt.diagonal().array().pow(2) * residual.array();
    delta_u = solver.solve(
      (parlist.Lambdat * datlist.Zt * weighted_residual) - parlist.u);
    if(delta_u.squaredNorm() < parlist.epsilon_u) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      lp = linpred(parlist, datlist);
      V.diagonal() = mod[0]->get_V(lp, datlist.trials, parlist.WSqrt);
      H = inner_hessian(parlist, datlist, V);
      solver.factorize(H);
      deviance_new = -2 * loss(parlist, datlist, lp, k, mod[0], solver);
      if(deviance_new < deviance_prev){
        break;
      }
      parlist.u -= step * delta_u;
      step /= 2;
      if(j == 9){
        Rcpp::Rcout << "Could not find reducing step: i = " << i << ", j = " << j << std::endl;
        Rcpp::stop("Error");
      }
    }
    deviance_prev = deviance_new;
  }

  logLikObject<T> ret;
  ret.logLikValue = - deviance_new / 2;
  ret.V = V.diagonal();
  ret.u = parlist.u;
  ret.phi = mod[0]->get_phi(lp, parlist.u, datlist.y, parlist.WSqrt);

  return ret;
}

template <typename T>
Rcpp::List wrapper(
    const Eigen::VectorXd& y,
    const Eigen::VectorXd& trials,
    const Eigen::MatrixXd& X,
    const Eigen::SparseMatrix<double>& Zt,
    const Eigen::SparseMatrix<double>& Lambdat,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& theta,
    const Eigen::VectorXi& theta_mapping,
    const Eigen::VectorXd& lambda,
    const Eigen::VectorXi& lambda_mapping_X,
    const Eigen::VectorXi& lambda_mapping_Zt,
    const Eigen::VectorXd& weights,
    const Eigen::VectorXi& weights_mapping,
    const std::string& family,
    const int& maxit_conditional_modes,
    const double& epsilon_u
  ){

  data<T> datlist{y, trials, X, Zt};
  parameters<T> parlist{
      theta, beta, lambda, Eigen::VectorXd::Zero(Zt.rows()), theta_mapping,
      lambda_mapping_X, lambda_mapping_Zt, Lambdat, weights, weights_mapping,
      maxit_conditional_modes, epsilon_u, y.size()};

  T k{0};
  if(family == "binomial"){
    k = static_cast<T>(((lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
      lgamma(trials.array() - y.array() + 1)).sum()));
  } else if(family == "poisson"){
    k = static_cast<T>(-(y.array() + 1).lgamma().sum());
  }

  std::vector<Model<T>*> mod;
  Gaussian<T> gaussianMod{};
  Binomial<T> binomialMod{};
  Poisson<T> poissonMod{};
  if(family == "gaussian") {
    mod.push_back(&gaussianMod);
  } else if(family == "binomial"){
    mod.push_back(&binomialMod);
  } else if(family == "poisson"){
    mod.push_back(&poissonMod);
  } else {
    Rcpp::stop("Unknown family.");
  }

  auto fx = [=, &mod, &datlist](parameters<T>& parlist){
    auto lll = logLik(parlist, datlist, k, mod);
    return lll.logLikValue;
  };
  auto gx = [=, &mod, &datlist](parameters<T>& parlist){
    return logLik(parlist, datlist, k, mod);;
  };

  return create_result(fx, gx, parlist);

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
//' @param weights Vector of weights.
//' @param family A length one \code{character} denoting the family.
//' @param maxit_conditional_modes Maximum number of iterations for
//' conditional models. Can be 1 when \code{family = "gaussian"}.
//' @param hessian Boolean specifying whether to include the Hessian matrix
//' at the given parameters. Defaults to \code{FALSE}.
//' @param epsilon_u Tolerance in the inner iteration. Defaults to \code{1e-10}.
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
    const Eigen::Map<Eigen::VectorXd> weights,
    const Eigen::Map<Eigen::VectorXi> weights_mapping,
    const std::string family,
    const int maxit_conditional_modes,
    const bool hessian = false,
    double epsilon_u = 1e-10
){

  if(hessian){
    return wrapper<dual2nd>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
      family, maxit_conditional_modes, epsilon_u);
  } else {
    return wrapper<dual1st>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
      family, maxit_conditional_modes, epsilon_u);
  }

}
