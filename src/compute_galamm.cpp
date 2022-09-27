#include "data.h"
#include "parameters.h"
#include "model.h"
#include "update_funs.h"
#include <unsupported/Eigen/SpecialFunctions>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
Vdual<T> linpred(
    const parameters<T>& parlist,
    const data<T>& datlist
  ){
  return datlist.X * parlist.beta + datlist.Zt.transpose() * parlist.Lambdat.transpose() * parlist.u;
};

template <typename T>
T loss(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Vdual<T>& lp,
    const T k,
    Model<T>* mod,
    ldlt<T>& solver){
  T phi = mod->get_phi(lp, parlist.u, datlist.y);
  T exponent_g = (datlist.y.dot(lp) - mod->cumulant(lp, datlist.trials)) / phi +
    mod->constfun(datlist.y, phi, k) - parlist.u.squaredNorm() / 2 / phi;

  return exponent_g - solver.vectorD().array().log().sum() / 2;
}

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
SpMdual<T> inner_hessian(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::DiagonalMatrix<T, Eigen::Dynamic>& V
  ){
  return parlist.Lambdat * datlist.Zt * V *
    datlist.Zt.transpose() * parlist.Lambdat.transpose();
};

template <typename T>
struct logLikObject {
  T logLikValue;
  Vdual<T> V;
  Vdual<T> u;
  T phi;
};

template <typename T>
logLikObject<T> logLik(
    parameters<T> parlist,
    data<T> datlist,
    const T k,
    Model<T>* mod
  ){

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);

  int n = datlist.X.rows();
  Vdual<T> lp = linpred(parlist, datlist);
  Ddual<T> V(n);
  V.diagonal() = mod->get_V(lp, datlist.trials);

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  ldlt<T> solver;
  solver.setShift(1);
  SpMdual<T> H = inner_hessian(parlist, datlist, V);
  solver.analyzePattern(H);

  Vdual<T> delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(parlist, datlist, lp, k, mod, solver);
  T deviance_new;

  for(int i{}; i < parlist.maxit_conditional_modes; i++){
    delta_u = solver.solve((parlist.Lambdat * datlist.Zt *
      (datlist.y - mod->meanfun(linpred(parlist, datlist), datlist.trials)) - parlist.u));
    if(delta_u.squaredNorm() < parlist.epsilon_u) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      lp = linpred(parlist, datlist);
      V.diagonal() = mod->get_V(lp, datlist.trials);
      H = inner_hessian(parlist, datlist, V);
      solver.factorize(H);
      deviance_new = -2 * loss(parlist, datlist, lp, k, mod, solver);
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
  ret.phi = mod->get_phi(lp, parlist.u, datlist.y);

  return ret;
}

template <typename T, typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<T>& parlist){
  T ll{};
  Eigen::VectorXd g{};
  g = gradient(fx, wrt(parlist.theta, parlist.beta, parlist.lambda),
               at(parlist), ll);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g
  );
}

// Specialization for dual2nd, gives Hessian matrix plus some extra info
template <typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<dual2nd>& parlist){
  dual2nd ll{};
  Eigen::VectorXd g{};
  Eigen::MatrixXd H{};
  H = hessian(fx, wrt(parlist.theta, parlist.beta, parlist.lambda),
              at(parlist), ll, g);
  logLikObject extras = gx(parlist);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g,
    Rcpp::Named("hessian") = H,
    Rcpp::Named("u") = extras.u.template cast<double>(),
    Rcpp::Named("V") = extras.V.template cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(extras.phi)
  );
}

template <typename T>
Rcpp::List wrapper(
    Eigen::VectorXd y,
    Eigen::VectorXd trials,
    Eigen::MatrixXd X,
    Eigen::SparseMatrix<double> Zt,
    Eigen::SparseMatrix<double> Lambdat,
    Eigen::VectorXd beta,
    Eigen::VectorXd theta,
    Eigen::VectorXi theta_mapping,
    Eigen::VectorXd lambda,
    Eigen::VectorXi lambda_mapping_X,
    Eigen::VectorXi lambda_mapping_Zt,
    Eigen::VectorXd weights,
    std::string family,
    int maxit_conditional_modes,
    double epsilon_u
  ){

  data<T> datlist{y, trials, X, Zt};
  parameters<T> parlist{
      theta, beta, lambda, Eigen::VectorXd::Zero(Zt.rows()), theta_mapping,
      lambda_mapping_X, lambda_mapping_Zt, Lambdat, weights,
      maxit_conditional_modes, epsilon_u};

  T k{0};
  if(family == "binomial"){
    k = static_cast<T>(((lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
      lgamma(trials.array() - y.array() + 1)).sum()));
  } else if(family == "poisson"){
    k = static_cast<T>(-(y.array() + 1).lgamma().sum());
  }

  Model<T>* mod;
  Gaussian<T> gaussianMod{};
  Binomial<T> binomialMod{};
  Poisson<T> poissonMod{};
  if(family == "gaussian") {
    mod = &gaussianMod;
  } else if(family == "binomial"){
    mod = &binomialMod;
  } else if(family == "poisson"){
    mod = &poissonMod;
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
//' @param epsilon_u Toleranse in the inner iteration. Defaults to \code{1e-10}.
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
    const std::string family,
    const int maxit_conditional_modes,
    const bool hessian = false,
    double epsilon_u = 1e-10
){

  if(hessian){
    return wrapper<dual2nd>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights,
      family, maxit_conditional_modes, epsilon_u);
  } else {
    return wrapper<dual1st>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights,
      family, maxit_conditional_modes, epsilon_u);
  }

}
