#include "data.h"
#include "parameters.h"
#include "model.h"
#include <unsupported/Eigen/SpecialFunctions>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]



template <typename T>
Eigen::Matrix<T, Eigen::Dynamic, 1> linpred(
    const parameters<T>& parlist,
    const data<T>& datlist,
    Model<T>& mod
  ){
  return datlist.X * parlist.beta + datlist.Zt.transpose() * parlist.Lambdat.transpose() * parlist.u;
};

template <typename T>
T exponent_g(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& lp,
    const T k,
    Model<T>& mod
  ){
  T phi = mod.get_phi(lp, parlist.u, datlist.y);
  return (datlist.y.dot(lp) - mod.cumulant(lp, datlist.trials)) / phi +
    mod.constfun(lp, parlist.u, datlist.y, datlist.trials, k) -
    parlist.u.squaredNorm() / 2 / phi;
};

template <typename T>
T loss(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& lp,
    const T k,
    Model<T>& mod,
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  return exponent_g(parlist, datlist, lp, k, mod) -
    solver.vectorD().array().log().sum() / 2;
}

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
Eigen::SparseMatrix<T> inner_hessian(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::Matrix<T, Eigen::Dynamic, 1>& lp,
    const T& phi,
    const Eigen::DiagonalMatrix<T, Eigen::Dynamic>& V,
    Model<T>& mod
  ){
  return (1 / phi) * parlist.Lambdat * datlist.Zt * V *
    datlist.Zt.transpose() * parlist.Lambdat.transpose();
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
    for(typename Eigen::SparseMatrix<T>::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind) * it.value();
      }
      counter++;
    }
  }
};

template <typename T>
struct logLikObject {
  T logLikValue;
  Eigen::Matrix<T, Eigen::Dynamic, 1> V;
  Eigen::Matrix<T, Eigen::Dynamic, 1> u;
  T phi;
};

template <typename T>
logLikObject<T> logLik(
    parameters<T> parlist,
    data<T> datlist,
    const T k,
    Model<T>& mod
  ){
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);

  int n = datlist.X.rows();
  Vdual lp = linpred(parlist, datlist, mod);
  Ddual V(n);
  V.diagonal() = mod.get_V(lp, parlist.u, datlist.y, datlist.trials);
  T phi = mod.get_phi(lp, parlist.u, datlist.y);

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> > solver;
  solver.setShift(1);
  SpMdual H = inner_hessian(parlist, datlist, lp, phi, V, mod);
  solver.analyzePattern(H);

  Vdual delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(parlist, datlist, lp, k, mod, solver);
  T deviance_new;

  for(int i{}; i < mod.maxit_conditional_modes; i++){
    delta_u = solver.solve((parlist.Lambdat * datlist.Zt *
      (datlist.y - mod.meanfun(linpred(parlist, datlist, mod), datlist.trials)) - parlist.u));
    if(delta_u.squaredNorm() < mod.epsilon_u) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      lp = linpred(parlist, datlist, mod);
      phi = mod.get_phi(lp, parlist.u, datlist.y);
      V.diagonal() = mod.get_V(lp, parlist.u, datlist.y, datlist.trials);
      H = inner_hessian(parlist, datlist, lp, phi, V, mod);
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
  ret.phi = phi;

  return ret;
}


template <typename T>
Rcpp::List wrapper(
    parameters<T>& parlist,
    data<T>& datlist,
    Model<T>& mod,
    const T k = 0
  ){

  T ll{};
  auto fx = [=, &mod](parameters<T>& parlist){
    auto lll = logLik(parlist, datlist, k, mod);
    return lll.logLikValue;
    };

  Eigen::VectorXd g = gradient(
    fx, wrt(parlist.theta, parlist.beta, parlist.lambda), at(parlist), ll);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g
  );
}

template <>
Rcpp::List wrapper(
    parameters<dual2nd>& parlist,
    data<dual2nd>& datlist,
    Model<dual2nd>& mod,
    const dual2nd k
){

  dual2nd ll{};
  auto fx = [=, &mod](parameters<dual2nd>& parlist){
    auto lll = logLik(parlist, datlist, k, mod);
    return lll.logLikValue;
  };

  Eigen::VectorXd g{};
  Eigen::MatrixXd H = hessian(
    fx, wrt(parlist.theta, parlist.beta, parlist.lambda), at(parlist), ll, g);

  logLikObject extras = logLik(parlist, datlist, k, mod);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g,
    Rcpp::Named("hessian") = H,
    Rcpp::Named("u") = extras.u.cast<double>(),
    Rcpp::Named("V") = extras.V.cast<double>(),
    Rcpp::Named("phi") = static_cast<double>(extras.phi)
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
//' @param u A \code{numeric} vector of initial values for the random effects.
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
    const Eigen::Map<Eigen::VectorXd> u,
    const std::string family,
    const int maxit_conditional_modes,
    const bool hessian = false,
    double epsilon_u = 1e-10
){

  if(hessian){
    data<dual2nd> datlist(y, trials, X, Zt);
    parameters<dual2nd> parlist(theta, beta, lambda, u, theta_mapping,
                                lambda_mapping_X, lambda_mapping_Zt, Lambdat);

    if(family == "gaussian"){
      Gaussian<dual2nd> mod{ maxit_conditional_modes, epsilon_u };
      return wrapper<dual2nd>(parlist, datlist, mod);
    } else if(family == "binomial"){

      Binomial<dual2nd> mod{ maxit_conditional_modes, epsilon_u };
      dual2nd k = static_cast<dual2nd>((lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
        lgamma(trials.array() - y.array() + 1)).sum());
      return wrapper<dual2nd>(parlist, datlist, mod, k);

    } else if(family == "poisson"){
      Poisson<dual2nd> mod{ maxit_conditional_modes, epsilon_u };
      dual2nd k = static_cast<dual2nd>(-(y.array() + 1).lgamma().sum());
      return wrapper<dual2nd>(parlist, datlist, mod, k);

    } else {
      Rcpp::stop("Unknown family.");
    }
  } else {
    data<dual1st> datlist(y, trials, X, Zt);
    parameters<dual1st> parlist(theta, beta, lambda, u, theta_mapping,
                                lambda_mapping_X, lambda_mapping_Zt, Lambdat);

    if(family == "gaussian"){
      Gaussian<dual1st> mod{ maxit_conditional_modes, epsilon_u };
      return wrapper<dual1st>(parlist, datlist, mod);
    } else if(family == "binomial"){

      Binomial<dual1st> mod{ maxit_conditional_modes, epsilon_u };
      dual1st k = static_cast<dual1st>((lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
        lgamma(trials.array() - y.array() + 1)).sum());
      return wrapper<dual1st>(parlist, datlist, mod, k);

    } else if(family == "poisson"){
      Poisson<dual1st> mod{ maxit_conditional_modes, epsilon_u };
      dual1st k = static_cast<dual1st>(-(y.array() + 1).lgamma().sum());
      return wrapper<dual1st>(parlist, datlist, mod, k);

    } else {
      Rcpp::stop("Unknown family.");
    }
  }


}
