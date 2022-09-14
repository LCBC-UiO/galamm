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
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(parlist, datlist, mod);
  T phi = mod.get_phi(lp, parlist.u, datlist.y);
  return (datlist.y.dot(lp) - mod.cumulant(lp, datlist.trials)) / phi +
    mod.constfun(lp, parlist.u, datlist.y, datlist.trials) -
    parlist.u.squaredNorm() / 2 / phi;
};

template <typename T>
T loss(
    const parameters<T>& parlist,
    const data<T>& datlist,
    Model<T>& mod,
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >& solver){
  return exponent_g(parlist, datlist, mod) -
    solver.vectorD().array().log().sum() / 2;
}

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
Eigen::SparseMatrix<T> inner_hessian(
    const parameters<T>& parlist,
    const data<T>& datlist,
    Eigen::DiagonalMatrix<T, Eigen::Dynamic> V,
    Model<T>& mod
  ){
  Eigen::Matrix<T, Eigen::Dynamic, 1> lp = linpred(parlist, datlist, mod);
  T inv_phi = (1 / mod.get_phi(lp, parlist.u, datlist.y));
  return inv_phi * parlist.Lambdat * datlist.Zt * mod.get_V(V, lp, parlist.u, datlist.y, datlist.trials) *
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
T logLik(
    parameters<T> parlist,
    data<T> datlist,
    Model<T>& mod,
    const int maxit_conditional_modes
  ){
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);

  int n = datlist.X.rows();
  Ddual V(n);

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> > solver;
  solver.setShift(1);
  SpMdual H = inner_hessian(parlist, datlist, V, mod);
  solver.analyzePattern(H);

  Vdual delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(parlist, datlist, mod, solver);
  T deviance_new;

  for(int i{}; i < maxit_conditional_modes; i++){
    delta_u = solver.solve((parlist.Lambdat * datlist.Zt *
      (datlist.y - mod.meanfun(linpred(parlist, datlist, mod), datlist.trials)) - parlist.u));
    if(delta_u.squaredNorm() < 1e-10) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      H = inner_hessian(parlist, datlist, V, mod);
      solver.factorize(H);
      deviance_new = -2 * loss(parlist, datlist, mod, solver);
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

  return - deviance_new / 2;
}


template <typename T>
Rcpp::List wrapper(
    parameters<T>& parlist,
    data<T>& datlist,
    Model<T>& mod,
    const int maxit_conditional_modes
  ){

  T ll{};
  Eigen::VectorXd g = gradient(
    logLik<T>, wrt(parlist.theta, parlist.beta, parlist.lambda),
    at(parlist, datlist, mod, maxit_conditional_modes), ll);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g.cast<double>()
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

  data<dual1st> datlist(
      y.cast<dual1st>(), trials.cast<dual1st>(),
      X.cast<dual1st>(), Zt.cast<dual1st>());

  parameters<dual1st> parlist(
      theta.cast<dual1st>(),
      beta.cast<dual1st>(),
      lambda.cast<dual1st>(),
      u.cast<dual1st>(),
      theta_mapping,
      lambda_mapping_X,
      lambda_mapping_Zt,
      Lambdat.cast<dual1st>());

  if(family == "gaussian"){
    Gaussian<dual1st> mod{0};

    return wrapper<dual1st>(
      parlist,
      datlist,
      mod, maxit_conditional_modes
    );
  } else if(family == "binomial"){

    double k = (lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
                lgamma(trials.array() - y.array() + 1)).sum();

    Binomial<dual1st> mod{ static_cast<dual1st>(k) };

    return wrapper<dual1st>(
      parlist,
      datlist,
      mod,
      maxit_conditional_modes
    );

  } else if(family == "poisson"){
    double k = -(y.array() + 1).lgamma().sum();
    Poisson<dual1st> mod{ static_cast<dual1st>(k) };


    return wrapper<dual1st>(
      parlist,
      datlist,
      mod, maxit_conditional_modes
    );

  } else {
    Rcpp::stop("Unknown family.");
  }

}
