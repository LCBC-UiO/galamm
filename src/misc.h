#ifndef MISC_H
#define MISC_H

#include "model.h"
#include "parameters.h"
#include "data.h"

//' Compute linear predictor
//'
//' Templated function evaluating the linear predictor, including random
//' effects. Template \code{T} is typically one of \code{double},
//' \code{autodiff:dual1st}, or \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param parlist An object of class \code{parameters<T>} containing the
//'   parameters at which to evaluate the marginal log-likelihood.
//' @param datlist An object of class \code{data<T>} containing the data with
//'   which to evaluate the marginal log-likelihood.
//' @return A scalar of the template type \code{T} containing the linear
//'   predictor.
//' @noRd
template <typename T>
Vdual<T> linpred(
    const parameters<T>& parlist,
    const data<T>& datlist
){
  return datlist.X * parlist.beta + datlist.Zt.transpose() *
    parlist.Lambdat.transpose() * parlist.u;
};

//' Compute Hessian matrix used in penalized iteratively reweighted least
//' squares
//'
//' Templated function evaluating the Hessian used in penalized iteratively
//' reweighted least squares, as defined in the unnumbered equations below
//' equation 13 in
//' \insertCite{sorensenLongitudinalModelingAgeDependent2023}{galamm}. Template
//' \code{T} is typically one of \code{double}, \code{autodiff:dual1st}, or
//' \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param parlist An object of class \code{parameters<T>} containing the
//'   parameters at which to evaluate the marginal log-likelihood.
//' @param datlist An object of class \code{data<T>} containing the data with
//'   which to evaluate the marginal log-likelihood.
//' @param V A diagonal matrix, whose diagonal has elements
//'   \eqn{b''(\nu_{i}) / \phi_{g(i)}}. See the paragraph below equation (13) in
 //'     \insertCite{sorensenLongitudinalModelingAgeDependent2023}{galamm} for
 //'     details.
//' @return A matrix of type \code{Eigen::SparseMatrix<T>}.
//' @noRd
template <typename T>
SpMdual<T> inner_hessian(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::DiagonalMatrix<T, Eigen::Dynamic>& V
){
  return parlist.Lambdat * datlist.Zt * V *
    datlist.Zt.transpose() * parlist.Lambdat.transpose();
};

//' Create object to be returned to R
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param fx Function object for computed the marginal log-likelihood.
//' @param gx Function object for computing the gradient of the marginal
//'   log-likelihood.
//' @param parlist An object of class \code{parameters<T>} containing the
//'   parameters at which to evaluate the marginal log-likelihood.
//' @param reduced_hessian Boolean specifying whether the Hessian matrix of
//'   second derivatives should be computed only with respect to \code{beta}
//'   and \code{lambda}, in that order. This may be useful for getting a very
//'   rough estimate of the inverse covariance matrix, when the full Hessian is
//'   not positive definite.
//'
//' @return An \code{Rcpp::List} object. For this generic function, the list
//'   has a single element:
//'.  * \code{logLik} Laplace approximate marginal log-likelihood at the
 //'     parameter values specified.
//' @noRd
template <typename T, typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<T>& parlist,
                         bool reduced_hessian = false){
  T ll = fx(parlist);
  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll)
  );
}

//' Create object to be returned to R
//'
//' This function is a specialization of the template function for the case
//' where \code{T} is \code{autodiff::dual1st}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param fx Function object for computed the marginal log-likelihood.
//' @param gx Function object for computing the gradient of the marginal
//'   log-likelihood.
//' @param parlist An object of class \code{parameters<autodiff::dual1st>}
//'   containing the parameters at which to evaluate the marginal
//'   log-likelihood.
//' @param reduced_hessian Boolean specifying whether the Hessian matrix of
//'   second derivatives should be computed only with respect to \code{beta}
//'   and \code{lambda}, in that order. This may be useful for getting a very
//'   rough estimate of the inverse covariance matrix, when the full Hessian is
//'   not positive definite.
//'
//' @return An \code{Rcpp::List} object. For this generic function, the list
//'   has a single element:
//'.  * \code{logLik} Laplace approximate marginal log-likelihood at the
//'     parameter values specified.
//'   * \code{g} The gradient of the marginal log-likelihood at the parameter
//'     values specified.
//' @noRd
template <typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<autodiff::dual1st>& parlist,
                         bool reduced_hessian = false){
  autodiff::dual1st ll{};
  Eigen::VectorXd g{};
  g = gradient(fx, wrt(parlist.theta, parlist.beta, parlist.lambda,
                       parlist.weights),
                       at(parlist), ll);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g
  );
}

//' Create object to be returned to R
//'
//' This function is a specialization of the template function for the case
//' where \code{T} is \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param fx Function object for computed the marginal log-likelihood.
//' @param gx Function object for computing the gradient of the marginal
//'   log-likelihood.
//' @param parlist An object of class \code{parameters<autodiff::dual2nd>}
//'   containing the parameters at which to evaluate the marginal
//'   log-likelihood.
//' @param reduced_hessian Boolean specifying whether the Hessian matrix of
//'   second derivatives should be computed only with respect to \code{beta}
//'   and \code{lambda}, in that order. This may be useful for getting a very
//'   rough estimate of the inverse covariance matrix, when the full Hessian is
//'   not positive definite.
//'
//' @return An \code{Rcpp::List} object. For this generic function, the list
//'   has a single element:
//'.  * \code{logLik} Laplace approximate marginal log-likelihood at the
//'     parameter values specified.
//'   * \code{g} The gradient of the marginal log-likelihood at the parameter
//'     values specified.
//'   * \code{H} If \code{hessian = TRUE}, the Hessian matrix is provided in
//'     this element as a double precision matrix.
//'   * \code{u} If \code{hessian = TRUE}, the conditional modes of the
//'     standardized random effects are provided as a double precision vector
//'     in this element.
//'   * \code{V} If \code{hessian = TRUE}, the diagonal matrix \eqn{V} with
//'     \eqn{b''(v_{i}) / \phi_{g(i)}} on the diagonal is included in this
//'     element. See the paragraph below equation (13) in
//'     \insertCite{sorensenLongitudinalModelingAgeDependent2023}{galamm} for
//'     details.
//'   * \code{phi} If \code{hessian = TRUE}, double precision vector containing
//'     the dispersion parameter of the model, for each model family.
//' @noRd
template <typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<autodiff::dual2nd>& parlist,
                         bool reduced_hessian = false){
  autodiff::dual2nd ll{};
  Eigen::VectorXd g{};
  Eigen::MatrixXd H{};
  g = gradient(fx, wrt(parlist.theta, parlist.beta, parlist.lambda, parlist.weights), at(parlist), ll);
  if(reduced_hessian){
    H = hessian(fx, wrt(parlist.beta, parlist.lambda), at(parlist));
  } else {
    H = hessian(fx, wrt(parlist.theta, parlist.beta, parlist.lambda, parlist.weights), at(parlist));
  }

  logLikObject extras = gx(parlist);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g,
    Rcpp::Named("hessian") = H,
    Rcpp::Named("u") = extras.u.template cast<double>(),
    Rcpp::Named("V") = extras.V.template cast<double>(),
    Rcpp::Named("phi") = extras.phi.template cast<double>()
  );
}

#endif

