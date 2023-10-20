#include <memory>
#include "data.h"
#include "parameters.h"
#include "model.h"
#include "misc.h"
#include "update_funs.h"
#include <unsupported/Eigen/SpecialFunctions>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

//' Evaluate the deviance at given values of random effects
//'
//'
//' @param parlist An object of class \code{parameters<T>} containing the
//'   parameters at which to evaluate the marginal log-likelihood.
//' @param datlist An object of class \code{data<T>} containing the data with
//'   which to evaluate the marginal log-likelihood.
//' @param lp Vector with linear predictor values, with arguments of same
//'   type as the template \code{T}.
//' @param modvec Reference to a vector of pointers to objects of class
//'   \code{Model<T>}, containing the necessary functions specific to the
//'   exponential families used in the model.
//' @param solver A solver for sparse linear systems of type
//'   \code{Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >}.
//' @param phi Vector of dispersion parameters, one for each model family.
//' @return Model deviance, after integrating out the random effects. This
//'   corresponds to \eqn{-2} times the marginal loglikelihood.
//' @noRd
template <typename T>
T loss(const parameters<T>& parlist, const data<T>& datlist,
       const Vdual<T>& lp, std::vector<std::unique_ptr<Model<T>>>& modvec,
       ldlt<T>& solver, Vdual<T>& phi){

  T ret{};
  for(int k{}; k < modvec.size(); k++){
    int size0 = (parlist.family_mapping.array() == k).template cast<int>().sum();
    Vdual<T> y0(size0);
    Vdual<T> trials0(size0);
    Vdual<T> lp0(size0);
    Ddual<T> WSqrt0(size0);
    int counter{0};
    for(int i{}; i < parlist.n; i++){
      if(parlist.family_mapping(i) == k){
        y0(counter) = datlist.y(i);
        trials0(counter) = datlist.trials(i);
        lp0(counter) = lp(i);
        WSqrt0.diagonal()(counter) = parlist.WSqrt.diagonal()(i);
        ++counter;
      }
    }
    phi(k) = modvec[k]->get_phi(lp0, parlist.u, y0, WSqrt0, size0);

    ret += ((WSqrt0 * y0).dot(WSqrt0 * lp0) -
      modvec[k]->cumulant(lp0, trials0, WSqrt0)) / phi(k) +
      modvec[k]->constfun(y0, phi(k), WSqrt0);
  }

  return ret - parlist.u.squaredNorm() / 2 / phi(0) -
    solver.vectorD().array().log().sum() / 2;

}

//' @title Evaluate marginal log-likelihood
//'
//' @description
//' Implements penalized iteratively reweighted least squares for finding
//' conditional modes of random effects, and returns the resulting marginal
//' log-likelihood. The template \code{T} will typically be one of
//' \code{double}, \code{autodiff:dual1st}, or \code{autodiff::dual2nd}.
//'
//'
//' @param parlist An object of class \code{parameters<T>} containing the
//'   parameters at which to evaluate the marginal log-likelihood.
//' @param datlist An object of class \code{data<T>} containing the data with
//'   which to evaluate the marginal log-likelihood.
//' @param modvec Reference to a vector of pointers to objects of class
//'   \code{Model<T>}, containing the necessary functions specific to the
//'   exponential families used in the model.
//' @return An object of class \code{logLikObject<T>}. See its definition for
//'   details.
//'
//' @noRd
template <typename T>
logLikObject<T> logLik(
    parameters<T> parlist, data<T> datlist, std::vector<std::unique_ptr<Model<T>>>& modvec){

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt, parlist.lambda_mapping_Zt_covs);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);
  update_WSqrt(parlist.WSqrt, parlist.weights, parlist.weights_mapping);

  Vdual<T> lp = linpred(parlist, datlist);
  Vdual<T> phi(modvec.size());

  Ddual<T> V(parlist.n);
  V.setZero();
  for(int k{}; k < modvec.size(); k++){
    Vdual<T> upd = modvec[k]->get_V(lp, datlist.trials, parlist.WSqrt).array() *
      (parlist.family_mapping.array() == k).array().template cast<T>();
    V.diagonal() += upd;
  }

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  ldlt<T> solver;
  solver.setShift(1);
  SpMdual<T> H = inner_hessian(parlist, datlist, V);
  solver.analyzePattern(H);
  Vdual<T> delta_u{};
  solver.factorize(H);
  T lossvalue_prev = -2 * loss(parlist, datlist, lp, modvec, solver, phi);
  T lossvalue_new{};

  for(int i{}; i < parlist.maxit_conditional_modes; i++){
    Vdual<T> meanvec(parlist.n);
    meanvec.setZero();
    for(int k{}; k < modvec.size(); k++){
      Vdual<T> upd = modvec[k]->meanfun(linpred(parlist, datlist), datlist.trials).array() *
        (parlist.family_mapping.array() == k).array().template cast<T>();
      meanvec += upd;
    }

    Vdual<T> weighted_residual = parlist.WSqrt.diagonal().array().pow(2) * (datlist.y - meanvec).array();
    delta_u = solver.solve((parlist.Lambdat * datlist.Zt * weighted_residual) - parlist.u);

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      lp = linpred(parlist, datlist);
      V.setZero();
      for(int k{}; k < modvec.size(); k++){
        Vdual<T> upd = modvec[k]->get_V(lp, datlist.trials, parlist.WSqrt).array() *
          (parlist.family_mapping.array() == k).array().template cast<T>();
        V.diagonal() += upd;
      }

      H = inner_hessian(parlist, datlist, V);
      solver.factorize(H);
      lossvalue_new = -2 * loss(parlist, datlist, lp, modvec, solver, phi);

      if(lossvalue_new <= lossvalue_prev){
        // Appropriate stepsize found for this candidate u, break out of innermost loop
        break;
      }
      parlist.u -= step * delta_u;
      step /= 2;
      // If we cannot find a reducing step, then it's not possible to reduce the
      // lossvalue any more, and also the outer loop should break
      if(j == 9){
        //Rcpp::Rcout << "Could not find reducing step: i = " << i << ", j = " << j << std::endl;
        goto jump; // go all the way down to after the loop
      }
    }
    // Cannot improve likelihood more in this PIRLS iteration
    if(abs(lossvalue_prev - lossvalue_new) < parlist.lossvalue_tol){
      break;
    }
    lossvalue_prev = lossvalue_new;
  }

  jump: // if reducing step could not be found, we end up here.
  logLikObject<T> ret;
  ret.logLikValue = - lossvalue_new / 2;
  ret.V = V.diagonal();
  ret.u = parlist.u;
  ret.phi = phi;

  return ret;
}

//' @title Set up parameter and model family
//'
//' @description
//' Templated wrapper function which sets up the necessary parameters to
//' evaluate the marginal likelihood. The template type \code{T} will typically
//' be one of \code{double}, \code{autodiff::dual1st}, and
//' \code{autodiff::dual2nd}.
//'
//'
//' @param y Double precision vector of response values.
//' @param trials Double precision vector with number of trials. When trials
//' are not applicable, e.g., with Gaussian or Poisson responses, this should
//' be a vector of ones.
//' @param X Fixed effect model matrix.
//' @param Zt Transpose of random effect model matrix.
//' @param Lambdat Lower Cholesky factor of random effect covariance matrix.
//' @param beta Double precision vector of fixed effects.
//' @param theta Double precision vector with the unique elements of
//'   \code{Lambdat}.
//' @param theta_mapping Integer vector mapping elements of \code{theta} to the
//'   positions in \code{Lambdat}.
//' @param u_init Double precision vector with initial values of random
//'   effects. These random effects should be standardized.
//' @param lambda Double precision vector of factor loadings.
//' @param lambda_mapping_X Integer vector mapping elements of
//'   \code{lambda} to elements of \code{X}, in row-major order.
//' @param lambda_mapping_Zt List of integer vectors mapping elements of
//'   \code{lambda} to non-zero elements of \code{Zt} assuming compressed
//'   sparse column format is used. If \code{lambda_mapping_Zt_covs} is of
//'   length zero, then each list element in \code{lambda_mapping_Zt} should be
//'   of length one, and it will then be multiplied by the corresponding element
//'   of \code{Zt}.
//' @param lambda_mapping_Zt_covs List of double precision vector. Must either
//'   be of length zero, or the same length as \code{lambda_mapping_Zt_covs}.
//'   Each list element contains potential covariates that the elements of
//'   \code{lambda_mapping_Zt} should be multiplied with. If the list is of
//'   length 0, all elements of \code{lambda_mapping_Zt} are implicitly
//'   multiplied by 1.
//' @param weights Double precision vector of weights, used in heteroscedastic
//'   models.
//' @param weights_mapping Integer vector mapping the elements of \code{weights}
//'   to the rows of \code{X}.
//' @param family Vector of strings defining the family or families. Each
//'   vector element must currently be one of \code{"gaussian"},
//'   \code{"binomial"}, or \code{"poisson"}.
//' @param family_mapping Integer vector mapping elements of \code{family} to
//'   the rows of \code{X}.
//' @param k Double precision vector with pre-computed constant term in the
//'   log-likelihood for each element in \code{family}.
//' @param maxit_conditional_modes Integer specifying the maximum number of
//'   iteration in penalized iteratively reweighted least squares algorithm
//'   used to find the conditional modes of the random effects.
//' @param lossvalue_tol Double precision scalar specifying the absolute
//'   convergence criterion for the penalized iteratively reweighted least
//'   squares algorithm used to find the conditional modes of the random
//'   effects.
//' @param reduced_hessian Boolean specifying whether the Hessian matrix of
//'   second derivatives should be computed only with respect to \code{beta}
//'   and \code{lambda}, in that order. This may be useful for getting a very
//'   rough estimate of the inverse covariance matrix, when the full Hessian is
//'   not positive definite.
//'
//' @return An \code{Rcpp::List} with the following elements. The element
//' \code{logLik} will always be there, while the other will be there or not
//' depending on the template type \code{T}.
//'   * \code{logLik} Laplace approximate marginal log-likelihood at the
//'     parameter values specified.
//'   * \code{g} If \code{T} is \code{autodiff::dual1st} or
//'     \code{autodiff::dual2nd}, the gradient is provided in this element as
//'     a double precision vector.
//'   * \code{H} If \code{T} is \code{autodiff::dual2nd}, the Hessian matrix
//'     is provided in this element as a double precision matrix.
//'   * \code{u} If \code{T} is \code{autodiff::dual2nd}, the conditional
//'     modes of the standardized random effects are provided as a double
//'     precision vector in this element.
//'   * \code{V} If \code{T} is \code{autodiff::dual2nd}, the diagonal matrix
//'     \eqn{V} with \eqn{b''(\nu_{i}) / \phi_{g(i)}} on the diagonal is
//'     included in this element. See the paragraph below equation (13) in
//'     \insertCite{sorensenLongitudinalModelingAgeDependent2023}{galamm} for
//'     details.
//'   * \code{phi} If \code{T} is \code{autodiff::dual2nd}, double precision
//'     scalar containing the dispersion parameter of the model.
//' @noRd
template <typename T>
Rcpp::List wrapper(
    const Eigen::VectorXd& y,
    const Eigen::VectorXd& trials,
    const Eigen::MatrixXd& X,
    const Eigen::SparseMatrix<double>& Zt,
    const Eigen::SparseMatrix<double>& Lambdat,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& theta,
    const std::vector<int>& theta_mapping,
    const Eigen::VectorXd& u_init,
    const Eigen::VectorXd& lambda,
    const Eigen::VectorXi& lambda_mapping_X,
    const Rcpp::ListOf<Rcpp::IntegerVector>& lambda_mapping_Zt,
    const Rcpp::ListOf<Rcpp::NumericVector>& lambda_mapping_Zt_covs,
    const Eigen::VectorXd& weights,
    const std::vector<int>& weights_mapping,
    const std::vector<std::string>& family,
    const Eigen::VectorXi& family_mapping,
    const Eigen::VectorXd& k,
    const int& maxit_conditional_modes,
    const double& lossvalue_tol,
    const bool reduced_hessian = false){


  data<T> datlist{y, trials, X, Zt};

  parameters<T> parlist{
      theta, beta, lambda, u_init, theta_mapping,
      lambda_mapping_X,
      lambda_mapping_Zt,
      lambda_mapping_Zt_covs,
      Lambdat,
      weights, weights_mapping,
      family_mapping, maxit_conditional_modes, lossvalue_tol,
      static_cast<int>(y.size())};

  auto mod = std::vector<std::unique_ptr<Model<T>>>{};

  for(size_t i{}; i < family.size(); i++){
    if(family[i] == "gaussian") {
      mod.push_back(std::make_unique<Gaussian<T>>());
    } else if(family[i] == "binomial"){
      mod.push_back(std::make_unique<Binomial<T>>(k(i)));
    } else if(family[i] == "poisson"){
      mod.push_back(std::make_unique<Poisson<T>>(k(i)));
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  auto fx = [=, &mod, &datlist](parameters<T>& parlist){
    auto lll = logLik(parlist, datlist, mod);
    return lll.logLikValue;
  };
  auto gx = [=, &mod, &datlist](parameters<T>& parlist){
    return logLik(parlist, datlist, mod);;
  };

  return create_result(fx, gx, parlist, reduced_hessian);

}

//' @title Evaluate the marginal likelihood
//'
//' @description
//' This function evaluate the Laplace approximate marginal likelihood of a
//' generalized additive latent and mixed model at a given set of parameters.
//' The code uses elements generated by \code{lme4::glFormula}, and the
//' documentation of \code{lme4} should be consulted for further details.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param y Double precision vector of response values.
//' @param trials Double precision vector with number of trials. When trials
//' are not applicable, e.g., with Gaussian or Poisson responses, this should
//' be a vector of ones.
//' @param X Fixed effect model matrix.
//' @param Zt Transpose of random effect model matrix.
//' @param Lambdat Lower Cholesky factor of random effect covariance matrix.
//' @param beta Double precision vector of fixed effects.
//' @param theta Double precision vector with the unique elements of
//'   \code{Lambdat}.
//' @param theta_mapping Integer vector mapping elements of \code{theta} to the
//'   positions in \code{Lambdat}.
//' @param u_init Double precision vector with initial values of random
//'   effects. These random effects should be standardized.
//' @param lambda Double precision vector of factor loadings.
//' @param lambda_mapping_X Integer vector mapping elements of
//'   \code{lambda} to elements of \code{X}, in row-major order.
//' @param lambda_mapping_Zt List of integer vectors mapping elements of
//'   \code{lambda} to non-zero elements of \code{Zt} assuming compressed
//'   sparse column format is used. If \code{lambda_mapping_Zt_covs} is of
//'   length zero, then each list element in \code{lambda_mapping_Zt} should be
//'   of length one, and it will then be multiplied by the corresponding element
//'   of \code{Zt}.
//' @param lambda_mapping_Zt_covs List of double precision vector. Must either
//'   be of length zero, or the same length as \code{lambda_mapping_Zt_covs}.
//'   Each list element contains potential covariates that the elements of
//'   \code{lambda_mapping_Zt} should be multiplied with. If the list is of
//'   length 0, all elements of \code{lambda_mapping_Zt} are implicitly
//'   multiplied by 1.
//' @param weights Double precision vector of weights, used in heteroscedastic
//'   models.
//' @param weights_mapping Integer vector mapping the elements of \code{weights}
//'   to the rows of \code{X}.
//' @param family Vector of strings defining the family or families. Each
//'   vector element must currently be one of \code{"gaussian"},
//'   \code{"binomial"}, or \code{"poisson"}.
//' @param family_mapping Integer vector mapping elements of \code{family} to
//'   the rows of \code{X}.
//' @param k Double precision vector with pre-computed constant term in the
//'   log-likelihood for each element in \code{family}.
//' @param maxit_conditional_modes Integer specifying the maximum number of
//'   iteration in penalized iteratively reweighted least squares algorithm
//'   used to find the conditional modes of the random effects.
//' @param lossvalue_tol Double precision scalar specifying the absolute
//'   convergence criterion for the penalized iteratively reweighted least
//'   squares algorithm used to find the conditional modes of the random
//'   effects.
//' @param gradient Boolean specifying whether to compute the gradient of the
//'   log-likelhood with respect to all elements of \code{theta}, \code{beta},
//'   \code{lambda}, and \code{weights}, in that order. If
//'   \code{gradient = TRUE}, and \code{hessian = FALSE}, forward mode
//'   automatic differentiation with first-order dual numbers are used. If also
//'   \code{hessian = TRUE}, then second-order dual numbers are used instead.
//' @param hessian Boolean specifying whether to compute the Hessian matrix of
//'   second derivatives of the log-likelihood with respect to all elements of
//'   \code{theta}, \code{beta}, \code{lambda}, and \code{weights}, in that
//'   order. If \code{hessian = TRUE}, forward mode automatic differentiation
//'   with second-order dual numbers are used.
//' @param reduced_hessian Boolean specifying whether the Hessian matrix of
//'   second derivatives should be computed only with respect to \code{beta}
//'   and \code{lambda}, in that order. This may be useful for getting a very
//'   rough estimate of the inverse covariance matrix, when the full Hessian is
//'   not positive definite.
//'
//' @return An \code{Rcpp::List}, which will be converted to a \code{list} in
//'   \code{R}, the following elements. The element \code{logLik} will always
//'   be there, while the other will be there or not depending on arguments
//'   \code{gradient} and \code{hessian}.
//'   * \code{logLik} Laplace approximate marginal log-likelihood at the
//'     parameter values specified.
//'   * \code{g} If \code{gradient = TRUE} or \code{hessian = TRUE}, the
//'     gradient is provided in this element as a double precision vector.
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
//'
//' @details
//' For many models, not all parameters exists. For example, without
//' heteroscedastic residuals, the weights don't exist, and other models don't
//' have factor loadings. For these cases, the corresponding argument (to
//' \code{weights} or \code{lambda}) should be a correctly typed vector of
//' length zero.
//' @noRd
// [[Rcpp::export]]
Rcpp::List marginal_likelihood(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::VectorXd> trials,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXd> theta,
    const std::vector<int> theta_mapping,
    const Eigen::Map<Eigen::VectorXd> u_init,
    const Eigen::Map<Eigen::VectorXd> lambda,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_X,
    Rcpp::ListOf<Rcpp::IntegerVector> lambda_mapping_Zt,
    Rcpp::ListOf<Rcpp::NumericVector> lambda_mapping_Zt_covs,
    const Eigen::Map<Eigen::VectorXd> weights,
    const std::vector<int> weights_mapping,
    const std::vector<std::string> family,
    const Eigen::Map<Eigen::VectorXi> family_mapping,
    const Eigen::Map<Eigen::VectorXd> k,
    const int maxit_conditional_modes,
    const double lossvalue_tol,
    const bool gradient,
    const bool hessian,
    bool reduced_hessian = false
){

  if(hessian){
    return wrapper<dual2nd>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X, lambda_mapping_Zt, lambda_mapping_Zt_covs,
      weights, weights_mapping, family, family_mapping, k,
      maxit_conditional_modes, lossvalue_tol, reduced_hessian);
  } else if(gradient){
    return wrapper<dual1st>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X,lambda_mapping_Zt, lambda_mapping_Zt_covs,
      weights, weights_mapping, family, family_mapping, k,
      maxit_conditional_modes, lossvalue_tol);
  } else {
    return wrapper<double>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X, lambda_mapping_Zt, lambda_mapping_Zt_covs,
      weights, weights_mapping, family, family_mapping, k,
      maxit_conditional_modes, lossvalue_tol);
  }

}
