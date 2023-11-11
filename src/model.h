#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include <unsupported/Eigen/SpecialFunctions>

template <typename T>
using Vdual = Eigen::Matrix<T, Eigen::Dynamic, 1>;
template <typename T>
using Ddual = Eigen::DiagonalMatrix<T, Eigen::Dynamic>;
template <typename T>
using SpMdual = Eigen::SparseMatrix<T>;
template <typename T>
using Mdual = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
template <typename T>
using ldlt = Eigen::SimplicialLDLT<Eigen::SparseMatrix<T> >;

//' @name Model
//' @title Encapsulates a model
//' @description The members of this virtual class are different generalized
//'   linear models.
//' @field new Constructor.
//' @field cumulant Cumulant function of the model family.
//' @field constfun Constant term in log-likelihood of the model family.
//' @field meanfun Expected value of the response at the given parameters.
//' @field get_V Extract diagonal matrix whose elements are
//'   \eqn{\eqn{b''(\nu_{i}) / \phi_{g(i)}}}.
//' @field get_phi Extract dispersion parameter.
//' @srrstats {G1.4a} Internal class documented.
//' @noRd
template <typename T>
struct Model {
  Model() {};
  virtual T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
                     const Ddual<T>& WSqrt) = 0;
  virtual T constfun(const Vdual<T>& y, const T& phi, const Ddual<T>& WSqrt) = 0;
  virtual Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) = 0;
  virtual Vdual<T> get_V(const Vdual<T>& linpred, const Vdual<T>& trials,
                         const Ddual<T>& WSqrt) = 0;
  virtual T get_phi(const Vdual<T>& linpred, const Vdual<T>& u,
                    const Vdual<T>& y, const Ddual<T>& WSqrt, int n) = 0;
};

//' @name Binomial
//' @title Member functions for binomial responses
//' @field new Constructor, which takes the constant term \code{k} as argument.
//' @field cumulant Cumulant function for logistic regression.
//' @field constfun Constant term in log-likelihood for logistic regression.
//' @field meanfun Expected value of the response at the given parameters.
//'   Includes the number of trials, so it does NOT return a proportion.
//' @field get_V Extract diagonal matrix whose elements are
//'   \eqn{\eqn{b''(\nu_{i}) / \phi_{g(i)}}}. Since \eqn{\phi = 1}, what we
//'   extract here is the binomial variance function. A comment to the
//'   implementation: since \code{meanfun} returns the number of expected
//'   successes, \eqn{\mu(\eta) = N \exp(\eta) / (1 + \exp(\eta))}, and hence
//'   \eqn{\mu'(\eta) = d''(\eta) = \mu(\eta) * (N - \mu(\eta)) / N}.
//' @field get_phi Extract dispersion parameter, which in this case equals 1.
//' @srrstats {G1.4a} Internal class documented.
//' @noRd
template <typename T>
struct Binomial : Model<T> {
  Binomial(double k) : k { static_cast<T>(k) } {}
  T k;
  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return ((1 + linpred.array().exp()).log() * trials.array()).sum();
  };
  T constfun(const Vdual<T>& y, const T& phi, const Ddual<T>& WSqrt) override {
    return k;
  };

  Vdual<T> meanfun(const Vdual<T>& linpred,
                                   const Vdual<T>& trials) override {
    return linpred.array().exp() / (1 + linpred.array().exp()) * trials.array();
  };

  Vdual<T> get_V(
      const Vdual<T>& linpred, const Vdual<T>& trials,
      const Ddual<T>& WSqrt) override {

        return meanfun(linpred, trials).array() / trials.array() *
            (trials.array() - meanfun(linpred, trials).array());
  };

  T get_phi(const Vdual<T>& linpred, const Vdual<T>& u,
            const Vdual<T>& y, const Ddual<T>& WSqrt, int n) override {
              return 1;
  };
};

//' @name Gaussian
//' @title Member functions for Gaussian responses
//' @field new Constructor is inherited from the \code{Model} class, and does
//'   nothing.
//' @field cumulant Cumulant function for linear regression.
//' @field constfun Constant term in log-likelihood for linear regression.
//' @field meanfun Expected value of the response at the given parameters.
//' @field get_V Extract diagonal matrix whose elements are
//'   \eqn{\eqn{b''(\nu_{i}) / \phi_{g(i)}}}.
//' @field get_phi Extract dispersion parameter.
//' @srrstats {G1.4a} Internal class documented.
//' @noRd
template <typename T>
struct Gaussian : Model<T> {

  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return (WSqrt * linpred).squaredNorm() / 2;
  };
  T constfun(const Vdual<T>& y, const T& phi, const Ddual<T>& WSqrt) override {
    int n = y.size();
    return -.5 * ((WSqrt * y).squaredNorm() / phi + n * log(2 * M_PI * phi))
      + WSqrt.diagonal().array().log().sum();
  };
  Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) override {
    return linpred;
  };

  Vdual<T> get_V(const Vdual<T>& linpred, const Vdual<T>& trials,
                 const Ddual<T>& WSqrt) override {
        return WSqrt.diagonal().array().pow(2);

  };

  T get_phi(
      const Vdual<T>& linpred, const Vdual<T>& u, const Vdual<T>& y,
      const Ddual<T>& WSqrt, int n) override {
        return ((WSqrt * (y - linpred)).squaredNorm() + u.squaredNorm()) / n;
  };

};

//' @name Poisson
//' @title Member functions for Poisson responses
//' @field new Constructor, which takes the constant term \code{k} as argument.
//' @field cumulant Cumulant function for Poisson regression.
//' @field constfun Constant term in log-likelihood for Poisson regression.
//' @field meanfun Expected value of the response at the given parameters.
//' @field get_V Extract diagonal matrix whose elements are
//'   \eqn{\eqn{b''(\nu_{i}) / \phi_{g(i)}}}.
//' @field get_phi Extract dispersion parameter, which in this case equals 1.
//' @srrstats {G1.4a} Internal class documented.
//' @noRd
template <typename T>
struct Poisson : Model<T> {
  Poisson(double k) : k {static_cast<T>(k)} {}
  T k;
  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return linpred.array().exp().sum();
  };
  T constfun(const Vdual<T>& y, const T& phi, const Ddual<T>& WSqrt) override {
    return k;
  };
  Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) override {
    return linpred.array().exp();
  };

  Vdual<T> get_V(
      const Vdual<T>& linpred, const Vdual<T>& trials, const Ddual<T>& WSqrt) override {
        return meanfun(linpred, trials).array();
  };

  T get_phi(const Vdual<T>& linpred, const Vdual<T>& u, const Vdual<T>& y,
            const Ddual<T>& WSqrt, int n) override {
    return 1;
  };

};


#endif

