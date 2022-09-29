#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

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

template <typename T>
struct Model {
  virtual T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
                     const Ddual<T>& WSqrt) = 0;
  virtual T constfun(const Vdual<T>& y, const T& phi, const T k,
                     const Ddual<T>& WSqrt) = 0;
  virtual Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) = 0;
  virtual Vdual<T> get_V(const Vdual<T>& linpred, const Vdual<T>& trials,
                         const Ddual<T>& WSqrt) = 0;
  virtual T get_phi(const Vdual<T>& linpred, const Vdual<T>& u,
                    const Vdual<T>& y, const Ddual<T>& WSqrt) = 0;
};

template <typename T>
struct Binomial : Model<T> {
  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return ((1 + linpred.array().exp()).log() * trials.array()).sum();
  };
  T constfun(const Vdual<T>& y, const T& phi, const T k,
             const Ddual<T>& WSqrt) override {
    return k;
  };

  Vdual<T> meanfun(const Vdual<T>& linpred,
                                   const Vdual<T>& trials) override {
    return linpred.array().exp() / (1 + linpred.array().exp()) * trials.array();
  };

  // Binomial variance function
  // meanfun() includes the number of trials, and hence returns the number of
  // expected successes, and not the expected proportion of successes.
  // Thus, mu(eta) = N * exp(eta) / (1 + exp(eta)) and
  // m'(eta) = d''(eta) = mu * (N - mu) / N.
  Vdual<T> get_V(
      const Vdual<T>& linpred, const Vdual<T>& trials,
      const Ddual<T>& WSqrt) override {

        return meanfun(linpred, trials).array() / trials.array() *
            (trials.array() - meanfun(linpred, trials).array());
  };

  T get_phi(const Vdual<T>& linpred, const Vdual<T>& u,
            const Vdual<T>& y, const Ddual<T>& WSqrt) override {
              return 1;
  };
};

template <typename T>
struct Gaussian : Model<T> {

  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return (WSqrt * linpred).squaredNorm() / 2;
  };
  T constfun(const Vdual<T>& y, const T& phi, const T k,
             const Ddual<T>& WSqrt) override {
    int n = y.size();
    return -.5 * ((WSqrt * y).squaredNorm() / phi + n * log(2 * M_PI * phi))
      + WSqrt.diagonal().array().log().sum();
  };
  Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  Vdual<T> get_V(const Vdual<T>& linpred, const Vdual<T>& trials,
                 const Ddual<T>& WSqrt) override {
        return WSqrt.diagonal().array().pow(2);

  };

  T get_phi(
      const Vdual<T>& linpred, const Vdual<T>& u, const Vdual<T>& y,
      const Ddual<T>& WSqrt) override {
        int n = y.size();
        return ((WSqrt * (y - linpred)).squaredNorm() + u.squaredNorm()) / n;
  };

};

template <typename T>
struct Poisson : Model<T> {
  T cumulant(const Vdual<T>& linpred, const Vdual<T>& trials,
             const Ddual<T>& WSqrt) override {
    return linpred.array().exp().sum();
  };
  T constfun(const Vdual<T>& y, const T& phi, const T k,
             const Ddual<T>& WSqrt) override {
    return k;
  };
  Vdual<T> meanfun(const Vdual<T>& linpred, const Vdual<T>& trials) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  Vdual<T> get_V(
      const Vdual<T>& linpred, const Vdual<T>& trials, const Ddual<T>& WSqrt) override {
        return meanfun(linpred, trials).array();
  };

  T get_phi(const Vdual<T>& linpred, const Vdual<T>& u, const Vdual<T>& y,
            const Ddual<T>& WSqrt) override {
    return 1;
  };

};


#endif

