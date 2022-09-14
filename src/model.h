#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>


template <typename T>
struct Model {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  virtual T cumulant(const Vdual& linpred, const Vdual& trials) = 0;
  virtual T constfun(const Vdual& linpred, const Vdual& u, const Vdual& y,
                     const Vdual& trials, const T k) = 0;

  virtual Vdual meanfun(const Vdual& linpred, const Vdual& trials) = 0;

  virtual Ddual get_V(Ddual V, const Vdual& linpred, const Vdual& u,
              const Vdual& y, const Vdual& trials) = 0;

  virtual T get_phi(const Vdual& linpred, const Vdual& u, const Vdual& y) = 0;
};

template <typename T>
struct Binomial : Model<T> {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  T cumulant(const Vdual& linpred,
             const Vdual& trials) override {
    return ((1 + linpred.array().exp()).log() *
            trials.array()).sum();
  };
  T constfun(const Vdual& linpred,
             const Vdual& u,
             const Vdual& y,
             const Vdual& trials,
             const T k) override {
    return k;
  };

  Vdual meanfun(const Vdual& linpred,
                                   const Vdual& trials) override {
    return linpred.array().exp() / (1 + linpred.array().exp()) * trials.array();
  };

  // Binomial variance function
  // meanfun() includes the number of trials, and hence returns the number of
  // expected successes, and not the expected proportion of successes.
  // Thus, mu(eta) = N * exp(eta) / (1 + exp(eta)) and
  // m'(eta) = d''(eta) = mu * (N - mu) / N.
  Ddual get_V(
      Ddual V,
      const Vdual& linpred,
      const Vdual& u,
      const Vdual& y,
      const Vdual& trials) override {
        Ddual V0 = V;
        V0.diagonal().array() = meanfun(linpred, trials).array() /
          trials.array() *
            (trials.array() - meanfun(linpred, trials).array());
        return V0;
  };

  T get_phi(const Vdual& linpred,
            const Vdual& u,
            const Vdual& y) override {
              return 1;
  };
};

template <typename T>
struct Gaussian : Model<T> {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  T cumulant(const Vdual& linpred,
             const Vdual& trials) override {
    return linpred.squaredNorm() / 2;
  };
  T constfun(
      const Vdual& linpred,
      const Vdual& u,
      const Vdual& y,
      const Vdual& trials,
      const T k) override {
        int n = y.size();
    return -.5 * (y.squaredNorm() / get_phi(linpred, u, y) +
                  n * log(2 * M_PI * get_phi(linpred, u, y)));
  };
  Vdual meanfun(const Vdual& linpred, const Vdual& trials) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  Ddual get_V(
      Ddual V, const Vdual& linpred, const Vdual& u, const Vdual& y,
      const Vdual& trials) override {
        Ddual V0 = V;
        V0.diagonal().array() = get_phi(linpred, u, y);
        return V0;
  };

  T get_phi(
      const Vdual& linpred, const Vdual& u, const Vdual& y) override {
        int n = y.size();
        return ((y - linpred).squaredNorm() + u.squaredNorm()) / n;
  };

};

template <typename T>
struct Poisson : Model<T> {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  T cumulant(const Vdual& linpred,
             const Vdual& trials) override {
    return linpred.array().exp().sum();
  };
  T constfun(const Vdual& linpred,
             const Vdual& u,
             const Vdual& y,
             const Vdual& trials,
             const T k) override {
    return k;
  };
  Vdual meanfun(const Vdual& linpred, const Vdual& trials) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  Ddual get_V(
      Ddual V, const Vdual& linpred, const Vdual& u, const Vdual& y,
      const Vdual& trials) override {
        Ddual V0 = V;
        V0.diagonal().array() = meanfun(linpred, trials).array();
        return V0;
  };

  T get_phi(const Vdual& linpred,
            const Vdual& u,
            const Vdual& y) override {
    return 1;
  };

};


#endif

