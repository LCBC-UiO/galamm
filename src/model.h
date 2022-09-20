#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>


template <typename T>
struct Model {
  Model(int maxit_conditional_modes, double epsilon_u) :
  maxit_conditional_modes { maxit_conditional_modes },
  epsilon_u { epsilon_u }{}

  int maxit_conditional_modes;
  double epsilon_u;

  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  virtual T cumulant(const T& linpred, const T& trials) = 0;
  virtual T constfun(const T& y, const T& phi, const T k) = 0;

  virtual Vdual meanfun(const Vdual& linpred, const Vdual& trials) = 0;

  virtual Vdual get_V(const Vdual& linpred, const Vdual& trials) = 0;

  virtual T get_phi_component(const Vdual& linpred, const Vdual& u, const Vdual& y) = 0;
};

template <typename T>
struct Binomial : Model<T> {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  using Model<T>::Model;

  T cumulant(const T& linpred, const T& trials) override {
    return log(1 + exp(linpred)) * trials;
  };
  T constfun(const T& y, const T& phi, const T k) override {
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
  Vdual get_V(
      const Vdual& linpred,
      const Vdual& trials) override {

        return meanfun(linpred, trials).array() / trials.array() *
            (trials.array() - meanfun(linpred, trials).array());
  };

  T get_phi_component(const Vdual& linpred,
            const Vdual& u,
            const Vdual& y) override {
              return 1;
  };
};

template <typename T>
struct Gaussian : Model<T> {
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  using Model<T>::Model;

  T cumulant(const T& linpred, const T& trials) override {
    return pow(linpred, 2) / 2;
  };
  T constfun(const T& y, const T& phi, const T k) override {
    return -.5 * (pow(y, 2) / phi + log(2 * M_PI * phi));
  };
  Vdual meanfun(const Vdual& linpred, const Vdual& trials) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  Vdual get_V(
      const Vdual& linpred, const Vdual& trials) override {
        int n = linpred.size();
        Vdual ret;
        ret.setConstant(n, 1);
        return ret;

  };

  T get_phi_component(
      const Vdual& linpred, const Vdual& u, const Vdual& y) override {
        int n = y.size();
        return ((y - linpred).squaredNorm() + u.squaredNorm()) / n;
  };

};

template <typename T>
struct Poisson : Model<T> {

  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  using Model<T>::Model;

  T cumulant(const T& linpred, const T& trials) override {
    return exp(linpred);
  };
  T constfun(const T& y, const T& phi, const T k) override {
    return k;
  };
  Vdual meanfun(const Vdual& linpred, const Vdual& trials) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  Vdual get_V(
      const Vdual& linpred,
      const Vdual& trials) override {
        return meanfun(linpred, trials).array();
  };

  T get_phi_component(const Vdual& linpred, const Vdual& u, const Vdual& y) override {
    return 1;
  };

};


#endif

