#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

template <typename T> struct Model{
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  // Scale parameter
  virtual void update_phi(const Vdual& linpred, const Vdual& u,
                          const Vdual& y) = 0;
  T phi{1};
  T& get_phi(const Vdual& linpred, const Vdual& u, const Vdual& y){
    update_phi(linpred, u, y);
    return phi;
  };

  // Diagonal variance matrix, common parts
  virtual Ddual get_V(Ddual V, const Vdual& linpred, const Vdual& u,
                      const Vdual& y, const Vdual& trials) = 0;

  // GLM functions defined in derived classes
  virtual T cumulant(const Vdual& linpred, const Vdual& trials) = 0;
  virtual T constfun(const Vdual& linpred, const Vdual& u,
                     const Vdual& y, const Vdual& trials, T k) = 0;
  virtual Vdual meanfun(const Vdual& linpred, const Vdual& trials) = 0;
  long n;

};

template <typename T>
struct Binomial : Model<T> {

  T cumulant(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& trials) override {
    return ((1 + linpred.array().exp()).log() *
            trials.array()).sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& u,
             const typename Model<T>::Vdual& y,
             const typename Model<T>::Vdual& trials,
             const T k) override {
    return k;
  };

  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred,
                                   const typename Model<T>::Vdual& trials) override {
    return linpred.array().exp() / (1 + linpred.array().exp()) * trials.array();
  };

  // Binomial variance function
  // meanfun() includes the number of trials, and hence returns the number of
  // expected successes, and not the expected proportion of successes.
  // Thus, mu(eta) = N * exp(eta) / (1 + exp(eta)) and
  // m'(eta) = d''(eta) = mu * (N - mu) / N.
  typename Model<T>::Ddual get_V(
      typename Model<T>::Ddual V,
      const typename Model<T>::Vdual& linpred,
      const typename Model<T>::Vdual& u,
      const typename Model<T>::Vdual& y,
      const typename Model<T>::Vdual& trials) override {
        typename Model<T>::Ddual V0 = V;
        V0.diagonal().array() = meanfun(linpred, trials).array() /
          trials.array() *
            (trials.array() - meanfun(linpred, trials).array());
        return V0;
  };
  void update_phi(const typename Model<T>::Vdual& linpred,
                  const typename Model<T>::Vdual& u,
                  const typename Model<T>::Vdual& y) override{};

};

template <typename T>
struct Gaussian : Model<T> {

  T cumulant(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& trials) override {
    return linpred.squaredNorm() / 2;
  };
  T constfun(
      const typename Model<T>::Vdual& linpred,
      const typename Model<T>::Vdual& u,
      const typename Model<T>::Vdual& y,
      const typename Model<T>::Vdual& trials,
      const T k) override {
        int n = y.size();
    return -.5 * (y.squaredNorm() / Model<T>::get_phi(linpred, u, y) +
                  n * log(2 * M_PI * Model<T>::get_phi(linpred, u, y)));
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred,
                                   const typename Model<T>::Vdual& trials) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  typename Model<T>::Ddual get_V(
      typename Model<T>::Ddual V,
      const typename Model<T>::Vdual& linpred,
      const typename Model<T>::Vdual& u,
      const typename Model<T>::Vdual& y,
      const typename Model<T>::Vdual& trials) override {
        typename Model<T>::Ddual V0 = V;
        V0.diagonal().array() = Model<T>::get_phi(linpred, u, y);
        return V0;
  };
  void update_phi(
      const typename Model<T>::Vdual& linpred,
      const typename Model<T>::Vdual& u,
      const typename Model<T>::Vdual& y) override {
        int n = y.size();
    Model<T>::phi = ((y - linpred).squaredNorm() +
      u.squaredNorm()) / n;
  };

};

template <typename T>
struct Poisson : Model<T> {

  T cumulant(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& trials) override {
    return linpred.array().exp().sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& u,
             const typename Model<T>::Vdual& y,
             const typename Model<T>::Vdual& trials,
             const T k) override {
    return k;
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred,
                                   const typename Model<T>::Vdual& trials) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  typename Model<T>::Ddual get_V(
      typename Model<T>::Ddual V,
      const typename Model<T>::Vdual& linpred,
      const typename Model<T>::Vdual& u,
      const typename Model<T>::Vdual& y,
      const typename Model<T>::Vdual& trials) override {
        typename Model<T>::Ddual V0 = V;
        V0.diagonal().array() = meanfun(linpred, trials).array();
        return V0;
  };
  void update_phi(const typename Model<T>::Vdual& linpred,
                  const typename Model<T>::Vdual& u,
                  const typename Model<T>::Vdual& y) override{};

};


#endif
