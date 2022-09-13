#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

template <typename T> struct Model{
  typedef Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> Mdual;
  typedef Eigen::SparseMatrix<T> SpMdual;
  typedef Eigen::Matrix<T, Eigen::Dynamic, 1> Vdual;
  typedef Eigen::DiagonalMatrix<T, Eigen::Dynamic> Ddual;

  // Constructor, converting objects to autodiff
  Model(
    const Eigen::VectorXd& y0,
    const Eigen::VectorXd& trials0
  ) : y { y0 },
    trials { trials0 }
  {
      n = y.size();
  };

  Eigen::VectorXd y;
  Eigen::VectorXd trials;

  // Scale parameter
  virtual void update_phi(const Vdual& linpred, const Vdual& u) = 0;
  T phi{1};
  T& get_phi(const Vdual& linpred, const Vdual& u){
    update_phi(linpred, u);
    return phi;
  };

  // Diagonal variance matrix, common parts
  virtual void update_V(const Vdual& linpred, const Vdual& u) = 0;
  Ddual V;
  virtual Ddual& get_V(const Vdual& linpred, const Vdual& u){
    update_V(linpred, u);
    return V;
  };

  // GLM functions defined in derived classes
  virtual T cumulant(const Vdual& linpred) = 0;
  virtual T constfun(const Vdual& linpred, const Vdual& u) = 0;
  virtual Vdual meanfun(const Vdual& linpred) = 0;
  long n;

};

template <typename T>
struct Binomial : Model<T> {

  // // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return ((1 + linpred.array().exp()).log() *
            Model<T>::trials.array()).sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& u) override {
    return (lgamma(Model<T>::trials.array() + 1) -
            lgamma(Model<T>::y.array() + 1) -
            lgamma(Model<T>::trials.array() -
            Model<T>::y.array() + 1)).sum();
  };

  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred) override {
    return linpred.array().exp() / (1 + linpred.array().exp()) * Model<T>::trials.array();
  };

  // Binomial variance function
  // meanfun() includes the number of trials, and hence returns the number of
  // expected successes, and not the expected proportion of successes.
  // Thus, mu(eta) = N * exp(eta) / (1 + exp(eta)) and
  // m'(eta) = d''(eta) = mu * (N - mu) / N.
  void update_V(const typename Model<T>::Vdual& linpred,
                const typename Model<T>::Vdual& u) override {
    Model<T>::V.diagonal().array() = meanfun(linpred).array() /
      Model<T>::trials.array() *
      (Model<T>::trials.array() - meanfun(linpred).array());
  };
  void update_phi(const typename Model<T>::Vdual& linpred,
                  const typename Model<T>::Vdual& u) override{};

};

template <typename T>
struct Gaussian : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return linpred.squaredNorm() / 2;
  };
  T constfun(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& u) override {
    return -.5 * (Model<T>::y.squaredNorm() / Model<T>::get_phi(linpred, u) +
                  Model<T>::n * log(2 * M_PI * Model<T>::get_phi(linpred, u)));
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  void update_V(const typename Model<T>::Vdual& linpred,
                const typename Model<T>::Vdual& u) override {
    Model<T>::V.diagonal().array() = Model<T>::get_phi(linpred, u);
  };
  void update_phi(const typename Model<T>::Vdual& linpred,
                  const typename Model<T>::Vdual& u) override {
    Model<T>::phi = ((Model<T>::y - linpred).squaredNorm() +
      u.squaredNorm()) / Model<T>::n;
  };

};

template <typename T>
struct Poisson : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return linpred.array().exp().sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred,
             const typename Model<T>::Vdual& u) override {
    return -(Model<T>::y.array() + 1).lgamma().sum();
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  void update_V(const typename Model<T>::Vdual& linpred,
                const typename Model<T>::Vdual& u) override {
    Model<T>::V.diagonal().array() = meanfun(linpred).array();
  };
  void update_phi(const typename Model<T>::Vdual& linpred,
                  const typename Model<T>::Vdual& u) override{};

};


#endif
