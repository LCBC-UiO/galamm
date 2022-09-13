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
    const Eigen::VectorXd& trials0,
    const Eigen::MatrixXd& X0,
    const Eigen::MappedSparseMatrix<double>& Zt0,
    const Eigen::MappedSparseMatrix<double>& Lambdat0,
    const Eigen::VectorXd& beta0,
    const Eigen::VectorXd& theta0,
    const Eigen::VectorXi& theta_mapping0,
    const Eigen::VectorXd& lambda0,
    const Eigen::VectorXi& lambda_mapping_X0,
    const Eigen::VectorXi& lambda_mapping_Zt0,
    const int maxit_conditional_modes0
  ) : y { y0 },
    trials { trials0 },
    X_init { X0.cast<T>() },
    X { X0.cast<T>() },
    Zt_init { Zt0.cast<T>() },
    Zt { Zt0.cast<T>() },
    Lambdat { Lambdat0.cast<T>() },
    beta { beta0.cast<T>() },
    theta { theta0.cast<T>() },
    theta_mapping { theta_mapping0 },
    lambda { lambda0.cast<T>() },
    lambda_mapping_X { lambda_mapping_X0 },
    lambda_mapping_Zt { lambda_mapping_Zt0 },
    maxit_conditional_modes { maxit_conditional_modes0 }
  {
      n = X.rows();
      p = X.cols();
      q = Zt.rows();
      u = Vdual::Zero(q);
      V = Ddual(n);
  };

  Eigen::VectorXd y;
  Eigen::VectorXd trials;
  Mdual X_init;
  Mdual X;
  SpMdual Zt_init;
  SpMdual Zt;
  SpMdual Lambdat;
  Vdual beta;
  Vdual theta;
  const Eigen::VectorXi theta_mapping;
  Vdual lambda;
  const Eigen::VectorXi lambda_mapping_X;
  const Eigen::VectorXi lambda_mapping_Zt;
  int maxit_conditional_modes;



  // Scale parameter
  virtual void update_phi(const Vdual& linpred) = 0;
  T phi{1};
  T& get_phi(const Vdual& linpred){
    update_phi(linpred);
    return phi;
  };

  // Diagonal variance matrix, common parts
  virtual void update_V(const Vdual& linpred) = 0;
  Ddual V;
  virtual Ddual& get_V(const Vdual& linpred){
    update_V(linpred);
    return V;
  };

  // GLM functions defined in derived classes
  virtual T cumulant(const Vdual& linpred) = 0;
  virtual T constfun(const Vdual& linpred) = 0;
  virtual Vdual meanfun(const Vdual& linpred) = 0;



  Vdual u;

  int n;
  int p;
  int q;

};

template <typename T>
struct Binomial : Model<T> {

  // // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return ((1 + linpred.array().exp()).log() *
            Model<T>::trials.array()).sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred) override {
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
  void update_V(const typename Model<T>::Vdual& linpred) override {
    Model<T>::V.diagonal().array() = meanfun(linpred).array() /
      Model<T>::trials.array() *
      (Model<T>::trials.array() - meanfun(linpred).array());
  };
  void update_phi(const typename Model<T>::Vdual& linpred) override{};

};

template <typename T>
struct Gaussian : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return linpred.squaredNorm() / 2;
  };
  T constfun(const typename Model<T>::Vdual& linpred) override {
    return -.5 * (Model<T>::y.squaredNorm() / Model<T>::get_phi(linpred) +
                  Model<T>::n * log(2 * M_PI * Model<T>::get_phi(linpred)));
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred) override {
    return linpred;
  };

  // How to update diagonal variance matrix is model dependent
  void update_V(const typename Model<T>::Vdual& linpred) override {
    Model<T>::V.diagonal().array() = Model<T>::get_phi(linpred);
  };
  void update_phi(const typename Model<T>::Vdual& linpred) override {
    Model<T>::phi = ((Model<T>::y - linpred).squaredNorm() +
      Model<T>::u.squaredNorm()) / Model<T>::n;
  };

};

template <typename T>
struct Poisson : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant(const typename Model<T>::Vdual& linpred) override {
    return linpred.array().exp().sum();
  };
  T constfun(const typename Model<T>::Vdual& linpred) override {
    return -(Model<T>::y.array() + 1).lgamma().sum();
  };
  typename Model<T>::Vdual meanfun(const typename Model<T>::Vdual& linpred) override {
    return linpred.array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  void update_V(const typename Model<T>::Vdual& linpred) override {
    Model<T>::V.diagonal().array() = meanfun(linpred).array();
  };
  void update_phi(const typename Model<T>::Vdual& linpred) override{};

};


#endif
