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

  // Function to compute regression coefficients in inner loop
  void get_conditional_modes(Eigen::SimplicialLDLT<SpMdual>& solver){
    Vdual delta_u{};
    solver.factorize(get_inner_hessian());
    T deviance_prev = -2 * (exponent_g() - solver.vectorD().array().log().sum() / 2);
    T deviance_new;

    for(int i{}; i < maxit_conditional_modes; i++){
      delta_u = solver.solve((get_Lambdat() * get_Zt() * (y - meanfun()) - u));
      if(delta_u.squaredNorm() < 1e-10) break;

      double step = 1;
      for(int j{}; j < 10; j++){
        update_u(delta_u, step);
        solver.factorize(get_inner_hessian());
        deviance_new = -2 * (exponent_g() - solver.vectorD().array().log().sum() / 2);
        if(deviance_new < deviance_prev){
          break;
        }
        update_u(delta_u, -step);
        step /= 2;
      }

      Rcpp::checkUserInterrupt();
      deviance_prev = deviance_new;
    }
  };

  // Exponent in the Laplace approximation
  T exponent_g(){
    return (y.dot(get_linpred()) - cumulant()) / get_phi() + constfun() -
      u.squaredNorm() / 2 / get_phi();
  };

  // Hessian matrix used in penalized iteratively reweighted least squares
  void update_inner_hessian(){
    inner_hessian = (1 / get_phi()) *
      get_Lambdat() * get_Zt() * get_V() *
      get_Zt().transpose() * get_Lambdat().transpose();
  };
  SpMdual& get_inner_hessian(){
    update_inner_hessian();
    return inner_hessian;
  };
  SpMdual inner_hessian;

  // Lower Cholesky factor of scaled covariance matrix
  void update_Lambdat(){
    int lind_counter{};
    for (int k{}; k < Lambdat.outerSize(); ++k){
      for (typename SpMdual::InnerIterator
             it(Lambdat, k); it; ++it)
      {
        it.valueRef() = theta(theta_mapping(lind_counter));
        lind_counter++;
      }
    }
  };
  SpMdual& get_Lambdat(){
    update_Lambdat();
    return Lambdat;
  };

  // Scale parameter
  virtual void update_phi() = 0;
  T phi{1};
  T& get_phi(){
    update_phi();
    return phi;
  };

  // Diagonal variance matrix, common parts
  virtual void update_V() = 0;
  Ddual V;
  Ddual& get_V(){
    update_V();
    return V;
  };

  // Linear predictor
  void update_linpred(){
    linpred = get_X() * beta +
      get_Zt().transpose() * get_Lambdat().transpose() * u;
  };
  Vdual& get_linpred(){
    update_linpred();
    return linpred;
  };
  Vdual linpred;

  // GLM functions defined in derived classes
  virtual T cumulant() = 0;
  virtual T constfun() = 0;
  virtual Vdual meanfun() = 0;

  // Regression coefficients
  void update_u(const Vdual& delta_u, double alpha_bar){
    u += alpha_bar * delta_u;
  };

  void update_X(){
    if(lambda_mapping_X.size() == 0) return;
    X = X_init;
    if(lambda_mapping_X.size() == 0) return;
    for(int i = 0; i < X.size(); i++){
      int newind = lambda_mapping_X(i);
      if(newind != -1){
        *(X.data() + i) *= lambda(newind);
      }
    }
  };
  Mdual& get_X(){
    update_X();
    return X;
  };

  void update_Zt(){
    if(lambda_mapping_Zt.size() == 0) return;
    Zt = Zt_init;
    int counter{};
    for(int k{}; k < Zt.outerSize(); ++k){
      for(typename Model<T>::SpMdual::InnerIterator it(Zt, k); it; ++it){
        int newind = lambda_mapping_Zt(counter);
        if(newind != -1){
          it.valueRef() = lambda(newind) * it.value();
        }
        counter++;
      }
    }
  };
  SpMdual& get_Zt(){
    update_Zt();
    return Zt;
  };
  Vdual u;

  int n;
  int p;
  int q;

};

template <typename T>
struct Binomial : Model<T> {

  // // Inherit base class constructor
  using Model<T>::Model;

  T cumulant() override {
    return ((1 + Model<T>::get_linpred().array().exp()).log() *
            Model<T>::trials.array()).sum();
  };
  T constfun() override {
    return (lgamma(Model<T>::trials.array() + 1) -
            lgamma(Model<T>::y.array() + 1) -
            lgamma(Model<T>::trials.array() -
            Model<T>::y.array() + 1)).sum();
  };

  typename Model<T>::Vdual meanfun() override {
    return Model<T>::get_linpred().array().exp() /
      (1 + Model<T>::get_linpred().array().exp()) *
      Model<T>::trials.array();
  };

  // Binomial variance function
  // meanfun() includes the number of trials, and hence returns the number of
  // expected successes, and not the expected proportion of successes.
  // Thus, mu(eta) = N * exp(eta) / (1 + exp(eta)) and
  // m'(eta) = d''(eta) = mu * (N - mu) / N.
  void update_V() override {
    Model<T>::V.diagonal().array() = meanfun().array() /
      Model<T>::trials.array() *
      (Model<T>::trials.array() - meanfun().array());
  };
  void update_phi() override{};

};

template <typename T>
struct Gaussian : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant() override {
    return Model<T>::get_linpred().squaredNorm() / 2;
  };
  T constfun() override {
    return -.5 * (Model<T>::y.squaredNorm() / Model<T>::get_phi() +
                  Model<T>::n * log(2 * M_PI * Model<T>::get_phi()));
  };
  typename Model<T>::Vdual meanfun() override {
    return Model<T>::get_linpred();
  };

  // How to update diagonal variance matrix is model dependent
  void update_V() override {
    Model<T>::V.diagonal().array() = Model<T>::get_phi();
  };
  void update_phi() override {
    Model<T>::phi = ((Model<T>::y - Model<T>::get_linpred()).squaredNorm() +
      Model<T>::u.squaredNorm()) / Model<T>::n;
  };

};

template <typename T>
struct Poisson : Model<T> {

  // Inherit base class constructor
  using Model<T>::Model;

  T cumulant() override {
    return Model<T>::get_linpred().array().exp().sum();
  };
  T constfun() override {
    return -(Model<T>::y.array() + 1).lgamma().sum();
  };
  typename Model<T>::Vdual meanfun() override {
    return Model<T>::get_linpred().array().exp();
  };

  // How to update diagonal variance matrix is model dependent
  void update_V() override {
    Model<T>::V.diagonal().array() = meanfun().array();
  };
  void update_phi() override{};

};


#endif
