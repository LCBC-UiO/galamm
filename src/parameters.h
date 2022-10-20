#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

template <typename T>
struct parameters{
  parameters(
    const Eigen::VectorXd& theta,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& lambda,
    const Eigen::VectorXd& u,
    const std::vector<int>& theta_mapping,
    const Rcpp::ListOf<Rcpp::IntegerVector>& lambda_mapping_X0,
    const Rcpp::ListOf<Rcpp::IntegerVector>& lambda_mapping_Zt0,
    const Eigen::SparseMatrix<double>& Lambdat,
    const Eigen::VectorXd& weights,
    const std::vector<int>& weights_mapping,
    const Eigen::VectorXi& family_mapping,
    const int& maxit_conditional_modes,
    const double& epsilon_u,
    const int& n
  ) :
  theta { theta.cast<T>() }, beta { beta.cast<T>() }, lambda { lambda.cast<T>() },
  u { u.cast<T>() }, theta_mapping { theta_mapping },
  Lambdat { Lambdat.cast<T>() },
  weights { weights.cast<T>() },
  weights_mapping { weights_mapping },
  family_mapping { family_mapping },
  maxit_conditional_modes { maxit_conditional_modes },
  epsilon_u { epsilon_u }, n { n }
  {
    for(int i{}; i < lambda_mapping_X0.size(); i++){
      lambda_mapping_X.push_back(Rcpp::as<std::vector<int>>(lambda_mapping_X0[i]));
    }
    for(int i{}; i < lambda_mapping_Zt0.size(); i++){
      lambda_mapping_Zt.push_back(Rcpp::as<std::vector<int>>(lambda_mapping_Zt0[i]));
    }
    WSqrt.diagonal() = Vdual<T>::Constant(n, 1);
  }


  Vdual<T> theta;
  Vdual<T> beta;
  Vdual<T> lambda;
  Vdual<T> u;
  std::vector<int> theta_mapping;
  std::vector<std::vector<int>> lambda_mapping_X;
  std::vector<std::vector<int>> lambda_mapping_Zt;
  Eigen::SparseMatrix<T> Lambdat;
  Vdual<T> weights;
  std::vector<int> weights_mapping;
  Eigen::VectorXi family_mapping;
  Ddual<T> WSqrt;
  int maxit_conditional_modes;
  double epsilon_u;
  double deviance_tol{.01};
  int n;
};

template <typename T>
struct logLikObject {
  T logLikValue;
  Vdual<T> V;
  Vdual<T> u;
  Vdual<T> phi;
};

#endif

