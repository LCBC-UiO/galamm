#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

//' @name parameters
//' @title Structure for keeping track of parameters
//' @description See the documentation of other functions, e.g.,
//'   \code{marginal_likelihood}, for an explanation of what the different
//'   parameters represent.
//' @field new Constructor, which takes parameters as arguments, and then
//'   if necessary converts them to the template type \code{T}.
//' @noRd
template <typename T>
struct parameters{
  parameters(
    const Eigen::VectorXd& theta,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& lambda,
    const Eigen::VectorXd& u,
    const std::vector<int>& theta_mapping,
    const Eigen::VectorXi& lambda_mapping_X,
    const Rcpp::ListOf<Rcpp::IntegerVector>& lambda_mapping_Zt0,
    const Rcpp::ListOf<Rcpp::NumericVector>& lambda_mapping_Zt_covs0,
    const Eigen::SparseMatrix<double>& Lambdat,
    const Eigen::VectorXd& weights,
    const std::vector<int>& weights_mapping,
    const Eigen::VectorXi& family_mapping,
    const int& maxit_conditional_modes,
    const double& lossvalue_tol,
    const int& n
  ) :
  theta { theta.cast<T>() },
  beta { beta.cast<T>() },
  lambda { lambda.cast<T>() },
  u { u.cast<T>() },
  theta_mapping { theta_mapping },
  lambda_mapping_X { lambda_mapping_X },
  Lambdat { Lambdat.cast<T>() },
  weights { weights.cast<T>() },
  weights_mapping { weights_mapping },
  family_mapping { family_mapping },
  maxit_conditional_modes { maxit_conditional_modes },
  lossvalue_tol { lossvalue_tol },
  n { n }
  {
    for(int i{}; i < lambda_mapping_Zt0.size(); i++){
      lambda_mapping_Zt.push_back(Rcpp::as<std::vector<int>>(lambda_mapping_Zt0[i]));
    }
    for(int i{}; i < lambda_mapping_Zt_covs0.size(); i++){
      lambda_mapping_Zt_covs.push_back(Rcpp::as<std::vector<double>>(lambda_mapping_Zt_covs0[i]));
    }

    WSqrt.diagonal() = Vdual<T>::Constant(n, 1);
  }


  Vdual<T> theta;
  Vdual<T> beta;
  Vdual<T> lambda;
  Vdual<T> u;
  std::vector<int> theta_mapping;
  Eigen::VectorXi lambda_mapping_X = {};
  std::vector<std::vector<int>> lambda_mapping_Zt = {};
  std::vector<std::vector<double>> lambda_mapping_Zt_covs = {};
  Eigen::SparseMatrix<T> Lambdat;
  Vdual<T> weights;
  std::vector<int> weights_mapping;
  Eigen::VectorXi family_mapping;
  Ddual<T> WSqrt;
  int maxit_conditional_modes;
  double lossvalue_tol;
  int n;
};

//' @name logLikObject
//' @title Structure containing values to be returned to R
//' @noRd
template <typename T>
struct logLikObject {
  T logLikValue;
  Vdual<T> V;
  Vdual<T> u;
  Vdual<T> phi;
};

#endif

