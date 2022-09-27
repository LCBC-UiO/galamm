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
    const Eigen::VectorXi& theta_mapping,
    const Eigen::VectorXi& lambda_mapping_X,
    const Eigen::VectorXi& lambda_mapping_Zt,
    const Eigen::SparseMatrix<double>& Lambdat,
    const int& maxit_conditional_modes,
    const double& epsilon_u
  ) :
  theta { theta.cast<T>() }, beta { beta.cast<T>() }, lambda { lambda.cast<T>() },
  u { u.cast<T>() }, theta_mapping { theta_mapping },
  lambda_mapping_X { lambda_mapping_X },
  lambda_mapping_Zt { lambda_mapping_Zt },
  Lambdat { Lambdat.cast<T>() },
  maxit_conditional_modes { maxit_conditional_modes },
  epsilon_u { epsilon_u }{}


  Vdual<T> theta;
  Vdual<T> beta;
  Vdual<T> lambda;
  Vdual<T> u;
  Eigen::VectorXi theta_mapping;
  Eigen::VectorXi lambda_mapping_X;
  Eigen::VectorXi lambda_mapping_Zt;
  Eigen::SparseMatrix<T> Lambdat;
  int maxit_conditional_modes;
  double epsilon_u;
};

#endif

