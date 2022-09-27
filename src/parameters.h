#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

template <typename T>
struct parameters{
  parameters(
    Eigen::VectorXd theta,
    Eigen::VectorXd beta,
    Eigen::VectorXd lambda,
    Eigen::VectorXd u,
    Eigen::VectorXi theta_mapping,
    Eigen::VectorXi lambda_mapping_X,
    Eigen::VectorXi lambda_mapping_Zt,
    Eigen::SparseMatrix<double> Lambdat
  ) :
  theta { theta.cast<T>() }, beta { beta.cast<T>() }, lambda { lambda.cast<T>() },
  u { u.cast<T>() }, theta_mapping { theta_mapping },
  lambda_mapping_X { lambda_mapping_X },
  lambda_mapping_Zt { lambda_mapping_Zt },
  Lambdat { Lambdat.cast<T>() }
  {}

  Vdual<T> theta;
  Vdual<T> beta;
  Vdual<T> lambda;
  Vdual<T> u;
  Eigen::VectorXi theta_mapping;
  Eigen::VectorXi lambda_mapping_X;
  Eigen::VectorXi lambda_mapping_Zt;
  Eigen::SparseMatrix<T> Lambdat;
};

#endif

