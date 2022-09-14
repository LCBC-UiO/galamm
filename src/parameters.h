#ifndef PARAMETERS_H
#define PARAMETERS_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

template <typename T>
struct parameters{
  parameters(
    Eigen::Matrix<T, Eigen::Dynamic, 1> theta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> beta,
    Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
    Eigen::Matrix<T, Eigen::Dynamic, 1> u,
    Eigen::VectorXi theta_mapping,
    Eigen::VectorXi lambda_mapping_X,
    Eigen::VectorXi lambda_mapping_Zt,
    Eigen::SparseMatrix<T> Lambdat
  ) :
  theta { theta }, beta { beta }, lambda { lambda },
  u { u }, theta_mapping { theta_mapping },
  lambda_mapping_X { lambda_mapping_X },
  lambda_mapping_Zt { lambda_mapping_Zt },
  Lambdat { Lambdat }
  {}

  Eigen::Matrix<T, Eigen::Dynamic, 1> theta;
  Eigen::Matrix<T, Eigen::Dynamic, 1> beta;
  Eigen::Matrix<T, Eigen::Dynamic, 1> lambda;
  Eigen::Matrix<T, Eigen::Dynamic, 1> u;
  Eigen::VectorXi theta_mapping;
  Eigen::VectorXi lambda_mapping_X;
  Eigen::VectorXi lambda_mapping_Zt;
  Eigen::SparseMatrix<T> Lambdat;
};

#endif

