#ifndef DATA_H
#define DATA_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "model.h"

template <typename T>
struct data{
  data(
    Eigen::VectorXd y,
    Eigen::VectorXd trials,
    Eigen::MatrixXd X,
    Eigen::SparseMatrix<double> Zt
  ) :
  y { y.cast<T>() }, trials { trials.cast<T>() }, X { X.cast<T>() },
  Zt { Zt.cast<T>() }
  {}

  Vdual<T> y;
  Vdual<T> trials;
  Mdual<T> X;
  SpMdual<T> Zt;
};

#endif

