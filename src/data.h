#ifndef DATA_H
#define DATA_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

template <typename T>
struct data{
  data(
    Eigen::Matrix<double, Eigen::Dynamic, 1> y,
    Eigen::Matrix<double, Eigen::Dynamic, 1> trials,
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> X,
    Eigen::SparseMatrix<double> Zt
  ) :
  y { y.template cast<T>() }, trials { trials.cast<T>() },
  X { X.cast<T>() }, Zt { Zt.cast<T>() }
  {}

  Eigen::Matrix<T, Eigen::Dynamic, 1> y;
  Eigen::Matrix<T, Eigen::Dynamic, 1> trials;
  Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> X;
  Eigen::SparseMatrix<T> Zt;
};

#endif

