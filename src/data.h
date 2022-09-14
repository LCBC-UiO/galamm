#ifndef DATA_H
#define DATA_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>

template <typename T>
struct data{
  data(
    Eigen::Matrix<T, Eigen::Dynamic, 1> y,
    Eigen::Matrix<T, Eigen::Dynamic, 1> trials,
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> X,
    Eigen::SparseMatrix<T> Zt
  ) :
  y { y }, trials { trials }, X { X }, Zt { Zt }
  {}

  Eigen::Matrix<T, Eigen::Dynamic, 1> y;
  Eigen::Matrix<T, Eigen::Dynamic, 1> trials;
  Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> X;
  Eigen::SparseMatrix<T> Zt;
};

#endif

