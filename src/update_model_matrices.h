#ifndef UPDATE_MODEL_MATRICES_H
#define UPDATE_MODEL_MATRICES_H

#include <RcppEigen.h>

Eigen::MatrixXd update_fixed(Eigen::MatrixXd X, Rcpp::IntegerVector,
                             Eigen::VectorXd);

Eigen::SparseMatrix<double> update_random(Eigen::SparseMatrix<double>,
                                          Eigen::VectorXd);

#endif
