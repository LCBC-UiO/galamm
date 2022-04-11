#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]


//' Update fixed effect model matrix when loadings have changed
//'
//' @param X                 model matrix
//' @param columns           integer vector of columns to update
//' @param lambda_update     double vector of values with which to multiply old
//'                          values. One for each row of \code{X}.
//' @export
//'
//' @return A matrix in which all columns specified in the
//'  \code{columns} argument have been divided by the values in
//'  \code{lambda_update}.
//'
// [[Rcpp::export]]
Eigen::MatrixXd update_fixed(
    Eigen::MatrixXd X,
    Rcpp::IntegerVector columns,
    Eigen::VectorXd lambda_update
    ) {
  for(Rcpp::IntegerVector::iterator j = columns.begin(); j != columns.end(); ++j){
    for(size_t i{}; i < X.rows(); ++i){
      X(i, *j) /= lambda_update(i);
    }
  }
  return X;
}


//' Update random effect model matrix when loadings have changed
//'
//' @param Zt transposed model matrix in compressed sparse column format
//' @param lambda_update dense vector containing values with which the current
//' nonzero elements of \code{Zt} should be multiplied.
//'
//' @export
//'
//' @return A matrix having the same dimension and sparsity pattern as
//' \code{Zt}, but with its nonzero values updated.
//'
// [[Rcpp::export]]
Eigen::SparseMatrix<double> update_random(
  Eigen::SparseMatrix<double> Zt,
  Eigen::VectorXd lambda_update
) {

  size_t l{};
  for(size_t j{}; j < Zt.outerSize(); ++j){
    for(Eigen::SparseMatrix<double>::InnerIterator it(Zt, j); it; ++it){
      Zt.coeffRef(it.row(), it.col()) /= lambda_update(l);
      ++l;
    }
  }
  return Zt;
}
