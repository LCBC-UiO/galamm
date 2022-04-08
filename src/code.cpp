#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]


//' Update the fixed effect model matrix when loadings have changed
//'
//' @param X                 model matrix
//' @param columns           integer vector of columns to update
//' @param lambda_update     double vector of values with which to multiply old
//'                          values. One for each row of \code{X}.
//' @export
//'
//' @return A matrix \code{X} in which all columns specified in the
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
