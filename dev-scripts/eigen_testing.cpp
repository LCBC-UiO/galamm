#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;                       // 'maps' rather than copies
using Eigen::MatrixXd;                  // variable size matrix, double precision
using Eigen::VectorXd;                  // variable size vector, double precision
using Eigen::SelfAdjointEigenSolver;    // one of the eigenvalue solvers

// [[Rcpp::export]]
VectorXd getEigenValues(Map<MatrixXd> M) {
  SelfAdjointEigenSolver<MatrixXd> es(M);
  return es.eigenvalues();
}


/*** R
M <- matrix(rnorm(12), nrow = 4)
getEigenValues(M)
*/
