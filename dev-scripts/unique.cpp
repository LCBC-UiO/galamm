#include <RcppEigen.h>

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
int set(Eigen::VectorXi x) {
  std::set<int> q{x.begin(), x.end()};
  return q.size();
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
set(c(1L, 1L, 1L, 2L))
*/
