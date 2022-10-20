#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int timesTwo(ListOf<NumericVector> x) {
  std::vector<std::vector<double>> y;
  for(int i{}; i < x.size(); i++){
    Rcpp::Rcout << x[i] << std::endl;
    y.push_back(as<std::vector<double>>(x[i]));

  }

  for(int i{}; i < y.size(); i++){
    for(int j{}; j < y[i].size(); j++){
      Rcout << y[i][j] << std::endl;
    }
  }

  Rcout << y.size() << std::endl;

  return 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(integer())
*/
