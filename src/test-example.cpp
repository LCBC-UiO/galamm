#include <testthat.h>
#include <RcppEigen.h>
#include <autodiff/forward/real.hpp>
#include <autodiff/forward/real/eigen.hpp>
using namespace autodiff;

real f(const VectorXreal& beta,
       const Eigen::SparseMatrix<real>& Xsp, const VectorXreal& y)
{
  VectorXreal v1 = Xsp * beta - y;
  return v1.squaredNorm();
}


Eigen::VectorXd deriv()
{
  VectorXreal beta(2);
  beta << 1, 2;
  Eigen::SparseMatrix<real> Xsp(4, 2);

  for(int row = 0; row < 4; row++){
    for(int col = 0; col < 2; col++){
      Xsp.insert(row, col) = row;
    }
  }
  VectorXreal y(4);
  y << 1, 2, 3, 4;

  real u;

  Eigen::VectorXd g = gradient(f, wrt(beta), at(beta, Xsp, y), u);

  return g;
}
// Initialize a unit test context. This is similar to how you
// might begin an R test file with 'context()', expect the
// associated context should be wrapped in braced.
context("Sample unit tests") {

  test_that("Forward derivative is correct") {
    Eigen::VectorXd g2(2);
    g2 << 44, 44;
    expect_true(deriv() == g2);
  }

}
