#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>
#include "family.h"
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

namespace Model {
struct Model {

  Model(
    Eigen::VectorXd y0,
    Eigen::MatrixXd X0,
    Eigen::SparseMatrix<double> Z0,
    Eigen::SparseMatrix<double> Lambda0,
    Eigen::VectorXi Lind0,
    Eigen::VectorXd theta0,
    Eigen::VectorXd beta0,
    double phi0,
    Family& family0
  ) :
    y { y0.cast<dual2nd>() },
    X { X0.cast<dual2nd>() },
    Z { Z0.cast<dual2nd>() },
    Lambda { Lambda0.cast<dual2nd>() },
    Lind { Lind0 },
    theta { theta0.cast<dual2nd>() },
    beta { beta0.cast<dual2nd>() },
    phi { static_cast<dual2nd>(phi0) },
    family { family0 }
    {
      q = Z.cols();
      p = X.cols();
      u = VectorXdual2nd::Zero(q, 1);
    }

    VectorXdual2nd get_eta(){
      return X * beta + Z * Lambda * u;
    }
    VectorXdual2nd get_mu(){
      return family.inv_link(get_eta());
    }
    Eigen::SparseMatrix<dual2nd> get_hessian(){

      Eigen::SparseMatrix<dual2nd> A = Lambda.transpose() * Z.transpose() *
        family.V(get_eta(), phi) * Z * Lambda /
          pow(family.a(phi), 2.0);
      return A;
    }
    VectorXdual2nd get_rhs(){
      return Lambda.transpose() * Z.transpose() * (y - get_mu()) / family.a(phi) - u;
    }
    void update_Lambda(){
      int lind_counter{};
      for (int k=0; k<Lambda.outerSize(); ++k)
        for (Eigen::SparseMatrix<dual2nd>::InnerIterator it(Lambda, k); it; ++it)
        {
          it.valueRef() = theta(Lind(lind_counter));
          lind_counter++;
        }
    }

    VectorXdual2nd y;
    MatrixXdual2nd X;
    Eigen::SparseMatrix<dual2nd> Z;
    Eigen::VectorXi Lind;
    const Family& family;
    Eigen::SparseMatrix<dual2nd> Lambda;
    VectorXdual2nd theta;
    VectorXdual2nd beta;
    VectorXdual2nd u;
    dual2nd phi;

  private:
    int q{};
    int p{};
  };

}

