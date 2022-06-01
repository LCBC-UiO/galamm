#ifndef FAMILY_H
#define FAMILY_H

#include <RcppEigen.h>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>


// [[Rcpp::depends(RcppEigen)]]



struct Family{
  virtual ~Family() = default;

  virtual autodiff::dual2nd a(const autodiff::dual2nd& phi) const = 0;

  virtual autodiff::dual2nd c(const autodiff::VectorXdual2nd& y,
                              const autodiff::dual2nd& phi)
    const = 0;

  virtual autodiff::dual2nd d(const autodiff::VectorXdual2nd& eta) const = 0;

  virtual Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic> V(
    const autodiff::VectorXdual2nd& eta,
    const autodiff::dual2nd& phi
  ) const = 0;

  virtual autodiff::VectorXdual2nd inv_link(const autodiff::VectorXdual2nd& eta)
    const = 0;

};

struct Gaussian : Family {
  autodiff::dual2nd a(const autodiff::dual2nd& phi) const override {
    return phi; }
  autodiff::dual2nd c(const autodiff::VectorXdual2nd& y,
                      const autodiff::dual2nd& phi) const override {
    return -y.squaredNorm() / 2 / phi - y.rows() / 2 * log(2 * M_PI * phi);
  }
  autodiff::dual2nd d(const autodiff::VectorXdual2nd& eta) const override {
    return eta.squaredNorm() / 2;
  }
  Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic> V(
      const autodiff::VectorXdual2nd& eta,
      const autodiff::dual2nd& phi
  ) const override {
    Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic> V(eta.rows());
    V.setIdentity();
    V.diagonal().array() = a(phi);
    return V;
  }

  autodiff::VectorXdual2nd inv_link(const autodiff::VectorXdual2nd& eta)
  const override {
    return eta;
  }
};

#endif
