#ifndef MISC_H
#define MISC_H

#include "model.h"
#include "parameters.h"
#include "data.h"

template <typename T>
Vdual<T> linpred(
    const parameters<T>& parlist,
    const data<T>& datlist
){
  return datlist.X * parlist.beta + datlist.Zt.transpose() *
    parlist.Lambdat.transpose() * parlist.u;
};

// Hessian matrix used in penalized iteratively reweighted least squares
template <typename T>
SpMdual<T> inner_hessian(
    const parameters<T>& parlist,
    const data<T>& datlist,
    const Eigen::DiagonalMatrix<T, Eigen::Dynamic>& V
){
  return parlist.Lambdat * datlist.Zt * V *
    datlist.Zt.transpose() * parlist.Lambdat.transpose();
};

template <typename T, typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<T>& parlist){
  T ll = fx(parlist);
  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll)
  );
}

// Specialization for dual1st, gives gradient vector
template <typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<autodiff::dual1st>& parlist){
  autodiff::dual1st ll{};
  Eigen::VectorXd g{};
  g = gradient(fx, wrt(parlist.theta, parlist.beta, parlist.lambda,
                       parlist.weights),
                       at(parlist), ll);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g
  );
}

// Specialization for dual2nd, gives Hessian matrix plus some extra info
template <typename Functor1, typename Functor2>
Rcpp::List create_result(Functor1 fx, Functor2 gx, parameters<autodiff::dual2nd>& parlist){
  autodiff::dual2nd ll{};
  Eigen::VectorXd g{};
  Eigen::MatrixXd H{};
  g = gradient(fx, wrt(parlist.theta, parlist.beta, parlist.lambda, parlist.weights), at(parlist), ll);
  H = hessian(fx, wrt(parlist.theta, parlist.beta, parlist.lambda, parlist.weights), at(parlist));
  logLikObject extras = gx(parlist);

  return Rcpp::List::create(
    Rcpp::Named("logLik") = static_cast<double>(ll),
    Rcpp::Named("gradient") = g,
    Rcpp::Named("hessian") = H,
    Rcpp::Named("u") = extras.u.template cast<double>(),
    Rcpp::Named("V") = extras.V.template cast<double>(),
    Rcpp::Named("phi") = extras.phi.template cast<double>()
  );
}

#endif

