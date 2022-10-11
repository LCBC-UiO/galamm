#include "data.h"
#include "parameters.h"
#include "model.h"
#include "misc.h"
#include "update_funs.h"
#include <unsupported/Eigen/SpecialFunctions>
using namespace autodiff;

// [[Rcpp::depends(RcppEigen)]]

template <typename T>
T loss(const parameters<T>& parlist, const data<T>& datlist, const Vdual<T>& lp,
       std::vector<Model<T>*> modvec, ldlt<T>& solver, Vdual<T>& phi){

  T ret{};
  for(int k{}; k < modvec.size(); k++){
    int size0 = (parlist.family_mapping.array() == k).template cast<int>().sum();
    Vdual<T> y0(size0);
    Vdual<T> trials0(size0);
    Vdual<T> lp0(size0);
    Ddual<T> WSqrt0(size0);
    int counter{0};
    for(int i{}; i < parlist.n; i++){
      if(parlist.family_mapping(i) == k){
        y0(counter) = datlist.y(i);
        trials0(counter) = datlist.trials(i);
        lp0(counter) = lp(i);
        WSqrt0.diagonal()(counter) = parlist.WSqrt.diagonal()(i);
        ++counter;
      }
    }
    phi(k) = modvec[k]->get_phi(lp0, parlist.u, y0, WSqrt0, size0);

    ret += ((WSqrt0 * y0).dot(WSqrt0 * lp0) -
      modvec[k]->cumulant(lp0, trials0, WSqrt0)) / phi(k) +
      modvec[k]->constfun(y0, phi(k), WSqrt0);
  }

  return ret - parlist.u.squaredNorm() / 2 / phi(0) -
    solver.vectorD().array().log().sum() / 2;

}

template <typename T>
logLikObject<T> logLik(
    parameters<T> parlist, data<T> datlist, std::vector<Model<T>*> modvec){

  update_Zt(datlist.Zt, parlist.lambda, parlist.lambda_mapping_Zt);
  update_X(datlist.X, parlist.lambda, parlist.lambda_mapping_X);
  update_WSqrt(parlist.WSqrt, parlist.weights, parlist.weights_mapping);

  Vdual<T> lp = linpred(parlist, datlist);
  Vdual<T> phi(modvec.size());
  Ddual<T> V(parlist.n);
  V.setZero();
  for(int k{}; k < modvec.size(); k++){
    Vdual<T> upd = modvec[k]->get_V(lp, datlist.trials, parlist.WSqrt).array() *
      (parlist.family_mapping.array() == k).array().template cast<T>();
    V.diagonal() += upd;
  }

  update_Lambdat(parlist.Lambdat, parlist.theta, parlist.theta_mapping);
  ldlt<T> solver;
  solver.setShift(1);
  SpMdual<T> H = inner_hessian(parlist, datlist, V);
  solver.analyzePattern(H);
  Vdual<T> delta_u{};
  solver.factorize(H);
  T deviance_prev = -2 * loss(parlist, datlist, lp, modvec, solver, phi);
  T deviance_new{};

  for(int i{}; i < parlist.maxit_conditional_modes; i++){
    Vdual<T> meanvec(parlist.n);
    meanvec.setZero();
    for(int k{}; k < modvec.size(); k++){
      Vdual<T> upd = modvec[k]->meanfun(linpred(parlist, datlist), datlist.trials).array() *
        (parlist.family_mapping.array() == k).array().template cast<T>();
      meanvec += upd;
    }

    Vdual<T> weighted_residual = parlist.WSqrt.diagonal().array().pow(2) * (datlist.y - meanvec).array();
    delta_u = solver.solve((parlist.Lambdat * datlist.Zt * weighted_residual) - parlist.u);

    double step = 1;
    for(int j{}; j < 10; j++){
      parlist.u += step * delta_u;
      lp = linpred(parlist, datlist);
      V.setZero();
      for(int k{}; k < modvec.size(); k++){
        Vdual<T> upd = modvec[k]->get_V(lp, datlist.trials, parlist.WSqrt).array() *
          (parlist.family_mapping.array() == k).array().template cast<T>();
        V.diagonal() += upd;
      }

      H = inner_hessian(parlist, datlist, V);
      solver.factorize(H);
      deviance_new = -2 * loss(parlist, datlist, lp, modvec, solver, phi);
      if(delta_u.array().abs().maxCoeff() < parlist.epsilon_u || deviance_new < deviance_prev){
        break;
      }
      parlist.u -= step * delta_u;
      step /= 2;
      if(j == 9){
        Rcpp::Rcout << "Could not find reducing step: i = " << i << ", j = " << j << std::endl;
        Rcpp::stop("Error");
      }
    }
    deviance_prev = deviance_new;
  }

  logLikObject<T> ret;
  ret.logLikValue = - deviance_new / 2;
  ret.V = V.diagonal();
  ret.u = parlist.u;
  ret.phi = phi;

  return ret;
}

template <typename T>
Rcpp::List wrapper(
    const Eigen::VectorXd& y,
    const Eigen::VectorXd& trials,
    const Eigen::MatrixXd& X,
    const Eigen::SparseMatrix<double>& Zt,
    const Eigen::SparseMatrix<double>& Lambdat,
    const Eigen::VectorXd& beta,
    const Eigen::VectorXd& theta,
    const Eigen::VectorXi& theta_mapping,
    const Eigen::VectorXd& u_init,
    const Eigen::VectorXd& lambda,
    const Eigen::VectorXi& lambda_mapping_X,
    const Eigen::VectorXi& lambda_mapping_Zt,
    const Eigen::VectorXd& weights,
    const Eigen::VectorXi& weights_mapping,
    const Rcpp::StringVector& family,
    const Eigen::VectorXi& family_mapping,
    const Eigen::VectorXd& k,
    const int& maxit_conditional_modes,
    const double& epsilon_u  ){


  data<T> datlist{y, trials, X, Zt};

  parameters<T> parlist{
      theta, beta, lambda, u_init, theta_mapping,
      lambda_mapping_X, lambda_mapping_Zt, Lambdat, weights, weights_mapping,
      family_mapping, maxit_conditional_modes, epsilon_u, y.size()};

  std::vector<Model<T>*> mod;

  for(size_t i{}; i < family.length(); i++){
    if(family(i) == "gaussian") {
      mod.push_back(new Gaussian<T>);
    } else if(family(i) == "binomial"){
      mod.push_back(new Binomial<T>{k(i)});
    } else if(family(i) == "poisson"){
      mod.push_back(new Poisson<T>{k(i)});
    } else {
      Rcpp::stop("Unknown family.");
    }
  }

  auto fx = [=, &mod, &datlist](parameters<T>& parlist){
    auto lll = logLik(parlist, datlist, mod);
    return lll.logLikValue;
  };
  auto gx = [=, &mod, &datlist](parameters<T>& parlist){
    return logLik(parlist, datlist, mod);;
  };

  return create_result(fx, gx, parlist);

}

// [[Rcpp::export]]
Rcpp::List marginal_likelihood_cpp(
    const Eigen::Map<Eigen::VectorXd> y,
    const Eigen::Map<Eigen::VectorXd> trials,
    const Eigen::Map<Eigen::MatrixXd> X,
    const Eigen::MappedSparseMatrix<double> Zt,
    const Eigen::MappedSparseMatrix<double> Lambdat,
    const Eigen::Map<Eigen::VectorXd> beta,
    const Eigen::Map<Eigen::VectorXd> theta,
    const Eigen::Map<Eigen::VectorXi> theta_mapping,
    const Eigen::Map<Eigen::VectorXd> u_init,
    const Eigen::Map<Eigen::VectorXd> lambda,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_X,
    const Eigen::Map<Eigen::VectorXi> lambda_mapping_Zt,
    const Eigen::Map<Eigen::VectorXd> weights,
    const Eigen::Map<Eigen::VectorXi> weights_mapping,
    const Rcpp::StringVector family,
    const Eigen::Map<Eigen::VectorXi> family_mapping,
    const Eigen::Map<Eigen::VectorXd> k,
    const int maxit_conditional_modes,
    const bool gradient,
    const bool hessian,
    double epsilon_u
){

  if(hessian){
    return wrapper<dual2nd>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
      family, family_mapping, k, maxit_conditional_modes, epsilon_u);
  } else if(gradient){
    return wrapper<dual1st>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
      family, family_mapping, k, maxit_conditional_modes, epsilon_u);
  } else {
    return wrapper<double>(
      y, trials, X, Zt, Lambdat, beta, theta, theta_mapping, u_init, lambda,
      lambda_mapping_X, lambda_mapping_Zt, weights, weights_mapping,
      family, family_mapping, k, maxit_conditional_modes, epsilon_u);
  }

}
