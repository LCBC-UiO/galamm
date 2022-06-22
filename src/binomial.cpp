#include "model.h"

autodiff::dual2nd
GALAMM::Binomial::cumulant(){
  return ((1 + get_linpred().array().exp()).log() * trials.array()).sum();
}

autodiff::dual2nd
GALAMM::Binomial::constfun(){
  return 1;
}

autodiff::VectorXdual2nd
GALAMM::Binomial::meanfun() {
  return get_linpred().array().exp() / (1 + get_linpred().array().exp()) *
    trials.array();
}

void GALAMM::Binomial::update_V(){
  V.diagonal().array() = meanfun().array() * (trials.array() - meanfun().array());
  inner_hessian_needs_update = true;
}

void GALAMM::Binomial::update_phi(){
  phi = 1;
}

void GALAMM::Binomial::update_linpred(){
  linpred = X * beta + Zt.transpose() * get_Lambdat().transpose() * u;
  phi_needs_update = false;
  V_needs_update = true;
}
