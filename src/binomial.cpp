#include "model.h"
using namespace autodiff;

dual1st GALAMM::Binomial::cumulant(){
    return ((1 + get_linpred().array().exp()).log() * trials.array()).sum();
  }

dual1st GALAMM::Binomial::constfun(){
    return 1;
  }

VectorXdual1st GALAMM::Binomial::meanfun() {
    return get_linpred().array().exp() / (1 + get_linpred().array().exp()) *
      trials.array();
  }

void GALAMM::Binomial::update_V(){
  V.diagonal().array() = meanfun().array() * (trials.array() - meanfun().array());
}

void GALAMM::Binomial::update_phi(){
  phi = 1;
}

void GALAMM::Binomial::update_linpred(){
  linpred = X * beta + Zt.transpose() * get_Lambdat().transpose() * u;
}
