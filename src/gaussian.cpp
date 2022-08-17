#include "model.h"
using namespace autodiff;

dual1st GALAMM::Gaussian::cumulant(){
    return get_linpred().squaredNorm() / 2;
  }

dual1st GALAMM::Gaussian::constfun(){
    return -.5 * (y.squaredNorm() / get_phi() + n * log(2 * M_PI * get_phi()));
  }

VectorXdual1st GALAMM::Gaussian::meanfun() {
    return get_linpred();
  }

void GALAMM::Gaussian::update_V(){
  V.diagonal().array() = get_phi();
  inner_hessian_needs_update = true;
}

void GALAMM::Gaussian::update_phi(){
  phi = ((y - get_linpred()).squaredNorm() + u.squaredNorm()) / n;
  inner_hessian_needs_update = true;
  V_needs_update = true;
}


void GALAMM::Gaussian::update_linpred(){
  linpred = X * beta + Zt.transpose() * get_Lambdat().transpose() * u;
  phi_needs_update = true;
}
