#include "model.h"

using dvec = autodiff::VectorXdual2nd;
using dmat = autodiff::MatrixXdual2nd;

void GALAMM::Model::get_conditional_modes(
    Eigen::SimplicialLLT<Eigen::SparseMatrix<autodiff::dual2nd> >& solver
  ){
  dvec delta_beta{};
  dvec delta_u{};

  for(int i{}; i < maxit_conditional_modes; i++){

    solver.factorize(get_inner_hessian());
    dvec b1 = solver.permutationP() *
      (get_Lambdat() * Zt * (y - meanfun()) - u );
    dvec cu = solver.matrixL().solve(b1);

    dmat b2 = solver.permutationP() * get_Lambdat() * Zt *
      get_V() * X / get_phi();
    dmat RZX = solver.matrixL().solve(b2);

    dmat RXtRX = (1/get_phi()) * X.transpose() * get_V() * X -
      RZX.transpose() * RZX;
    delta_beta = RXtRX.colPivHouseholderQr().solve(X.transpose() * (y - meanfun()) -
      RZX.transpose() * cu);

    delta_u = solver.permutationPinv() * solver.matrixU().solve(cu - RZX * delta_beta);
    update_beta(delta_beta);
    update_u(delta_u);
  }
}

autodiff::dual2nd GALAMM::Model::exponent_g(){
  return (y.dot(get_linpred()) - cumulant()) / get_phi() + constfun() -
    u.squaredNorm() / 2 / get_phi();
}

void GALAMM::Model::update_inner_hessian(){
  inner_hessian = (1 / get_phi()) *
    get_Lambdat() * Zt * get_V() *
    Zt.transpose() * get_Lambdat().transpose();
}

Eigen::SparseMatrix<autodiff::dual2nd>& GALAMM::Model::get_inner_hessian(){
  if(inner_hessian_needs_update){
    update_inner_hessian();
    inner_hessian_needs_update = false;
  }
  return inner_hessian;
}

void GALAMM::Model::update_Lambdat(){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (Eigen::SparseMatrix<autodiff::dual2nd>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      it.valueRef() = theta(Lind(lind_counter));
      lind_counter++;
    }
  }
  inner_hessian_needs_update = true;
  linpred_needs_update = true;

}

Eigen::SparseMatrix<autodiff::dual2nd>& GALAMM::Model::get_Lambdat(){
  if(Lambdat_needs_update){
    update_Lambdat();
    Lambdat_needs_update = false;
  }
  return Lambdat;
}

Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic>&
  GALAMM::Model::get_V(){
    if(V_needs_update){
      update_V();
      V_needs_update = false;
    }
    return V;
  }

autodiff::dual2nd& GALAMM::Model::get_phi(){
  if(phi_needs_update){
    update_phi();
    phi_needs_update = false;
  }
  return phi;
}

void GALAMM::Model::update_linpred(){
  linpred = X * beta + Zt.transpose() * get_Lambdat().transpose() * u;
  phi_needs_update = true;
}

autodiff::VectorXdual2nd& GALAMM::Model::get_linpred(){
  if(linpred_needs_update){
    update_linpred();
    linpred_needs_update = false;
  }
  return linpred;
}


void GALAMM::Model::update_beta(const autodiff::VectorXdual2nd& delta_beta){
  beta += delta_beta;
  linpred_needs_update = true;
}

void GALAMM::Model::update_u(const autodiff::VectorXdual2nd& delta_u){
  u += delta_u;
  linpred_needs_update = true;
}
