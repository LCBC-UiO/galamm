#ifndef MODEL_H
#define MODEL_H

#include <RcppEigen.h>
#include <unsupported/Eigen/SpecialFunctions>
#include <autodiff/forward/dual.hpp>
#include <autodiff/forward/dual/eigen.hpp>


namespace GALAMM {

  struct Model{

    // Constructor, converting objects to autodiff
    Model(
      const Eigen::VectorXd y0,
      const Eigen::MatrixXd X0,
      const Eigen::MappedSparseMatrix<double> Zt0,
      const Eigen::MappedSparseMatrix<double> Lambdat0,
      const Eigen::VectorXi Lind0,
      const Eigen::VectorXd theta0,
      const Eigen::VectorXd trials0,
      const int maxit_conditional_modes0 = 1
    );

    // Function to compute regression coefficients in inner loop
    void get_conditional_modes(
        Eigen::SimplicialLLT<Eigen::SparseMatrix<autodiff::dual2nd> >& solver
    );
    int maxit_conditional_modes;

    // Exponent in the Laplace approximation
    autodiff::dual2nd exponent_g();

    // Hessian matrix used in penalized iteratively reweighted least squares
    void update_inner_hessian();
    Eigen::SparseMatrix<autodiff::dual2nd>& get_inner_hessian();
    bool inner_hessian_needs_update{true};
    Eigen::SparseMatrix<autodiff::dual2nd> inner_hessian;

    // Lower Cholesky factor of scaled covariance matrix
    void update_Lambdat();
    Eigen::SparseMatrix<autodiff::dual2nd>& get_Lambdat();
    bool Lambdat_needs_update{true};
    Eigen::SparseMatrix<autodiff::dual2nd> Lambdat;

    // Scale parameter
    virtual void update_phi() = 0;
    autodiff::dual2nd phi;
    autodiff::dual2nd& get_phi();
    bool phi_needs_update{true};

    // Diagonal variance matrix, common parts
    virtual void update_V() = 0;
    Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic> V;
    Eigen::DiagonalMatrix<autodiff::dual2nd, Eigen::Dynamic>& get_V();
    bool V_needs_update{true};

    // Linear predictor
    void update_linpred();
    autodiff::VectorXdual2nd& get_linpred();
    bool linpred_needs_update{true};
    autodiff::VectorXdual2nd linpred;

    // GLM functions defined in derived classes
    virtual autodiff::dual2nd cumulant() = 0;
    virtual autodiff::dual2nd constfun() = 0;
    virtual autodiff::VectorXdual2nd meanfun() = 0;

    // Regression coefficients
    void update_beta(const autodiff::VectorXdual2nd& delta_beta);
    void update_u(const autodiff::VectorXdual2nd& delta_u);

    Eigen::VectorXd y;
    autodiff::MatrixXdual2nd X;
    Eigen::SparseMatrix<autodiff::dual2nd> Zt;

    const Eigen::VectorXi Lind;
    autodiff::VectorXdual2nd theta;
    autodiff::VectorXdual2nd beta;
    autodiff::VectorXdual2nd u;
    Eigen::VectorXd trials;

    int n;
    int p;
    int q;

  };

  struct Gaussian : Model {

    // Inherit base class constructor
    using Model::Model;

    autodiff::dual2nd cumulant() override;
    autodiff::dual2nd constfun() override;
    autodiff::VectorXdual2nd meanfun() override;

    // How to update diagonal variance matrix is model dependent
    void update_V() override;
    void update_phi() override;

  };

  struct Binomial : Model {

    // Inherit base class constructor
    using Model::Model;

    autodiff::dual2nd cumulant() override;
    autodiff::dual2nd constfun() override;
    autodiff::VectorXdual2nd meanfun() override;

    // How to update diagonal variance matrix is model dependent
    void update_V() override;
    void update_phi() override;

  };
}



#endif
