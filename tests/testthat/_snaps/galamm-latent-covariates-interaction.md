# Interaction between latent and observed covariates works

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: formula
         Data: data
      
           AIC      BIC   logLik deviance df.resid 
         138.3    177.9    -60.2    120.3      591 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -2.20  -0.53  -0.03   0.51   3.50 
      
      Lambda:
                loading    SE
      lambda1      1.00     .
      lambda2      1.30 0.013
      lambda3     -0.32 0.016
      lambda4_x    0.23 0.029
      
      Random effects:
       Groups   Name    Variance Std.Dev.
       id       loading 0.982    0.99    
       Residual         0.012    0.11    
      Number of obs: 600, groups:  id, 200
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      (Intercept)       -0.0106      0.070  -0.150  8.8e-01
      typemeasurement2  -0.0022      0.024  -0.091  9.3e-01
      typeresponse       0.0340      0.094   0.361  7.2e-01
      x:response         0.4625      0.033  14.016  1.3e-44
      
      

---

    Code
      print(summary(modq), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: formula
         Data: data
      
           AIC      BIC   logLik deviance df.resid 
         140.3    184.2    -60.1    120.3      590 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -2.20  -0.52  -0.02   0.52   3.50 
      
      Lambda:
                     loading    SE
      lambda1          1.000     .
      lambda2          1.303 0.013
      lambda3         -0.314 0.024
      lambda4_x        0.209 0.113
      lambda5_I(x^2)   0.025 0.111
      
      Random effects:
       Groups   Name    Variance Std.Dev.
       id       loading 0.982    0.99    
       Residual         0.012    0.11    
      Number of obs: 600, groups:  id, 200
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      (Intercept)       -0.0099      0.071  -0.141  8.9e-01
      typemeasurement2  -0.0020      0.024  -0.083  9.3e-01
      typeresponse       0.0333      0.094   0.353  7.2e-01
      x:response         0.4622      0.033  13.984  2.0e-44
      
      

---

    Code
      print(anova(modq, mod), digits = 3)
    Output
      Data: data
      Models:
      mod: formula
      modq: formula
           npar AIC BIC logLik deviance Chisq Df Pr(>Chisq)
      mod     9 138 178  -60.2      120                    
      modq   10 140 184  -60.1      120  0.05  1       0.83

# Crossed latent-observed interaction models work

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: formula
         Data: data
      Control: galamm_control(optim_control = list(maxit = 0))
      
           AIC      BIC   logLik deviance df.resid 
        1860.4   1905.8   -920.2   1840.4      683 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -6.51  -0.83  -0.08   0.40   4.39 
      
      Lambda:
                loading   SE
      lambda1     1.000    .
      lambda2     2.124 0.40
      lambda3     0.850 0.23
      lambda4_x   0.038 0.15
      
      Random effects:
       Groups   Name     Variance Std.Dev.
       id       loading  0.50     0.71    
       id.1     response 0.91     0.96    
       Residual          0.44     0.66    
      Number of obs: 693, groups:  id, 99
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      (Intercept)        0.2766      0.073   3.795  1.5e-04
      typemeasurement2   0.0021      0.137   0.016  9.9e-01
      typeresponse      -0.2382      0.140  -1.696  9.0e-02
      x:response         0.5185      0.113   4.594  4.3e-06
      
      

