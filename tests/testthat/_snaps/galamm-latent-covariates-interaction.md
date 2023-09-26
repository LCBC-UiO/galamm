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
      
           AIC      BIC   logLik deviance df.resid 
          63.2    103.0    -21.6     43.2      386 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -3.27  -0.52  -0.04   0.50   3.80 
      
      Lambda:
                loading    SE
      lambda1      1.00     .
      lambda2      1.31 0.024
      lambda3     -0.37 0.023
      lambda4_x    0.31 0.040
      
      Random effects:
       Groups   Name     Variance Std.Dev.
       id       loading  0.904    0.95    
       id.1     response 0.000    0.00    
       Residual          0.019    0.14    
      Number of obs: 396, groups:  id, 99
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      (Intercept)        -0.033      0.097   -0.34  7.3e-01
      typemeasurement2   -0.012      0.036   -0.35  7.3e-01
      typeresponse        0.073      0.134    0.55  5.8e-01
      x:response          0.432      0.048    8.98  2.7e-19
      
      

