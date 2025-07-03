# Heteroscedastic model works

    Code
      print(summary(mod), digits = 3)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ x + (1 | id)
         Data: hsced
      Weights: ~(1 | item)
      
           AIC      BIC   logLik deviance df.resid 
        4126.3   4151.7  -2058.1   4116.3     1195 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
      -5.654 -0.711  0.029  0.683  4.326 
      
      Random effects:
       Groups   Name        Variance Std.Dev.
       id       (Intercept) 0.988    0.994   
       Residual             0.960    0.980   
      Number of obs: 1200, groups:  id, 200
      
      Variance function:
         1    2 
      1.00 1.99 
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)    0.129     0.0992    1.30 1.94e-01
      x              0.706     0.1213    5.82 5.82e-09
      
      

# Heteroscedastic model works with more than one group

    Code
      print(summary(mod), digits = 3)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ x + (1 | id)
         Data: hsced2
      Weights: ~(1 | item)
      
           AIC      BIC   logLik deviance df.resid 
        4128.3   4158.8  -2058.1   4116.3     1194 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
      -5.656 -0.709  0.028  0.683  4.323 
      
      Random effects:
       Groups   Name        Variance Std.Dev.
       id       (Intercept) 0.988    0.994   
       Residual             0.960    0.980   
      Number of obs: 1200, groups:  id, 200
      
      Variance function:
         1    2    3 
      1.00 2.01 1.98 
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)    0.129     0.0992    1.30 1.94e-01
      x              0.706     0.1213    5.82 5.88e-09
      
      

