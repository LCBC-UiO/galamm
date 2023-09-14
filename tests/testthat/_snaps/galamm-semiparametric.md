# galamm reproduces gamm4

    Code
      print(summary(mod), digits = 2)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: y ~ s(x)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        1127.0   1142.9   -559.5   1119.0      388 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -2.33  -0.61  -0.01   0.69   3.22 
      
      Random effects:
       Groups   Name Variance Std.Dev.
       Xr       s(x) 2.88     1.70    
       Residual      0.99     0.99    
      Number of obs: 392, groups:  Xr, 8
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)    1.342       0.05  26.737 1.7e-157
      s(x)Fx1       -0.022       0.41  -0.055  9.6e-01
      
      Approximate significance of smooth terms:
           edf Ref.df  F p-value
      s(x) 4.2    4.2 36  <2e-16
      

