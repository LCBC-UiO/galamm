# multiple factors and factors in fixed effects are allowed

    Code
      print(summary(kyps.model), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: esteem ~ as.factor(time) + (0 + hs | hid) + (0 + ms | mid)
         Data: KYPSsim
      Control: 
      galamm_control(optim_control = list(maxit = 1), maxit_conditional_modes = 1)
      
           AIC      BIC   logLik deviance df.resid 
       24141.8  24222.7 -12059.9  24119.8    11483 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -3.46  -0.57   0.03   0.64   3.52 
      
      Lambda:
               ms   SE   hs    SE
      lambda1 1.0    .    .     .
      lambda2 1.1 0.21    .     .
      lambda3 1.0 0.40 1.00     .
      lambda4 1.0 0.43 0.98 0.056
      
      Random effects:
       Groups   Name Variance Std.Dev.
       hid      hs   0.39     0.63    
       mid      ms   0.58     0.76    
       Residual      0.40     0.63    
      Number of obs: 11494, groups:  hid, 860; mid, 104
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      (Intercept)         0.071     0.0028  25.457 5.9e-143
      as.factor(time)2    0.030     0.6255   0.048  9.6e-01
      as.factor(time)3    0.013     1.1704   0.011  9.9e-01
      as.factor(time)4    0.014     1.2638   0.011  9.9e-01
      
      

---

    Code
      print(summary(kyps.model), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: esteem ~ 1 + ms:time2 + (1 | sid)
         Data: subset(KYPSsim, time %in% c(1, 2))
      Control: 
      galamm_control(optim_control = list(maxit = 1), maxit_conditional_modes = 1)
      
           AIC      BIC   logLik deviance df.resid 
       17612.0  17645.3  -8801.0  17602.0     5749 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -2.18  -0.36   0.21   0.77   2.51 
      
      Lambda:
               ms    SE
      lambda1 1.0     .
      lambda2 1.1 0.016
      
      Random effects:
       Groups   Name        Variance Std.Dev.
       sid      (Intercept) 2.02     1.42    
       Residual             0.35     0.59    
      Number of obs: 5754, groups:  sid, 2924
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)     1.69      0.021      81        0
      ms:time2        0.75      0.011      67        0
      
      

# multiple factors in fixed effects works

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: 
      y ~ 0 + x:domain1:lambda1 + x:domain2:lambda2 + (0 + domain1:lambda1 +  
          domain2:lambda2 | id)
         Data: dat
      Control: galamm_control(optim_control = list(maxit = 3))
      
           AIC      BIC   logLik deviance df.resid 
        6517.7   6577.8  -3248.9   6497.7     2990 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -4.11  -0.63   0.10   0.88   4.85 
      
      Lambda:
              lambda1    SE lambda2    SE
      lambda1    1.00     .       .     .
      lambda2    0.95 0.030       .     .
      lambda3    1.45 0.037       .     .
      lambda4       .     .    1.00     .
      lambda5       .     .    0.83 0.026
      lambda6       .     .    1.91 0.052
      
      Random effects:
       Groups   Name            Variance Std.Dev. Corr
       id       domain1:lambda1 0.47     0.68         
                domain2:lambda2 0.73     0.85     0.25
       Residual                 0.22     0.47         
      Number of obs: 3000, groups:  id, 500
      
      Fixed effects:
                        Estimate Std. Error t value Pr(>|t|)
      x:domain1:lambda1     0.99      0.040      25 5.1e-134
      x:domain2:lambda2     1.66      0.048      35 7.5e-265
      
      

