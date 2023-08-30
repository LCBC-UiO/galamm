# Logistic GLMM with simple factor works

    Code
      summary(mod)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: form
         Data: IRTsim
      
           AIC      BIC   logLik deviance df.resid 
        2966.4   3030.5  -1472.2   2372.3     2489 
      
      Scaled residuals: 
          Min      1Q  Median      3Q     Max 
      -2.8884 -0.7335  0.4266  0.6235  3.0410 
      
      Lambda:
              abil.sid     SE
      lambda1   1.0000      .
      lambda2   0.7370 0.1456
      lambda3   0.9351 0.1872
      lambda4   0.6069 0.1261
      lambda5   0.5860 0.1163
      
      Random effects:
       Groups     Name     Variance Std.Dev.
       sid:school abil.sid 1.466    1.211   
       school     abil.sid 1.300    1.140   
      Number of obs: 2500, groups:  sid:school, 500; school, 26
      
      Fixed effects:
                  Estimate Std. Error z value Pr(>|z|)
      (Intercept)   0.5112     0.2617   1.953 0.050777
      item2         0.3256     0.1791   1.818 0.069068
      item3        -0.4491     0.1623  -2.768 0.005644
      item4         0.4930     0.1924   2.562 0.010402
      item5         0.4585     0.1922   2.385 0.017067

