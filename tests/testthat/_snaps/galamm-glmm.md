# Logistic GLMM with simple factor works

    Code
      summary(mod)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: form
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
         315.3    353.8   -146.7    224.7      234 
      
      Scaled residuals: 
          Min      1Q  Median      3Q     Max 
      -2.5366 -0.7186  0.4074  0.5952  1.5062 
      
      Lambda:
              abil.sid     SE
      lambda1   1.0000      .
      lambda2   1.0765 1.0012
      lambda3   0.8904 0.5977
      lambda4   0.7358 0.5887
      lambda5   1.2040 1.0143
      
      Random effects:
       Groups     Name     Variance Std.Dev.
       sid:school abil.sid 1.631    1.277   
       school     abil.sid 0.000    0.000   
      Number of obs: 245, groups:  sid:school, 49; school, 4
      
      Fixed effects:
                  Estimate Std. Error z value Pr(>|z|)
      (Intercept)   0.6079     0.4118  1.4763   0.1399
      item2         0.9278     0.7036  1.3185   0.1873
      item3        -0.6684     0.4927 -1.3566   0.1749
      item4         0.3654     0.5292  0.6906   0.4898
      item5         0.3130     0.5966  0.5246   0.5999
      
      

