# LMM with simple factor works

    Code
      print(VarCorr(mod), digits = 2)
    Output
       Groups     Name     Std.Dev. Variance
       sid:school abil.sid 0.24     0.055   
       school     abil.sid 0.21     0.046   
       Residual            0.37     0.137   

---

    Code
      round(confint(mod, parm = "beta"), 2)
    Output
                       2.5 % 97.5 %
      as.factor(item)1  0.52   0.77
      as.factor(item)2  0.48   0.72
      as.factor(item)3  0.42   0.67

---

    Code
      round(confint(mod, parm = "lambda"), 2)
    Output
              2.5 % 97.5 %
      lambda1  0.63   1.48
      lambda2  0.56   1.49

---

    Code
      round(confint(mod, parm = "theta"), 2)
    Output
             2.5 % 97.5 %
      theta1  0.26   1.00
      theta2  0.29   0.86

---

    Code
      print(summary(mod2), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ 0 + as.factor(item) + (0 + abil.sid | school/sid)
         Data: IRTsub
      Control: galamm_control(reduced_hessian = TRUE)
      
           AIC      BIC   logLik deviance df.resid 
         403.1    432.8   -193.6    387.1      292 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -1.75  -0.78   0.29   0.60   1.82 
      
      Lambda:
              abil.sid   SE
      lambda1      1.0    .
      lambda2      1.1 0.19
      lambda3      1.0 0.22
      
      Random effects:
       Groups     Name     Variance Std.Dev.
       sid:school abil.sid 0.055    0.24    
       school     abil.sid 0.046    0.21    
       Residual            0.137    0.37    
      Number of obs: 300, groups:  sid:school, 237; school, 26
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      as.factor(item)1     0.65      0.063    10.3  1.2e-24
      as.factor(item)2     0.60      0.062     9.6  6.2e-22
      as.factor(item)3     0.55      0.063     8.6  5.9e-18
      
      

# LMM with simple factor works with Nelder-Mead

    Code
      print(VarCorr(mod), digits = 2)
    Output
       Groups     Name     Std.Dev. Variance
       sid:school abil.sid 0.24     0.055   
       school     abil.sid 0.21     0.046   
       Residual            0.37     0.137   

---

    Code
      round(confint(mod, parm = "beta"), 2)
    Output
                       2.5 % 97.5 %
      as.factor(item)1  0.52   0.77
      as.factor(item)2  0.48   0.72
      as.factor(item)3  0.42   0.67

---

    Code
      round(confint(mod, parm = "lambda"), 2)
    Output
              2.5 % 97.5 %
      lambda1  0.63   1.48
      lambda2  0.56   1.49

---

    Code
      round(confint(mod, parm = "theta"), 2)
    Output
             2.5 % 97.5 %
      theta1  0.26   1.00
      theta2  0.29   0.86

---

    Code
      print(summary(mod2), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ 0 + as.factor(item) + (0 + abil.sid | school/sid)
         Data: IRTsub
      Control: galamm_control(reduced_hessian = TRUE)
      
           AIC      BIC   logLik deviance df.resid 
         403.1    432.8   -193.6    387.1      292 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -1.75  -0.78   0.29   0.60   1.82 
      
      Lambda:
              abil.sid   SE
      lambda1      1.0    .
      lambda2      1.1 0.19
      lambda3      1.0 0.22
      
      Random effects:
       Groups     Name     Variance Std.Dev.
       sid:school abil.sid 0.055    0.23    
       school     abil.sid 0.046    0.21    
       Residual            0.137    0.37    
      Number of obs: 300, groups:  sid:school, 237; school, 26
      
      Fixed effects:
                       Estimate Std. Error t value Pr(>|t|)
      as.factor(item)1     0.65      0.063    10.3  1.2e-24
      as.factor(item)2     0.60      0.062     9.6  6.2e-22
      as.factor(item)3     0.55      0.063     8.6  5.9e-18
      
      

