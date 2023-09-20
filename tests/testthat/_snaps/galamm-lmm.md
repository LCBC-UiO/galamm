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

