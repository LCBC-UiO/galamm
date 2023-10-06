rm(list=ls())
devtools::load_all()
dat <- subset(cognition, domain == 1 & item == "11" & id < 50)
dat$x2 <- rnorm(nrow(dat))
dat$y <- dat$y + dat$x2 * 10
object <- galamm(formula = y ~ x2 + s(x), data = dat)

nd <- data.frame(x = runif(100), x2 = runif(100))
plot(predict(object), predict(object, newdata = dat))

predict(object, newdata = nd)
