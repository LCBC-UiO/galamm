devtools::load_all()

data(Orthodont, package = "nlme")

x <- galamm(distance ~ age + (age | Subject), data = Orthodont)

plot(fm1)
