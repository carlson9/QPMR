library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd('~/QPMR/classwork/day14Stan/')

set.seed(123)
x = rnorm(500)
y = 1.5*x - 3 + rnorm(500)

to_stan = list(
  N = length(y),
  x = x,
  y = y
)

mod1 = stan('linear.stan',
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)

summary(mod1)$summary
summary(summary(mod1)$summary[, 'Rhat'])
plot(mod1)
traceplot(mod1)

set.seed(123)
N = 500
J = 5
x = rnorm(N)
group.ef = rep(rnorm(J), each = N/J)
y = 1.5*x - 3 + group.ef + rnorm(N)
j = rep(1:J, each = N/J)

to_stan = list(
  N = N,
  J = J,
  x = x,
  y = y,
  j = j
)

mod2 = stan('hier.stan',
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)

summary(mod2)$summary
summary(summary(mod2)$summary[, 'Rhat'])
plot(mod2)
traceplot(mod2)



setwd('~/QPMR/classwork/day15SURStan/')
set.seed(123)
x1 = rnorm(1000)
x2 = rnorm(1000)
mu_1 = 1.5 * x1 + 3 + rnorm(1000)
mu_2 = -5 * x2 + 2 + rnorm(1000)
library(MASS)
y = matrix(ncol = 2)
for(i in 1:1000) y = rbind(y, as.vector(mvrnorm(1, c(mu_1[1], mu_2[1]), Sigma = matrix(c(1,.25,.25,1), nrow = 2))))
y = y[-1, ]

to_stan = list(
  N = 1000,
  x1 = x1,
  x2 = x2,
  y = y
) 

mod2 = stan('sur.stan',
            data = to_stan,
            chains = 2,
            iter = 1000,
            seed = 1453)
summary(summary(mod2)$summary['Rhat'])

plot(density(unlist(extract(mod2, pars = 'L_Omega[2,1]'))))
