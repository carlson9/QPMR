set.seed(1919)

x1 = rnorm(100)+2
x_maybe = rnorm(300)+1
x0 = rnorm(200)
x= c(x1, x_maybe, x0)
y = c(rpois(300, 0.5*x[1:300]+1), rep(0, 300))

to_stan= list(
  N=600,
  y=y
)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

model_code = '
data {
  int<lower=0> N;
int<lower=0> y[N];
}
parameters {
real<lower=0, upper=1> theta;
real<lower=0> lambda;
}
model {
for (n in 1:N) {
if (y[n] == 0)
target += log_sum_exp(bernoulli_lpmf(1 | theta),
bernoulli_lpmf(0 | theta)
+ poisson_lpmf(y[n] | lambda));
else
target += bernoulli_lpmf(0 | theta)
+ poisson_lpmf(y[n] | lambda);
}
}
'

mod1 = stan(model_code = model_code,
            data = to_stan,
            iter = 1000,
            chains = 2)

model_code = '
data {
  int<lower=0> N;
int<lower=0> K; 
matrix[N, K] x; 
int<lower=0> y[N];
}

parameters {
vector[K] beta_theta;                 //
vector[K] beta_lambda;                 //
real alpha_theta;
real alpha_lambda;
}

transformed parameters {
vector[N] theta ;
vector[N] lambda ;
theta =  inv_logit(alpha_theta + x * beta_theta) ; //<lower=0, upper=1>
lambda =  alpha_lambda + x * beta_lambda ; // how do i make sure that this is >0 ??
}


model {
beta_theta ~ normal(0,1) ; 
beta_lambda ~ normal(0,1) ; 
alpha_theta ~ normal(0,1) ;
alpha_lambda ~ normal(0,1) ;
for(n in 1:N){
if(y[n] == 0)
target += log_sum_exp(bernoulli_lpmf(1 | theta[n]),
bernoulli_lpmf(0 | theta[n])
+ poisson_log_lpmf(y[n] | lambda[n]));
else
target += bernoulli_lpmf(0 | theta[n])
+ poisson_log_lpmf(y[n] | lambda[n]);
}
}

generated quantities{
int<lower=0> y_sim[N];
int zero;
for(n in 1:N){
zero=bernoulli_rng(theta[n]);
y_sim[n]=(1-zero)*poisson_log_rng(lambda[n]);
}
}
'

## GPs

model_code = '
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N,K] X;
  vector[N] y;
  matrix[K,K] B;
  vector[K] nu;
}
parameters {
  real<lower=0> nug;
  real<lower=0> sig_sq;
  real<lower=0> tau_sq;
  vector<lower=0>[K] d1;
  vector<lower=0>[K] d2;
  vector[K] b;
  vector[K] b0;
  cholesky_factor_corr[K] Lcorr;
  vector<lower=0>[K] sigma;
}
model {
  matrix[N,N] Sigma;
  vector[N] mu;
  matrix[N,K] Mu;
  vector[K] d;
  
  for(k in 1:K){
    d1[k] ~ gamma(1,20);
    d2[k] ~ gamma(10,10);
    d[k] = .5*(d1[k] + d2[k]);
  }
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      vector[K] summand;
      for(k in 1:K){
        summand[k] = -pow(X[i,k] - X[j,k],2)/d[k];
      }
      Sigma[i,j] = exp(sum(summand));
      Sigma[j,i] = Sigma[i,j];
    }
  }
  for (i in 1:N){
    for(k in 1:K){
      Mu[i,k] = X[i,k]*b[k];
    }
    mu[i]=sum(Mu[i,1:K]);
  }
  for (i in 1:N)
    Sigma[i,i] = 1 + nug; // + jitter
  
  sig_sq ~ inv_gamma(1,1);
  tau_sq ~ inv_gamma(1,1);
  nug ~ exponential(1);
  
  b0 ~ multi_normal(nu,B);
  sigma ~ cauchy(0,5);
  Lcorr ~ lkj_corr_cholesky(1);
  b ~ multi_normal_cholesky(b0, sig_sq*tau_sq*diag_pre_multiply(sigma,Lcorr));
  y ~ multi_normal(mu,sig_sq*Sigma);
}
'






