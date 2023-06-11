data {
  int<lower=1> N;
  int<lower=0> k;
  int y[N];
  //int<lower=0> pregnancies[N];
  int<lower=0> glucose[N];
  real<lower=0> bmi[N];
  real<lower=0> diabetes_pedigree_func[N];
  int<lower=0> age[N];
}


parameters {
  vector[k] beta;
  real<lower=0> sigma;
}


model {
  target += normal_lpdf(beta[1] | -20.76, 46.63);    // intercept
  //target += normal_lpdf(beta[2] | 0.33, 0.04);       // pregnancies
  target += normal_lpdf(beta[3] | 0.04, 0.01);       // glucose
  target += normal_lpdf(beta[4] | 0.13, 0.02);       // bmi
  target += normal_lpdf(beta[5] | 0.79, 7.2);        // diabetes_pedigree_func
  target += normal_lpdf(beta[6] | 0.01, 0.005);       // age

  for (nn in 1:N)
  target += bernoulli_logit_lpmf(y[nn] | beta[1] + beta[3] * glucose[nn] + beta[4] * bmi[nn] + beta[5] * diabetes_pedigree_func[nn] + beta[6] * age[nn]);
}
