data {
  int k;
  int n;
  int x[n];
  vector[n] y;
}
parameters {
  vector[k] theta;
  vector[n] mu;
}
model {
    y ~ normal(mu, 1);
    mu ~ normal(theta[x], 1);
    theta ~ normal(0, 2);
}
