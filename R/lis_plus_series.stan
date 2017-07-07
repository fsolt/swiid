data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=1> S; 				                // number of series
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_b] gini_b;   // baseline gini for obs n
  vector<lower=0, upper=1>[N_b] gini_b_se; // std error of baseline gini for obs n
  
  matrix[S, 2] u;                         // group predictors (wd[ss] and es[ss])
}  
  
parameters {
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.02> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t_raw;   // centered gini_t
  // Cholesky-transformed multilevel model (for obs nested in series; see Stan Ref at 148-150)
  matrix[2, S] z;
  cholesky_factor_corr[2] L_Omega;
  vector<lower=0,upper=pi()/2>[2] tau_unif;
  matrix[2, 2] gamma;                       // group coefs (for wd and es)
  real<lower=0> sigma;                      // prediction error scale
}

transformed parameters {
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini for obs n given gini_m and gini_m_se
  vector<lower=0>[S] rho_s;                 // ratio of series s 
  matrix[N_b, 2] x;                         // ind predictors (constant and gini_t)
  matrix[S, 2] beta;                        // ind coefs
  vector<lower=0>[2] tau;                   // prior scale

  gini_t = gini_m + gini_m_se .* gini_t_raw; // decenter
  for (n in 1:N_b) {
    x[n, 1] = 1;
    x[n, 2] = gini_t[n];
  }
  
  for (i in 1:2) tau[i] = 2.5 * tan(tau_unif[i]);
  beta = u * gamma + (diag_pre_multiply(tau,L_Omega) * z)';
}

model {
  gini_t_raw ~ normal(0, 1);
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  to_vector(gamma) ~ normal(0, 5);
  
  for (k in 1:K) {
    gini[k][1] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
    gini[k][2:T] ~ normal(gini[k][1:T-1], sigma_gini[k]); // otherwise a random walk from previous year 
  }
  
  gini_b ~ normal(rows_dot_product(beta[ss] , x), sigma); // estimate beta
  
  for (n in 1:N) {
    if (n <= N_b) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
    } else {
      gini[kk[n]][tt[n]] ~ normal(beta[ss[n], 1] + gini_t[n] * beta[ss[n], 2], sigma); // estimate gini
    }
  }

}
