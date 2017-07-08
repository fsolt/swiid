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
  
//  vector[N] u;                            // group predictors (wd[ss] and es[ss], but just wd for now)
}  
  
parameters {
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.02> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;   
  
  vector[S] alpha_s;
  vector[S] beta_s;
  real mu_alpha_s;
  real mu_beta_s;
  real<lower=0> sigma_s; 	            // series noise  
  real<lower=0> sigma_alpha_s;
  real<lower=0> sigma_beta_s;
}

model {
  gini_t ~ normal(gini_m, gini_m_se);
  
  mu_alpha_s ~ normal(0, .1);
  mu_beta_s ~ normal(0, .5);
  
  sigma_alpha_s ~ normal(0, .1);
  sigma_beta_s ~ normal(0, .5);
  
  alpha_s ~ normal(mu_alpha_s, sigma_alpha_s);
  beta_s ~ normal(mu_beta_s, sigma_beta_s);

  for (k in 1:K) {
    gini[k][1] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
    gini[k][2:T] ~ normal(gini[k][1:T-1], sigma_gini[k]); // otherwise a random walk from previous year 
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(alpha_s[ss[n]] + beta_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s 
    } else {
      gini[kk[n]][tt[n]] ~ normal(alpha_s[ss[n]] + gini_t[n] * beta_s[ss[n]], sigma_s); // estimate gini
    }
  }

}
