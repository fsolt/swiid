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
  
//  vector<lower=0, upper=1>[N] gini_t_raw;   // centered gini_t
  vector<lower=0>[S] rho_s;             // centered rho_s
  real<lower=0> sigma_s; 	            // series noise       
}

// transformed parameters {
//   vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini for obs n given gini_m and gini_m_se
//   vector<lower=0>[S] rho_s;                 // ratio of series s 
//   
//  gini_t = gini_m + gini_m_se .* gini_t_raw; // decenter
//  rho_s = 1 + rho_s_raw .* (sigma_rho_s * .4);  // decenter
// }

model {
  gini_t ~ normal(gini_m, gini_m_se);
  rho_s ~ normal(1, .25);

  for (k in 1:K) {
    gini[k][1] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
    gini[k][2:T] ~ normal(gini[k][1:T-1], sigma_gini[k]); // otherwise a random walk from previous year 
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s 
    } else {
      gini[kk[n]][tt[n]] ~ normal(gini_t[n] * rho_s[ss[n]], sigma_s); // estimate gini
    }
  }

}
