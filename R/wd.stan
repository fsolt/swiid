data {
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=1> R;                         // number of regions
  
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> KW;                        // number of combos of country and welfare definition
  
  int<lower=1> P;                         // number of observed ratios of baseline to wd (rho_wd)
  int<lower=1, upper=K> kkp[P]; 	        // country for rho_wd observation p
  int<lower=1, upper=R> rrp[P];           // region for rho_wd observation p
  int<lower=1, upper=T> ttp[P];	          // year for rho_wd observation p
  int<lower=1, upper=W> wdp[P];           // wd for rho_wd observation p
  int<lower=1, upper=KW> kwp[P];          // kw for rho_wd observation p
  int<lower=1> WK;                        // number of countries with observed rho_wd
  int<lower=1, upper=WK> wkp[P];          // wk for rho_wd observation p
  real<lower=0> rho_wd[P];                // observed ratio of baseline to wd
  real<lower=0> rho_wd_se[P];             // std error of rho_wd
}

parameters {
  vector<lower=0, upper=1>[P] rho_wd_t;     // unknown "true" rho_wd given rho_wd and rho_wd_se
  
  vector[KW] rho_kw_hat;        // estimated rho_wd by country
  real<lower=0> sigma_kw;       // rho_wd noise
  
  vector[R] rho_rw_hat;         // estimated rho_wd by region
  real<lower=0> sigma_rw;       // rho_rd noise
}

transformed parameters {
  real sigma_krw; 
  
  sigma_krw = sqrt(square(sigma_kw) + square(sigma_rw));
}

model {
  rho_wd_t ~ normal(rho_wd, rho_wd_se);

  rho_kw_hat ~ normal(1, .25);
  rho_rw_hat ~ normal(1, .25);

  for (p in 1:P) {
    rho_kw_hat[kwp[p]] ~ normal(rho_wd_t[p], sigma_kw);  // estimate rho_kw_hat
    rho_rw_hat[rrp[p]] ~ normal(rho_wd_t[p], sigma_rw);  // estimate rho_rw_hat
  }

} 
