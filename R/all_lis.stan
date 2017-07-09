data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> E;                         // number of equivalence scales
  int<lower=0> N;                         // total number of obs
  int<lower=0> N_bl;                      // total number of obs with baseline
  int<lower=0> N_obl;                     // total number of obs in series with some baseline ("overlap baseline")
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=1, upper=R> rk[K];            // region for country k
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b;   // baseline gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b_se; // std error of baseline gini for obs n
}  
  
parameters {
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.02> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini given gini_m and gini_m_se 
  
  vector<lower=0>[S] rho_s;     // ratio of baseline to series s
  real<lower=0> sigma_s; 	      // series noise 
  
  real beta_rwe_0;              // rho_we population intercept (need to add index)
  real<lower=0> sigma_rwe_e0;   // level 1 (wd_es) error
  real u_0kr[K];                // level 2 (country k) intercept random effect
  real<lower=0> sigma_rwe_u0kr; // level 2 error
  real u_0r[R];                 // level 3 (region r) intercept random effect
  real<lower=0> sigma_rwe_u0r;  // level 3 error
}

transformed parameters  {
 real beta_rwe_0kr[K];            // varying intercepts for level 2 (country k)
 real beta_rwe_0r[R];             // varying intercepts for level 3 (region r)
 real rho_we[N_bl];               // observed ratio of gini_b to gini_m

 rho_we = gini_bt/gini_t

 // define varying intercepts
 for (r in 1:R) {
   beta_rwe_0r[r] = beta_rwe_0 + u_0r[r];             // level 3 (R level-3 random intercepts)
 }
 for (k in 1:K) {
   beta_rwe_0kr[k] = beta_rwe_0r[rk[k]] + u_0kr[k];   // level 2 (K level-2 random intercepts)
 }
}


model {
  gini_t ~ normal(gini_m, gini_m_se);
  gini_bt ~ normal(gini_b, gini_b_se);
  rho_s ~ normal(1, .25);
  u_0r  ~ normal(0, sigma_rwe_u0r);
  u_0kr ~ normal(0, sigma_rwe_u0kr);

  for (k in 1:K) {
    gini[k][1] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
    gini[k][2:T] ~ normal(gini[k][1:T-1], sigma_gini[k]); // otherwise a random walk from previous year 
  }
  
  for (n in 1:N) {
    if (n <= N_bl) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s
      rho_we[n] ~ normal(beta_rwe_0kr[kk[n]], sigma_rwe_e0); // estimate beta_rwe_0kr
    } else if n <= N_obl {
      gini[kk[n]][tt[n]] ~ normal(gini_t[n] * rho_s[ss[n]], sigma_s); // estimate gini for series w overlap
    } else {
      // need to break out obs with in-country data versus only in-region
      gini[kk[n]][tt[n]] ~ beta_rwe_0kr[kk[n]] * gini_t[n], sigma_rwe_e0); // estimate gini for series w/o overlap
    }
  }
  
  for (n in 1:N) {
    if (n <= N_bl) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s
      rho_we[n] ~ normal(beta_rwe_0kr[kk[n]], sigma_rwe_e0); // estimate beta_rwe_0kr
    } else if n <= N_obl {
      gini[kk[n]][tt[n]] ~ normal(gini_t[n] * rho_s[ss[n]], sigma_s); // estimate gini for series w overlap
    } else {
      // need to break out obs with in-country data versus only in-region
      gini[kk[n]][tt[n]] ~ beta_rwe_0kr[kk[n]] * gini_t[n], sigma_rwe_e0); // estimate gini for series w/o overlap
    }
  }
  
  // Estimate ratios (obs in countries in regions [in adv/dev?])
  // loop over obs with lis and disp_sqrt to find sigma?
  
  // loop over observations with gini_disp_sqrt (to find one-step ratio)
  
  // loop over observations with gini_disp_same (to find ratio to disp for market, gross, con)
  
  // loop over observations with gini_same_sqrt (to find ratio to sqrt for hh, pc, oecdm, ae)
  
  // Estimate ginis.  Loop over
  // obs in countries for which rho_disp_sqrt was estimated for that wd_es
  // obs in countries for which rho_disp_same and rho_same_sqrt were estimated for that wd_es 
  // obs with rho_disp_same[k] and rho_same_sqrt[region[k]] for that wd_es
  // obs with rho_disp_same[region[k]] and rho_same_sqrt[k] for that wd_es

}
