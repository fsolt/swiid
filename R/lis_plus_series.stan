data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=1> S; 				                // number of series
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=K*T> kktt[N];        // country-year for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=0, upper=N> nn[K*T];          // observation for country-year kt
  int<lower=1, upper=T> ktt[K*T]; 	      // year for country-year kt ("country-year-year")
  int<lower=1, upper=K> ktk[K*T]; 	      // country for country-year kt ("country-year-country")
  real<lower=0, upper=1> gini_m[N]; 	    // measured gini for observation n
  real<lower=0, upper=1> gini_m_se[N];    // std error of measured gini for obs n
  real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
  real<lower=0, upper=1> gini_b_se[N_b];  // std error of baseline gini for obs n
}  
  
parameters {
  real<lower=0, upper=1> gini[K*T];       // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)

  real<lower=0, upper=1> gini_t_raw[N];   // centered gini_t
  real<lower=0> rho_s_raw[S];             // centered rho_s
  real<lower=0> sigma_rho_s[S];           // scale for series ratios
  real<lower=0> sigma_s[S]; 	            // series noise
}

transformed parameters {
  real<lower=0, upper=1> gini_t[N];       // unknown "true" gini for obs n given gini_m and gini_m_se
  real<lower=0> rho_s[S];                 // ratio of series s 

  for (n in 1:N) {
    gini_t[n] = gini_m[n] + gini_m_se[n] * gini_t_raw[n]; // decenter
  }
  
  for (s in 1:S) {
    rho_s[s] = 1 + rho_s_raw[s] * sigma_rho_s[s] * .4; // decenter
  }
  
}

model {
  gini_t_raw ~ normal(0, 1);
  rho_s_raw ~ normal(0, 1);
  sigma_rho_s ~ cauchy(0, 1);
  
  for (kt in 1:K*T) {
    if (ktt[kt] == 1) {             // if this country-year is the first year for a country
      gini[kt] ~ normal(.35, .1);   // give it a random draw from N(.35, .1)
    } else {                        // otherwise, give it
      gini[kt] ~ normal(gini[kt-1], sigma_gini[ktk[kt]]); // a random walk from previous year
    }
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
      gini[kktt[n]] ~ normal(gini_b[n], gini_b_se[n]);              // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s[ss[n]]); // estimate rho_s 
    } else {
      gini[kktt[n]] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s[ss[n]]); // estimate gini from rho_s
    }
  }
}
