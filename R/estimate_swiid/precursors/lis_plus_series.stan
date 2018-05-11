data {
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> S; 				                // number of series
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=T> ktt[KT]; 	        // year for country-year kt ("country-year-year")
  int<lower=1, upper=K> ktk[KT]; 	        // country for country-year kt ("country-year-country")
  int<lower=1, upper=T> kn[K];            // number of observed & interpolated country-years by country
  int<lower=1, upper=KT> kt1[K];          // location of first kt for country k
  int<lower=1, upper=S> ss[N];            // series for observation n
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_b] gini_b;   // baseline gini for obs n
  vector<lower=0, upper=1>[N_b] gini_b_se; // std error of baseline gini for obs n
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini[K];            // country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;   
  
  vector<lower=0>[S] rho_s;            
  real<lower=0> sigma_s; 	            // series noise       
}

model {
  sigma_gini ~ normal(0, .015);
  gini_t ~ normal(gini_m, gini_m_se);
  rho_s ~ normal(1, .25);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
      gini[kktt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s 
    } else {
      gini[kktt[n]] ~ normal(gini_t[n] * rho_s[ss[n]], sigma_s); // estimate gini
    }
  }
}
