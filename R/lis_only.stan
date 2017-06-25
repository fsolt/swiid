data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
  real<lower=0, upper=1> gini_b_se[N_b];  // baseline gini for obs n
}  
parameters {
  real<lower=0, upper=1> gini[K, T];    // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
}
model {
  for (n in 1:N) {
    if (tt[n] == 1) {
      gini[kk[n], tt[n]] ~ normal(.4, .2) 
    } else {
      gini[kk[n], tt[n]] ~ normal(gini[kk[n], yy[n-1]], sigma_gini[kk[n]])
    }
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
        gini[cc[n], yy[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
    }
  }
}

