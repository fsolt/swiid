data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=K*T> kktt[N];        // country-year for observation n
  int<lower=0, upper=N> nn[K*T];          // observation for country-year kt
  int<lower=1, upper=T> ktt[K*T]; 	      // year for country-year kt ("country-year-year")
  int<lower=1, upper=K> ktk[K*T]; 	      // country for country-year kt ("country-year-country")
  real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
  real<lower=0, upper=1> gini_b_se[N_b];  // baseline gini for obs n
}

parameters {
  real<lower=0, upper=1> gini[K*T];    // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
}

model {
  for (kt in 1:K*T) {
    if (ktt[kt] == 1) { // if this country-year is the first year for a country
      gini[kt] ~ normal(.35, .1); // give it a random draw from N(.35, .1)
    } else {            // otherwise
      gini[kt] ~ normal(gini[kt-1], sigma_gini[ktk[kt]]); // a random walk from previous year
    }
  }
  
  for (n in 1:N) {
    if (n <= N_b) {
        gini[kktt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
    }
  }
}
