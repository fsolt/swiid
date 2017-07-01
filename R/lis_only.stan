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
  vector[K*T] gini_raw;    // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
}

transformed parameters {
  real gini[K*T];

  for (kt in 1:K*T) {
    if (nn[kt] != 0) {    // if the baseline gini is observed for this country-year . . .
      gini[kt] = gini_b[nn[kt]] + (gini_b_se[nn[kt]] * gini_raw[kt]); // use it as the estimate
    } else {              // otherwise
      if (ktt[kt] == 1) { // if this country-year is the first year
        gini[kt] = .35 + .1*gini_raw[kt]; // give it a random draw from N(.35, .1)
      } else {            // otherwise
        gini[kt] = gini[kt-1] + (sigma_gini[ktk[kt]] * gini_raw[kt]); // a random walk from previous year
      }
    }
  }
}

model {
  for (kt in 1:K*T) {
    if (nn[kt] == 0) {    // if no gini is observed for this country-year . . .
      if (ktt[kt] == 1) { // if this country-year is the first year for a country
        gini_raw[kt] ~ normal(.35, .1); // give it a random draw from N(.35, .1)
      } else {            // otherwise
        gini_raw[kt] ~ normal(gini[kt-1], sigma_gini[ktk[kt]]); // a random walk from previous year
      }
    }
  }
}
 

