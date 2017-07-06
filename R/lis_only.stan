data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=0> N;                         // total number of observations
  int<lower=0> N_b;                       // total number of observations with baseline
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  // int<lower=1, upper=K*T> kktt[N];        // country-year for observation n
  // int<lower=1, upper=T> ktt[K*T]; 	      // year for country-year kt ("country-year-year")
  // int<lower=1, upper=K> ktk[K*T]; 	      // country for country-year kt ("country-year-country")
  // real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
  // real<lower=0, upper=1> gini_b_se[N_b];  // baseline gini for obs n
}

parameters {
  real gini_raw[K*T];       // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
}

transformed parameters {
  real gini[K, T];
  vector<lower=0, upper=1>[N] gb_t;       // unknown "true" gini for obs n given gini_b and gini_b_se

  for (k in 1:K) {
    gini[k][1] = .35 + .1 * gini_raw[(k-1)*T+1];    // a random draw for the first year
    for (t in 2:T) {
      gini[k, t] = gini[k, t-1] + sigma_gini[k] * gini_raw[(k-1)*T+t]; // then a random walk from previous year
    }
  }
  
  
}

model {
  to_vector(gini_raw) ~ normal(0, 1);


  // for (n in 1:N) {
  //   if (n <= N_b) {
  //     gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
  //   }
  // }
}
