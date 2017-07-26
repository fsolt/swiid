data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> RWE;                       // number of combos of region and wd_es (in countries with baseline)
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> KW;                        // number of combos of country and welfare definition

  int<lower=1> N;                         // total number of obs
  int<lower=1> N_kr;                      // number of obs with ratios to baseline welfare_def 

  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=T> ktt[KT]; 	        // year for country-year kt ("country-year-year")
  int<lower=1, upper=K> ktk[KT]; 	        // country for country-year kt ("country-year-country")
  int<lower=1, upper=T> kn[K];            // number of observed & interpolated country-years by country
  int<lower=1, upper=KT> kt1[K];          // location of first kt for country k
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=WE> wen[N];          // wd_es for observation n
  int<lower=1, upper=KWE> kwen[N];        // kwe for observation n
  int<lower=1, upper=RWE> rwen[N];        // rwe for region-welfare_def-equiv_sc of observation n
  int<lower=0, upper=RWE> rwen2[N];       // rwe for region-*baseline_wd*-equiv_sc of observation n
  int<lower=0, upper=KW> kwn[N];          // kw for observation n

  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  
  int<lower=1> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  int<lower=1, upper=R> rrm[M];           // region for rho_we observation m
  int<lower=1, upper=RWE> rwem[M];        // region-welfare_def-equiv_sc for rho_we observation m
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];          // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.03> sigma_gini[K];   // country variance parameter (see Linzer and Stanton 2012, 12)
  real<lower=.005, upper=.015> msg;         // hyperprior for mean of sigma_gini
  real<lower=.001, upper=.007> ssg;         // hyperprior for scale of sigma_gini
  vector<lower=0.1, upper=0.8>[N] gini_t;   // unknown "true" gini given gini_m and gini_m_se
  vector<lower=.3, upper=1.7>[M] rho_we_t;  // unknown "true" rho_we given rho_we and rho_we_se

  vector<lower=.3, upper=1.7>[RWE] rho_rwe_hat; // estimated rho_rwe by country
  real<lower=0, upper=.1> sigma_rwe[R];         // rho_rwe noise
  real<lower=.02, upper=.04> msrwe;               // hyperprior for mean of sigma_rwe
  real<lower=.01, upper=.02> ssrwe;               // hyperprior for scale of sigma_rwe
}

transformed parameters {
  real<lower=0> sigma_rrcat[R];
  
  for (r in 1:R) {
    sigma_rrcat[r] = 4 * square(sigma_rwe[r]); 
  }
}

model {
  sigma_gini ~ normal(msg, ssg);
  sigma_rwe ~ normal(msrwe, ssrwe);

  gini_m ~ normal(gini_t, gini_m_se);
  rho_we ~ normal(rho_we_t, rho_we_se);

  rho_rwe_hat ~ normal(1, .25);

  for (k in 1:K) { 
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }

  rho_rwe_hat[rwem] ~ normal(rho_we_t, sigma_rwe[rrm]);  // estimate rho_rwe_hat (over 1:M)

  // use one-step estimates from rho_rwe_hat
  gini[kktt] ~ normal(rho_rwe_hat[rwen] .* gini_t, sigma_rrcat[rr]);
}
