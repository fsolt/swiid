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
  int<lower=1, upper=RWE> rwen2[N];       // rwe for region-*baseline_wd*-equiv_sc of observation n
  int<lower=0, upper=KW> kwn[N];          // kw for observation n

  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  
  int<lower=1> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  int<lower=1, upper=R> rrm[M];           // region for rho_we observation m
  int<lower=1, upper=RWE> rwem[M];        // region-welfare_def-equiv_sc for rho_we observation m
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
  
  int<lower=1> P;                         // number of observed ratios of baseline_wd to wd (rho_wd)
  int<lower=1, upper=K> kkp[P]; 	        // country for rho_wd observation p
  int<lower=1, upper=R> rrp[P];           // region for rho_wd observation p
  int<lower=1, upper=W> wdp[P];           // wd for rho_wd observation p
  int<lower=1, upper=KW> kwp[P];          // kw for rho_wd observation p
  real<lower=0> rho_wd[P];                // observed ratio of baseline_wd to wd
  real<lower=0> rho_wd_se[P];             // std error of rho_wd
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];          // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini[K]; 	            // country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini given gini_m and gini_m_se
  vector<lower=.3, upper=1.7>[M] rho_we_t;  // unknown "true" rho_we given rho_we and rho_we_se
  vector<lower=.3, upper=1.7>[P] rho_wd_t;  // unknown "true" rho_wd given rho_wd and rho_wd_se

  vector<lower=.3, upper=1.7>[RWE] rho_rwe_hat; // estimated rho_rwe by country
  real<lower=0, upper=.25> sigma_rwe[R];        // rho_rwe noise
  
  vector<lower=.3, upper=1.7>[KW] rho_kw_hat;   // estimated rho_wd by country
  real<lower=0, upper=.1> sigma_kw;             // rho_kw noise
}

transformed parameters {
  real<lower=0> sigma_krcat[R];
  real<lower=0> sigma_rrcat[R];
  
  for (r in 1:R) {
    sigma_krcat[r] = sqrt(square(sigma_kw) + 2 * square(sigma_rwe[r])); 
    sigma_rrcat[r] = sqrt(2 * square(sigma_kw) + 2 * square(sigma_rwe[r])); 
  }
}

model {
  sigma_gini ~ normal(0, .015);
  sigma_rwe ~ normal(0, .05);

  gini_t ~ normal(gini_m, gini_m_se);
  rho_we_t ~ normal(rho_we, rho_we_se);
  rho_wd_t ~ normal(rho_wd, rho_wd_se);

  rho_rwe_hat ~ normal(1, .25);
  rho_kw_hat ~ normal(1, .25);

  for (k in 1:K) { 
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }

  rho_rwe_hat[rwem] ~ normal(rho_we_t, sigma_rwe[rrm]);  // estimate rho_rwe_hat (over 1:M)
  rho_kw_hat[kwp] ~ normal(rho_wd_t, sigma_kw);   // estimate rho_kw_hat (over 1:P)

  // observations with rho_kw_hat use two-step estimates from rho_kw_hat and rho_rwe_hat
  gini[kktt[1:N_kr]] ~ normal(rho_kw_hat[kwn[1:N_kr]] .* rho_rwe_hat[rwen2[1:N_kr]] .* gini_t[1:N_kr], sigma_krcat[rr[1:N_kr]]);

  // observations without rho_kw_hat use one-step estimates from rho_rwe_hat
  gini[kktt[(N_kr+1):N]] ~ normal(rho_rwe_hat[rwen[(N_kr+1):N]] .* gini_t[(N_kr+1):N], sigma_rrcat[rr[(N_kr+1):N]]);


}
