data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> RWE;                       // number of combos of region and wd_es (in countries with baseline)
  
  int<lower=1> N;                         // total number of obs
  int<lower=1> N_bl;                      // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
  int<lower=1> N_kbl;                     // number of obs in baseline countries
  
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=T> ktt[KT]; 	        // year for country-year kt ("country-year-year")
  int<lower=1, upper=K> ktk[KT]; 	        // country for country-year kt ("country-year-country")
  int<lower=1, upper=T> kn[K];            // number of observed & interpolated country-years by country
  int<lower=1, upper=KT> kt1[K];          // location of first kt for country k
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=1, upper=WE> wen[N];          // wd_es for observation n
  int<lower=1, upper=KWE> kwen[N];        // kwe for observation n
  int<lower=1, upper=RWE> rwen[N];        // kwe for observation n

  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b;  // baseline gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b_se; // std error of baseline gini for obs n
  
  int<lower=1> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  int<lower=1, upper=K> kkm[M]; 	        // country for rho_we observation m
  int<lower=1, upper=R> rrm[M];           // region for rho_we observation m
  int<lower=1, upper=T> ttm[M];	          // year for rho_we observation m
  int<lower=1, upper=WE> wem[M];          // wd_es for rho_we observation m
  int<lower=1, upper=KWE> kwem[M];        // kwe for rho_we observation m
  int<lower=1, upper=RWE> rwem[M];        // rwe for rho_we observation m
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini given gini_m and gini_m_se
  vector<lower=0>[M] rho_we_t;     // unknown "true" rho_we given rho_we and rho_we_se
  
  vector<lower=0>[S] rho_s;     // ratio of baseline to series s
  real<lower=0> sigma_s; 	      // series noise 

  vector[KWE] rho_kwe_hat;        // estimated rho_kwe by country
  real<lower=0> sigma_kwe;       // rho_kwe noise
  vector[RWE] rho_rwe_hat;        // estimated rho_rwe by region
  real<lower=0> sigma_rwe;       // rho_rwe noise
}

transformed parameters {
  real<lower=0> sigma_rcat;

  sigma_rcat = sqrt(square(sigma_kwe) + square(sigma_rwe));
}

model {
  sigma_gini ~ normal(0, .015);

  gini_t ~ normal(gini_m, gini_m_se);
  rho_we_t ~ normal(rho_we, rho_we_se);
  
  rho_s ~ normal(1, .25);
  rho_kwe_hat ~ normal(1, .2);
  rho_rwe_hat ~ normal(1, .2);
  
  sigma_s ~ cauchy(0, .05);
  sigma_kwe ~ cauchy(0, .05);
  sigma_rwe ~ cauchy(0, .1);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }

  rho_kwe_hat[kwem] ~ normal(rho_we_t, sigma_kwe);  // estimate rho_kwe_hat
  rho_rwe_hat[rwem] ~ normal(rho_we_t, sigma_rwe);  // estimate rho_rwe_hat
 
  gini[kktt[1:N_bl]] ~ normal(gini_b[1:N_bl], gini_b_se[1:N_bl]); // use baseline series where observed
  gini_b[1:N_bl] ~ normal(rho_s[ss[1:N_bl]] .* gini_t[1:N_bl], sigma_s); // estimate rho_s
  gini[kktt[(N_bl+1):N_obl]] ~ normal(gini_t[(N_bl+1):N_obl] .* rho_s[ss[(N_bl+1):N_obl]], sigma_s); // estimate gini with rho_s (for series w/ overlap)
  gini[kktt[(N_obl+1):N_kbl]] ~ normal(rho_kwe_hat[kwen[(N_obl+1):N_kbl]] .* gini_t[(N_obl+1):N_kbl], sigma_kwe); // estimate gini with rho_kwe_hat (for series w/o overlap)
  gini[kktt[(N_kbl+1):N]] ~ normal(rho_rwe_hat[rwen[(N_kbl+1):N]] .* gini_t[(N_kbl+1):N], sigma_rcat);  // estimate gini with rho_rwe_hat (all region ratios for series w/o baseline)
}
