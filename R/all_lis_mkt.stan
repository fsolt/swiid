data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> E;                         // number of equivalence scales
  int<lower=1> N;                         // total number of obs
  int<lower=1> N_ibl;                     // number of baseline obs ("is baseline")
  int<lower=1> N_bl;                      // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
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
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];          // SWIID gini estimate for baseline in country k at time t
  real<lower=0, upper=.1> sigma_gini[K]; 	  // country variance parameter (see Linzer and Stanton 2012, 12)
  real<lower=1, upper=2> asg;             // hyperprior for shape of sigma_gini
  real<lower=70, upper=110> bsg;             // hyperprior for rate of sigma_gini
  vector<lower=.1, upper=.8>[N] gini_t;     // unknown "true" gini given gini_m and gini_m_se
  vector<lower=.3, upper=1.7>[M] rho_we_t;  // unknown "true" rho_we given rho_we and rho_we_se
  
  vector<lower=0>[S] rho_s;                 // ratio of baseline to series s
  real<lower=0, upper=.1> sigma_s; 	        // series noise 
  
  vector<lower=.3, upper=1.7>[KWE] rho_kwe_hat;  // estimated rho_kwe by country
  real<lower=0, upper=.1> sigma_kwe;         // rho_we noise
}

model {
  sigma_gini ~ gamma(asg, bsg);
 
  gini_m ~ normal(gini_t, gini_m_se);
  rho_we ~ normal(rho_we_t, rho_we_se);
  
  rho_s ~ normal(1, .25);
  rho_kwe_hat ~ normal(1, .25);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }
  
  rho_kwe_hat[kwem] ~ normal(rho_we_t, sigma_kwe);  // estimate rho_kwe_hat

  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); // use baseline series where observed
  gini_b[N_ibl+1:N_bl] ~ normal(rho_s[ss[N_ibl+1:N_bl]] .* gini_t[N_ibl+1:N_bl], sigma_s); // estimate rho_s
  gini[kktt[(N_bl+1):N_obl]] ~ normal(gini_t[(N_bl+1):N_obl] .* rho_s[ss[(N_bl+1):N_obl]], sigma_s); // estimate gini with rho_s (for series w/ overlap)
  gini[kktt[(N_obl+1):N]] ~ normal(rho_kwe_hat[kwen[(N_obl+1):N]] .* gini_t[(N_obl+1):N], sigma_kwe); // estimate gini with rho_we_hat (one-step, for wdes w/ overlap)
}
