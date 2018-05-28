data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> SO;                        // number of series with some baseline ("series [with] overlap")
  int<lower=1> SN;                        // number of series without baseline ("series no overlap")
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> KW;                        // number of combos of country and welfare-def (in countries with baseline)
  int<lower=1> KE;                        // number of combos of country and equiv-scale (in countries with baseline)
  int<lower=1> RW;                        // number of combos of region and welfare-def (in countries with baseline)
  int<lower=1> RE;                        // number of combos of region and equiv-scale (in countries with baseline)
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> E;                         // number of equivalence scales
  
  int<lower=1> N;                         // total number of obs
  int<lower=1> N_ibl;                     // number of baseline obs ("is baseline")
  int<lower=1> N_wbl;                     // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
  int<lower=1> N_nbl;                     // number of obs in series without baseline ("no baseline")
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=T> kn[K];            // number of observed & interpolated country-years by country
  int<lower=1, upper=KT> kt1[K];          // location of first kt for country k
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=1, upper=SN> sn[N_nbl];       // series without overlap for observation nbl
  int<lower=1, upper=WE> wen[N];          // wd_es for observation n
  int<lower=1, upper=KWE> kwen[N];        // kwe for observation n
  vector<lower=0, upper=1>[N] gini_n; 	  // observed gini for observation n
  vector<lower=0, upper=1>[N] gini_n_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b; // baseline gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b_se; // std error of baseline gini for obs n

  int<lower=1, upper=R> rk[K];            // region for country k

  int<lower=1, upper=W> wrw[RW];          // welfare-def for rwe  
  int<lower=1, upper=E> ere[RE];          // equiv-scale for rwe 
  int<lower=1, upper=RW> rwkw[KW];        // region--welfare-def for kw  
  int<lower=1, upper=RE> reke[KE];        // region--equiv-scale for ke 
  int<lower=0, upper=KW> kwkwe[KWE];      // country--welfare-def for kwe
  int<lower=1, upper=KE> kekwe[KWE];      // country--equiv-scale for kwe
  int<lower=1, upper=KWE> kwes[S];        // country--welfare-def--equiv-scale for s
  
  int<lower=1> M;                         // number of observations of each es to baseline_es with constant wd
  int<lower=1, upper=K> kkm[M];           // country for observation m
  int<lower=1, upper=KE> kem[M];          // country--equiv-scale for observation m
  vector<lower=0, upper=1>[M] blm;        // baseline gini for observation m
  vector<lower=0, upper=1>[M] gini_m;     // observed gini for observation m
  vector<lower=0, upper=1>[M] blm_se;     // std error of baseline gini for observation m
  vector<lower=0, upper=1>[M] gini_m_se;  // std error of observed gini for observation m

  int<lower=1> P;                         // number of observations of each wd to baseline_wd with constant es
  int<lower=1, upper=K> kkp[P];           // country for observation p
  int<lower=1, upper=KW> kwp[P];          // country--welfare-def for observation p
  vector<lower=0, upper=1>[P] blp;        // baseline gini for observation p
  vector<lower=0, upper=1>[P] gini_p;     // observed gini for observation p
  vector<lower=0, upper=1>[P] blp_se;     // std error of baseline gini for observation p
  vector<lower=0, upper=1>[P] gini_p_se;  // std error of observed gini for observation p
}  

parameters {
  real<lower=0, upper=1> gini[KT];  // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini; 	      // random-walk variance parameter
  vector[N_wbl] gini_bt;            // unknown "true" baseline gini given gini_b and gini_b_se
  vector[N] gini_nt;                // unknown "true" gini given gini_n and gini_n_se
  vector[M] blm_t;                  // unknown "true" baseline gini given blm and blm_se
  vector[M] gini_mt;                // unknown "true" gini given gini_m and gini_m_se
  vector[P] blp_t;                  // unknown "true" baseline gini given blp and blp_se
  vector[P] gini_pt;                // unknown "true" gini given gini_p and gini_p_se

  real gamma_0_0rw;           // overall average region--welfare-def intercept
  real gamma_0_0re;           // overall average region--equiv-scale intercept
  real gamma_0_1rw;           // overall average region--welfare-def slope
  real gamma_0_1re;           // overall average region--equiv-scale slope
  
  real dev_0rw[RW];           // deviation for intercept of each region--welfare-def
  real dev_0re[RE];           // deviation for intercept of each region--equiv-scale
  real dev_0kw[KW];           // deviation for intercept of each country--welfare-def
  real dev_0ke[KE];           // deviation for intercept of each country--equiv-scale
  real dev_0kwe[KWE];         // deviation for intercept of each country--welfare-def--equiv-scale
  real dev_0s[S];             // deviation for intercept of each series (within kwe)
  
  real dev_1rw[RW];           // deviation for slope of each region--welfare-def
  real dev_1re[RE];           // deviation for slope of each region--equiv-scale
  real dev_1kw[KW];           // deviation for slope of each country--welfare-def
  real dev_1ke[KE];           // deviation for slope of each country--equiv-scale
  real dev_1kwe[KWE];         // deviation for slope of each country--welfare-def--equiv-scale
  real dev_1s[S];             // deviation for slope of each series (within kwe)

  real<lower=0> sigma_k;      // sd for the country deviations in alpha_0
  real<lower=0> sigma_r; 	    // sd for the region deviations in alpha_0

  real<lower=0> sigma_0rw;    // sd for the rw intercept deviations
  real<lower=0> sigma_0re;    // sd for the re intercept deviations
  real<lower=0> sigma_0kw;    // sd for the kw intercept deviations
  real<lower=0> sigma_0ke;    // sd for the ke intercept deviations
  real<lower=0> sigma_0kwe;   // sd for the kwe intercept deviations
  real<lower=0> sigma_0s; 	  // sd for the series intercept deviations 

  real<lower=0> sigma_1w;     // sd for the w slope deviations
  real<lower=0> sigma_1e;     // sd for the e slope deviations
  real<lower=0> sigma_1rw;    // sd for the rw slope deviations
  real<lower=0> sigma_1re;    // sd for the re slope deviations
  real<lower=0> sigma_1kw;    // sd for the kw slope deviations
  real<lower=0> sigma_1ke;    // sd for the ke slope deviations
  real<lower=0> sigma_1kwe;   // sd for the kwe slope deviations
  real<lower=0> sigma_1s; 	  // sd for the series slope deviations 

  real<lower=0> sigma_00e;    // observation errors for country--equiv-scales
  real<lower=0> sigma_00w;    // observation errors in country--welfare-defs
  real<lower=0> sigma_00s;    // observation errors in series
  
  vector[SN] beta_0s_tilde;   // estimated series intercept for series without overlap
  vector[SN] beta_1s_tilde;   // estimated series slope for series without overlap
}

transformed parameters {
  vector[RW] gamma_0rw;       // varying intercept component for region--welfare-def
  vector[RE] gamma_0re;       // varying intercept component for region--equiv-scale
  vector[KW] gamma_0kw;       // varying intercept component for country--welfare-def
  vector[KE] gamma_0ke;       // varying intercept component for country--equiv-scale
  vector[KWE] beta_0kwe;      // varying intercept for country--welfare-def--equiv-scale
  vector[SO] beta_0s;         // varying intercept for series (within kwe)

  vector[RW] gamma_1rw;       // varying slope component for region--welfare-def
  vector[RE] gamma_1re;       // varying slope component for region--equiv-scale
  vector[KW] gamma_1kw;       // varying slope component for country--welfare-def
  vector[KE] gamma_1ke;       // varying slope component for country--equiv-scale
  vector[KWE] beta_1kwe;      // varying slope for country--welfare-def--equiv-scale
  vector[SO] beta_1s;         // varying slope for series (within kwe)

  // varying intercept and slope components for region--welfare-defs 
  for (rw in 1:RW) {
    gamma_0rw[rw] = gamma_0_0rw + dev_0rw[rw];
    gamma_1rw[rw] = gamma_0_1rw + dev_1rw[rw];
  }
  
  // varying intercept and slope components for region--equiv-scales
  for (re in 1:RE) {
    gamma_0re[re] = gamma_0_0re + dev_0re[re];
    gamma_1re[re] = gamma_0_1re + dev_1re[re];
  }

  // varying intercept and slope components for country--welfare-defs 
  for (kw in 1:KW) {
    gamma_0kw[kw] = gamma_0rw[rwkw[kw]] + dev_0kw[kw];
    gamma_1kw[kw] = gamma_1rw[rwkw[kw]] + dev_1kw[kw];
  }
  
  // varying intercepts and slope components for country--equiv-scales
  for (ke in 1:KE) {
    gamma_0ke[ke] = gamma_0re[reke[ke]] + dev_0ke[ke];
    gamma_1ke[ke] = gamma_1re[reke[ke]] + dev_1ke[ke]; 
  }
 
  // varying intercepts and slopes for country--welfare-def--equiv-scales 
  for (kwe in 1:KWE) {
    beta_0kwe[kwe] = gamma_0kw[kwkwe[kwe]] + gamma_0ke[kekwe[kwe]] + dev_0kwe[kwe];
    beta_1kwe[kwe] = gamma_1kw[kwkwe[kwe]] * gamma_1ke[kekwe[kwe]] + dev_1kwe[kwe];
  }
  
  // varying intercepts and slopes for series s (only series with overlap)
  for (so in 1:SO) {
    beta_0s[so] = beta_0kwe[kwes[so]] + dev_0s[so];
    beta_1s[so] = beta_1kwe[kwes[so]] + dev_1s[so];
  }
}

model {
  // priors
  sigma_gini ~ normal(0, .01);
  
  sigma_0rw ~ normal(0, .06); 
  sigma_0re ~ normal(0, .06); 
  sigma_0kw ~ normal(0, .05); 
  sigma_0ke ~ normal(0, .05); 
  sigma_0kwe ~ normal(0, .05);  
  sigma_0s ~ normal(0, .03);

  sigma_1rw ~ normal(0, .06); 
  sigma_1re ~ normal(0, .06); 
  sigma_1kw ~ normal(0, .05); 
  sigma_1ke ~ normal(0, .05); 
  sigma_1kwe ~ normal(0, .1);  
  sigma_1s ~ normal(0, .03);

  sigma_00e ~ normal(0, .02);
  sigma_00w ~ normal(0, .02);
  sigma_00s ~ normal(0, .02);

  gamma_0_0rw ~ normal(0, .1);
  gamma_0_0re ~ normal(0, .1);
  gamma_0_1rw ~ normal(1, .25);
  gamma_0_1re ~ normal(1, .25);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N() in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N()
    }
  }

  // estimation of "true" ginis
  gini_b ~ normal(gini_bt, gini_b_se);
  gini_n ~ normal(gini_nt, gini_n_se);
  blm ~ normal(blm_t, blm_se);
  gini_m ~ normal(gini_mt, gini_m_se);
  blp ~ normal(blp_t, blp_se);
  gini_p ~ normal(gini_pt, gini_p_se);

  
  // distribution of varying intercepts
  dev_0rw ~ normal(0, sigma_0rw);
  dev_0re ~ normal(0, sigma_0re);
  dev_0kw ~ normal(0, sigma_0kw);
  dev_0ke ~ normal(0, sigma_0ke);
  dev_0kwe ~ normal(0, sigma_0kwe);
  dev_0s ~ normal(0, sigma_0s);
  
  dev_1rw ~ normal(0, sigma_1rw);
  dev_1re ~ normal(0, sigma_1re);
  dev_1kw ~ normal(0, sigma_1kw);
  dev_1ke ~ normal(0, sigma_1ke);
  dev_1kwe ~ normal(0, sigma_1kwe);
  dev_1s ~ normal(0, sigma_1s);
  
  // estimate series-specific slope for series with overlap
  gini_bt[(N_ibl+1):N_wbl] ~ normal(beta_0s[ss[(N_ibl+1):N_wbl]] + beta_1s[ss[(N_ibl+1):N_wbl]] .* gini_nt[(N_ibl+1):N_wbl], sigma_00s);
  
  // estimate country--equiv-scale deviations
  blm_t ~ normal(gamma_0ke[kem] + gamma_1ke[kem] .* gini_mt, sigma_00e);
  
  // estimate country--welfare-def deviations
  blp_t ~ normal(gamma_0kw[kwp] + gamma_1kw[kwp] .* gini_pt, sigma_00w);
  
  // estimate series-specific slope for series without overlap
  beta_0s_tilde[1:SN] ~ normal(beta_0kwe[kwes[SO+1:SN]], sigma_0s);
  beta_1s_tilde[1:SN] ~ normal(beta_1kwe[kwes[SO+1:SN]], sigma_1s);
  
  // predict gini
  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); // use baseline series where observed
  
  gini[kktt[(N_wbl+1):N_obl]] ~ normal(beta_0s[ss[(N_wbl+1):N_obl]] + beta_1s[ss[(N_wbl+1):N_obl]] .* gini_nt[(N_wbl+1):N_obl], sigma_00s); // estimate gini for series with overlap
  
  gini[kktt[(N_obl+1):N]] ~ normal(beta_0s_tilde[sn[1:N_nbl]] + beta_1s_tilde[sn[1:N_nbl]] .* gini_nt[(N_obl+1):N], sigma_00s); // estimate gini for series without overlap 
}
