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
  vector<lower=0, upper=1>[N_wbl] gini_b;  // baseline gini for obs n
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

  int<lower=1> P;                         // number of observations of each wd to baseline_wd with constant es
  int<lower=1, upper=K> kkp[P];           // country for observation p
  int<lower=1, upper=KW> kwp[P];          // country--welfare-def for observation p
  vector<lower=0, upper=1>[P] blp;        // baseline gini for observation p
  vector<lower=0, upper=1>[P] gini_p;     // observed gini for observation p
}  

parameters {
  real<lower=0, upper=1> gini[KT];        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini; 	            // random-walk variance parameter
  vector[N] gini_t;                       // unknown "true" gini given gini_n and gini_n_se

  real beta_0;                // overall average intercept
  real gamma_0w;              // overall average welfare-def slope
  real gamma_0e;              // overall average equiv-scale slope

  real dev_0r[R];             // deviation for each region
  real dev_0k[K];             // deviation for each country (within regions)

  real dev_1w[W];             // deviation for each welfare-def
  real dev_1e[E];             // deviation for each equiv-scale
  real dev_1rw[RW];           // deviation for each region--welfare-def
  real dev_1re[RE];           // deviation for each region--equiv-scale
  real dev_1kw[KW];           // deviation for each country--welfare-def
  real dev_1ke[KE];           // deviation for each country--equiv-scale
  real dev_1kwe[KWE];         // deviation for each country--welfare-def--equiv-scale
  real dev_1s[S];             // deviation for each series (within kwe)
          
  real<lower=0> sigma_0r; 	  // sd for the region deviations in alpha_0
  real<lower=0> sigma_0k;     // sd for the country deviations in alpha_0

  real<lower=0> sigma_1w;     // sd for the w deviations
  real<lower=0> sigma_1e;     // sd for the e deviations
  real<lower=0> sigma_1rw;    // sd for the rw deviations
  real<lower=0> sigma_1re;    // sd for the re deviations
  real<lower=0> sigma_1kw;    // sd for the kw deviations
  real<lower=0> sigma_1ke;    // sd for the ke deviations
  real<lower=0> sigma_1kwe;   // sd for the kwe deviations
  real<lower=0> sigma_1s;     // sd for the series deviations 

  real<lower=0> sigma_00e;    // observation errors for country--equiv-scales
  real<lower=0> sigma_00w;    // observation errors in country--welfare-defs
  real<lower=0> sigma_00s;    // observation errors in series
  
  vector[SN] beta_1s_tilde;   // estimated series-specific slope for series without overlap
}

transformed parameters {
  vector[R] beta_0r;          // varying intercept for region
  vector[K] beta_0k;          // varying intercept for country

  vector[W] gamma_1w;         // varying slope component for welfare-def
  vector[E] gamma_1e;         // varying slope component for equiv-scale
  vector[RW] gamma_1rw;       // varying slope component for region--welfare-def
  vector[RE] gamma_1re;       // varying slope component for region--equiv-scale
  vector[KW] gamma_1kw;       // varying slope component for country--welfare-def
  vector[KE] gamma_1ke;       // varying slope component for country--equiv-scale
  vector[KWE] beta_1kwe;      // varying slope for country--welfare-def--equiv-scale
  vector[SO] beta_1s;         // varying slope for series (within kwe)

  // varying intercepts for regions
  for (r in 1:R) {
    beta_0r[r] = beta_0 + dev_0r[r];
  }
  
  // varying intercepts for countries 
  for (k in 1:K) {
    beta_0k[k] = beta_0r[rk[k]] + dev_0k[k];
  }

  // varying slope components for welfare-defs 
  for (w in 1:W) {
    gamma_1w[w] = gamma_0w + dev_1w[w]; 
  }
  
  // varying slope components for equiv-scales
  for (e in 1:E) {
    gamma_1e[e] = gamma_0e + dev_1e[e]; 
  }

  // varying slope components for region--welfare-defs 
  for (rw in 1:RW) {
    gamma_1rw[rw] = gamma_1w[wrw[rw]] + dev_1rw[rw]; 
  }
  
  // varying slope components for region--equiv-scales
  for (re in 1:RE) {
    gamma_1re[re] = gamma_1e[ere[re]] + dev_1re[re]; 
  }

  // varying slope components for country--welfare-defs 
  for (kw in 1:KW) {
    gamma_1kw[kw] = gamma_1rw[rwkw[kw]] + dev_1kw[kw]; 
  }
  
  // varying slope components for country--equiv-scales
  for (ke in 1:KE) {
    gamma_1ke[ke] = gamma_1re[reke[ke]] + dev_1ke[ke]; 
  }
 
  // varying slopes for country--welfare-def--equiv-scales 
  for (kwe in 1:KWE) {
    beta_1kwe[kwe] = gamma_1kw[kwkwe[kwe]] * gamma_1ke[kekwe[kwe]] + dev_1kwe[kwe];
  }
  
  // varying slopes for series s (only series with overlap)
  for (so in 1:SO) {
    beta_1s[so] = beta_1kwe[kwes[so]] + dev_1s[so];
  }
}

model {
  // priors
  sigma_gini ~ normal(0, .01);
  
  sigma_0r ~ normal(0, .04);
  sigma_0k ~ normal(0, .02);

  sigma_1w ~ normal(0, .07);
  sigma_1e ~ normal(0, .07);
  sigma_1rw ~ normal(0, .06); 
  sigma_1re ~ normal(0, .06); 
  sigma_1kw ~ normal(0, .05); 
  sigma_1ke ~ normal(0, .05); 
  sigma_1kwe ~ normal(0, .1);  
  sigma_1s ~ normal(0, .03);

  sigma_00e ~ normal(0, .02);
  sigma_00w ~ normal(0, .02);
  sigma_00s ~ normal(0, .02);

  beta_0 ~ normal(0, .1);
  gamma_0w ~ normal(1, .25);
  gamma_0e ~ normal(1, .25);
  
  beta_1s_tilde ~ normal(1, .25);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N() in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N()
    }
  }

  // estimation of gini_t
  gini_n ~ normal(gini_t, gini_n_se);  
  
  // distribution of varying intercepts
  dev_0r ~ normal(0, sigma_0r);
  dev_0k ~ normal(0, sigma_0k);
  
  dev_1w ~ normal(0, sigma_1w);
  dev_1e ~ normal(0, sigma_1e);
  dev_1rw ~ normal(0, sigma_1rw);
  dev_1re ~ normal(0, sigma_1re);
  dev_1kw ~ normal(0, sigma_1kw);
  dev_1ke ~ normal(0, sigma_1ke);
  dev_1kwe ~ normal(0, sigma_1kwe);
  dev_1s ~ normal(0, sigma_1s);
  
  // estimate series-specific slope for series with overlap
  gini_b[(N_ibl+1):N_wbl] ~ normal(beta_0k[kk[(N_ibl+1):N_wbl]] + beta_1s[ss[(N_ibl+1):N_wbl]] .* gini_t[(N_ibl+1):N_wbl], sigma_00s);
  
  // estimate country--equiv-scale deviations
  blm[1:M] ~ normal(beta_0k[kkm[1:M]] + gamma_1ke[kem[1:M]] .* gini_m[1:M], sigma_00e);
  
  // estimate country--welfare-def deviations
  blp[1:P] ~ normal(beta_0k[kkp[1:P]] + gamma_1kw[kwp[1:P]] .* gini_p[1:P], sigma_00w);

  // estimate series-specific slope for series without overlap but with ke and kw
  beta_1s_tilde[1:SN] ~ normal(beta_1kwe[kwes[SO+1:SN]], sigma_00s);  
  
  // // estimate varying slope components for country--equiv-scales for series without ke
  // gamma_1ke_tilde[1:XXX] ~ normal(gamma_1re[reke[kekwe[kwes[XXX]]]], sigma_1ke);
  
  // // estimate varying slope components for country--welfare-defs for series without kw
  // gamma_1kw_tilde[1:XXX] ~ normal(gamma_1rw[rwkw[kwkwe[kwes[XXX]]]], sigma_1kw);
  
  // beta_1kwe_tilde ~ normal(gamma_1kw * gamma_1ke_tilde, sigma_1kwe)
  // beta_1kwe_tilde ~ normal(gamma_1kw_tilde * gamma_1ke, sigma_1kwe)
  // beta_1kwe_tilde ~ normal(gamma_1kw_tilde * gamma_1ke_tilde, sigma_1kwe)
  
  // beta_1s_tilde ~ normal(beta_1kwe_tilde[kwes[SN+1:S]], sigma_00s);
  
  //// predict gini
  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); // use baseline series where observed
  
  gini[kktt[(N_wbl+1):N_obl]] ~ normal(beta_0k[kk[(N_wbl+1):N_obl]] + beta_1s[ss[(N_wbl+1):N_obl]] .* gini_t[(N_wbl+1):N_obl], sigma_00s); // estimate gini with rho_s (for series with overlap)
  
  gini[kktt[(N_obl+1):N]] ~ normal(beta_0k[kk[(N_obl+1):N]] + beta_1s_tilde[(sn[1:N_nbl])] .* gini_t[(N_obl+1):N], sigma_00s); // estimate gini with rho_s (for series without overlap) 
}
