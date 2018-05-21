data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> SO;                        // number of series with some baseline ("series [with] overlap") 
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
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=T> kn[K];            // number of observed & interpolated country-years by country
  int<lower=1, upper=KT> kt1[K];          // location of first kt for country k
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=1, upper=WE> wen[N];          // wd_es for observation n
  int<lower=1, upper=KWE> kwen[N];        // kwe for observation n
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
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
}  
  
parameters {
  real<lower=0, upper=1> gini[KT];        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini; 	            // random-walk variance parameter
  vector[N] gini_t;                       // unknown "true" gini given gini_m and gini_m_se

  real beta_0;                // overall average intercept
  real gamma_0w;              // overall average welfare-def slope
  real gamma_0e;              // overall average equiv-scale slope
  
  real dev_k[K];              // deviation for each country (within regions)
  real dev_r[R];              // deviation for each region

  real dev_w[W];              // deviation for each welfare-def
  real dev_e[E];              // deviation for each equiv-scale
  real dev_rw[RW];            // deviation for each region--welfare-def
  real dev_re[RE];            // deviation for each region--equiv-scale
  real dev_kw[KW];            // deviation for each country--welfare-def
  real dev_ke[KE];            // deviation for each country--equiv-scale
  real dev_kwe[KWE];          // deviation for each country--welfare-def--equiv-scale
  real dev_s[S];              // deviation for each series (within kwe)
  
  real<lower=0> sigma_k;      // sd for the country deviations in alpha_0
  real<lower=0> sigma_r; 	    // sd for the region deviations in alpha_0

  real<lower=0> sigma_w;      // sd for the w deviations
  real<lower=0> sigma_e;      // sd for the e deviations
  real<lower=0> sigma_rw;     // sd for the rw deviations
  real<lower=0> sigma_re;     // sd for the re deviations
  real<lower=0> sigma_kw;     // sd for the kw deviations
  real<lower=0> sigma_ke;     // sd for the ke deviations
  real<lower=0> sigma_kwe;    // sd for the kwe deviations
  real<lower=0> sigma_s; 	    // sd for the series deviations 
  real<lower=0> sigma_0;      // observation errors
  
  vector[S-SO] beta_1s_tilde;         // estimated series-specific slope for series without overlap
}

transformed parameters {
  vector[K] beta_0k;
  vector[R] beta_0r;

  vector[W] gamma_w;
  vector[E] gamma_e;
  vector[RW] gamma_rw;
  vector[RE] gamma_re;
  vector[KW] gamma_kw;
  vector[KE] gamma_ke;
  vector[KWE] beta_1kwe;          // varying slope for country--welfare-def--equiv-scale
  vector[SO] beta_1s;             // varying slope for series (within kwe)

  // varying intercepts for regions
  for (r in 1:r) {
    beta_0r[r] = beta_0 + dev_r[r];
  }
  
  // varying intercepts for countries 
  for (k in 1:K) {
    beta_0k[k] = beta_0r[rk[k]] + dev_k[k];
  }

  // varying slope components for welfare-defs 
  for (w in 1:W) {
    gamma_w[w] = gamma_0w + dev_w[w]; 
  }
  
  // varying slope components for equiv-scales
  for (e in 1:E) {
    gamma_e[e] = gamma_0e + dev_e[e]; 
  }

  // varying slope components for region--welfare-defs 
  for (rw in 1:RW) {
    gamma_rw[rw] = gamma_w[wrw[rw]] + dev_rw[rw]; 
  }
  
  // varying slope components for region--equiv-scales
  for (re in 1:RE) {
    gamma_re[re] = gamma_e[ere[re]] + dev_re[re]; 
  }

  // varying slope components for country--welfare-defs 
  for (kw in 1:KW) {
    gamma_kw[kw] = gamma_rw[rwkw[kw]] + dev_kw[kw]; 
  }
  
  // varying slope components for country--equiv-scales
  for (ke in 1:KE) {
    gamma_ke[ke] = gamma_re[reke[ke]] + dev_ke[ke]; 
  }
 
  // varying slopes for country--welfare-def--equiv-scales 
  for (kwe in 1:KWE) {
    beta_1kwe[kwe] = gamma_kw[kwkwe[kwe]] * gamma_ke[kekwe[kwe]] + dev_kwe[kwe];
  }
  
  // varying slopes for series s (only series with overlap)
  for (so in 1:SO) {
    beta_1s[so] = beta_1kwe[kwes[so]] + dev_s[so];
  }
}

model {
  // priors
  sigma_gini ~ normal(0, .05);
  
  sigma_r ~ normal(0, .05);
  sigma_k ~ normal(0, .04);

  sigma_w ~ normal(0, .03);
  sigma_e ~ normal(0, .03);
  sigma_rw ~ normal(0, .03); 
  sigma_re ~ normal(0, .03); 
  sigma_kw ~ normal(0, .03); 
  sigma_ke ~ normal(0, .03); 
  sigma_kwe ~ normal(0, .03);  
  sigma_s ~ normal(0, .03);

  beta_0 ~ normal(.35, .1);
  gamma_0w ~ normal(1, .25);
  gamma_0e ~ normal(1, .1);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N() in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini); // otherwise a random walk from previous year 
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N()
    }
  }

  // estimation of gini_t
  gini_m ~ normal(gini_t, gini_m_se);  
  
  // distribution of varying intercepts
  dev_r ~ normal(0, sigma_r);
  dev_k ~ normal(0, sigma_k);
  
  dev_w ~ normal(0, sigma_w);
  dev_e ~ normal(0, sigma_e);
  
  dev_rw ~ normal(0, sigma_rw);
  dev_re ~ normal(0, sigma_re);
  dev_kw ~ normal(0, sigma_kw);
  dev_ke ~ normal(0, sigma_ke);
  
  dev_kwe ~ normal(0, sigma_kwe);
  dev_s ~ normal(0, sigma_s);
  
  // estimate series-specific slope for series with overlap
  gini_b[(N_ibl+1):N_wbl] ~ normal(beta_0k[kk[(N_ibl+1):N_wbl]] + beta_1s[ss[(N_ibl+1):N_wbl]] .* gini_t[(N_ibl+1):N_wbl], sigma_e);
  
  // estimate series-specific slope for series without overlap
  beta_1s_tilde[(SO+1):S] ~ normal(beta_1kwe[kwes[(SO+1):S]], sigma_s);
  
  // predict gini
  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); // use baseline series where observed
  
  gini[kktt[(N_wbl+1):N_obl]] ~ normal(beta_0k[kk[(N_wbl+1):N_obl]] + beta_1s[ss[(N_wbl+1):N_obl]] .* gini_t[(N_wbl+1):N_obl], sigma_0); // estimate gini with rho_s (for series with overlap)
  
  gini[kktt[(N_obl+1):N]] ~ normal(beta_0k[kk[(N_obl+1):N]] + beta_1s_tilde[ss[(N_obl+1):N]] .* gini_t[(N_obl+1):N], sigma_0); // estimate gini with rho_s (for series without overlap) 
}
