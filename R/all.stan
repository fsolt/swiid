data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T; 				                // number of years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> KW;                        // number of combos of country and welfare definition
  int<lower=1> E;                         // number of equivalence scales
  int<lower=1> KE;                        // number of combos of country and equivalence scale
  int<lower=1> N;                         // total number of obs
  int<lower=1> N_bl;                      // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
  int<lower=1> N_obl;
  int<lower=1> N_obl;
  int<lower=1> N_obl;
  int<lower=1> N_obl;
  
  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1, upper=T> tt[N]; 	          // year for observation n
  int<lower=1, upper=S> ss[N];            // series for observation n
  int<lower=1, upper=WE> wen[N];          // wd_es for observation n
  int<lower=1, upper=KWE> kwen[N];        // kwe for observation n
  int<lower=1, upper=KW> kwn[N];          // kw for observation n
  int<lower=1, upper=KE> ken[N];          // ke for observation n
  int<lower=1, upper=R> rk[K];            // region for country k
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b;  // baseline gini for obs n
  vector<lower=0, upper=1>[N_bl] gini_b_se; // std error of baseline gini for obs n
  
  int<lower=0> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  int<lower=1, upper=K> kkm[M]; 	        // country for rho_we observation m
  int<lower=1, upper=R> rrm[M];           // region for rho_we observation m
  int<lower=1, upper=T> ttm[M];	          // year for rho_we observation m
  int<lower=1, upper=WE> wem[M];          // wd_es for rho_we observation m
  int<lower=1, upper=KWE> kwem[M];        // kwe for rho_we observation m
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
  
  int<lower=0> P;                         // number of observed ratios of baseline to wd (rho_wd)
  int<lower=1, upper=K> kkp[P]; 	        // country for rho_wd observation p
  int<lower=1, upper=R> rrp[P];           // region for rho_wd observation p
  int<lower=1, upper=T> ttp[P];	          // year for rho_wd observation p
  int<lower=1, upper=W> wdp[P];           // wd for rho_wd observation p
  int<lower=1, upper=KW> kwp[P];          // kw for rho_wd observation p
  real<lower=0> rho_wd[P];                // observed ratio of baseline to wd
  real<lower=0> rho_wd_se[P];             // std error of rho_wd
  
  int<lower=0> Q;                         // number of observed ratios of baseline to es (rho_es)
  int<lower=1, upper=K> kkq[Q]; 	        // country for rho_es observation p
  int<lower=1, upper=R> rrq[Q];           // region for rho_es observation p
  int<lower=1, upper=T> ttq[Q];	          // year for rho_es observation p
  int<lower=1, upper=E> esq[Q];           // es for rho_es observation p
  int<lower=1, upper=KE> keq[Q];          // ke for rho_es observation p
  real<lower=0> rho_es[Q];                // observed ratio of baseline to es
  real<lower=0> rho_es_se[Q];             // std error of rho_es
}  
  
parameters {
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.02> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini given gini_m and gini_m_se
  vector<lower=0, upper=1>[M] rho_we_t;     // unknown "true" rho_we given rho_we and rho_we_se
  vector<lower=0, upper=1>[P] rho_wd_t;     // unknown "true" rho_wd given rho_wd and rho_wd_se
  vector<lower=0, upper=1>[Q] rho_es_t;     // unknown "true" rho_es given rho_es and rho_es_se
  
  vector<lower=0>[S] rho_s;     // ratio of baseline to series s
  real<lower=0> sigma_s; 	      // series noise 
  
  vector[KWE] rho_we_hat;       // estimated rho_we 
  real<lower=0> sigma_we;       // rho_we noise
  
  vector[KW] rho_wd_hat;        // estimated rho_wd 
  real<lower=0> sigma_wd;       // rho_wd noise
  
  vector[KE] rho_es_hat;        // estimated rho_es 
  real<lower=0> sigma_es;       // rho_es noise

  // 
  // vector[K] a;  
  // real beta_rwe_0[WE];              // rho_we population intercept (need to add index)
  // real<lower=0> sigma_rwe_e0[WE];   // level 1 (wd_es) error
  // real u_0kr[K, WE];                // level 2 (country k) intercept random effect
  // real<lower=0> sigma_rwe_u0kr[WE]; // level 2 error
  // real u_0r[R, WE];                 // level 3 (region r) intercept random effect
  // real<lower=0> sigma_rwe_u0r[WE];  // level 3 error
}

transformed parameters  {
  // real beta_rwe_0kr[K, WE];            // varying intercepts for level 2 (country k)
  // real beta_rwe_0r[R, WE];             // varying intercepts for level 3 (region r)
  // real<lower=0> rho_we_hat[WE]; // predicted rho_we in country k for wd_es we
  // 
  // for (m in 1:M) {
  //   rho_we_hat[wem[m]] = rho_we_t[m] + a[kkm[m]];
  // }
 // define varying intercepts
 // for (r in 1:R) {
 //   beta_rwe_0r[r] = beta_rwe_0 + u_0r[r];             // level 3 (R level-3 random intercepts)
 // }
 // for (k in 1:K) {
 //   beta_rwe_0kr[k] = beta_rwe_0r[rk[k]] + u_0kr[k];   // level 2 (K level-2 random intercepts)
 // }
}


model {
  gini_t ~ normal(gini_m, gini_m_se);
  rho_we_t ~ normal(rho_we, rho_we_se);
  
  rho_s ~ normal(1, .25);
  rho_we_hat ~ normal(1, .25);
  rho_wd_hat ~ normal(1, .25);
  rho_es_hat ~ normal(1, .25);
  
  // u_0r  ~ normal(0, sigma_rwe_u0r);
  // u_0kr ~ normal(0, sigma_rwe_u0kr);

  for (k in 1:K) {
    gini[k][1] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
    gini[k][2:T] ~ normal(gini[k][1:T-1], sigma_gini[k]); // otherwise a random walk from previous year 
  }
  
  for (m in 1:M) {
    rho_we_hat[kwem[m]] ~ normal(rho_we_t[m], sigma_we);  // estimate rho_we_hat
  }
  
  for (p in 1:P) {
    rho_wd_hat[kwp[p]] ~ normal(rho_wd_t[p], sigma_wd);  // estimate rho_wd_hat
  }
  
  for (q in 1:Q) {
    rho_es_hat[keq[q]] ~ normal(rho_es_t[q], sigma_es);  // estimate rho_es_hat
  }
  
  for (n in 1:N) {
    if (n <= N_bl) {
      gini[kk[n]][tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      gini_b[n] ~ normal(rho_s[ss[n]] * gini_t[n], sigma_s); // estimate rho_s
    } else if (n <= N_obl) {
      gini[kk[n]][tt[n]] ~ normal(gini_t[n] * rho_s[ss[n]], sigma_s); // estimate gini for series with overlap
    } else if LISCOUNTRIES {
      gini[kk[n]][tt[n]] ~ normal(rho_we_hat[kwen[n]] * gini_t[n], sigma_we); // estimate gini for series w/o overlap
    } else if COUNTRIES_WITH_KW_AND_KE {
      gini[kk[n]][tt[n]] ~ normal(rho_wd_hat[kw[n]] * rho_es_hat[ke[n]] * gini_t[n], sigma_we); // estimate gini for series w/o overlap
    // } else if COUNTRIES_WITH_KW_ONLY {
    //   
    // } else if COUNTRIES_WITH_KE_ONLY {
    //   
    // } else {
    //   
    }
  }
}
