data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series (in countries with baseline)
  int<lower=1> WE;                        // number of combos of welfare def and eq scale ("wd_es")
  int<lower=1> KWE;                       // number of combos of country and wd_es (in countries with baseline)
  int<lower=1> W;                         // number of welfare definitions
  int<lower=1> KW;                        // number of combos of country and welfare definition
  int<lower=1> RW;                        // number of combos of region and welfare definition
  int<lower=1> E;                         // number of equivalence scales
  int<lower=1> KE;                        // number of combos of country and equivalence scale
  int<lower=1> RE;                        // number of combos of region and equivalence scale
  
  int<lower=1> N;                         // total number of obs
  int<lower=1> N_bl;                      // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
  int<lower=1> N_kbl;                     // number of obs in baseline countries
  int<lower=1> N_kk;                      // last n of obs with ratios to both baseline welfare def and equiv scale
  int<lower=1> N_kr;                      // last n of obs with ratios to baseline welfare def only
  int<lower=1> N_rk;                      // last n of obs with ratios to baseline equiv scale only
  
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
  int<lower=0, upper=KW> kwn[N];          // kw for observation n
  int<lower=0, upper=KE> ken[N];          // ke for observation n
  int<lower=1, upper=RW> rwn[N];          // rw for observation n
  int<lower=1, upper=RE> ren[N];          // re for observation n
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
  
  int<lower=1> P;                         // number of observed ratios of baseline to wd (rho_wd)
  int<lower=1, upper=K> kkp[P]; 	        // country for rho_wd observation p
  int<lower=1, upper=R> rrp[P];           // region for rho_wd observation p
  int<lower=1, upper=T> ttp[P];	          // year for rho_wd observation p
  int<lower=1, upper=W> wdp[P];           // wd for rho_wd observation p
  int<lower=1, upper=KW> kwp[P];          // kw for rho_wd observation p
  int<lower=1, upper=RW> rwp[P];          // rw for rho_wd observation p
  real<lower=0> rho_wd[P];                // observed ratio of baseline to wd
  real<lower=0> rho_wd_se[P];             // std error of rho_wd
  
  int<lower=1> Q;                         // number of observed ratios of baseline to es (rho_es)
  int<lower=1, upper=K> kkq[Q]; 	        // country for rho_es observation q
  int<lower=1, upper=R> rrq[Q];           // region for rho_es observation q
  int<lower=1, upper=T> ttq[Q];	          // year for rho_es observation q
  int<lower=1, upper=E> esq[Q];           // es for rho_es observation q
  int<lower=1, upper=KE> keq[Q];          // ke for rho_es observation q
  int<lower=1, upper=RE> req[Q];          // re fir rho_es observation q
  real<lower=0> rho_es[Q];                // observed ratio of baseline to es
  real<lower=0> rho_es_se[Q];             // std error of rho_es
}  
  
parameters {
<<<<<<< HEAD
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
||||||| merged common ancestors
  row_vector<lower=0, upper=1>[T] gini[K];  // SWIID gini estimate of baseline in country k at time t
  real<lower=0, upper=.02> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
=======
  real<lower=0, upper=1> gini[KT];          // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini[K]; 	            // country variance parameter (see Linzer and Stanton 2012, 12)
>>>>>>> kt
  vector<lower=0, upper=1>[N] gini_t;       // unknown "true" gini given gini_m and gini_m_se
  vector<lower=.3, upper=1.7>[M] rho_we_t;  // unknown "true" rho_we given rho_we and rho_we_se
  vector<lower=.3, upper=1.7>[P] rho_wd_t;     // unknown "true" rho_wd given rho_wd and rho_wd_se
  vector<lower=.3, upper=1.7>[Q] rho_es_t;     // unknown "true" rho_es given rho_es and rho_es_se
  
  vector<lower=0>[S] rho_s;                 // ratio of baseline to series s
  real<lower=0, upper=.1> sigma_s; 	        // series noise 

  vector<lower=.3, upper=1.7>[KWE] rho_kwe_hat; // estimated rho_kwe by country
  real<lower=0, upper=.1> sigma_kwe;            // rho_kwe noise
  
  vector<lower=.3, upper=1.7>[KW] rho_kw_hat;   // estimated rho_wd by country
  real<lower=0, upper=.1> sigma_kw;             // rho_kw noise
  vector<lower=.3, upper=1.7>[RE] rho_rw_hat;   // estimated rho_wd by region
  real<lower=0, upper=.1> sigma_rw;             // rho_rw noise
  
  vector<lower=.3, upper=1.7>[KE] rho_ke_hat;   // estimated rho_es by country
  real<lower=0, upper=.1> sigma_ke;             // rho_ke noise
  vector<lower=.3, upper=1.7>[RE] rho_re_hat;   // estimated rho_es by region
  real<lower=0, upper=.1> sigma_re;             // rho_re noise
}

transformed parameters {
  real<lower=0> sigma_kkcat;
  real<lower=0> sigma_krcat;
  real<lower=0> sigma_rkcat;
  real<lower=0> sigma_rrcat;
  
  sigma_kkcat = sqrt(square(sigma_kw) + square(sigma_ke));
  sigma_krcat = sqrt(square(sigma_kw) + sqrt(square(sigma_re) + square(sigma_ke)));
  sigma_rkcat = sqrt(sqrt(square(sigma_rw) + square(sigma_kw)) + square(sigma_ke));
  sigma_rrcat = sqrt(sqrt(square(sigma_rw) + square(sigma_kw)) + sqrt(square(sigma_re) + square(sigma_ke)));
}

model {
  sigma_gini ~ normal(0, .015);

  gini_t ~ normal(gini_m, gini_m_se);
  rho_we_t ~ normal(rho_we, rho_we_se);
  rho_wd_t ~ normal(rho_wd, rho_wd_se);
  rho_es_t ~ normal(rho_es, rho_es_se);

  rho_s ~ normal(1, .25);
  rho_kwe_hat ~ normal(1, .25);
  rho_kw_hat ~ normal(1, .25);
  rho_rw_hat ~ normal(1, .25);
  rho_ke_hat ~ normal(1, .25);
  rho_re_hat ~ normal(1, .25);

  for (k in 1:K) {
    if (kn[k] > 1) {
      gini[kt1[k]] ~ normal(.35, .1);                         // a random draw from N(.35, .1) in first year
      gini[(kt1[k]+1):(kt1[k]+kn[k]-1)] ~ normal(gini[(kt1[k]):(kt1[k]+kn[k]-2)], sigma_gini[k]); // otherwise a random walk from previous year
    } else {
      gini[kt1[k]] ~ normal(.35, .1);                            // a random draw from N(.35, .1)
    }
  }

  rho_kwe_hat[kwem] ~ normal(rho_we_t, sigma_kwe);  // estimate rho_kwe_hat

  rho_rw_hat[rwp] ~ normal(rho_wd_t, sigma_rw);   // estimate rho_rw_hat
  rho_kw_hat[kwp] ~ normal(rho_wd_t, sigma_kw);   // estimate rho_kw_hat

  rho_re_hat[req] ~ normal(rho_es_t, sigma_re);   // estimate rho_re_hat
  rho_ke_hat[keq] ~ normal(rho_es_t, sigma_ke);   // estimate rho_ke_hat

  gini[kktt[1:N_bl]] ~ normal(gini_b[1:N_bl], gini_b_se[1:N_bl]); // use baseline series where observed
  gini_b[1:N_bl] ~ normal(rho_s[ss[1:N_bl]] .* gini_t[1:N_bl], sigma_s); // estimate rho_s
  gini[kktt[(N_bl+1):N_obl]] ~ normal(gini_t[(N_bl+1):N_obl] .* rho_s[ss[(N_bl+1):N_obl]], sigma_s); // estimate gini with rho_s (for series w/ overlap)
  gini[kktt[(N_obl+1):N_kbl]] ~ normal(rho_kwe_hat[kwen[(N_obl+1):N_kbl]] .* gini_t[(N_obl+1):N_kbl], sigma_kwe); // estimate gini with rho_we_hat (for series w/o overlap)
  gini[kktt[(N_kbl+1):N_kk]] ~ normal(rho_kw_hat[kwn[(N_kbl+1):N_kk]] .* rho_ke_hat[ken[(N_kbl+1):N_kk]] .* gini_t[(N_kbl+1):N_kk], sigma_kkcat); // estimate gini with rho_kw_hat & rho_ke_hat (all country ratios)
  gini[kktt[(N_kk+1):N_kr]] ~ normal(rho_kw_hat[kwn[(N_kk+1):N_kr]] .* rho_re_hat[ren[(N_kk+1):N_kr]] .* gini_t[(N_kk+1):N_kr], sigma_krcat); // estimate gini with rho_kw_hat & rho_re_hat (country & region ratios)
  gini[kktt[(N_kr+1):N_rk]] ~ normal(rho_rw_hat[rwn[(N_kr+1):N_rk]] .* rho_ke_hat[ken[(N_kr+1):N_rk]] .* gini_t[(N_kr+1):N_rk], sigma_rkcat); // estimate gini with rho_rw_hat & rho_ke_hat (region & country ratios)
  gini[kktt[(N_rk+1):N]] ~ normal(rho_rw_hat[rwn[(N_rk+1):N]] .* rho_re_hat[ren[(N_rk+1):N]] .* gini_t[(N_rk+1):N], sigma_rrcat);  // estimate gini with rho_rw_hat & rho_re_hat (all region ratios)
}
