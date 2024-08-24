data{
  int<lower=1> K;     		                // number of countries
  int<lower=1> T;     		                // (maximum) number of years
  int<lower=1> KT;                        // number of observed & interpolated country-years
  int<lower=1> R;                         // number of regions
  int<lower=1> S; 				                // number of series that overlap baseline (in countries with baseline)
  int<lower=1> SKT;                       // number of series-country-years (in countries with baseline)
  int<lower=1> KWE;                       // number of combos of country and welfare def and eq scale ("wd_es")
  int<lower=1> RWE;                       // number of combos of region and wd_es
  int<lower=1> KW;                        // number of combos of country and welfare definition

  int<lower=1> N;                         // total number of obs
  int<lower=1> N_ibl;                     // number of baseline obs ("is baseline")
  int<lower=1> N_wbl;                     // number of obs with baseline
  int<lower=1> N_obl;                     // number of obs in series with some baseline ("overlap baseline")
  int<lower=1> N_bk;                      // number of obs in countries with some baseline data ("baseline countries")
  int<lower=1> N_kw;                      // number of obs with in-country ratios to baseline welfare_def 

  array[N] int<lower=1, upper=K> kk; 	          // country for observation n
  array[N] int<lower=1, upper=KT> kktt;         // country-year for observation n
  array[N] int<lower=1, upper=R> rr;            // region for observation n
  array[N] int<lower=1, upper=SKT> skt;         // series-country-year for observation n
  array[N] int<lower=1, upper=S> ss;            // series for observation n
  array[N] int<lower=1, upper=KWE> kwen;        // country-wd_es for observation n
  array[N] int<lower=0, upper=KW> kwn;          // kw for observation n
  array[N] int<lower=1, upper=RWE> rwen;        // rwe for region-welfare_def-equiv_sc of observation n
  array[N] int<lower=1, upper=RWE> rwen2;       // rwe for region-*baseline_wd*-equiv_sc of observation n
  
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b; // baseline gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b_se; // std error of baseline gini for obs n
  
  array[K] int<lower=0, upper=1> bk;            // baseline availability indicator for country k
  array[K] int<lower=1, upper=KT> kt_k_start;   // location of first kt for country k
  array[K] int<lower=1, upper=KT> kt_k_end;     // location of last kt for country k
  array[KT] int<lower=0, upper=N_ibl> nbkt;     // obs n with baseline for country-year kt
  
  int<lower=1> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  array[M] int<lower=1, upper=R> rrm;           // region for rho_we observation m
  array[M] int<lower=1, upper=KWE> kwem;        // country-wd_es for rho_we observation m
  array[M] int<lower=1, upper=RWE> rwem;        // region-wd_es for rho_we observation m
  array[M] real<lower=0> rho_we;                // observed ratio of baseline to wd_es
  array[M] real<lower=0> rho_we_se;             // std error of rho_we
  
  int<lower=1> P;                         // number of observed ratios of baseline_wd to wd (rho_w)
  array[P] int<lower=1, upper=KW> kwp;          // kw for rho_w observation p
  array[P] real<lower=0> rho_w;                 // observed ratio of baseline_wd to wd
  array[P] real<lower=0> rho_w_se;              // std error of rho_w
  
  real prior_m_s;                         // mean for lognormal prior 
  real<lower=0> prior_s_s;                // sigma for lognormal prior
  array[KWE] real prior_m_kwe;                  // vector of means for lognormal prior that vary by welfare definition 
  array[KWE] real<lower=0> prior_s_kwe;         // vector of sigmas for lognormal prior that vary by welfare definition
  array[RWE] real prior_m_rwe;                  // vector of means for lognormal prior that vary by welfare definition 
  array[RWE] real<lower=0> prior_s_rwe;         // vector of sigmas for lognormal prior that vary by welfare definition
  array[KW] real prior_m_kw;                    // vector of means for lognormal prior that vary by welfare definition 
  array[KW] real<lower=0> prior_s_kw;           // vector of sigmas for lognormal prior that vary by welfare definition
}
  
parameters {
  array[KT] real<lower=0, upper=1> gini;        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini; 	            // random-walk variance parameter
  vector[N] gini_t;                       // unknown "true" gini given gini_m and gini_m_se
  vector[N_wbl] gini_b_t;                 // unknown "true" gini given gini_b and gini_b_se
  vector[M] rho_we_t;                     // unknown "true" rho_we given rho_we and rho_we_se
  vector[P] rho_w_t;                      // unknown "true" rho_wd given rho_w and rho_w_se
  
  vector<lower=0>[S] rho_s;             // ratio of baseline to series s
  real<lower=0> sigma_s; 	                // series noise 
  
  vector<lower=0>[KWE] rho_kwe_hat;       // estimated rho_we by country
  real<lower=0> sigma_kwe;                // rho_kwe_hat noise
  
  vector<lower=0>[KW] rho_kw_hat;         // estimated rho_w by country
  real<lower=0> sigma_kw;                 // rho_kw_hat noise

  vector<lower=0>[RWE] rho_rwe_hat;       // estimated rho_we by region
  array[R] real<lower=0> sigma_rwe;             // rho_rwe_hat noise (by region)
}

transformed parameters {
  array[R] real<lower=0> sigma_krcat;
  array[R] real<lower=0> sigma_rrcat;

  for (r in 1:R) {
    sigma_krcat[r] = sqrt(square(sigma_kw) + square(sigma_rwe[r]));
    sigma_rrcat[r] = sqrt(2 * square(sigma_rwe[r]));
  }
}

model {
  sigma_gini ~ normal(.01, .0025);
  sigma_s ~ normal(.01, .0025);
  sigma_kwe ~ normal(0, .05);
  for (r in 1:R) {
    sigma_rwe[r] ~ normal(.04, .015) T[0,];
  }
  sigma_kw ~ normal(.01, .0025);

  rho_s ~ lognormal(prior_m_s, prior_s_s);
  rho_kwe_hat ~ lognormal(prior_m_kwe, prior_s_kwe);
  rho_rwe_hat ~ lognormal(prior_m_rwe, prior_s_rwe);
  rho_kw_hat ~ lognormal(prior_m_kw, prior_s_kw);

  gini_m ~ normal(gini_t, gini_m_se);
  gini_b ~ normal(gini_b_t, gini_b_se);
  rho_we ~ normal(rho_we_t, rho_we_se);
  rho_w ~ normal(rho_w_t, rho_w_se);
  
  for (k in 1:K) {
    if (nbkt[kt_k_start[k]] == 0) {                              // if first year has no baseline observation:
      gini[kt_k_start[k]] ~ lognormal(-1, .25);                  // first year is a random draw,
    } else {                                              // but if first year has a baseline observation:
      gini[kt_k_start[k]] ~ normal(gini_b[nbkt[kt_k_start[k]]], gini_b_se[nbkt[kt_k_start[k]]]); // use the baseline
    }
    if (kt_k_start[k] != kt_k_end[k]) {                   // if more than one year:
      // after first year, a random walk
      gini[(kt_k_start[k]+1):kt_k_end[k]] ~ normal(gini[kt_k_start[k]:(kt_k_end[k]-1)], sigma_gini);
    }
  }

  gini_b_t[N_ibl+1:N_wbl] ~ normal(rho_s[ss[N_ibl+1:N_wbl]] .* gini_t[N_ibl+1:N_wbl], sigma_s); // estimate rho_s for obs with baseline
  rho_kwe_hat[kwem] ~ normal(rho_we_t, sigma_kwe);            // estimate rho_kwe_hat (over 1:M)
  rho_rwe_hat[rwem] ~ normal(rho_we_t, sigma_rwe[rrm]);       // estimate rho_rwe_hat (over 1:M)
  rho_kw_hat[kwp] ~ normal(rho_w_t, sigma_kw);                // estimate rho_kw_hat (over 1:P)

  // obs w/ baseline use baseline
  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); 
  
  // obs in countries w/ baseline in series w/ overlap use rho_s
  gini[kktt[(N_wbl+1):N_obl]] ~ normal(gini_t[(N_wbl+1):N_obl] .* rho_s[ss[(N_wbl+1):N_obl]], sigma_s); 
  
  // obs in countries w/ baseline in series w/o overlap use rho_kwe_hat
  gini[kktt[(N_obl+1):N_bk]] ~ normal(rho_kwe_hat[kwen[(N_obl+1):N_bk]] .* gini_t[(N_obl+1):N_bk], sigma_kwe);
  
  // obs in countries w/o baseline but w/ rho_w use rho_kw_hat for wd adj and then rho_rwe_hat for es adj
  gini[kktt[N_bk:N_kw]] ~ normal(rho_kw_hat[kwn[N_bk:N_kw]] .* rho_rwe_hat[rwen2[N_bk:N_kw]] .* gini_t[N_bk:N_kw], sigma_krcat[rr[N_bk:N_kw]]);

  // obs in countries w/o rho_w use one-step estimates from rho_rwe_hat
  gini[kktt[(N_kw+1):N]] ~ normal(rho_rwe_hat[rwen[(N_kw+1):N]] .* gini_t[(N_kw+1):N], sigma_rrcat[rr[(N_kw+1):N]]);

}
