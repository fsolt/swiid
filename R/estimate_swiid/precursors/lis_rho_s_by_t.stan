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

  int<lower=1, upper=K> kk[N]; 	          // country for observation n
  int<lower=1, upper=KT> kktt[N];         // country-year for observation n
  int<lower=1, upper=R> rr[N];            // region for observation n
  int<lower=1> ss[N];                     // series for observation n
  int<lower=1, upper=SKT> skt[N];         // series-country-year for observation n
  int<lower=1, upper=KWE> kwen[N];        // country-wd_es for observation n
  int<lower=0, upper=KW> kwn[N];          // kw for observation n
  int<lower=1, upper=RWE> rwen[N];        // rwe for region-welfare_def-equiv_sc of observation n
  int<lower=1, upper=RWE> rwen2[N];       // rwe for region-*baseline_wd*-equiv_sc of observation n
  
  vector<lower=0, upper=1>[N] gini_m; 	  // measured gini for observation n
  vector<lower=0, upper=1>[N] gini_m_se;  // std error of measured gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b; // baseline gini for obs n
  vector<lower=0, upper=1>[N_wbl] gini_b_se; // std error of baseline gini for obs n
  
  int<lower=0, upper=1> bk[K];            // baseline availability indicator for country k
  int<lower=1, upper=KT> kt_k_start[K];   // location of first kt for country k
  int<lower=1, upper=KT> kt_k_end[K];     // location of last kt for country k
  int<lower=0, upper=N_ibl> nbkt[KT];     // obs n with baseline for country-year kt
  
  int<lower=1, upper=T> sn[S];            // number of observed & interpolated country-years by series
  int<lower=0, upper=1> shnoo[S];         // indicator for whether series has non-overlapping observations
  int<lower=0> s_bl_obs[S];               // number of series observations with baseline
  int<lower=1, upper=SKT> skt1[S];        // location of first skt for series s
  
  int<lower=1> M;                         // number of observed ratios of baseline to wd_es (rho_we)
  int<lower=1, upper=R> rrm[M];           // region for rho_we observation m
  int<lower=1, upper=KWE> kwem[M];        // country-wd_es for rho_we observation m
  int<lower=1, upper=RWE> rwem[M];        // region-wd_es for rho_we observation m
  real<lower=0> rho_we[M];                // observed ratio of baseline to wd_es
  real<lower=0> rho_we_se[M];             // std error of rho_we
  
  int<lower=1> P;                         // number of observed ratios of baseline_wd to wd (rho_w)
  int<lower=1, upper=KW> kwp[P];          // kw for rho_w observation p
  real<lower=0> rho_w[P];                 // observed ratio of baseline_wd to wd
  real<lower=0> rho_w_se[P];              // std error of rho_w
  
  real prior_m_s;                         // mean for lognormal prior 
  real<lower=0> prior_s_s;                // sigma for lognormal prior
  real prior_m_kwe[KWE];                  // vector of means for lognormal prior that vary by welfare definition 
  real<lower=0> prior_s_kwe[KWE];         // vector of sigmas for lognormal prior that vary by welfare definition
  real prior_m_rwe[RWE];                  // vector of means for lognormal prior that vary by welfare definition 
  real<lower=0> prior_s_rwe[RWE];         // vector of sigmas for lognormal prior that vary by welfare definition
  real prior_m_kw[KW];                    // vector of means for lognormal prior that vary by welfare definition 
  real<lower=0> prior_s_kw[KW];           // vector of sigmas for lognormal prior that vary by welfare definition
}
  
parameters {
  real<lower=0, upper=1> gini[KT];        // SWIID gini estimate for baseline in country k at time t
  real<lower=0> sigma_gini; 	            // random-walk variance parameter
  vector[N] gini_t;                       // unknown "true" gini given gini_m and gini_m_se
  vector[N_wbl] gini_b_t;                 // unknown "true" gini given gini_b and gini_b_se
  vector[M] rho_we_t;                     // unknown "true" rho_we given rho_we and rho_we_se
  // vector[P] rho_w_t;                      // unknown "true" rho_wd given rho_w and rho_w_se
  
  vector<lower=0>[SKT] rho_s;             // ratio of baseline to series s
  real<lower=0> sigma_s0;                 // random-walk variance parameter
  real<lower=0> sigma_s; 	                // series noise 
  
  vector<lower=0>[KWE] rho_kwe_hat;       // estimated rho_we by country
  real<lower=0> sigma_kwe;                // rho_kwe_hat noise
  
  // vector<lower=0>[KW] rho_kw_hat;         // estimated rho_w by country
  // real<lower=0> sigma_kw;                 // rho_kw_hat noise
  // 
  // vector<lower=0>[RWE] rho_rwe_hat;       // estimated rho_we by region
  // real<lower=0> sigma_rwe[R];             // rho_rwe_hat noise (by region)
}

// transformed parameters {
//   real<lower=0> sigma_krcat[R];
//   real<lower=0> sigma_rrcat[R];
//   
//   for (r in 1:R) {
//     sigma_krcat[r] = sqrt(square(sigma_kw) + square(sigma_rwe[r])); 
//     sigma_rrcat[r] = sqrt(2 * square(sigma_rwe[r])); 
//   }
// }

model {
  sigma_gini ~ normal(.01, .0025);
  sigma_s0 ~ normal(.01, .0025);
  sigma_s ~ normal(.01, .0025);
  sigma_kwe ~ normal(0, .05);
  // for (r in 1:R) {
  //   sigma_rwe[r] ~ normal(.04, .015) T[0,];
  // }
  // sigma_kw ~ normal(0, .01);

  rho_s ~ lognormal(prior_m_s, prior_s_s);
  rho_kwe_hat ~ lognormal(prior_m_kwe, prior_s_kwe);
//  rho_rwe_hat ~ lognormal(prior_m_rwe, prior_s_rwe);
//  rho_kw_hat ~ lognormal(prior_m_kw, prior_s_kw);

  gini_m ~ normal(gini_t, gini_m_se);
  gini_b ~ normal(gini_b_t, gini_b_se);
  rho_we ~ normal(rho_we_t, rho_we_se);
//  rho_w ~ normal(rho_w_t, rho_w_se);
  
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

  for (s in 1:S) {        // for each series
    if (shnoo[s] == 1) {  // check if series has 3+ overlapping obs and some non-overlapping obs
      if (s_bl_obs[s] > 1) {
        rho_s[skt1[s]] ~ lognormal(prior_m_s, prior_s_s);
        rho_s[(skt1[s]+1):(skt1[s]+sn[s]-1)] ~ normal(rho_s[(skt1[s]):(skt1[s]+sn[s]-2)], sigma_s0);
      }
    }
  }

  gini_b_t[N_ibl+1:N_wbl] ~ normal(rho_s[skt[N_ibl+1:N_wbl]] .* gini_t[N_ibl+1:N_wbl], sigma_s); // estimate rho_s
  rho_kwe_hat[kwem] ~ normal(rho_we_t, sigma_kwe);            // estimate rho_kwe_hat (over 1:M)
  // rho_rwe_hat[rwem] ~ normal(rho_we_t, sigma_rwe[rrm]);       // estimate rho_rwe_hat (over 1:M)
  // rho_kw_hat[kwp] ~ normal(rho_w_t, sigma_kw);                // estimate rho_kw_hat (over 1:P)

  // obs w/ baseline use baseline
  gini[kktt[1:N_ibl]] ~ normal(gini_b[1:N_ibl], gini_b_se[1:N_ibl]); 
  
  // obs in countries w/ baseline in series w/ overlap use rho_s
  gini[kktt[(N_wbl+1):N_obl]] ~ normal(gini_t[(N_wbl+1):N_obl] .* rho_s[skt[(N_wbl+1):N_obl]], sigma_s); 
  
  // obs in countries w/ baseline in series w/o overlap use rho_kwe_hat
  gini[kktt[(N_obl+1):N_bk]] ~ normal(rho_kwe_hat[kwen[(N_obl+1):N_bk]] .* gini_t[(N_obl+1):N_bk], sigma_kwe);
  
  // obs in countries w/o baseline but w/ rho_w use rho_kw_hat for wd adj and then rho_rwe_hat for es adj
  // gini[kktt[N_bk:N_kw]] ~ normal(rho_kw_hat[kwn[N_bk:N_kw]] .* rho_rwe_hat[rwen2[N_bk:N_kw]] .* gini_t[N_bk:N_kw], sigma_krcat[rr[N_bk:N_kw]]);

  // obs in countries w/o rho_w use one-step estimates from rho_rwe_hat
  // gini[kktt[(N_kw+1):N]] ~ normal(rho_rwe_hat[rwen[(N_kw+1):N]] .* gini_t[(N_kw+1):N], sigma_rrcat[rr[(N_kw+1):N]]);

}
