p_load(rstan, beepr)

seed <- 324
iter <- 1000
chains <- 4
cores <- chains

x <- ineq %>% filter(country=="United Kingdom") %>% 
  mutate(ccode = ccode %>% factor %>% as.numeric,
         tcode = tcode - min(tcode) + 1,
         mcode = mcode %>% factor %>% as.numeric)


source_data <- list(  K=max(x$ccode),
                      T=max(x$tcode),
                      M=max(x$mcode),
                      N=length(x$gini_m),
                      N_b=length(x$gini_b[!is.na(x$gini_b)]),
                      kk=x$ccode,
                      tt=x$tcode,
                      mm=x$mcode,
                      gini_m=x$gini_m,
                      gini_m_se=x$gini_m_se,
                      gini_b=x$gini_b[!is.na(x$gini_b)],
                      gini_b_se=x$gini_b_se[!is.na(x$gini_b_se)]
)

model_code <- '
  data{
    int<lower=1> K;     		                // number of countries
    int<lower=1> T; 				                // number of years
    int<lower=1> M; 				                // number of series
    int<lower=0> N;                         // total number of observations
    int<lower=0> N_b;                       // number of observations with baseline
    int<lower=1, upper=K> kk[N]; 	          // country for observation n
    int<lower=1, upper=T> tt[N]; 	          // year for observation n
    int<lower=1, upper=M> mm[N];            // series for observation n
    real<lower=0, upper=1> gini_m[N]; 	    // measured gini for observation n
    real<lower=0, upper=1> gini_m_se[N];    // std error of measured gini for obs n
    real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
    real<lower=0, upper=1> gini_b_se[N_b];  // baseline gini for obs n
  }  
  transformed data {
    int G[N-1];				// number of missing years until next observed country-year (G for "gap")
    for (n in 1:N-1) {
      G[n] <- tt[n+1] - tt[n] - 1;
    }
  }
  parameters {
    real<lower=0, upper=1> gini[K, T]; // SWIID gini estimate for baseline in country k at time t
    real<lower=0, upper=1> gini_t[N]; // unknown "true" gini for obs n given gini_m and gini_m_se
    real<lower=0> rho[M]; // discrimination of series m (see Stan Development Team 2015, 61; Gelman and Hill 2007, 314-320; McGann 2014, 118-120 (using 1/alpha))
    real<lower=0> sigma_rho[M];  // scale of series discriminations (see Stan Development Team 2015, 61)
    real<lower=0, upper=1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  }
  model {
    rho ~ normal(1, sigma_rho);
    sigma_rho ~ cauchy(0, .5);
    
    gini_t ~ normal(gini_m, gini_m_se);
    for (n in 1:N) {
      if (n<=N_b)
        gini[kk[n], tt[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
      else
        gini[kk[n], tt[n]] ~ normal(rho[mm[n]] * gini_t[n], sigma_rho[mm[n]]);
      // prior for gini for the next observed year by country as well as for all intervening missing years
      if (n < N) {
        if (tt[n] < T) {
          for (g in 0:G[n]) {
            gini[kk[n], tt[n]+g+1] ~ normal(gini[kk[n], tt[n]+g], sigma_gini[kk[n]]);
          }
        }
      }
    }
  }
'

start <- proc.time()
out1 <- stan(model_code = model_code,
             data = source_data,
             seed = seed,
             iter = 1000,
             cores = cores,
             chains = chains,
             control = list(max_treedepth = 20,
                            adapt_delta = .8))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

# Chime
beep()
