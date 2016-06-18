library(rstan)
library(beepr)

# dcpo <- function(x,
#                  model_code = NULL,
#                  seed = 324,
#                  iter = 100,
#                  cores = 1,
#                  chains = 4)

### Delete these when turning into a function
seed <- 324
iter <- 1000
chains <- 4
cores <- chains
x <- ineq
###


source_data <- list(  K=max(x$ccode),
                      T=max(x$tcode),
                      M=max(x$mcode),
                      N=length(x$gini),
                      kk=x$ccode,
                      tt=x$tcode,
                      mm=x$mcode,
                      gini_m=x$gini,
                      gini_se=x$gini_se
)

irt_code <- '
  data {
    int<lower=1> K;     		// number of countries
    int<lower=1> T; 				// number of years
    int<lower=1> M; 				// number of series
    int<lower=1> N; 				// number of KTM observations
    int<lower=1, upper=K> kk[N]; 	// country for observation n
    int<lower=1, upper=T> tt[N]; 	// year for observation n
    int<lower=1, upper=M> mm[N];  // series for observation n
    real<lower=0, upper=1> gini_m[N]; 	// measured gini for observation n
    real<lower=0, upper=1> gini_se[N];  // standard error of measured gini for observation n

  }
  transformed data {
    int G[N-1];				// number of missing years until next observed country-year (G for "gap")
    for (n in 1:N-1) {
        G[n] <- tt[n+1] - tt[n] - 1;
    }
  }
  parameters {
    real<lower=0, upper=1> gini[K, T]; // SWIID gini estimate for baseline in country k at time t
    real<lower=0> rho[M]; // discrimination of series m (see Stan Development Team 2015, 61; Gelman and Hill 2007, 314-320; McGann 2014, 118-120 (using 1/alpha))
    real<lower=0> sigma_rho;  // scale of series discriminations (see Stan Development Team 2015, 61)
    real<lower=0, upper=1> sigma_gini[K]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  }
  transformed parameters {
    real<lower=0, upper=1> gini_t[N]; // unknown "true" gini for obs n given gini_m and gini_se
    for (n in 1:N) {
      if (mm[n]==1) 
        gini_t[n] <- 1;
      else
        
    }
  }
  model {
    rho ~ lognormal(0, sigma_rho);
    sigma_rho ~ cauchy(0, 1);

    gini_t ~ normal(gini_m, gini_se)
    for (n in 1:N) {
      if (mm[n]==1)
        gini[kk[n], tt[n]] ~ normal(gini_m[n], gini_se[n]); // use baseline series where observed
      else
        gini[n] <- normal(rho[mm[n]] * gini_t[kk[n], tt[n]], sigma_rho[mm[n]]);
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
out1 <- stan(model_code = irt_code,
             data = source_data,
             seed = seed,
             iter = 100,
             cores = cores,
             chains = 3,
             control = list(max_treedepth = 20,
                            adapt_delta = .8))
runtime <- proc.time() - start
runtime

lapply(get_sampler_params(out1, inc_warmup = FALSE),
       summary, digits = 2)

# Chime
beep()
