p_load(rstan, beepr)

seed <- 3034
iter <- 1000
chains <- 4
cores <- chains

#names(table(lis$country)[5])

x <- ineq %>% filter(country=="United States") %>% 
  mutate(ccode = ccode %>% factor %>% as.numeric,
         ycode = ycode - min(ycode) + 1,
         scode = scode %>% factor %>% as.numeric)


source_data <- list(  C=max(x$ccode),
                      Y=max(x$ycode),
                      S=max(x$scode),
                      N=length(x$gini_m),
                      N_b=length(x$gini_b[!is.na(x$gini_b)]),
                      cc=x$ccode,
                      yy=x$ycode,
                      ss=x$scode,
                      gini_m=x$gini_m,
                      gini_m_se=x$gini_m_se,
                      gini_b=x$gini_b[!is.na(x$gini_b)],
                      gini_b_se=x$gini_b_se[!is.na(x$gini_b_se)]
)

model_code <- '
  data{
    int<lower=1> C;     		                // number of countries
    int<lower=1> Y; 				                // number of years
    int<lower=1> S; 				                // number of series
    int<lower=0> N;                         // total number of observations
    int<lower=0> N_b;                       // number of observations with baseline
    int<lower=1, upper=C> cc[N]; 	          // country for observation n
    int<lower=1, upper=Y> yy[N]; 	          // year for observation n
    int<lower=1, upper=S> ss[N];            // series for observation n
    real<lower=0, upper=1> gini_m[N]; 	    // measured gini for observation n
    real<lower=0, upper=1> gini_m_se[N];    // std error of measured gini for obs n
    real<lower=0, upper=1> gini_b[N_b];     // baseline gini for obs n
    real<lower=0, upper=1> gini_b_se[N_b];  // baseline gini for obs n
  }  
  transformed data {
    int G[N-1];				// number of missing years until next observed country-year (G for "gap")
    for (n in 1:N-1) {
      G[n] <- yy[n+1] - yy[n] - 1;
    }
  }
  parameters {
    real<lower=0, upper=1> gini[C, Y];      // SWIID gini estimate for baseline in country k at time t
    real<lower=0, upper=1> gini_t[N];       // unknown "true" gini for obs n given gini_m and gini_m_se
    real<lower=0> rho[S];                   // ratio of series s (see Stan Development Team 2015, 61; Gelman and Hill 2007, 314-320; McGann 2014, 118-120 (using 1/alpha))
    real<lower=0> sigma_rho[S];             // scale for ratios (see Stan Development Team 2015, 61)
    real<lower=0, upper=.1> sigma_series[S]; 	// series noise  
    real<lower=0, upper=.1> sigma_country[C]; 	// country variance parameter (see Linzer and Stanton 2012, 12)
  }
  model {
    rho ~ normal(1, sigma_rho);
    sigma_rho ~ cauchy(0, .5);
    
    gini_t ~ normal(gini_m, gini_m_se);
    for (n in 1:N) {
      if (n <= N_b) {
        gini[cc[n], yy[n]] ~ normal(gini_b[n], gini_b_se[n]); // use baseline series where observed
        gini_b[n] ~ normal(rho[ss[n]] * gini_t[n], sigma_series[ss[n]]);
      }
      else {
        gini[cc[n], yy[n]] ~ normal(rho[ss[n]] * gini_t[n], sigma_series[ss[n]]);
      }
      // prior for gini for the next observed year by country as well as for all intervening missing years
      if (n < N) {
        if (yy[n] < Y) {
          for (g in 0:G[n]) {
            gini[cc[n], yy[n]+g+1] ~ normal(gini[cc[n], yy[n]+g], sigma_country[cc[n]]);
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
