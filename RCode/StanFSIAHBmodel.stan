// Fehr-Schmidt Inequality Aversion (FSIA) model

data {
  int <lower=1> N; // number of subjects
  int <lower=1> T; // number of maxtrials among all sbjs
  int <lower=1> C; // number of conditions
  int <lower=1, upper=T> Tsubj[N]; // numbers of valid trials per sbj
  real <lower=0> s1[N,T]; // trial-wise self payoff for left option
  real <lower=0> o1[N,T]; // trial-wise other payoff for left option
  real <lower=0> s2[N,T]; // trial-wise self payoff for right option
  real <lower=0> o2[N,T]; // trial-wise other payoff for right option
  int <lower=0,upper=1> choice[N,T]; // 0 for left option, 1 for right option
}

parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[3] mu_p;  
  vector<lower=0>[3] sigma;
    
  // Subject-level raw parameters (for Mathematics trick)
  vector[N] alpha_pr;   // distaste of having less than other - inadvantageous inequality aversion 
  vector[N] beta_pr;    // distaste of having more than other - advantageous inequality aversion 
  vector[N] IT_pr;      // inverse temperature
}

transformed parameters {
  // Transform subject-level raw parameters 
  vector<lower=-5>[N] alpha; 
  vector<lower=-5>[N] beta;  
  vector<lower=0>[N] IT; 
  
  // Theoretically, alpha should be larger than beta,
  // as people should be more aversive to disadvantageous
  // inequality (alpha) than advantageous equality (beta)
  
  for (i in 1:N) {
    beta[i]   = mu_p[2] + sigma[2] * beta_pr[i]; 
    alpha[i]  = mu_p[1] + sigma[1] * alpha_pr[i]; //* (5 - beta[i]) + beta[i]; // this makes alpha > beta
    IT[i]     = exp( mu_p[3] + sigma[3] * IT_pr[i] );
  }
}

model {
  // Hyperparameters
  mu_p[1]  ~ uniform(-5, 5); 
  mu_p[2]  ~ uniform(-5, 5); 
  mu_p[3]  ~ normal(0, 1); 
  sigma ~ normal(0, 1);
    
  // individual parameters
  alpha_pr ~ uniform(-5, 5);
  beta_pr  ~ uniform(-5, 5);
  IT_pr    ~ normal(0, 1);
  
  for (i in 1:N) {
    // Define values
    real U1; // U1 is the utility for the left option
    real U2; // U2 is the utility for the right option
      
    for (t in 1:(Tsubj[i])) {
      // Calculating the (dis)advantageous inequality
      //U1 = s1[i,t]-alpha[i]*fmax(o1[i,t]-s1[i,t], 0.0)-beta[i]*fmax(s1[i,t]-o1[i,t], 0.0);
      //U2 = s2[i,t]-alpha[i]*fmax(o2[i,t]-s2[i,t], 0.0)-beta[i]*fmax(s2[i,t]-o2[i,t], 0.0);
      U1 = alpha[i]*s1[i,t]+beta[i]*fmax(s1[i,t]-o1[i,t], 0.0); // alternative equation
      U2 = alpha[i]*s2[i,t]+beta[i]*fmax(s2[i,t]-o2[i,t], 0.0);
      choice[i,t] ~ bernoulli_logit(IT[i] * (U2 - U1) );
    } 
  } 
} 

generated quantities {
  // For group level parameters 
  real<lower=-5> mu_alpha;
  real<lower=-5> mu_beta;
  real<lower=0> mu_IT;
  
  // For log-likelihood and utility calculation (model regressors)
  real U1[N,T]; // U1 is the utility for the left option
  real U2[N,T]; // U2 is the utility for the right option
  real log_lik[N];
  
  // For posteior predictive check
  int y_pred[N,T];

  mu_beta  = mu_p[2];
  mu_alpha = mu_p[1]; //* (5 - mu_beta) + mu_beta;
  mu_IT    = exp(mu_p[3]);
  
  // Initialize the U and y_pred  (concerning the missing trial)
  U1 = rep_array(-1,N,T); 
  U2 = rep_array(-1,N,T);
  y_pred = rep_array(-1,N,T);
  
  { // local section, this saves time and space
    for (i in 1:N) {
      // Define values
      log_lik[i] = 0;
          
      for (t in 1:(Tsubj[i])) {
          //U1[i,t] = s1[i,t]-alpha[i]*fmax(o1[i,t]-s1[i,t], 0.0)-beta[i]*fmax(s1[i,t]-o1[i,t], 0.0);
          //U2[i,t] = s2[i,t]-alpha[i]*fmax(o2[i,t]-s2[i,t], 0.0)-beta[i]*fmax(s2[i,t]-o2[i,t], 0.0);
          U1[i,t] = alpha[i]*s1[i,t]+beta[i]*fmax(s1[i,t]-o1[i,t], 0.0);
          U2[i,t] = alpha[i]*s2[i,t]+beta[i]*fmax(s2[i,t]-o2[i,t], 0.0);
          
          log_lik[i] += bernoulli_logit_lpmf(choice[i,t] | IT[i]*(U2[i,t] - U1[i,t]));
          
          // Posterior predictive check of choice
          y_pred[i,t] = bernoulli_logit_rng( IT[i] * (U2[i,t] - U1[i,t]) );         
      }       
    }  
  }   
}
