//Joe Barnby RW model //
//joe.barnby@kcl.ac.uk//

// The input data is a vector 'y' of length 'N'.
data {
  int <lower=1>          N; // number of subjects
  int <lower=1>          T; // number of maxtrials among all sbjs
  int <lower=1>          C; // number of conditions
  int <lower=1, upper=T> Tsubj[N]; // numbers of valid trials per sbj
  int <lower=0>          action[N,T];
  real<lower=0>          reward[N,T];
}
transformed data{
  vector[3] initV;  // initial values for V
  initV = rep_vector(0.0, 3);
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {

  real<lower = 0, upper = 1> lambda[N];
  real<lower = 0>            tau[N];

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (s in 1:N) {
    vector[3] v; 
    real pe;    
    v = initV;

    for (t in 1:Tsubj[s]) {        
      action[s,t] ~ categorical_logit( tau[s] * v );
      pe = reward[s,t] - v[action[s,t]];      
      v[action[s,t]] = v[action[s,t]] + lambda[s] * pe;
    }
  } 
}

