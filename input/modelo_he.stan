// Referências
// https://statmodeling.stat.columbia.edu/2016/08/06/state-space-poll-averaging-model/
// http://freerangestats.info/blog/2017/06/24/oz-polls-statespace
// https://pkremp.github.io/report.html
// https://projects.economist.com/us-2020-forecast/president/how-this-works
// https://github.com/TheEconomist/us-potus-model
// https://marktheballot.blogspot.com/2018/08/model-updates-are-coming.html

data {
  int<lower=1> total_dias; // total de dias analisados
  int<lower=1> n_pesquisas; // n de pesquisas (serve como id de cada campo)
  real<lower=0, upper=1> percentual[n_pesquisas]; // valores das pesquisas
  int<lower=0> n_dias[n_pesquisas]; // n de dias sob análise quando cada pesquisa foi feita (t = 1 é a primeira pesquisa no banco)
  real<lower=0> sigma[n_pesquisas]; // incerteza
  int<lower=0> instituto_id[n_pesquisas];
  int<lower=1> n_institutos;
}

parameters { // Parâmetros a serem estimados
  real<lower=0, upper=1> mu[total_dias]; // intenção de votos
  real pHouseEffects[n_institutos]; // house effects
}

transformed parameters {
    real houseEffect[n_institutos];

    for (j in 1:n_institutos) {
      houseEffect[j] = pHouseEffects[j] - mean(pHouseEffects[]);
    }


}

model {
  mu[1] ~ uniform(0, 1);
  for (i in 2:total_dias)
      mu[i] ~ normal(mu[i - 1], 0.003);

  // -- house effects model

  pHouseEffects ~ normal(0, 0.025); // up to +/- 5 percentage points

  for(x in 1:n_pesquisas) // Verossimilhança
      percentual ~ normal(mu[n_dias[x]] + pHouseEffects[n_institutos], sigma);
}

