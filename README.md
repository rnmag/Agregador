# Poll Aggregation for the 2022 Brazilian Presidential Election

State special model aimed at estimating the voting intentions for candidates in Brazil's 2022 presidential elections. It includes time-constant biases specific to each pollster,normalizing them to a zero sum.

Implementation is via Markov chain Monte Carlo (MCMC) in `stan`, producing arbitrarily many samples from the posterior density of conditional on the poll estimates, their samples sizes, their field dates and the model. These samples are then summarised so as to produce graphical and tabular summaries
