# Description
Research based on a dataset prepared by the National Institute of Diabetes and Kidney Diseases in the United States. The goal of the research is to apply the Bayesian approach to discover factors leading to diabetes and check the prior assumptions on the basis of the previous papers.

# Content
- data - folder with raw and cleaned datasets as well as .rstan files with prior features' distributions.
- data_preparation.R - initial data preparation procedure stored as R script
- analysis.R - analysis, modeling and testing procedure stored as R script
- Research.pdf - main file containing full research description

# Methodology
Final estimates were achieved by applying the Hamiltonian Monte Carlo algorithm, which combined prior assumptions and estimations from the logistic regression model.

# Findings
Variables indicating the number of pregnancies, glucose and bmi levels, as well as age of the respondent met a priori assumptions. The variable diabetes_pedigree_func,i.e. the variable responsible for the patient's genealogy, had a higher expected value than it was assumed a priori. The model also suggests that diabetes_pedigree_func is the most important factor: the greater the respondent's proximity in time and kinship to the last case of diabetes in the family, the greater the probability that the respondent will also suffer from diabetes.
