<div align="center">
  <img src="https://github.com/yauheni-se/BayesianDiabetes/assets/84158821/38cb75fc-5db3-4862-8c5e-a92a9fd44525" alt="" width="100"/>
</div>

# ©️ Tags
Bayesian inference, Monte Carlo, logistic regression, classification

# :bulb: About
The goal of the project is to discover factors leading to diabetes and to verify the prior assumptions based on the previous papers by relying on **Bayesian inference**.\
The research is based on a dataset prepared by the National Institute of Diabetes and Kidney Diseases in the United States.


# :open_file_folder: Content
- [data](https://github.com/yauheni-se/BayesianDiabetes/blob/main/data/) – folder with raw and cleaned datasets as well as .rstan files with prior features' distributions.
- [data_preparation.R](https://github.com/yauheni-se/BayesianDiabetes/blob/main/data_preparation.R) – initial data preparation procedure stored as R script
- [analysis.R](https://github.com/yauheni-se/BayesianDiabetes/blob/main/analysis.R) – analysis, modeling and testing procedure stored as R script
- [Research.pdf](https://github.com/yauheni-se/BayesianDiabetes/blob/main/Research.pdf) – main file containing full research description

# :test_tube: Methodology
Final estimates were achieved by applying the Hamiltonian Monte Carlo algorithm, which combined prior assumptions and estimations from the logistic regression model.

# 🏆 Findings

<div align="center">
  <img src="https://github.com/yauheni-se/BayesianDiabetes/assets/84158821/6269a535-3590-4e48-8afc-2de7f3c458b4" alt="" width="400"/>
  <img src="https://github.com/yauheni-se/BayesianDiabetes/assets/84158821/67e7749f-5d0e-4222-a28f-6f9af96271aa" alt="" width="400" />
</div>

- Variables indicating the number of **pregnancies**, **glucose**, and **BMI** levels, as well as the **age** of the respondent met a priori assumptions 🎯
- The **diabetes_pedigree_func** variable, i.e. the variable responsible for the patient's genealogy, had a higher expected value than it was assumed a priori ❌
- The model also suggests that **diabetes_pedigree_func** is the most important factor: the greater the respondent's proximity in time and kinship to the last case of diabetes in the family, the greater the probability that the respondent will also suffer from diabetes ❤️‍🩹
