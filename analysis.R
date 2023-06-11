# 0. Presets and data import ----
setwd("C:/University/Ekonometria bayesowska/hometask_2/")
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(rstan)
library(coda)
library(bayesplot)
library(bridgesampling)
library(parallel)
library(doParallel)

dataset <- readr::read_csv("data/data_cleared.csv")


# 1. EDA ----
make_density <- function(variable, df, plot_type = "density") {
  df %>% 
    ggplot(aes_string(x = variable)) +
    do.call(paste0("geom_", plot_type), list(fill = "darkgreen", alpha = 0.6, color = "#131313")) +
    labs(y = "", title = plot_type) +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}
make_box <- function(variable, df, variable_x = "y") {
  df %>% 
    ggplot(aes_string(variable_x, variable)) +
    geom_boxplot(fill = "darkgreen") +
    labs(title = "box plot") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}
make_density_cat <- function(variable, df) {
  df %>% 
    ggplot(aes_string(variable)) +
    geom_bar(color = "#131313", fill = "darkgreen", alpha = 0.6) +
    geom_text(stat='count', aes(label=..count..), vjust=1.2) +
    labs(y = "", title = "histogram") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank()
    )
}
make_point <- function(variable, df=df_sel, variable_y = "price") {
  df %>% 
    ggplot(aes_string(variable, variable_y)) +
    geom_point(color = "darkgreen") +
    labs(title = "point plot")+
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}

# y
make_density_cat("y", dataset)

# pregnancies
df_temp <- dataset %>% 
  mutate(y = as.factor(y))
ggarrange(
  make_density_cat("pregnancies", dataset),
  make_box("pregnancies", df_temp),
  nrow = 2
)

# overview
vars_continuous <- c("glucose", "blood_pressure", "skin_thickness", "insulin", "bmi", "diabetes_pedigree_func", "age")
plt_lst <- list()


for (i in seq_along(vars_continuous)) {
  plt_lst[[i]] <- ggarrange(
    make_density(vars_continuous[i], dataset),
    make_density(vars_continuous[i], dataset, "histogram"),
    make_box(vars_continuous[i], df_temp),
    nrow = 3
  )
}
plt_lst


sum(dataset$skin_thickness==0)
sum(dataset$insulin==0)

chi_df <- dataset %>% 
  transmute(
    y = y,
    insulin = ifelse(insulin == 0, 1, 0),
    skin_thickness = ifelse(skin_thickness == 0, 1, 0)
  )
table(chi_df$y, chi_df$insulin)
chisq.test(chi_df$y, chi_df$insulin)
table(chi_df$y, chi_df$skin_thickness)
chisq.test(chi_df$y, chi_df$skin_thickness)

# skin_thickness
df_temp2 <- df_temp %>% 
  mutate(
    skin_thickness_zero = ifelse(skin_thickness == 0, 1, 0),
  )
table(df_temp2$skin_thickness_zero, df_temp2$y)
df_chisq <- chisq.test(table(df_temp2$skin_thickness_zero, df_temp2$y))
df_chisq
df_chisq$expected %>% round(2)
# => not important


# diabetes_pedigree_func
df_temp2 <- df_temp %>% 
  mutate(
    diabetes_pedigree_func_log = log(diabetes_pedigree_func),
    diabetes_pedigree_func_log = ifelse(is.infinite(diabetes_pedigree_func_log), 0, diabetes_pedigree_func_log)
  )
ggarrange(
  make_density("diabetes_pedigree_func_log", df_temp2),
  make_density("diabetes_pedigree_func_log", df_temp2, "histogram"),
  make_box("diabetes_pedigree_func_log", df_temp2),
  nrow = 3
)
# => keep as it is

dataset %>% 
  filter(blood_pressure==0) %>% 
  nrow()

dataset %>% 
  filter(blood_pressure!=0) %>% 
  pull(y) %>% 
  table()

df_sel <- dataset %>% 
  filter(blood_pressure != 0) %>% 
  mutate(y = as.factor(y)) %>% 
  select(-skin_thickness, -insulin)
df_sel

df_sel %>% 
  mutate(y = as.numeric(y)-1) %>% 
  cor() %>% 
  as_tibble(rownames = "variable") %>% 
  mutate_if(is.numeric, round, 2)

# 2. Logit model ----
df_sel %>% colnames()
model <- glm(y ~ ., family = binomial(link = "logit"), df_sel)
summary(model)

df_sel <- df_sel %>% select(-blood_pressure)

model <- glm(y ~ ., family = binomial(link = "logit"), df_sel)
summary(model)


# 3. Algorithm ----
N <- nrow(df_sel)
k <- ncol(df_sel)
df_sel2 <- df_sel %>% 
  mutate(across(c("y", "pregnancies", "glucose", "age"), as.integer)) %>% 
  mutate(y = y-1)


list2env(as.list(df_sel2), .GlobalEnv) # define tibble's columns in the global environment

columns_all <- c("N", colnames(df_sel), "k")
n_iters <- 3500
n_chains <- 8

no_cores <- round(detectCores()-2)
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl, cores = 10)
rstan_options(auto_write = TRUE)
start <- Sys.time()

model_stan <- stan(
  file = "data/dist_priors.stan",
  data = columns_all,
  iter = n_iters,
  chains = n_chains,
  seed = 82591
)

end <- Sys.time()
parallel::stopCluster(cl)
end - start


# 4. Diagnostics -----
print(model_stan)

simulated.chains.all <- extract(model_stan, inc_warmup = F, par = "beta")
simulated.chains.1by1 <- extract(model_stan, permuted = FALSE, inc_warmup = FALSE, par = c("beta"))

combined.chains <- mcmc.list(
  mcmc(simulated.chains.1by1[, 1, ]),
  mcmc(simulated.chains.1by1[, 2, ]),
  mcmc(simulated.chains.1by1[, 3, ]),
  mcmc(simulated.chains.1by1[, 4, ]),
  mcmc(simulated.chains.1by1[, 5, ]),
  mcmc(simulated.chains.1by1[, 6, ]),
  mcmc(simulated.chains.1by1[, 7, ]),
  mcmc(simulated.chains.1by1[, 8, ])
)

summary(combined.chains, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
S0 <- spectrum0(combined.chains)
S_1 <- dim(simulated.chains.1by1)[1]
numerical.SE <- (S0$spec / S_1) ^ 0.5


plot(combined.chains)
gelman.diag(combined.chains, confidence=0.95)
gelman.plot(combined.chains)
geweke.diag(combined.chains, frac1=0.1, frac2=0.5)
heidel.diag(combined.chains)
raftery.diag(combined.chains, q = 0.5, r = 0.01, s = 0.95)


# 5. Posterior ----
colors_vec <- c(
  rgb(0, 116, 129, maxColorValue = 255),
  rgb(0, 120, 0, maxColorValue = 255),
  rgb(206, 220, 0, maxColorValue = 255)
)

vars_all <- c("beta[1]", columns_all[3:(length(columns_all)-1)])

model_stan@sim[["fnames_oi"]] <- c(vars_all, "sigma","lp_")    #"lp__"

par(mfrow = c(4,2))
stan_hist(model_stan, pars = vars_all, fill = colors_vec[2], bins = 30)
mcmc_intervals(model_stan, pars = vars_all)

chain.b <- mcmc(simulated.chains.all$beta)
hpdi.b <- HPDinterval(chain.b)
rownames(hpdi.b) <- vars_all
hpdi.b


# 6. BF ----
marg_lik <- bridge_sampler(model_stan)
bf_res_lst <- list()

no_cores <- round(detectCores()-2)
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl, cores = 10)
rstan_options(auto_write = TRUE)
start <- Sys.time()

for (i in 1:(k-1)) {
  tmp_model_stan <- stan(
    file = paste0("data/dist_priors", i+1, ".stan"),
    data = columns_all[columns_all != columns_all[i+2]],
    iter = n_iters,
    chains = n_chains,
    seed = 82591
  )
  tmp_marg_lik <- bridge_sampler(tmp_model_stan)
  
  bf_res_lst[[i]] <- bayes_factor(marg_lik, tmp_marg_lik)
}

end <- Sys.time()
parallel::stopCluster(cl)
end - start

bf_res_lst