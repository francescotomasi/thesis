library(haven)
library(mice)
library(tidyverse)
library(RMThreshold)
library(dplyr)
library(caTools)
library(randomForest)
library(caret)   
library(MASS)
library(brms)
library(GGally)
library(scico)
library(cowplot)
library(ggdist)
library(loo)
library(rstan)
library(shinystan)
library(rstanarm)
library(bayesplot)
library(fastDummies)
library(TSPred) #errore: NON si riesce ad installare
library(ggpubr)
library(glmnet)
library(bayesplot)
library(ggplot2)

rm(list=ls())

set.seed(021024)
options(mc.cores=6)
sl <- scico(palette = "lajolla", n = 9) #colori non male

# data loading
VY <- read.csv("FINAL_CLEANED_END.csv")

dim(VY)

VY[771,seq(82,101,1)]
#VY <- read.csv("LAST_CleanedDataset_mice.csv")
ID <- VY[,2]
VY <- VY[,-c(1,2)]

dim(VY)
######## QUESTE RIGHE SERVONO PER METTERE 0 o 1 NEI TOPICS #####

# Ottieni gli indici delle colonne "Topic1" e "Topic24"
# start_col <- which(names(VY) == "Topic1")
# end_col <- which(names(VY) == "Topic20")

# Subset delle colonne
# subset_columns <- VY[, start_col:end_col]

# tengo i topics che rappresentanto almeno al 5% un documento
# for (col_name in names(subset_columns)) {
#   VY[,col_name]<-ifelse(VY[,col_name]<0.1,0,1)
# }
###

table(VY$prec) #prec Ë la risposta ordinale
VY[,"prec"]=VY[,"prec"]+1 #il prima valore Ë 1

densityplot(VY$prec)

# creazione delle dummies
VYD <- dummy_cols(VY, select_columns = c('region', 'migrant', "locality",'hhtype',
                                      'genedu',"disab","housing","unexpmix",'cond3','eman','stnst_rev'),
                  remove_selected_columns = TRUE, remove_first_dummy = TRUE)

## la prof Maestriperi mi ha chiesto di eliminare queste variabili dall'analisi
## un po' all'ultimo quindi non sono ripassata dalla creazione del dataset
## elimino gli ecinsreas separati e who
VYD["ecinsreas"] <- ifelse(rowSums(VYD[, paste0("ecinsreas_", 1:10)]) > 0, 1, 0)
VYD <- VYD[,-which(names(VYD) %in% c("ecinsreas_1","ecinsreas_2","ecinsreas_3","ecinsreas_4","ecinsreas_5","ecinsreas_6","ecinsreas_7","ecinsreas_8","ecinsreas_9","ecinsreas_10"))]
VYD=VYD[,-52]

# 0 maggiore di 30 anni, 1 minore di 30 anni
VYD[,"ageyear"]=ifelse(VY$ageyear<30,1,0)

# resampling di edu in anni di scuola
VYD[,"edu"]<-ifelse(VYD[,"edu"]==2,10,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==3,12,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==4,12,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==5,15,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==6,19,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==7,20,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==8,24,VYD[,"edu"])
VYD[,"edu"]<-ifelse(VYD[,"edu"]==1,6,VYD[,"edu"])

### Si normalizzano le covariate continue
VYD[,"hh_size"]=minmax(VYD[,"hh_size"]) #NON funziona il comando minmax
VYD[,"edu"]=minmax(VYD[,"edu"])
VYD[,"housimp"]=minmax(VYD[,"housimp"])
#VYD[,"invtemp"]=minmax(VYD[,"invtemp"])
VYD[,"overedu"]=minmax(VYD[,"overedu"])
VYD[,"lenght"]=minmax(VYD[,"lenght"])
VYD[,"jobinsec"]=minmax(VYD[,"jobinsec"])
VYD[,"workhours"]=minmax(VYD[,"workhours"])
VYD[,"health"]=minmax(VYD[,"health"])
VYD[,"carecap_1"]=minmax(VYD[,"carecap_1"])
VYD[,"carecap_2"]=minmax(VYD[,"carecap_2"])
VYD[,"ind_inc"]=minmax(VYD[,"ind_inc"])
VYD[,"endmeet"]=minmax(VYD[,"endmeet"])
VYD[,"wlb"]=minmax(VYD[,"wlb"])
VYD[,"diff_wb_finantialhh"]=minmax(VYD[,"diff_wb_finantialhh"])
VYD[,"diff_wb_job"]=minmax(VYD[,"diff_wb_job"])
VYD[,"diff_wb_like"]=minmax(VYD[,"diff_wb_like"])
VYD[,"diff_wb_rel"]=minmax(VYD[,"diff_wb_rel"])
VYD[,"diff_wb_life"]=minmax(VYD[,"diff_wb_life"])


## MODELLO
model3<-stan_model("OLS2.stan")

p0 <- 15 # prior guess for the number of relevant variables
sigma <- 1
scale_global  <- p0 /( 52 - p0 ) * 1 / sqrt (2459) 
stan_data <- list(
  K = 11,
  N = 2459,
  D = 102,
  y = VYD[,"prec"],
  x = VYD[,which(names(VYD)!=c("prec"))],
  scale_icept = 3,
  scale_global = scale_global,
  nu_global = 1,
  nu_local = 1,
  slab_scale = 2.5,
  slab_df = 7
)


fit1<-sampling(model3,data=stan_data,iter=10000,warmup=3000,chains=4,control=list(adapt_delta=0.97))
# controllo se ci sono state iterazioni divergenti
div1<-check_divergences(fit1)

# traceplots per le beta
traceplot(fit1,pars=c('beta[1]', 'beta[2]', 'beta[3]', 'beta[4]', 'beta[5]', 'beta[6]', 'beta[7]', 'beta[8]', 'beta[9]', 'beta[10]',
                      'beta[11]','beta[12]'))
traceplot(fit1, pars = c('beta[13]', 'beta[14]', 'beta[15]', 'beta[16]', 'beta[17]', 'beta[18]', 'beta[19]', 'beta[20]', 'beta[21]', 'beta[22]', 'beta[23]', 'beta[24]'))
traceplot(fit1, pars = c('beta[25]', 'beta[26]', 'beta[27]', 'beta[28]', 'beta[29]', 'beta[30]', 'beta[31]', 'beta[32]', 'beta[33]', 'beta[34]', 'beta[35]', 'beta[36]'))
traceplot(fit1, pars = c('beta[37]', 'beta[38]', 'beta[39]', 'beta[40]', 'beta[41]', 'beta[42]', 'beta[43]', 'beta[44]', 'beta[45]', 'beta[46]', 'beta[47]', 'beta[48]'))
traceplot(fit1, pars = c('beta[49]', 'beta[50]', 'beta[51]', 'beta[52]', 'beta[53]', 'beta[54]', 'beta[55]', 'beta[56]', 'beta[57]', 'beta[58]', 'beta[59]', 'beta[60]'))
traceplot(fit1, pars = c('beta[61]', 'beta[62]', 'beta[63]', 'beta[64]', 'beta[65]', 'beta[66]', 'beta[67]', 'beta[68]', 'beta[69]', 'beta[70]', 'beta[71]', 'beta[72]'))
traceplot(fit1, pars = c('beta[73]', 'beta[74]', 'beta[75]', 'beta[76]', 'beta[77]', 'beta[78]', 'beta[79]', 'beta[80]', 'beta[81]', 'beta[82]', 'beta[83]', 'beta[84]'))
traceplot(fit1, pars = c('beta[85]', 'beta[86]', 'beta[87]', 'beta[88]', 'beta[89]', 'beta[90]', 'beta[91]', 'beta[92]', 'beta[93]', 'beta[94]', 'beta[95]', 'beta[96]'))
traceplot(fit1, pars = c('beta[97]', 'beta[98]', 'beta[99]', 'beta[100]', 'beta[101]', 'beta[102]'))

# estraggo i parametri del modello
params3 <-extract(fit1)

## stima di densit‡ della risposta 
plot1<-ppc_dens_overlay(VY$prec,params3$y_ppc[1:100,])
plot1 + 
  geom_vline(xintercept = 1:11, color = "grey", linetype = "dashed")

## ecdf # funzione di rip discreta ? 
ppc_ecdf_overlay(VY$prec,params3$y_ppc[1:100,],discrete = TRUE)

## istogrammi
pvalueforse<-ppc_stat(VY$prec,params3$y_ppc)
pvalueforse$
ppc_hist(VY$prec,params3$y_ppc[1:50,])

## metriche dei beta
summary(params3$beta)

## vari plot
x11()
print(fit1)

stan_diag(fit1)

stan_rhat(fit1)
stan_ess(fit1)
stan_mcse(fit1)

# plot intervalli
stan_plot(fit1,pars="beta",ci_level = 0.9,outer_level=0.95)
stan_plot(fit1, pars = paste0("beta[", 1:36, "]"), ci_level = 0.9,outer_level = 0.95)
stan_plot(fit1, pars = paste0("beta[", 37:75, "]"), ci_level = 0.9,outer_level = 0.95)
stan_plot(fit1, pars = paste0("beta[", 76:112, "]"), ci_level = 0.9,outer_level = 0.95)

stan_plot(fit1, pars = paste0("beta[", 1:34, "]"), ci_level = 0.9,outer_level = 0.95, xlim = c(-5,3))
stan_plot(fit1, pars = paste0("beta[", 35:68, "]"), ci_level = 0.9,outer_level = 0.95,  xlim = c(-5,3))
stan_plot(fit1, pars = paste0("beta[", 69:102, "]"), ci_level = 0.9,outer_level = 0.95, xlim = c(-5,3))

# Plot for beta[1] to beta[5]
parHS1 = stan_plot(fit1, pars = paste0("beta[", 1:5, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[6] to beta[10]
parHS2 = stan_plot(fit1, pars = paste0("beta[", 6:10, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[11] to beta[15]
parHS3 = stan_plot(fit1, pars = paste0("beta[", 11:15, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[16] to beta[20]
parHS4 = stan_plot(fit1, pars = paste0("beta[", 16:20, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[21] to beta[25]
parHS5 = stan_plot(fit1, pars = paste0("beta[", 21:25, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[26] to beta[30]
parHS6 = stan_plot(fit1, pars = paste0("beta[", 26:30, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[31] to beta[35]
parHS7 = stan_plot(fit1, pars = paste0("beta[", 31:35, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[36] to beta[40]
parHS8 = stan_plot(fit1, pars = paste0("beta[", 36:40, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[41] to beta[45]
parHS9 = stan_plot(fit1, pars = paste0("beta[", 41:45, "]"), ci_level = 0.9,outer_level = 0.95)

# Plot for beta[46] to beta[50]
parHS10 = stan_plot(fit1, pars = paste0("beta[", 46:50, "]"), ci_level = 0.9,outer_level = 0.95)

# Continue this pattern for the remaining chunks
# CI 
parHS11 = stan_plot(fit1, pars = paste0("beta[", 51:55, "]"), ci_level = 0.9,outer_level = 0.95)
parHS12 = stan_plot(fit1, pars = paste0("beta[", 56:60, "]"), ci_level = 0.9,outer_level = 0.95)
parHS13 = stan_plot(fit1, pars = paste0("beta[", 61:65, "]"), ci_level = 0.9,outer_level = 0.95)
parHS14 = stan_plot(fit1, pars = paste0("beta[", 66:70, "]"), ci_level = 0.9,outer_level = 0.95)
parHS15 = stan_plot(fit1, pars = paste0("beta[", 71:75, "]"), ci_level = 0.9,outer_level = 0.95)
parHS16 = stan_plot(fit1, pars = paste0("beta[", 76:80, "]"), ci_level = 0.9,outer_level = 0.95)
parHS17 = stan_plot(fit1, pars = paste0("beta[", 81:85, "]"), ci_level = 0.9,outer_level = 0.95)
parHS18 = stan_plot(fit1, pars = paste0("beta[", 86:90, "]"), ci_level = 0.9,outer_level = 0.95)
parHS19 = stan_plot(fit1, pars = paste0("beta[", 91:95, "]"), ci_level = 0.9,outer_level = 0.95)
parHS20 = stan_plot(fit1, pars = paste0("beta[", 96:100, "]"), ci_level = 0.9,outer_level = 0.95)
parHS21 = stan_plot(fit1, pars = paste0("beta[", 101:102, "]"), ci_level = 0.9,outer_level = 0.95)


# Arrange the plots in a grid
x11()
ggarrange(parHS1, parHS2, parHS3, parHS4, parHS5,parHS6, 
          labels = c("beta[1-5]", "beta[6-10]", "beta[11-15]", "beta[16-20]", "beta[21-25]",
                     "beta[26-30]"),ncol = 3, nrow = 2)
x11()
ggarrange( parHS7, parHS8, parHS9, parHS10, parHS11, parHS12,
          labels = c("beta[31-35]", "beta[36-40]", "beta[41-45]","beta[46-50]", "beta[51-55]", "beta[56-60]"),
          ncol = 3, nrow = 2)
x11()
ggarrange( parHS13, parHS14, parHS15, parHS16, parHS17, parHS18,
           labels = c("beta[61-65]", "beta[66-70]", "beta[71-75],beta[76-80]", "beta[81-85]", "beta[85-90]"),
           ncol = 3, nrow = 2)
x11()
ggarrange( parHS19, parHS20, parHS21, 
           labels = c("beta[91-95]", "beta[96-100]", "beta[101-102]"),
           ncol = 3, nrow = 2)


#### waic computation ####

log_lik<-extract_log_lik(fit1,parameter_name = "log_lik")
waic(log_lik)

# Estimate        SE
# elpd_waic  -5158.4 34.5
# p_waic        76.3  1.5
# waic       10316.8 68.9

#### calcolo dell'indice definito nella tesi p. 25 (un chi-quadrato col modulo####
y_ppc_tot<-params3$y_ppc

y_ppc_median <- apply(y_ppc_tot, 2, median, na.rm = TRUE)

diff_pred <- y_ppc_median - VY$prec

quantile(y_ppc_tot, probs = c(0.25, 0.5, 0.75))
## 25% 50% 75% 
## 3   6   8 

abs_ratio <- abs(diff_pred / 5)  # 5= quant_0.75 - quant_0.25
ratio<- (diff_pred / 5)

total_sum <- sum(abs_ratio, na.rm = TRUE)
total_mean <- total_sum/2459
diff_pred_norm <- diff_pred/5

df_diff <- data.frame(index = 1:length(diff_pred), difference = diff_pred_abs)

# Creazione del grafico
x11()
ggplot(df_diff, aes(x = index, y = difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Differenze tra Previsioni e Valori Reali",
       x = "Osservazione",
       y = "Differenza (y_ppc_mode - y_real)")

## varie altre prove per creare l'istogramma

# Raggruppare le osservazioni in chunk da 50
df_diff <- data.frame(index = 1:length(diff_pred), difference = diff_pred_abs) %>%
  mutate(chunk = ceiling(index / 50)) %>%
  group_by(chunk) %>%
  summarise(mean_difference = mean(difference, na.rm = TRUE))

# Creare il grafico con chunk pi√π ampi
ggplot(df_diff, aes(x = chunk, y = mean_difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Differenze tra Previsioni e Valori Reali (Chunk da 50)",
       x = "Gruppo di Osservazioni",
       y = "Media della Differenza")
