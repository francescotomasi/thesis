library(stm)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(tidyverse)
library(data.table)
library(haven)
library(readxl)
library(quanteda) 
library(quanteda.textplots) 
library(quanteda.textstats) 
library(text2map)
library(text2vec)
library(fpc) 
library(factoextra) 
library(textmineR) 
library(Matrix)
df <- read_xlsx("~/Desktop/Tesi/file_r/data/df_complete_3.xlsx")
processed <- textProcessor(
  documents = df$text, 
  metadata = df,
  language = "english",
  stem = TRUE,
  removestopwords = TRUE,
  customstopwords = words,
  custompunctuation = c("-"),
  verbose = FALSE
)

out <- prepDocuments(
  documents = processed$documents, 
  vocab = processed$vocab, 
  meta = processed$meta
)

### MEAN COSINE SIMILARITY -----
row_indices <- rep(1:length(out$documents), sapply(out$documents, ncol))
col_indices <- unlist(lapply(out$documents, function(x) x[1, ]))
counts <- unlist(lapply(out$documents, function(x) x[2, ]))

sparse_mat <- sparseMatrix(
  i = row_indices,
  j = col_indices,
  x = counts,
  dims = c(length(out$documents), length(out$vocab))
)

filtered_dtm <- as.dfm(sparse_mat)
colnames(filtered_dtm) <- out$vocab
docvars(filtered_dtm) <- out$meta

filtered_tfidf <- dfm_tfidf(filtered_dtm)

sim_matrix <- textstat_simil(filtered_dtm, method = "cosine")
sim_matrix_raw <- as.matrix(sim_matrix)
diag(sim_matrix_raw) <- NA
mean_cosine <- mean(sim_matrix_raw, na.rm = TRUE)

print(mean_cosine)


### TF -IDF -----

# Calcola la somma dei pesi TF-IDF per ogni termine in tutto il corpus
tfidf_sums <- colSums(filtered_tfidf) 

# Ordina i termini dal più importante al meno importante
top_tfidf_terms <- sort(tfidf_sums, decreasing = TRUE) 

# Visualizza i primi 20 termini
head(top_tfidf_terms, 20) 


# SCATTERPLOT between V_predicted	A_predicted	D_predicted, from df dataset. ----
# Do the three scaatterplot between V_predicted	A_predicted	D_predicted, from df dataset using this color #5F7D8F. Draw the axis as if the center is (3,3). Since i don't have 1 or 5, you can focus on
library(ggplot2)
library(patchwork)

p1 <- ggplot(df, aes(x = V_predicted, y = A_predicted)) +
  geom_point(color = "#5F7D8F", alpha = 0.5) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 3, color = "blue", linetype = "dotdash", size = 1) +
  theme_bw() +
  labs(title = "Valence vs Arousal")

p2 <- ggplot(df, aes(x = V_predicted, y = D_predicted)) +
  geom_point(color = "#5F7D8F", alpha = 0.5) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 3, color = "blue", linetype = "dotdash", size = 1) +
  theme_bw() +
  labs(title = "Valence vs Dominance")

p3 <- ggplot(df, aes(x = A_predicted, y = D_predicted)) +
  geom_point(color = "#5F7D8F", alpha = 0.5) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 3, color = "blue", linetype = "dotdash", size = 1) +
  theme_bw() +
  labs(title = "Arousal vs Dominance")

combined_plot <- p1 + p2 + p3 + plot_annotation(title = "Scatterplots")
combined_plot

ggsave(
  filename = "scatterplots_v_a_d.png",
  plot = combined_plot,
  path = "~/Desktop/Tesi/file_r/output",
  width = 18,
  height = 6,
  units = "in",
  dpi = 300
)


## marginal distribution -----

library(ggplot2)
library(patchwork)

p_v <- ggplot(df, aes(x = V_predicted)) +
  geom_density(fill = "#5F7D8F", color = "#5F7D8F", alpha = 0.6) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(title = "Valence (positivity)", x = "V_predicted")

p_a <- ggplot(df, aes(x = A_predicted)) +
  geom_density(fill = "#5F7D8F", color = "#5F7D8F", alpha = 0.6) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(title = "Arousal (intensity)", x = "A_predicted")

p_d <- ggplot(df, aes(x = D_predicted)) +
  geom_density(fill = "#5F7D8F", color = "#5F7D8F", alpha = 0.6) +
  geom_vline(xintercept = 3, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(title = "Dominance (control)", x = "D_predicted")

combined_dist <- p_v + p_a + p_d + plot_annotation(title = "Marginal distributions")

ggsave(
  filename = "marginal_distributions.png",
  plot = combined_dist,
  path = "~/Desktop/Tesi/file_r/output",
  width = 18,
  height = 5,
  units = "in",
  dpi = 300
)




## Frequency of predicted emotions -----
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)  # for pivot_longer

df <- read_xlsx("~/Desktop/Tesi/file_r/data/df_complete_3.xlsx")
emotions <- c("admiration_pred", "amusement_pred", "anger_pred", "annoyance_pred", "approval_pred", 
              "caring_pred", "confusion_pred", "curiosity_pred", "desire_pred", "disappointment_pred", 
              "disapproval_pred", "disgust_pred", "embarrassment_pred", "excitement_pred", "fear_pred", 
              "gratitude_pred", "grief_pred", "joy_pred", "love_pred", "nervousness_pred", 
              "optimism_pred", "pride_pred", "realization_pred", "relief_pred", "remorse_pred", 
              "sadness_pred", "surprise_pred", "neutral_pred")

df_long <- df %>%
  select(all_of(emotions)) %>%
  pivot_longer(cols = all_of(emotions), names_to = "emotion", values_to = "predicted") %>%
  group_by(emotion) %>%
  summarise(
    freq = sum(predicted > 0, na.rm = TRUE),
    perc = mean(predicted > 0, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(freq > 1) %>%
  mutate(emotion = gsub("_pred", "", emotion),  # Clean labels
         perc_label = paste0(round(perc, 1), "%")) %>%
  arrange(desc(perc))

print(df_long)  # Check data
ggplot(df_long, aes(x = perc, y = reorder(emotion, perc))) +
  geom_col(fill = "#5F7D8F", color = "white", size = 0.5, width = 0.7) +
  geom_text(aes(label = perc_label), hjust = -0.25, size = 7, color = "black") +
  coord_cartesian(clip = "off", xlim = c(0, max(df_long$perc) * 1.12)) +
  labs(
    title = "Frequency of predicted emotions in VulnYouth survey",
    x = "% predicted emotions",     # X-axis label
    y = "Emotions"  
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),  # Center title
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 22),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 60, b = 10, l = 40, unit = "pt"),  # Balanced margins
    plot.title.position = "plot"  # Ensure title centering
  ) +
  scale_x_continuous(
    expand = c(0, 0),  # No extra padding beyond xlim
    labels = scales::percent_format(scale = 1, accuracy = 0.1)
  )

ggsave("Predicted emotions from self-definition of precarity.png", width = 12, height = 10, dpi = 300, bg = "white")

###  6.2.2 Socio-demographic differences: the gender gap ------
### {6.2.3. Predicting subjective precarity: emotions vs. material conditions} ----


#Fist we define dum_prec, V_dum, A_dum, D_dum, dum_normepres, dum_jobinsec, dum_endmeet, dum_unexpmix, ecoind


#this are used for the information of the subsection 6.2.3


data <- out$meta 

library(broom)
# Defining the socio-demographic categorical variables to test
# 1. Synchronize data with the filtered STM output
data <- out$meta 

# 2. Define ALL categorical variables in the SAME block
data <- data |> mutate(
  age_dum       = ifelse(ageyear >= 30, 1, 0),    # Define this here
  migr_dum      = ifelse(migrant == 3, 1, 0),     # Define this here
  dum_prec      = ifelse(prec >= 8, 1, 0),
  V_dum         = if_else(V_predicted > 3, 1, 0),
  A_dum         = if_else(A_predicted > 3, 1, 0),
  D_dum         = if_else(D_predicted > 3, 1, 0),
  dum_normepres = if_else(normepres > 0.8, 1, 0),
  dum_jobinsec  = if_else(jobinsec %in% c(1, 2), 1, 0),
  dum_endmeet   = if_else(endmeet %in% c(1, 2), 1, 0),
  dum_unexpmix  = if_else(unexpmix %in% c(1), 1, 0),
  sentg_dum = ifelse(sentiment_group == "neutral", 0, 1)
)
## Defined a dummy, as "to be assigned to a sentiment group vs being neutral"
data |> 
  count(sentg_dum)

# 3. Now run the Chi-Square map
socio_cat_vars <- c("gender", "age_dum", "migr_dum", "dum_prec")

model_logit <- glm(dum_prec ~ factor(V_dum) + factor(A_dum) + factor(D_dum), data = data, family = binomial)
model_socio1 <- glm(dum_prec ~ factor(V_dum) + factor(A_dum) + factor(D_dum) + factor(gender), data = data, family = binomial)
model_prec5 <- glm(dum_prec ~ factor(V_dum) + factor(A_dum) + factor(D_dum) + factor(dum_normepres) + factor(dum_jobinsec) + factor(dum_endmeet) + factor(dum_unexpmix) + factor(ecoind), data = data, family = binomial)
summary(model_logit)
summary(model_socio1)
summary(model_prec5)
## 1. Chi-Square test for Valence and Age
V_socio_chi_results <- map_dfr(
  socio_cat_vars,
  ~ chisq.test(data[[.x]], data$V_dum) |>
    tidy() |>
    mutate(var = .x),
  .id = NULL
)
# Check the row where var == "gender_dum" for p = 0.05, the others p value are high
tibble(V_socio_chi_results) 

## 2. Chi-Square test for Arousal and gender
A_socio_chi_results <- map_dfr(
  socio_cat_vars,
  ~ chisq.test(data[[.x]], data$A_dum) |>
    tidy() |>
    mutate(var = .x),
  .id = NULL
)
# Check the row where var == "gender_dum" for p ≈ 0.06, the others p value are high
tibble(A_socio_chi_results) 

## 3. Chi-Square test for Dominance and Age
D_socio_chi_results <- map_dfr(
  socio_cat_vars,
  ~ chisq.test(data[[.x]], data$D_dum) |>
    tidy() |>
    mutate(var = .x),
  .id = NULL
)
# Check the row where var == "gender_dum" for p ≈ 0.01, the others p value are high
tibble(D_socio_chi_results)

### chi square age_dum x V
tab_age_V <- table(data$age_dum, data$V_dum)
chi_age_V <- chisq.test(tab_age_V, correct = FALSE) |> print()
chi_age_V$expected
chi_age_V$observed



# 6.3.3 Sentiment groups and neutrality paradox: Check neutrality paradox ------

# I will use the original wide dataset (data non emo_data)
data <- data |>
  rowwise() |>
  mutate(
    any_pos = any(c_across(c(admiration_pred, amusement_pred, approval_pred,
                             caring_pred, curiosity_pred, desire_pred,
                             excitement_pred, gratitude_pred, joy_pred,
                             love_pred, optimism_pred, pride_pred,
                             realization_pred, relief_pred, surprise_pred)) == 1),
    any_neg = any(c_across(c(anger_pred, annoyance_pred, confusion_pred,
                             disappointment_pred, disapproval_pred, disgust_pred,
                             embarrassment_pred, fear_pred, grief_pred,
                             nervousness_pred, remorse_pred, sadness_pred)) == 1),
    only_neu = (neutral_pred == 1 & !any_pos & !any_neg),
    sentiment_group = case_when(
      only_neu           ~ "Neutral",
      any_pos & !any_neg ~ "Positive",
      any_neg & !any_pos ~ "Negative",
      any_pos & any_neg  ~ "Ambivalent",
      TRUE               ~ "Neutral"
    ),
    sentiment_group = factor(
      sentiment_group,
      levels = c("Neutral","Positive", "Negative", "Ambivalent")
    )
  ) |>
  ungroup()

data |> 
  count(sentiment_group) # I confirm frequencies on Tomasi's python sintax 

# bar plot sentiment_group
data |>
  ggplot(aes(x = sentiment_group)) +
  geom_bar(aes(fill = factor(sentiment_group))) +
  scale_fill_manual(
    values = c("1" = "#B0B0B0",  "2" = "#1B9E77",  "3" = "#D95F02",  "4" = "#7570B3"),
    labels = c("Neutral","Positive", "Negative", "Ambivalent"),
    name   = NULL
  ) +
  labs(
    title = "Predicted Sentiment Groups - bar plot",
    x = "Predicted emotions (sentiment groups recode)"
  ) +
  theme_minimal()
ggsave("Emo_Results/Predicted Sentiment Groups - bar plot.png", width  = 6, height = 4, units = "in", dpi = 300)

mod_glm_sentprec <- glm(
  dum_prec ~ factor(sentiment_group),
  data   = data,
  family = binomial
)
summary(mod_glm_sentprec)
# factor(sentiment_group)Negative -0.3091 0.1282 -2.411 0.0159 *; perché l'estimate ha segno negativo

# Update the binary dummy to reflect the newly calculated sentiment_group
data <- data |> 
  mutate(sentg_dum = ifelse(sentiment_group == "Neutral", 0, 1))


mod_glm_sentdum <- glm(
  dum_prec ~ factor(sentg_dum),
  data   = data,
  family = binomial
)
summary(mod_glm_sentdum) # Even emotional vs neutral gives a negative beta factor(sentg_dum)1 -0.2364 0.1252 -1.889 0.0589 *.  

## Sociodemographic characteristics
mod_socio_sent <- glm(
  sentg_dum ~ factor(gender) + factor(age_dum) + factor(migr_dum),
  data   = data,
  family = binomial
)
summary(mod_socio_sent) # factor(gender)1    0.33790    0.10201   3.312 0.000925 *** it appear to be a relationship with gender (positive)
# questo era per il Gender Result ($\beta \approx 0.34$)The finding that women are significantly more likely to employ emotional language comes from the mod_socio_sent model:

# Precarity-related drivers
mod_prec_sent <- glm(
  sentg_dum ~ factor(dum_normepres) + factor(dum_jobinsec) + factor(dum_endmeet) + factor(dum_unexpmix) + factor(ecoind),
  data   = data,
  family = binomial
)
summary(mod_prec_sent) # factor(dum_jobinsec)1  -0.26023    0.11936  -2.180   0.0292 *, (why is it negative?)
# this is Job Insecurity Result ($\beta \approx -0.26$)The finding that high objective labor instability reduces emotional expression (the "neutrality paradox") comes from the mod_prec_sent model:

library(car)
vif(mod_prec_sent)

# 6.4.1 The 20 latent themes of precariousness -------

#1. Estimating the Effects
#The "engine" that calculates these relationships is the estimateEffect function. You define the formula where the 20 topics are the dependent variables and the predicted emotions are the independent variables (covariates):
K <- 20

effects_emopred_ungrp <- estimateEffect(1:K ~ admiration_pred + amusement_pred + anger_pred + annoyance_pred + approval_pred + caring_pred + confusion_pred + curiosity_pred + desire_pred + disappointment_pred + disapproval_pred + disgust_pred + embarrassment_pred + excitement_pred + fear_pred + gratitude_pred + grief_pred + joy_pred + love_pred + nervousness_pred + optimism_pred + pride_pred + neutral_pred,  
                                        stmobj = m_ungrp_predemo,
                                        metadata = data, 
                                        uncertainty = "Global")

effects_emopred_grp <- estimateEffect(1:K ~ sentiment_group,  
                                      stmobj = m_grp_predemo,
                                      metadata = data, 
                                      uncertainty = "Global")

effects_emopred_ungrp <- estimateEffect(1:K ~ admiration_pred + ... + disappointment_pred + sadness_pred + anger_pred + ...,  
                                        stmobj = m_ungrp_predemo,
                                        metadata = data, 
                                        uncertainty = "Global")
#2. The Quantitative Summary (Your Primary Check)
#The actual numerical verification for your claims about Disappointment, Sadness, and Anger happens here:
  
summary_effects_emopred_ungrp <- capture.output((summary(effects_emopred_ungrp, topic = 1:20)), sep = "\n")
print(summary_effects_emopred_ungrp)


#4. sentiment groups con emozioni discrete ---------
library(readxl)
library(dplyr)
library(broom)

df <- read_xlsx("~/Desktop/Tesi/file_r/data/df_complete_3.xlsx")
nested_df_topics <- readRDS("/Users/macpro/Desktop/Tesi/file_r/Rev_STM 2/Processed_data/DiLiberatorePro/nested_dataframe_topics_proportions_DiLiberatore.rds")

emotions <- c("admiration_pred", "amusement_pred", "anger_pred", "annoyance_pred", 
              "approval_pred", "caring_pred", "confusion_pred", "curiosity_pred", 
              "desire_pred", "disappointment_pred", "disapproval_pred", "disgust_pred", 
              "embarrassment_pred", "excitement_pred", "fear_pred", "gratitude_pred", 
              "grief_pred", "joy_pred", "love_pred", "nervousness_pred", "optimism_pred", 
              "pride_pred", "neutral_pred")

df_emotions <- df |> select(ID, all_of(emotions))

nested_binary <- inner_join(nested_df_topics, df_emotions, by = "ID")

for (i in 1:20) {
  topic_col <- paste0("Topic", i)
  nested_binary[[topic_col]] <- ifelse(nested_binary[[topic_col]] < 0.1, 0, 1)
}

results_df <- data.frame()

for (i in 1:20) {
  topic_col <- paste0("Topic", i)
  
  formula_str <- paste(topic_col, "~", paste(emotions, collapse = " + "))
  
  mod <- glm(as.formula(formula_str), data = nested_binary, family = binomial)
  
  mod_tidy <- tidy(mod) |>
    filter(term != "(Intercept)") |>
    mutate(Topic = topic_col)
  
  results_df <- bind_rows(results_df, mod_tidy)
}

significant_effects <- results_df |>
  filter(p.value < 0.05) |>
  arrange(Topic, p.value)

print(significant_effects)

# 5 - testing stm vs emotions dataset -----
# By uncovering latent themes in the open-ended answers, STM allows us to connect specific emotional profiles (e.g. sadness vs. anger) to concrete
# topics of precariousness (such as low wages, inability to leave the parental home or the

load("/Users/macpro/Desktop/Tesi/file_r/Rev_STM 2/Processed_data/environment_diLiberatore")
df_iniziale <- read_xlsx("~/Desktop/Tesi/file_r/data/df_complete_3.xlsx")

library(dplyr)

chiavi <- c(
  "gender", "ageyear", "migrant", "region", "locality",
  "ecoind", "hh_size", "minor", "partner", "careresp",
  "carecap_1", "carecap_2", "edu", "workhours", "overedu",
  "lenght", "jobinsec", "second", "everwork", "ecosup_1",
  "ecosup_2", "ecosup_3", "ecosup_4", "ecosup_5", "ecosup_6",
  "ecosup_7", "ecosup_8", "ecosup_9", "ecosup_10", "ind_inc",
  "ecoinsec_1", "ecoinsec_2", "ecoinsec_3", "ecoinsec_4", "ecoinsec_5",
  "ecoinsec_6", "ecoinsec_7", "ecoinsec_8", "ecoinsec_9", "ecoinsec_10",
  "ecoinsec_11", "ecoinsec_12", "ecoinsec_13", "earner", "endmeet",
  "housing", "housimp", "finstrain_1", "finstrain_2", "finstrain_3",
  "finstrain_4", "finstrain_5", "ecinsreas_1", "ecinsreas_2", "ecinsreas_3",
  "ecinsreas_4", "ecinsreas_5", "ecinsreas_6", "ecinsreas_7", "ecinsreas_8",
  "ecinsreas_9", "ecinsreas_10", "health", "disab", "wlb",
  "cond3", "stnst_rev", "eman", "unexpmix", "genedu",
  "ifdropout", "who", "prec", "diff_wb_life", "diff_wb_finantialhh",
  "diff_wb_job", "diff_wb_like", "diff_wb_rel", "normepres"
)

# Trova le righe di VY che NON sono in VY_filter
casi_persi <- VY %>%
  mutate(row_originale = row_number()) %>%
  anti_join(VY_filter, by = chiavi) 

rows_da_togliere <- casi_persi$row_originale

df_finale <- df_iniziale[-rows_da_togliere, ]
df_finale <- cbind(df_finale, theta_20K)


#NOW SOME CHECK ON TOPICS AND VARIABLES:

# sadness (1211 casi) è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison <- df_finale %>%
  group_by(sadness_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  # Reshape for easier comparison
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = sadness_pred, names_prefix = "Sad_", values_from = Mean_Prevalence) %>%
  # Calculate the difference and ratio
  mutate(
    Difference = Sad_1 - Sad_0,
    Likelihood_Ratio = Sad_1 / Sad_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with sadness
print(topic_comparison)
library(pROC)
library(effsize)

#1. regression
summary(lm(sadness_pred ~ Topic13, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res <- wilcox.test(Topic13 ~ sadness_pred, data = df_finale)
print(wilcox_res)

#3. Cohen's d (Effect Size)
cohen_res <- cohen.d(Topic13 ~ sadness_pred, data = df_finale)
print(cohen_res)

# 4. Point-Biserial Correlation
cor_res <- cor.test(df_finale$Topic13, as.numeric(df_finale$sadness_pred))
print(cor_res)

# 5. Area Under the ROC Curve (AUC)
roc_res <- roc(df_finale$sadness_pred, df_finale$Topic13)
auc(roc_res)

# relationship between Topic 13 and sadness_pred is statistically significant but weak in magnitude.


## topic 13, 16, 9 with sadness

library(dplyr)
library(effsize)
library(pROC)

# 0. Logistic Regression to see if the topics should be weighted differently
model_logit <- glm(sadness_pred ~ Topic13 + Topic9 + Topic16, 
                   data = df_finale, 
                   family = binomial)
summary(model_logit)

# 1. Create the combined variable: we keep only Topic13 and Topic9

df_finale <- df_finale %>%
  mutate(Topic_Combined = Topic13 + Topic9 )
# 2. Calculate Cohen's d for the combined variable
cohen_comb <- cohen.d(Topic_Combined ~ sadness_pred, data = df_finale)

# 3. Calculate AUC for the combined variable
roc_comb <- roc(df_finale$sadness_pred, df_finale$Topic_Combined, quiet = TRUE)
auc_comb <- auc(roc_comb)

# 4. Compare results
results_comparison <- data.frame(
  Metric = c("Cohen's d (Effect Size)", "AUC (Predictive Power)"),
  Topic_13_Only = c(-0.2339, 0.5566),
  Combined_Topics = c(cohen_comb$estimate, auc_comb)
)

print(results_comparison)

# 5. Visual check: Comparison of ROC curves
plot(roc_res, col = "blue", main = "ROC Comparison: Topic 13 vs Combined")
plot(roc_comb, col = "red", add = TRUE)
legend("bottomright", legend = c("Topic 13", "Combined (13+9+16)"), col = c("blue", "red"), lwd = 2)



# disappointment (1272 casi) è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)
library(pROC)
library(effsize)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison_disappointment <- df_finale %>%
  group_by(disappointment_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = disappointment_pred, names_prefix = "Disappointed_", values_from = Mean_Prevalence) %>%
  mutate(
    Difference = Disappointed_1 - Disappointed_0,
    Likelihood_Ratio = Disappointed_1 / Disappointed_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with disappointment
print(topic_comparison_disappointment)

#1. regression
summary(lm(disappointment_pred ~ Topic14, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res_disappointment <- wilcox.test(Topic14 ~ disappointment_pred, data = df_finale)
print(wilcox_res_disappointment)

#3. Cohen's d (Effect Size)
cohen_res_disappointment <- cohen.d(Topic14 ~ disappointment_pred, data = df_finale)
print(cohen_res_disappointment)

# 4. Point-Biserial Correlation
cor_res_disappointment <- cor.test(df_finale$Topic14, as.numeric(df_finale$disappointment_pred))
print(cor_res_disappointment)

# 5. Area Under the ROC Curve (AUC)
roc_res_disappointment <- roc(df_finale$disappointment_pred, df_finale$Topic14)
auc(roc_res_disappointment)

# relationship between Topic 14 and disappointment_pred is statistically significant but weak in magnitude.


## topic 14, 13, 6 with disappointment

library(dplyr)
library(effsize)
library(pROC)

# 0. Logistic Regression to see if the topics should be weighted differently
model_logit <- glm(disappointment_pred ~ Topic14 + Topic13 + Topic6, 
                   data = df_finale, 
                   family = binomial)
summary(model_logit)

# 1. Create the combined variable: we keep all Topic14 + Topic13 + Topic6

df_finale <- df_finale %>%
  mutate(Topic_Combined_disappointment = Topic14 + Topic13 + Topic6 )
# 2. Calculate Cohen's d for the combined variable
cohen_comb_disappointment <- cohen.d(Topic_Combined_disappointment ~ disappointment_pred, data = df_finale)

# 3. Calculate AUC for the combined variable
roc_comb_disappointment <- roc(df_finale$disappointment_pred, df_finale$Topic_Combined_disappointment, quiet = TRUE)
auc_comb_disappointment <- auc(roc_comb_disappointment)


# 4. Compare results
results_comparison <- data.frame(
  Metric = c("Cohen's d (Effect Size)", "AUC (Predictive Power)"),
  Topic_14_Only = c(cohen_res_disappointment$estimate, roc_res_disappointment$auc),
  Combined_Topics = c(cohen_comb_disappointment$estimate, auc_comb_disappointment)
)

print(results_comparison)

# 5. Visual check: Comparison of ROC curves
plot(roc_res_disappointment, col = "blue", main = "ROC Comparison disappointment: Topic 13 vs Combined")
plot(roc_comb_disappointment, col = "red", add = TRUE)
legend("bottomright", legend = c("Topic 14", "Combined (14+13+6)"), col = c("blue", "red"), lwd = 2)




# # disapproval (567 casi) è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)
library(pROC)
library(effsize)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison_disapproval <- df_finale %>%
  group_by(disapproval_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = disapproval_pred, names_prefix = "Disappointed_", values_from = Mean_Prevalence) %>%
  mutate(
    Difference = Disappointed_1 - Disappointed_0,
    Likelihood_Ratio = Disappointed_1 / Disappointed_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with disapproval
print(topic_comparison_disapproval)

#1. regression
summary(lm(disapproval_pred ~ Topic5, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res_disapproval <- wilcox.test(Topic5 ~ disapproval_pred, data = df_finale)
print(wilcox_res_disapproval)

#3. Cohen's d (Effect Size)
cohen_res_disapproval <- cohen.d(Topic5 ~ disapproval_pred, data = df_finale)
print(cohen_res_disapproval)

# 4. Point-Biserial Correlation
cor_res_disapproval <- cor.test(df_finale$Topic5, as.numeric(df_finale$disapproval_pred))
print(cor_res_disapproval)

# 5. Area Under the ROC Curve (AUC)
roc_res_disapproval <- roc(df_finale$disapproval_pred, df_finale$Topic5)
auc(roc_res_disapproval)

# relationship between Topic 5 and disapproval_pred is statistically significant but weak in magnitude.


## topic 5, 3, 18 with disapproval

library(dplyr)
library(effsize)
library(pROC)

# 0. Logistic Regression to see if the topics should be weighted differently
model_logit <- glm(disapproval_pred ~ Topic5 + Topic3 + Topic18, 
                   data = df_finale, 
                   family = binomial)
summary(model_logit)

# 1. Create the combined variable: we keep all Topic14 + Topic13 + Topic6

df_finale <- df_finale %>%
  mutate(Topic_Combined_disapproval = Topic3 )
# 2. Calculate Cohen's d for the combined variable
cohen_comb_disapproval <- cohen.d(Topic_Combined_disapproval ~ disapproval_pred, data = df_finale)

# 3. Calculate AUC for the combined variable
roc_comb_disapproval <- roc(df_finale$disapproval_pred, df_finale$Topic_Combined_disapproval, quiet = TRUE)
auc_comb_disapproval <- auc(roc_comb_disapproval)


# 4. Compare results
results_comparison <- data.frame(
  Metric = c("Cohen's d (Effect Size)", "AUC (Predictive Power)"),
  Topic_5_Only = c(cohen_res_disapproval$estimate, roc_res_disapproval$auc),
  Combined_Topics = c(cohen_comb_disapproval$estimate, auc_comb_disapproval)
)

print(results_comparison)

# 5. Visual check: Comparison of ROC curves
plot(roc_res_disapproval, col = "blue", main = "ROC Comparison disapproval: Topic 5 vs Combined")
plot(roc_comb_disapproval, col = "red", add = TRUE)
legend("bottomright", legend = c("Topic 5", "Combined 3"), col = c("blue", "red"), lwd = 2)


# # neutral (464 casi) è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)
library(pROC)
library(effsize)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison_neutral <- df_finale %>%
  group_by(neutral_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = neutral_pred, names_prefix = "Disappointed_", values_from = Mean_Prevalence) %>%
  mutate(
    Difference = Disappointed_1 - Disappointed_0,
    Likelihood_Ratio = Disappointed_1 / Disappointed_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with neutral
print(topic_comparison_neutral)

#1. regression
summary(lm(neutral_pred ~ Topic5, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res_neutral <- wilcox.test(Topic5 ~ neutral_pred, data = df_finale)
print(wilcox_res_neutral)

#3. Cohen's d (Effect Size)
cohen_res_neutral <- cohen.d(Topic5 ~ neutral_pred, data = df_finale)
print(cohen_res_neutral)

# 4. Point-Biserial Correlation
cor_res_neutral <- cor.test(df_finale$Topic5, as.numeric(df_finale$neutral_pred))
print(cor_res_neutral)

# 5. Area Under the ROC Curve (AUC)
roc_res_neutral <- roc(df_finale$neutral_pred, df_finale$Topic5)
auc(roc_res_neutral)

# relationship between Topic 5 and neutral_pred is statistically significant but weak in magnitude.


## topic 5, 3, 18 with neutral

library(dplyr)
library(effsize)
library(pROC)

# 0. Logistic Regression to see if the topics should be weighted differently
model_logit <- glm(neutral_pred ~ Topic5 + Topic18 + Topic2, 
                   data = df_finale, 
                   family = binomial)
summary(model_logit)

# 1. Create the combined variable: we keep all Topic5 + Topic18

df_finale <- df_finale %>%
  mutate(Topic_Combined_neutral = Topic5 + Topic18 )
# 2. Calculate Cohen's d for the combined variable
cohen_comb_neutral <- cohen.d(Topic_Combined_neutral ~ neutral_pred, data = df_finale)

# 3. Calculate AUC for the combined variable
roc_comb_neutral <- roc(df_finale$neutral_pred, df_finale$Topic_Combined_neutral, quiet = TRUE)
auc_comb_neutral <- auc(roc_comb_neutral)


# 4. Compare results
results_comparison <- data.frame(
  Metric = c("Cohen's d (Effect Size)", "AUC (Predictive Power)"),
  Topic_5_Only = c(cohen_res_neutral$estimate, roc_res_neutral$auc),
  Combined_Topics = c(cohen_comb_neutral$estimate, auc_comb_neutral)
)

print(results_comparison)

# 5. Visual check: Comparison of ROC curves
plot(roc_res_neutral, col = "blue", main = "ROC Comparison neutral: Topic 5 vs Combined")
plot(roc_comb_neutral, col = "red", add = TRUE)
legend("bottomright", legend = c("Topic 5", "Combined 5 18"), col = c("blue", "red"), lwd = 2)


# # fear (39 casi): è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)
library(pROC)
library(effsize)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison_fear <- df_finale %>%
  group_by(fear_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = fear_pred, names_prefix = "Disappointed_", values_from = Mean_Prevalence) %>%
  mutate(
    Difference = Disappointed_1 - Disappointed_0,
    Likelihood_Ratio = Disappointed_1 / Disappointed_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with fear
print(topic_comparison_fear)

#1. regression
summary(lm(fear_pred ~ Topic2, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res_fear <- wilcox.test(Topic2 ~ fear_pred, data = df_finale)
print(wilcox_res_fear)

#3. Cohen's d (Effect Size)
cohen_res_fear <- cohen.d(Topic2 ~ fear_pred, data = df_finale)
print(cohen_res_fear)

# 4. Point-Biserial Correlation
cor_res_fear <- cor.test(df_finale$Topic2, as.numeric(df_finale$fear_pred))
print(cor_res_fear)

# 5. Area Under the ROC Curve (AUC)
roc_res_fear <- roc(df_finale$fear_pred, df_finale$Topic2)
auc(roc_res_fear)

# relationship between Topic 5 and fear_pred is statistically significant but weak in magnitude.


## topic 5, 3, 18 with fear

library(dplyr)
library(effsize)
library(pROC)

# 0. Logistic Regression to see if the topics should be weighted differently
model_logit <- glm(fear_pred ~ Topic2 + Topic1 + Topic3, 
                   data = df_finale, 
                   family = binomial)
summary(model_logit)

# 1. Create the combined variable: we keep all Topic2 + Topic1 + Topic3

df_finale <- df_finale %>%
  mutate(Topic_Combined_fear = Topic2 + Topic1 + Topic3 )
# 2. Calculate Cohen's d for the combined variable
cohen_comb_fear <- cohen.d(Topic_Combined_fear ~ fear_pred, data = df_finale)

# 3. Calculate AUC for the combined variable
roc_comb_fear <- roc(df_finale$fear_pred, df_finale$Topic_Combined_fear, quiet = TRUE)
auc_comb_fear <- auc(roc_comb_fear)


# 4. Compare results
results_comparison <- data.frame(
  Metric = c("Cohen's d (Effect Size)", "AUC (Predictive Power)"),
  Topic_2_Only = c(cohen_res_fear$estimate, roc_res_fear$auc),
  Combined_Topics = c(cohen_comb_fear$estimate, auc_comb_fear)
)

print(results_comparison)

# 5. Visual check: Comparison of ROC curves
plot(roc_res_fear, col = "blue", main = "ROC Comparison fear: Topic 2 vs Combined")
plot(roc_comb_fear, col = "red", add = TRUE)
legend("bottomright", legend = c("Topic 2", "Combined 2 1 3"), col = c("blue", "red"), lwd = 2)

# # anger (13 casi): è prevalente in qualche specifico gruppo? ----

library(dplyr)
library(tidyr)
library(pROC)
library(effsize)

# Calculate mean topic prevalence for sadness vs no-sadness
topic_comparison_anger <- df_finale %>%
  group_by(anger_pred) %>%
  summarise(across(starts_with("Topic"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = starts_with("Topic"), names_to = "Topic", values_to = "Mean_Prevalence") %>%
  pivot_wider(names_from = anger_pred, names_prefix = "Disappointed_", values_from = Mean_Prevalence) %>%
  mutate(
    Difference = Disappointed_1 - Disappointed_0,
    Likelihood_Ratio = Disappointed_1 / Disappointed_0
  ) %>%
  arrange(desc(Difference))

# View the topics most associated with anger
print(topic_comparison_anger)

#1. regression
summary(lm(anger_pred ~ Topic5, data = df_finale))  

#2. wilcoxon rank-sum test (mann-whitney-U)
wilcox_res_anger <- wilcox.test(Topic5 ~ anger_pred, data = df_finale)
print(wilcox_res_anger)

#3. Cohen's d (Effect Size)
cohen_res_anger <- cohen.d(Topic5 ~ anger_pred, data = df_finale)
print(cohen_res_anger)

# 4. Point-Biserial Correlation
cor_res_anger <- cor.test(df_finale$Topic5, as.numeric(df_finale$anger_pred))
print(cor_res_anger)

# 5. Area Under the ROC Curve (AUC)
roc_res_anger <- roc(df_finale$anger_pred, df_finale$Topic5)
auc(roc_res_anger)


## distribution of word_count ----
library(readxl)
library(ggplot2)
library(dplyr)

df <- read_xlsx("~/Desktop/Tesi/file_r/data/df_complete_3.xlsx")

# Create exact bins
df_binned <- df %>%
  mutate(
    bin = case_when(
      word_count <= 10 ~ "0-10",
      word_count <= 20 ~ "11-20", 
      word_count <= 30 ~ "21-30",
      word_count <= 40 ~ "31-40",
      word_count <= 50 ~ "41-50",
      word_count <= 60 ~ "51-60",
      word_count <= 70 ~ "61-70",
      word_count <= 80 ~ "71-80",
      word_count <= 90 ~ "81-90",
      TRUE ~ ">91"
    ),
    bin = factor(bin, levels = c("0-10", "11-20", "21-30", "31-40", "41-50", 
                                 "51-60", "61-70", "71-80", "81-90", ">91"))
  ) %>%
  count(bin, name = "frequency") %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, 1))

print(df_binned)  # See counts & %

ggplot(df_binned, aes(x = bin, y = frequency, fill = frequency)) +
  geom_col(color = "white", size = 0.8, width = 0.7, alpha = 0.85) +
  geom_text(aes(label = paste0(frequency, "\n(", percentage, "%)")), 
            vjust = -0.3, size = 7, color = "black") +
  labs(
    title = "Distribution of word count in VulnYouth dataset",
    x = "Word count bins",
    y = "Frequency (N responses)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size = 28, hjust = 0.5),
    axis.text.x = element_text(size = 18, angle = 0),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 22),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 60, 20),  # Extra bottom for labels
    legend.position = "none"
  ) +
  scale_fill_gradient(low = "#5F7D8F", high = "#5F7D8F") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("word_count_binned.png", width = 14, height = 9, dpi = 300, bg = "white")

