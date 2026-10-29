# Thesis Project: Textual Emotion Analysis

This repository contains all the code, analysis notebooks, trained models, and processed data for the Master's thesis project in Mathematical Engineering, Statistical Learning.

The main objective of the project is the analysis of emotions in text using three main approaches:
1.  **Componential Model**: Multi-label classification of discrete emotions (e.g., *anger, joy, sadness*).
2.  **Dimensional Model**: Regression onto three continuous scales (Valence, Arousal, Dominance) to map emotions in a VAD space.
3.  **Structural Topic Model (STM)**: To discover latent topics in the text and analyze how their prevalence and content relate to document metadata (such as emotions or demographics).

## Repository Structure

The `tesi` root folder is organized into the following main sub-directories:

### 1. `/dimensional_model`
Contains the Python-based implementation, training, and inference for the VAD regression model (based on EmoBank/Warriner).
* `/data`: Original datasets.
* script that you need to imported in Google Colab to use GPU
Please note that here we create [text](scripts/analysis/data/english_documents_cleaned.xlsx) that will be used in componential_model

### 2. `/componential_model`
Contains the Python-based implementation, training, and inference for the multi-label classification model (based on GoEmotions/Emobank).

### 3. `/stm`
Contains the R-based project for Structural Topic Modeling. This analysis discovers latent themes in the text and models their relationship with metadata (e.g., prevalence of a topic based on an emotion).
* `/data`: dataset_last.rds of Di Liberatore, containing all her environment where she obtain the 20 latent themes labels. In addition, there is also df_complete_3 that is english responses already labeled in V,A,D and 28 Emotion labels.
* `Stm_Model_0125.Rmd` I retrained the STM processed, and noticed the non difference between the two framework (baseline of DI Liberatore and Topic Prevalence with Covariates)
* `Rev_Emotion_0125.Rmd` Preliminary study of Emotions and Stm model, more detail in analysis script R below.

### 4. `/analysis`
This folder contains the script used to analyze, combine, and visualize the results from all models that have been added in the thesis.

