# shrews_food_preferences_lead
Ecotoxicological &amp; behavioral analysis: Consumption of lead-contaminated food by shrews
This repository contains the reproducible data processing pipelines, statistical models, and visualization scripts developed for our scientific study investigating whether selected shrew species (the common shrew Sorex araneus and the lesser white-toothed shrew Crocidura gueldenstaedtii) can detect and actively avoid contaminated food. 

---

## 🔬 Project Background & Objectives
Environmental pollutants, particularly heavy metals, pose severe threats to wildlife populations across urban and natural habitats. Shrews are a group of mammals particularly vulnerable to the adverse effects of metals in the soil due to their diet, which primarily consists of soil-dwelling invertebrates
This research integrates standardized ecological sampling with advanced statistical modeling to evaluate:
* **Differences in food intake quantity and type** variation in the mass of food types consumed by shrews.
* **Distinct behaviours signalling preference** the responses of the tested animals to the offered prey were classified as positive reactions, indicating preference, or negative reactions, indicating lack of preference and avoidance of a given food type.
* **Order of food selection** The sequence in which different food types were consumed was examined using the method proposed by Rodgers in combination with the Kaplan–Meier estimator. 

---

## 🛠️ Tech Stack & Methodology
* **Statistical Environment:** R / RStudio (`tidyverse`, `lme4`, `lmerTest`, `emmeans`, `ggplot2`,`ggpattern`,`scales`,`survival`,`survminer`).
* **Statistical Modeling:** Linear mixed-effects models (LMMs), estimated marginal means (EMMs; least-squares means), the Kaplan–Meier estimator.

---

## 📁 Repository Structure
```text
├── data/                 # Raw and processed anonymized datasets (.csv / .xlsx)
├── r_scripts/            # Core R scripts for statistical modeling and data visualization
│   ├── 01_data_preprocessing.R
│   ├── 02_lmm_models.R
│   └── 03_visualizations.R
├── outputs/              # Generated high-resolution figures (ggplot2) and model summaries
└── README.md             # Project documentation
