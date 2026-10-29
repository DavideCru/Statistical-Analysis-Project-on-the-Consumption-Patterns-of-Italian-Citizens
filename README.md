# Statistical Analysis of Italian Household Consumption Patterns

**Project type:** University group project (6 students), BSc Economics and Big Data – Roma Tre University, June 2025<br>
**Tools:** R (`lm`, `glm`, `kmeans`, `corrplot`, `ROCR`, `ggplot2`)<br>
**Data:** [ADD SOURCE – e.g. survey name and year], household-level microdata on income, consumption and socio-economic characteristics

## ❓ Question
What drives household consumption in Italy, and how do consumption patterns differ across income levels, employment conditions and geographic regions?

## 🔎 Methodology
- **Simple linear regression** – relationship between net available income and consumption (propensity to consume)
- **Multiple linear regression** – joint effect of income, household size, employment status, region, transportation expenses and number of income earners
- **Logistic regression** – probability of having experienced long-term unemployment (> 6 months), evaluated with ROC/AUC
- **K-means clustering** – identification of homogeneous economic behaviour groups
- **Boxplot comparison** – consumption distributions across North, Centre and South

## 📈 Key findings
- Net available income is the strongest predictor of consumption: **R² ≈ 0.56** in the simple model, rising to **R² ≈ 0.65** in the multiple model
- Household size and transportation expenses have a positive effect on consumption
- Geographic region and employment status show statistically significant differences
- The logistic model shows moderate discriminative ability (**AUC ≈ 0.65**) for long-term unemployment
- Clustering identifies three profiles: low-income/low-consumption, middle-income/moderate-consumption and high-income/high-consumption households
- Southern Italy shows generally lower consumption levels than the North and Centre

## 🧠 Conclusion
Income is the main determinant of household consumption in Italy, but household composition, employment status, transport costs and region also play a significant role, revealing clear socio-economic and territorial disparities.

## 📁 Repository structure
| File | Content |
|---|---|
| `Statistical Analysis Project on the Consumption Patterns of Italian Citizens.R` | Full R code (regressions, logistic model, ROC, clustering, plots) |
| `Data Set - Statistical Analysis Project on the Consumption Patterns of Italian Citizens.rda` | Dataset used in the analysis |
| `Relation - Statistical Analysis Project on the Consumption Patterns of Italian Citizens.docx` | Written report with methodology and interpretation |

<p align="center">
<img width="47%" alt="image" src="https://github.com/user-attachments/assets/a1fd94b3-2885-47eb-bf07-a943b0ff2a1f" />
<img width="47%" alt="image" src="https://github.com/user-attachments/assets/ff1de298-3601-4491-8ec3-0967e55d7f22" />
<img width="47%" alt="image" src="https://github.com/user-attachments/assets/df5ca83e-bb71-475b-b505-977e7a6ae10a" />
<img width="47%" alt="image" src="https://github.com/user-attachments/assets/9efa1f66-b9c9-4db0-9b54-4b841dfafb56" />
</p>


