# Credit Risk Classification Analysis  
**Programming for Data Analysis (092024-FHI)**  
Asia Pacific University  


## 📌 Project Overview  

This repository contains the source code, dataset, and report for a **Credit Risk Classification Analysis** project developed as part of the *Programming for Data Analysis (092024-FHI)* module at Asia Pacific University.

The project applies data analytics and statistical techniques to examine factors influencing **credit risk classification (Good vs Bad)**. The analysis focuses on customer demographics, loan characteristics, financial commitments, and credit history to identify patterns and predictors of high-risk borrowers.

The dataset used contains approximately **6000 customer records**, each representing an individual’s financial and demographic profile.


## 🎯 Objectives  

The main objectives of this project are:

1. To investigate the relationship between customer demographics and credit risk classification.  
2. To analyse the effect of loan-related factors on credit risk classification.  
3. To assess the role of financial commitments on credit risk classification.  
4. To explore the impact of customer credit history on credit risk classification.  


## 📊 Dataset Description  

Dataset used:  
`3. credit_risk_classification.csv`

Each record includes:

### A. Customer Demographics
- Gender  
- Marital Status  
- Employment Status  
- Age  

### B. Credit Behaviour
- Credit Amount  
- Loan Duration  
- Instalment Commitment  
- Purpose of Loan  
- Credit History  
- Account Type  
- Residence Duration  
- Number of Dependents  

### C. Target Variable
- Credit Class (Good / Bad)


## ⚙️ Technologies & Libraries Used  

This project was implemented in **R** using the following libraries:

- `dplyr` – Data manipulation  
- `ggplot2` – Data visualization  
- `ggridges` – Ridgeline plots  
- `igraph` – Network analysis  
- `plotrix` – Advanced plotting  
- `vcd` – Visualization of categorical data  


## 🧹 Data Preparation  

The following preprocessing steps were performed:

- Handling missing values  
- Removing duplicate records  
- Filtering invalid or out-of-range values  
- Rounding numeric variables  
- Replacing missing categorical values with mode  
- Converting appropriate columns into categorical data types  
- Saving and reloading cleaned dataset  


## 🔎 Analytical Methods  

### 📌 Statistical Tests Used
- Chi-Squared Test  
- Welch Two Sample T-Test  
- Fisher’s Exact Test  
- ANOVA  
- Kruskal-Wallis Test  
- Logistic Regression  

### 📈 Visualizations Used
- Bar Charts  
- Density Plots  
- Ridgeline Plots  
- Boxplots  
- Heatmaps  
- Interaction Plots  
- Violin Plots  
- Scatter Plots  
- Tile Plots  
- Network Graphs  


## 📈 Key Findings  

### 1️⃣ Demographics & Credit Risk
- Gender and marital status show significant relationships with credit classification.  
- Divorced/separated males show higher risk patterns.  
- Age alone does not significantly moderate credit risk.  

### 2️⃣ Loan-Related Factors
- Loan purpose is significantly associated with credit classification.  
- Higher loan amounts are statistically linked to “Bad” credit classification.  
- Longer loan durations increase risk under certain employment conditions.  
- Higher instalment commitments increase probability of “Bad” classification.  

### 3️⃣ Financial Commitments
- Higher instalment commitments are strongly associated with bad credit risk.  
- Income level (proxied by savings status) significantly influences credit classification.  
- Bank payment plans are more associated with bad credit risk compared to store plans.  

### 4️⃣ Credit History
- Credit history is one of the strongest predictors of credit risk.  
- Customers with critical accounts and payment delays are significantly more likely to be classified as high-risk.  
- Logistic regression confirms predictive power of credit history variables.

  
## 📁 Project Structure  

```
├── 3. credit_risk_classification.csv
├── R Scripts
├── PFDA Group 13 Report.pdf
└── README.md
```


## ▶️ How to Run  

1. Download all files in this repository.  
2. Open the project in **RStudio**.  
3. Install required libraries (if not already installed):

```R
install.packages(c("dplyr", "ggplot2", "ggridges", "igraph", "plotrix", "vcd"))
```

4. Load the dataset:

```R
data <- read.csv("3. credit_risk_classification.csv")
```

5. Run the R scripts in sequence:
   - Data Preparation  
   - Data Analysis  


## 🧠 Hypothesis  

1. Credit risk classification is influenced by customer demographics and credit behaviours.  
2. Higher loan amounts and longer loan durations are associated with higher probability of being classified as “Bad” credit risk.  


## ⚠️ Limitations  

- Dataset limited to available features.  
- Income represented using savings status proxy.  
- Model performance metrics not deeply evaluated beyond statistical testing.  
- Results dependent on data quality after cleaning.  


## 👥 Contributors  

- Lee Jun Keat (TP067856)  
- Tham Wing Hein (TP067080)  
- Eleanor Permata Fry (TP072606)  
- Chong Zhi Yue (TP067869)  


## 📚 Conclusion  

This project demonstrates the application of statistical data analysis techniques in financial risk modelling. While loan-related factors and instalment commitments influence credit risk, **credit history emerges as one of the strongest predictors** of borrower risk classification.

The findings provide useful insights for financial institutions to improve credit evaluation strategies and risk assessment models.
