# Fraud Detection Model Benchmarking

This project presents a comparative study of ensemble aggregation strategies and advanced supervised models for detecting fraudulent e-commerce transactions in a highly imbalanced dataset. For more detailed information, please refer to the full project report:  
[Project Report](Project%20Report.pdf)

---

## 1. Introduction

Fraud detection in e-commerce is a critical business problem that involves financial risk, operational cost, and customer experience. Fraudulent transactions lead to direct financial loss and reputational damage, while overly strict detection systems may incorrectly block legitimate users.

This project approaches fraud detection as a **binary classification problem**. The goal is not only to build accurate models, but also to evaluate how different modelling approaches perform in terms of **fraud detection (recall), prediction reliability (precision), and overall usability in a real-world setting**.

---

## 2. Objective

The main objective of this project is:

> To compare baseline ensemble methods and advanced supervised models in order to identify the most effective approach for detecting fraud while balancing performance and operational constraints.

The study focuses on:

* Evaluating multiple models using a consistent framework
* Understanding trade-offs between false positives and false negatives
* Identifying models suitable for different business scenarios

---

## 3. Dataset Description

The dataset contains anonymised e-commerce transaction data with the following characteristics:

* Approximately **300,000 transactions**
* Around **6,000 users**
* Final dataset size: **~299,695 observations with 15 features**
* Each user has approximately **40–60 transactions**

### Feature Categories

* **Transaction details**: amount, merchant category, channel
* **User behaviour**: average transaction amount, total transactions
* **Security features**: CVV result, AVS match, 3DS flag
* **Geographical data**: country, BIN country, shipping distance
* **Time-related features**: transaction timestamp

### Class Distribution

* Fraudulent transactions: ~2%
* Legitimate transactions: ~98%

This strong class imbalance significantly affects model evaluation and selection.

---

## 4. Data Preparation

Data preprocessing and feature engineering were performed in **R**, while modelling was carried out in **Python**.

### 4.1 Data Cleaning

* Removed identifier columns (`transaction_id`, `user_id`) to prevent data leakage
* Verified that the dataset contains:

  * No missing values
  * No duplicate records
  * No invalid transaction amounts

### 4.2 Feature Engineering

The following features were created:

* **Time-based features**:

  * Hour, day, month, and weekday
  * Weekend indicator
  * Evening indicator

* **Cyclical encoding**:

  * Sine and cosine transformations for time variables

* **Categorical encoding**:

  * Conversion of categorical variables into encoded formats

* **Channel encoding**:

  * Binary representation for web and app transactions

### 4.3 Data Splitting and Balancing

* Stratified 80/20 train-test split
* Class imbalance handled using **SMOTE-based resampling techniques**

---

## 5. Exploratory Data Analysis

The exploratory analysis revealed several important patterns:

### 5.1 Class Imbalance

Fraudulent transactions make up a very small proportion of the data (~2%), which means accuracy alone is not a reliable performance measure.

### 5.2 Transaction Amount Patterns

Fraudulent transactions tend to appear in two main regions:

* **Low-value transactions**: often associated with card testing
* **High-value transactions**: associated with account exploitation

Mid-range transactions show overlap between fraud and legitimate activity, making them harder to classify.

### 5.3 Shipping Distance
![Shipping Distance](Data%20Understanding,%20Exploration%20&%20Feature%20Engineering/Figures/plot_3_geolocation.png)

Legitimate transactions are typically associated with shorter shipping distances, with the median distance appearing to lie in the range of approximately 50–150 km, and the majority of transactions concentrated below 500 km, reflecting stable and localised customer purchasing behaviour. In contrast, fraudulent transactions exhibit a substantially higher median shipping distance, with the median positioned closer to 800–1,000 km and a broader interquartile range extending beyond this level. Additionally, fraudulent transactions show a greater spread of long-distance outliers, frequently exceeding 1,000 km, indicating cross-regional or cross-border delivery patterns. From a business perspective, this spatial divergence suggests that fraud is more likely to involve geographically anomalous shipping locations, increasing operational risk through higher delivery costs, delayed detection, and elevated chargeback exposure. Consequently, these findings support the inclusion of distance-based geolocation features in fraud detection models and justify the use of risk controls that flag unusually distant transactions while minimising unnecessary friction for legitimate customers with consistent delivery behaviour

### 5.4 High Risk Sector
![High Risk Sector](Data%20Understanding,%20Exploration%20&%20Feature%20Engineering/Figures/plot_4_high_risk_sectors.png)
Higher fraud rates were observed in categories such as:

* Travel
* Electronics

### 5.5 Time Patterns
![Temporal Patterns](Data%20Understanding,%20Exploration%20&%20Feature%20Engineering/Figures/plot_5_temporal_patterns.png)
Legitimate transaction volume remains relatively stable throughout the day, fluctuating narrowly between approximately 12,000 and 12,500 transactions per hour, with modest increases during typical active hours and no pronounced overnight decline. In contrast, fraudulent transaction volume is consistently present across all hours, ranging roughly between 240 and 310 transactions per hour, and does not exhibit a clear reduction during standard sleeping hours (approximately 00:00–06:00 UTC). Notably, fraud volumes remain elevated during late-night and early-morning periods especially between **2am to 5am**, while legitimate activity shows less variability. From a business perspective, this temporal persistence suggests that fraudulent activity is less constrained by human behavioural cycles and is likely driven by automated or coordinated processes. This insight underscores the importance of continuous, round-the-clock fraud monitoring and supports the inclusion of time-based features in fraud detection models, enabling institutions to identify elevated risk during periods when legitimate customer activity is relatively stable but fraud remains active.

### 5.6 Security Indicators

Transactions with **AVS mismatches**, particularly on web channels, are more likely to be fraudulent.

---

## 6. Modelling Approach

The project uses two main modelling strategies.

### 6.1 Baseline Ensemble Framework
![Baseline Architecture](Comparative%20Evaluation%20Figures%20of%20Advanced%20and%20Baseline%20Models/Baseline%20Architecture%20Diagram.png)

A shared two-layer ensemble was used to allow fair comparison across aggregation methods.

#### Layer 1: Base Models

* Logistic Regression
* K-Nearest Neighbours
* Random Forest
* Gaussian Naive Bayes

These models capture different types of relationships in the data.

#### Layer 2: Aggregation Strategies

The following methods were used to combine predictions:

* Entropy-Based Confidence Weighting
* Max Probability Aggregation
* Threshold-Aware Aggregation
* Weighted Average Aggregation
* Trimmed Mean Aggregation

This allows analysis of how different aggregation strategies influence performance.

---

### 6.2 Advanced Models

The following standalone models were also implemented:

* XGBoost
* Linear SVM
* LDA (Linear Discriminant Analysis)
* RIPPER (rule-based model)
* Autoencoder + MLP

These models provide a comparison with the ensemble approach.

---

## 7. Evaluation Methodology

Due to class imbalance, the following metrics were used:

* **Recall** – ability to detect fraudulent transactions
* **Precision** – accuracy of fraud predictions
* **F1-score** – balance between precision and recall
* **ROC-AUC** – overall classification performance
* **PR-AUC (Average Precision)** – performance on imbalanced data
* **Confusion matrix** – interpretation of prediction outcomes

Accuracy was not used as a primary metric.

---

## 8. Results & Key Insights

This project evaluates fraud detection models from a practical perspective.  
Instead of focusing only on accuracy, the analysis considers:

- **Recall** (how many fraud cases are detected)  
- **Precision** (how reliable the alerts are)  
- **False positives** (operational cost of incorrect alerts)  

This is important because fraud datasets are highly imbalanced, and accuracy alone can be misleading.

---

### 1. Impact of Aggregation Strategy

All baseline models used the same set of base learners:
- Logistic Regression  
- K-Nearest Neighbours  
- Random Forest  
- Gaussian Naive Bayes  

However, changing only the **aggregation method** produced large differences in performance.

- Recall ranged from approximately **65% to 91%**  
- Precision ranged from approximately **10% to 51%**  
- False positives reduced from around **10,000 to 800**

**Key Insight:**  
The way models are combined has a major impact on results. Aggregation is not just a technical detail—it directly affects how the system performs in real-world use.

---

### 2. Trade-off Between Recall and Precision

Two clear types of model behaviour were observed:

#### High Recall Models (Max Probability, Threshold-Aware)
- Detect more than **90% of fraud cases**
- Generate a large number of false positives (~10,000)

These models are useful when the goal is to detect as much fraud as possible.  
However, they are not suitable for direct use in production due to the high number of incorrect alerts.

---

#### Balanced Models (Trimmed Mean, Weighted Average)
- Recall between **65% and 82%**
- Much higher precision (up to **51%**)
- Significantly fewer false positives (around **800 to 3,800**)

These models produce more reliable alerts and are more suitable for real-world deployment.

---

### 3. Best Baseline Model

The **Trimmed Mean aggregation** provided the best balance between fraud detection and alert quality.

- Precision: **0.514**  
- Recall: **0.658**  
- F1-score: **0.577**  
- False positives: **~823**

**Key Insight:**  
This model generates high-quality alerts while keeping false positives low, making it suitable for operational use.

---

### 4. Best Advanced Model

Among the advanced models, **XGBoost** achieved the strongest overall performance.

- Recall: **0.870**  
- Precision: **0.334**  
- ROC-AUC: **0.974**  
- PR-AUC: **0.820**

**Key Insight:**  
XGBoost is highly effective at ranking transactions by risk, making it well-suited for fraud detection systems that prioritise investigation.

---

### 5. Baseline vs Advanced Model Comparison

The most important comparison in this project is between:

- **Trimmed Mean (best baseline)**  
- **XGBoost (best advanced model)**  

| Model | Strength | Limitation |
|------|--------|-----------|
| Trimmed Mean | High precision, fewer false positives | Lower recall |
| XGBoost | High recall, strong detection ability | More false positives |

**Conclusion:**
- If the goal is **accurate and reliable alerts**, Trimmed Mean is more suitable  
- If the goal is **maximum fraud detection**, XGBoost performs better  

---

### 6. Visual Comparison

#### ROC Curve Comparison
![ROC Comparison](Comparative%20Evaluation%20Figures%20of%20Advanced%20and%20Baseline%20Models/unique_vs_bestbaseline_roc_curves.png)

#### Precision-Recall Trade-off
![PR Comparison](Comparative%20Evaluation%20Figures%20of%20Advanced%20and%20Baseline%20Models/unique_vs_bestbaseline_recall_precision.png)

---

### 7. Final Takeaway

There is no single model that performs best in all situations.

- High-recall models detect more fraud but increase operational cost  
- High-precision models reduce false alerts but may miss some fraud  

The most effective approach is to **combine models and adjust them based on business needs**.

This project shows that **model selection should be guided by the desired balance between fraud detection and operational efficiency**, rather than relying on a single metric.

---

## 9. Business Implications

The results show that model selection depends on business priorities:

| Objective                       | Recommended Approach            |
| ------------------------------- | ------------------------------- |
| Maximise fraud detection        | Recall-focused ensemble methods |
| Balanced performance            | Trimmed Mean aggregation        |
| Risk scoring and prioritisation | XGBoost                         |

In practice, an effective fraud detection system should combine:

* A high-recall screening model
* A ranking model for prioritisation
* Human review processes

---

## 10. Project Structure

```text
FRAUD-DETECTION-MODEL-BENCHMARKING/
│
├── Advance Models/
│   ├── results/
│   ├── Autoencoder+MLP_model.ipynb
│   ├── LDA_Model.ipynb
│   ├── Linear_SVM_Model.ipynb
│   ├── RIPPER_model.ipynb
│   └── XGBoost_Model.ipynb
│
├── Baseline Models/
│   ├── results/
│   ├── Baseline_model_with_entropy_aggregation.ipynb
│   ├── Baseline_model_with_max_probability_aggregation.ipynb
│   ├── Baseline_model_with_threshold_aware_aggregation.ipynb
│   ├── Baseline_model_with_weighted_average_aggregation.ipynb
│   └── Trimmed_Mean_Aggregation_Baseline_model.ipynb
│
├── Comparative Evaluation of Advanced and Baseline Models/
│
├── Data Understanding, Exploration & Feature Engineering/
│   ├── Dataset & Feature Engineered data/
│   │   ├── transactions.csv
│   │   ├── transactions_pre_processed.csv
│   │   ├── train.csv
│   │   └── test.csv
│   │
│   ├── Figures/
│   └── Data_preprocessing.R
│
├── Project Report.pdf
├── venv/
├── .gitignore
├── requirements.txt
└── README.md
```

---

## 11. Technologies Used

* Python (scikit-learn, XGBoost, imbalanced-learn)
* R (tidyverse for data preprocessing and analysis)
* Jupyter Notebook
* Matplotlib and Seaborn

---

## 12. Conclusion

This project shows that no single model is optimal for fraud detection. The best approach depends on the balance between fraud detection performance and operational cost.

Key conclusions:

* Ensemble aggregation significantly influences performance
* XGBoost provides strong standalone performance
* High-recall models are useful but can generate excessive false positives
* A multi-stage approach combining different models is most effective

---

