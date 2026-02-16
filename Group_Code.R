#Install packages and load libraries needed
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggridges")
library(ggridges)
install.packages("igraph")
library(igraph)
install.packages("plotrix")
library(plotrix)
install.packages("vcd")
library(vcd)

#Read Data Table
AData <- read.csv("C:\\Users\\Dev\\Downloads\\Assignment\\Assignment\\3. credit_risk_classification.csv", sep=",", header=TRUE)
#Table View
View(AData)

#View Missing Data
colSums(is.na(AData))
sum(complete.cases(AData))
which(is.na(AData))
sum(is.na(AData))

#Remove Unused Column
AData<- AData %>% select(-X)

#Remove Duplicated Data
AData <- AData %>% distinct()
sum(duplicated(AData))

#Remove Error In age
AData <- AData[AData$age >= 18 & AData$age <= 100, ]
AData$age <- as.integer(AData$age)

#Remove Error In duration
AData$duration <- round(AData$duration)
AData <- AData[AData$duration >= 4 & AData$duration <= 72, ]

#Remove Error In installment commitment
AData$installment_commitment <- round(AData$installment_commitment)
AData <- AData[AData$installment_commitment >= 1 & AData$installment_commitment <= 4, ]

#Remove Error In residence since
AData$residence_since <- round(AData$residence_since)
AData <- AData[AData$residence_since >= 1 & AData$residence_since <= 4, ]

#Remove Error In existing credits
AData$existing_credits <- round(AData$existing_credits)
AData <- AData[AData$existing_credits >= 1 & AData$existing_credits <= 4, ]

#Remove Error In number dependents
AData$num_dependents <- round(AData$num_dependents)
AData <- AData[AData$num_dependents >= 1 & AData$num_dependents <= 2, ]

#Filling other_payment_plans 
AData$other_payment_plans[AData$other_payment_plans == ""] <- NA
mode_value <- names(sort(table(AData$other_payment_plans), decreasing = TRUE))[1]
print(mode_value) # shows stores
AData$other_payment_plans[is.na(AData$other_payment_plans)] <- mode_value

#Change Label Name
AData <- AData%>%
  mutate(
    personal_status = case_when(
      grepl("male", personal_status) & grepl("single", personal_status) ~ "male single",
      grepl("female", personal_status) & grepl("div|dep|mar", personal_status) ~ "female divorced/dependent/married",
      grepl("male", personal_status) & grepl("div|sep", personal_status) ~ "male divorced/separated",
      grepl("male", personal_status) & grepl("mar|wid", personal_status) ~ "male married/widowed",
      TRUE ~ personal_status
    )
  )

AData <- AData %>%
  mutate(
    job = case_when(
      job == "skilled" ~ "skilled",
      job == "unskilled resident" ~ "unskilled, resident",
      job == "high qualif/self emp/mgmt" ~ "high qualification, self-employed, management",
      job == "unemp/unskilled non res" ~ "unemployed, unskilled, non-resident",
      TRUE ~ job
    )
  )

AData <- AData %>%
  mutate(
    employment = case_when(
      employment == ">=7" ~ "7 or more years",
      employment == "1<=X<4" ~ "1 to 3 years",
      employment == "4<=X<7" ~ "4 to 6 years",
      employment == "<1" ~ "less than 1 year",
      employment == "unemployed" ~ "unemployed",
      TRUE ~ "other"
    )
  )

#Conversion
AData$age <- as.numeric(AData$age)
AData$credit_amount <- as.numeric(AData$credit_amount)
AData$installment_commitment <- as.numeric(AData$installment_commitment)
AData$duration <- as.numeric(AData$duration)
AData$residence_since <- as.numeric(AData$residence_since)
AData$existing_credits <- as.numeric(AData$existing_credits)
AData$num_dependents <- as.numeric(AData$num_dependents)

#credit history conversion
AData$credit_history <- gsub("delayed previously", "delays", AData$credit_history)
AData$credit_history <- gsub("critical/order existing credit", "critical accounts", AData$credit_history)
AData$credit_history <- gsub("no credits/all paid", "all paid",AData$credit_history)

#add column "gender"
AData$gender <- ifelse(grepl("female", AData$personal_status, ignore.case =TRUE), "female",
                       ifelse(grepl("male", AData$personal_status, ignore.case = TRUE), "male", NA))

#Validate Dataset
#Recheck structure and summary
str(AData)
summary(AData)

#Save Cleaned Dataset
write.csv(AData, "C:\\Users\\Dev\\Downloads\\Assignment\\Assignment\\3. cleaned_credit_risk_classification.csv", row.names = FALSE)

#Read Cleaned Dataset
AData <- read.csv("C:\\Users\\Dev\\Downloads\\Assignment\\Assignment\\3. cleaned_credit_risk_classification.csv")
View(AData)

############################################################################################################################################

#Objective 1: To investigate the relationship between customer demographics and credit risk classification.
#analysis 1-1: Is there a significant relationship between customer gender and credit risk classification?
personal_status_frequency <- AData %>%
  count(personal_status,class) %>%
  as_tibble()

personal_status_frequency_tibble <- personal_status_frequency

status_class_counts <- as.data.frame(table(AData$personal_status, AData$class))
colnames(status_class_counts) <- c("personal_status", "class", "count")

status_class_counts <- status_class_counts %>%
  group_by(personal_status) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(status_class_counts, aes(x = personal_status, y = count, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
            size = 3) +
  labs(title = "Comparison of Personal Status by Class (Count with Percentage Labels)", 
       x = "Personal Status", y = "Count") +
  scale_fill_manual(values = c("good" = "blue", "bad" = "red"))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#chi square test
chi_table <- table(AData$personal_status, AData$class)
cstest <- chisq.test(chi_table)
cstest

#----------

#analysis 1-2: Does marital status significantly influence credit risk classification?
job_frequency <- AData %>%
  count(job,class) %>%
  as_tibble()
job_frequency

skilled_data <- job_frequency %>% filter(job == "skilled")
labels <- paste0(skilled_data$class, " (", skilled_data$n, ") ", "(",round(skilled_data$n / sum(skilled_data$n) * 100, 1), "%)")
pie3D(skilled_data$n, 
      labels = labels, 
      main = "Credit Class Distribution for Job Skills: Skilled", 
      explode = 0.15, 
      col = c("blue", "red"), 
      labelcex = 1)

unskilled_data <- job_frequency %>% filter(job == "unskilled, resident")
labels <- paste0(unskilled_data$class, " (", unskilled_data$n, ") ", "(",round(unskilled_data$n / sum(unskilled_data$n) * 100, 1), "%)")
pie3D(unskilled_data$n, 
      labels = labels, 
      main = "Credit Class Distribution for Job Skills: Unskilled/resident", 
      explode = 0.15, 
      col = c("blue", "red"), 
      labelcex = 1)

highskilled_data <- job_frequency %>% filter(job == "high qualification, self-employed, management")
labels <- paste0(highskilled_data$class, " (", highskilled_data$n, ") ", "(",round(highskilled_data$n / sum(highskilled_data$n) * 100, 1), "%)")
pie3D(skilled_data$n, 
      labels = labels, 
      main = "Credit Class Distribution for Job Skills: High qualification/ self-employed/ management", 
      explode = 0.15, 
      col = c("blue", "red"), 
      labelcex = 1)

#----------

#analysis 1-3: How does employment status interact with credit risk classification?
foreign_worker_frequency <- AData %>%
  count(foreign_worker,class) %>%
  as_tibble()
foreign_worker_frequency

foreign_worker_frequency2 <- AData %>%
  count(foreign_worker) %>%
  as_tibble()
foreign_worker_frequency2

ggplot(AData, aes(x = foreign_worker, fill = class)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Credit Class by Foreign Worker Status",
       x = "Foreign Worker Status", y = "Proportion") +
  scale_fill_manual(values = c("good" = "blue", "bad" = "red")) +
  scale_y_continuous(labels = scales::percent)

#----------

#analysis 1-4: What are the combined effects of gender, marital status, and employment status on credit risk classification?
data_summary <- AData %>%
  count(gender, class) %>%
  group_by(gender) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(data_summary, aes(x = gender, y = n, group = class, color = class)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(n, " (", round(percentage, 1), "%)")), 
            vjust = -0.5, size = 3.5) +
  labs(title = "Line Graph of Gender and Credit Class",
       x = "Gender", y = "Count") +
  scale_color_manual(values = c("good" = "blue", "bad" = "red")) +
  theme_minimal()

#----------
#analysis 1-5: What are the combined effects of gender, job skills, on credit risk classification?
ggplot(AData, aes(x = "", fill = class)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar("y") +
  facet_wrap(~gender, scales = "free_y") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  labs(title = "Credit Risk Distribution by Gender",
       x = "",
       y = "",
       fill = "Credit Risk") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom")

ggplot(AData, aes(x = factor(1), fill = class)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  facet_wrap(~job, labeller = label_both, scales = "free_y") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  labs(title = "Credit Risk Distribution by Job Skills",
       x = "",
       y = "",
       fill = "Credit Risk") +
  theme_void() +
  theme(legend.position = "bottom")

ggplot(AData, aes(x = factor(1), fill = class)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  facet_wrap(~foreign_worker, labeller = label_both,scales = "free_y") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  labs(title = "Credit Risk Distribution by Foreign Status",
       x = "",
       y = "",
       fill = "Credit Risk") +
  theme_void() +
  theme(legend.position = "bottom")

############################################################################################################################################

#Objective 2: To analyze the effect of loan-related factors on credit risk classification.
#analysis 2-1: Is there any relationship between loan purpose and credit risk classification? (THAM WING HEIN TP067080)
#view frequency of values for 'purpose' and 'class' separately
table(AData$purpose)
table(AData$class)

#create contingency table for loan purpose and credit risk classification
PurposeClass_table <- table(AData$purpose,AData$class)
print(PurposeClass_table)
prop.table(PurposeClass_table,1)

#generate bar plot of the contingency table
ggplot(AData, aes(x = purpose, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Loan Purpose vs Credit Risk Classification", x = "Loan Purpose", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pink","lightgreen"),name = "Credit Risk") +
  geom_text(stat = "count", aes(label = ..count..), position =position_dodge(width = 0.8), vjust = -0.5)

#run Chi-squared Test of Independence
PurposeClass_chisqr <- chisq.test(PurposeClass_table)
print(PurposeClass_chisqr)

#----------

#analysis 2-2: Does the duration of the loan (installment duration) influence the credit risk classification?
str(AData)    #check structure of data
unique(AData$class)   #check unique values in 'class' column
summary(AData$duration)   #statistics of 'duration' column

#create contingency table for loan duration and credit risk classification
DurationClass_table <- table(AData$duration,AData$class)
print(DurationClass_table)

#generate density plot of the loan duration across credit risk classifications
ggplot(AData, aes(x = duration, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Loan Duration by Credit Risk Classification",
       x = "Loan Duration (months)",
       y = "Density") +
  scale_fill_manual(values = c("magenta","green"),name = "Credit Risk")

#----------

#analysis 2-3: Are loan amounts a strong predictor for credit risk classification?
#summarize credit amount by credit risk classification
summary(AData$credit_amount)
summary(AData$class)
str(AData$credit_amount)
str(AData$class)

Class_summary <- AData %>%
  group_by(class) %>%
  summarize(CreditAmount_mean = mean(credit_amount,na.rm = TRUE),
            CreditAmount_median = median(credit_amount,na.rm = TRUE),
            CreditAmount_sd = sd(credit_amount,na.rm = TRUE))
print(Class_summary)

#generate box plot of the loan amount vs credit risk classification
ggplot(AData, aes(x = class, y = credit_amount, fill = class)) +
  geom_boxplot() +
  labs(title = "Loan Amount vs Credit Risk Classification",
       x = "Credit Risk Classification",
       y = "Loan Amount") +
  scale_fill_manual(values = c("pink","lightgreen"),name = "Credit Risk")

#Run Welch Two Sample t-test (Extra Feature)
CreditAmountClass_ttest <- t.test(credit_amount ~ class, data = AData)
print(CreditAmountClass_ttest)

#----------

#analysis 2-4: Does employment status interacts with loan duration to influence credit risk classification?
#convert employment status and credit risk classification columns to factors
AData$employment <- as.factor(AData$employment)
AData$class <- as.factor(AData$class)
AData$duration <- as.numeric(AData$duration)
str(AData)   #check the structure

#create and output summarized dataset for interaction plotting
summary2_4 <- AData %>%
  group_by(employment, duration, class) %>%
  summarize(count = n()) %>%
  ungroup()
print(summary2_4, n = 267)

#create interaction plot of employment status and loan duration by credit risk classification (Extra Feature)
ggplot(summary2_4, aes(x = duration, y = count, color = employment, group = employment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ class) +
  labs(
    title = "Interaction of Employment Status and Loan Duration on Credit Risk Classification",
    x = "Loan Duration (Months)",
    y = "Count of Classification",
    color = "Employment Status"
  )

#Run Logistic Regression Model with interaction term
model2_4 <- glm(class ~ employment * duration, data =AData, family = "binomial")
summary(model2_4)

#----------

#analysis 2-5: Does the proportion of income committed to loan payments(installment commitment) influence the likelihood of higher credit risk classification?
str(AData)    #check structure of data
summary(AData$installment_commitment)   #statistics of 'installment_commitment' column

#summarize installment commitment and credit risk classification
summary2_5 <- AData %>%
  count(installment_commitment, class)
print(summary2_5)

#generate histogram of installment commitment by credit risk classification
ggplot(AData, aes(x = installment_commitment, fill = class)) +
  geom_histogram(binwidth = 0.5, position = "stack") +
  labs(title = "Distribution of Installment Commitment by Credit Risk Classification",
       x = "Installment Commitment (Proportion of Income)",
       y = "Frequency") +
  scale_fill_manual(values = c("magenta","green"),name = "Credit Risk")

############################################################################################################################################

#Objective 3: To Assess the Role of Financial Commitments on Credit Risk Classification.
#analysis 3-1: Is There a Relationship Between Installment Commitments and Credit Risk Classification?
# Step 1: Data Overview
str(AData)   # Check Structure of Data

# Step 2: Descriptive Statistics
# Calculate Mean, Median, and Standard Deviation of Installment Commitments by Credit Risk Class
installment_summary <- AData %>%
  group_by(class) %>%
  summarise(
    avg_installment = mean(installment_commitment, na.rm = TRUE),
    median_installment = median(installment_commitment, na.rm = TRUE),
    sd_installment = sd(installment_commitment, na.rm = TRUE)
  )
print(installment_summary)

# Step 2: Data Visualization
# 2.1 Density Plot: Counts of Credit Risk by Other Payment Plans
ggplot(AData, aes(x = installment_commitment, fill = class)) +
  geom_density(alpha = 0.5, color = "black") + # Add Border
  scale_fill_manual(values = c("good" = "green", "bad" = "red")) +
  labs(
    title = "Density Plot of Installment Commitments by Credit Risk Classification",
    x = "Number of Installment Commitments",
    y = "Density",
    fill = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) +  # Increase Font Size
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),  # Add Grid Lines
    panel.grid.minor = element_blank()
  )

# 2.2 Density Ridgeline Plot: Comparing Installment Between 'Good' and 'Bad' Credit Risk Classes
ggplot(AData, aes(x = installment_commitment, y = class, fill = class)) +
  geom_density_ridges(alpha = 0.6, color = "white") +
  scale_fill_manual(values = c("good" = "green", "bad" = "red")) +
  labs(
    title = "Ridgeline Plot of Installment Commitments by Credit Risk Classification",
    x = "Number of Installment Commitments",
    y = "Credit Risk Class",
    fill = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major = element_line(color = "lightgray"), 
    panel.grid.minor = element_blank()
  )

# Step 3: Statistical Test
# T-Test (Evaluate Differences in Mean Installment Commitments Between Credit Risk Classes)
good_class <- AData %>% 
  filter(class == "good") %>% 
  pull(installment_commitment)

bad_class <- AData %>% 
  filter(class == "bad") %>% 
  pull(installment_commitment)

t_test_class_result <- t.test(good_class, bad_class, var.equal = FALSE)
print(t_test_class_result)

#----------

#analysis 3-2: Does Income Level Predict Credit Risk Classification When Considered Alongside Installment Commitment?
# Step 1: Data Visualization
# 2.1 Bar Chart: Proportion of Credit Risk Classifications by Income Level and Installment Commitment

# Calculate Proportions
bar_data <- AData %>%
  group_by(savings_status, installment_commitment, class) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Visualize
ggplot(bar_data, aes(x = savings_status, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  facet_wrap(~ installment_commitment) + 
  scale_fill_manual(values = c("good" = "darkgreen", "bad" = "red")) + 
  labs(title = "Credit Risk Classifications by Income Level and Installment Commitment",
       x = "Income Level",
       y = "Proportion",
       fill = "Credit Risk Class") +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2.2 Heat Map: Bad Credit Risk Proportions by Income Level and Installment Commitment
heatmap_data_bad <- AData %>%
  group_by(savings_status, installment_commitment, class) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  filter(class == "bad")

ggplot(heatmap_data_bad, aes(x = savings_status, y = installment_commitment, fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Proportion") +
  labs(title = "Proportion of Bad Credit Risks by Income Level and Installment Commitment",
       x = "Income Level",
       y = "Number of Installment Commitment"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), color = "black", size = 4) 

# 2.3 Heat Map: Good Credit Risk Proportions by Income Level and Installment Commitment
heatmap_data_good <- AData %>%
  group_by(savings_status, installment_commitment, class) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  filter(class == "good")

ggplot(heatmap_data_good, aes(x = savings_status, y = installment_commitment, fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkgreen", name = "Proportion") +
  labs(title = "Proportion of Good Credit Risks by Income Level and Installment Commitment",
       x = "Income Level",
       y = "Number of Installment Commitment"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), color = "black", size = 4) 

# Step 2: Statistical Tests
# Fisher's Exact Test 
# Association Between Income Level and Credit Risk Classification
fisher_income_class <- fisher.test(
  table(AData$savings_status, 
        AData$class), 
  simulate.p.value = TRUE
)

# Association Between Installment Commitments and Credit Risk Classification
fisher_installment_class <- fisher.test(
  table(AData$installment_commitment, 
        AData$class), 
  simulate.p.value = TRUE
)

# Association Between Installment Commitments and Income Level
fisher_income_installment <- fisher.test(
  table(AData$savings_status, 
        AData$installment_commitment), 
  simulate.p.value = TRUE
)

print(fisher_income_class)
print(fisher_installment_class)
print(fisher_income_installment)

#----------

#analysis 3-3: How Does the Presence of Other Financial Commitments (e.g., Other Payment Plans) Influence Credit Risk Classification?
# Step 1: Data Visualization
# 1.1 Bar Chart: Counts of Credit Risk by Other Payment Plans
ggplot(AData, aes(x = other_payment_plans, fill = class)) +
  geom_bar(position = "dodge", color = "white", width = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("lightcoral", "lightgreen")) + 
  labs(
    title = "Counts of Credit Risk by Other Payment Plans",
    x = "Other Payment Plans",
    y = "Number of Individuals (Counts)",
    fill = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate X-Axis
  )

# 1.2 Pie Chart: Proportion of Credit Risk Classes (Good and Bad) by Other Payment Plans
# Prepare Data
pie_chart_data <- AData %>%
  group_by(other_payment_plans, class) %>%
  summarise(count = n()) %>%  # Count Occurrence of Each Class
  ungroup() %>%
  group_by(other_payment_plans) %>%
  mutate(proportion = count / sum(count) * 100)  # Calculate Number of Individuals as Precentage

# Generate Pie Chart
ggplot(pie_chart_data, aes(x = "", y = proportion, fill = class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +  # Convert Bar Chart to Pie Chart
  facet_wrap(~ other_payment_plans, ncol = 2) +  # Facet by Other Payment Plans
  scale_fill_manual(values = c("lightcoral", "lightgreen")) +
  labs(
    title = "Proportion of Credit Risk Classes by Other Payment Plans",
    fill = "Credit Risk Class"
  ) +
  theme_void(base_size = 12) +
  geom_text(
    aes(label = paste0(round(proportion, 1), "%")),
    position = position_stack(vjust = 0.5),  
    size = 4 
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  )

# Step 2: Statistical Test
# Fisher's Exact Test (Association Between Other Payment Plans and Credit Risk Classification)
fisher_plans_class <- fisher.test(
  table(AData$other_payment_plans, 
        AData$class)
)
print(fisher_plans_class)

#----------

#analysis 3-4: What Demographic Factors (e.g., Age, Marital Status) Moderate The Relationship Between Installment Commitments and Credit Risk Classification?
# Step 1: Data Visualization
# 1.1 Jittered Scatter Plot: Visualize Age by Installment Commitments and Credit Risk
ggplot(AData, aes(x = age, y = installment_commitment, color = class)) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0.2) + 
  labs(
    title = "Relationship between Age, Installment Commitments, and Credit Risk Classification",
    x = "Age (years)",
    y = "Number of Installment Commitments",
    color = "Credit Risk Class"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray")
  ) +
  scale_color_manual(name = "Credit Risk Class", values = c("red", "darkgreen"))


# 1.2 Dot Plot: Visualize Marital Status by Installment Commitments and Credit Risk
ggplot(AData, aes(x = personal_status, y = installment_commitment, color = class)) +
  geom_jitter(width = 0.2, alpha = 0.6, height = 0.2) +
  labs(
    title = "Relationship between Marital Status, Installment Commitments, and Credit Risk Classification by Marital Status",
    x = "Marital Status",
    y = "Number of Installment Commitments",
    color = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_line(color = "lightgray")
  ) +
  scale_color_manual(name = "Credit Risk Class", values = c("red", "darkgreen"))

# Step 2: Statistical Test
# ANOVA Test

#Convert Categorical Data to Factor
AData <- AData %>%
  mutate(class = as.factor(class),
         personal_status = as.factor(personal_status))

# Specify Formula for ANOVA
formula_str <- "installment_commitment ~ class + personal_status + age"

# Perform ANOVA
anova_result <- aov(as.formula(formula_str), data = AData)

# Summarize the ANOVA Results
summary(anova_result)

# Revert Variable Back to Original Class
AData$class <- as.character(AData$class)
AData$personal_status <- as.character(AData$personal_status)

#Check
str(AData)

#----------

#analysis 3-5: How Does The Duration Of Residence Impact The Relationship Between Financial Commitments And Credit Risk Classification?
# Step 1: Data Visualization
# 1.1 Tile Plot: Relationship Between Residence Duration, Installment Commitments, and Credit Risk Class

# Summarize Data
tile_data <- AData %>%
  group_by(residence_since, class) %>%
  summarise(mean_commitment = mean(installment_commitment, na.rm = TRUE)) %>%
  ungroup()

# Generate Tile Plot
ggplot(tile_data, aes(x = factor(residence_since), y = class, fill = mean_commitment)) +
  geom_tile(color = "white", size = 0.5) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean Commitment") +
  labs(
    title = "Tile Plot of Mean Financial Commitments by Residence Duration and Credit Risk Class",
    x = "Duration of Residence (years)",
    y = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  ) +
  geom_text(aes(label = round(mean_commitment, 1)), color = "white", size = 4)  # Mean Commitment Labels


# 1.2 Violin Plot: Distribution of Installment Commitments by Residence Duration and Credit Risk Class
ggplot(AData, aes(x = factor(residence_since), y = installment_commitment, fill = class)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, color = "black") +
  scale_fill_manual(values = c("good" = "darkblue", "bad" = "red")) +
  labs(
    title = "Violin Plot of Financial Commitments by Residence Duration and Credit Risk Class",
    x = "Duration of Residence (years)",
    y = "Number of Installment Commitment",
    fill = "Credit Risk Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
  )

# Step 2: Statistical Test
# Kruskal-Wallis Test ( Compare Distributions Among Multiple Groups )
kruskal_result <- kruskal.test(
  installment_commitment ~ interaction(residence_since, class),
  data = AData
)
print(kruskal_result)

############################################################################################################################################

#Objective 4: To explore the impact of customer credit history on credit risk classification.
#analysis 4-1: Is there a significant relationship between the history of past loans and current credit risk classification?

#4.1.1 Is Any Relationship between Credit History and Credit Risk?
#Create Table for Credit Risk
table_credit_risk <- table(AData$credit_history, AData$class)
#Perform Chi-Square Test
chi_sq_test_risk <- chisq.test(table_credit_risk)
chi_sq_test_risk

#Bar Chart
ggplot(AData, aes(x = credit_history, fill = class)) +
  geom_bar(position = "fill", color = "deeppink") +
  labs(title = "Credit History and Credit Risk Class",
       x = "Credit History",
       y = "Proportion",
       fill = "Credit Risk") +
  theme_minimal()

#4.1.2 Is Credit History A Strong Predictor of Credit Risk Classification?
#Conversion
AData$class <- as.factor(AData$class)

#Create Logistic Regression Model
logistic_risk <-glm(class ~ credit_history,  data = AData, family = binomial)
summary(logistic_risk)

#Predicted Probabilities
AData$predicted_risk <- predict(logistic_risk, type = "response")

#Calculate Mean Predicted Probabilities for Each Credit History Category
mean_prob_risk <- AData %>%
  group_by(credit_history) %>%
  summarise(mean_predicted_risk = mean(predicted_risk, na.rm = TRUE))

#Bar Plot
ggplot(mean_prob_risk, aes(x = credit_history, y = mean_predicted_risk, fill = credit_history)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Mean Predicted Probability of Credit Risk by Credit History",
       x = "Credit History",
       y = "Mean Predicted Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

#----------

#analysis 4-2: Does the number of previous loan defaults predict the likelihood of being classified as a high-risk customer?
#Conversion
AData$class <- as.factor(AData$class)

#Logistic Regression Model
logistic_defaults <- glm(class ~ credit_history, data = AData, family = binomial)
summary(logistic_defaults)

#Predicted Probabilities
AData$predicted_risk_defaults <- predict(logistic_defaults, type = "response")

#Calculate Mean Predicted Probabilities for Each Number of Defaults
mean_prob_defaults <- AData %>%
  group_by(credit_history) %>%
  summarise(mean_predicted_risk = mean(predicted_risk_defaults, na.rm = TRUE))

#Line Plot for Predicted Probabilities
ggplot(mean_prob_defaults, aes(x = credit_history, y = mean_predicted_risk, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Mean Predicted Probability of High Risk By Credit History",
       x = "Number of Previous Defaults",
       y = "Mean Predicted Probabilit") +
  theme_minimal()

#Violin Plot
ggplot(AData, aes(x = as.factor(credit_history), y = predicted_risk_defaults, fill = as.factor(credit_history))) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Predicted Probability by Credit History",
       x = "Number of Previous Defaults",
       y = "Predicted Probability") +
  theme_minimal()

#----------

#analysis 4-3: How does the length of credit history impact the credit risk classification?
#Histogram for distribution of credit history length
ggplot(AData, aes(x = duration)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Credit History Length",
       x = "Length of Credit History (years)",
       y = "Frequency") +
  theme_minimal()

#Boxplot
ggplot(AData, aes(x = class, y = duration, fill = class)) +
  geom_boxplot() +
  labs(title = "Credit History Length and Credit Risk Classification",
       x = "Credit Class",
       y = "Duration (Credit History Length)") +
  theme_minimal()

#Conversion
AData$duration <- as.numeric(as.character(AData$duration))

#Convert Duration into Broader Categorical Group
AData$duration <- cut(AData$duration,
                      breaks = c(-Inf, 12, 24, 36, Inf), # Adjust ranges as needed
                      labels = c("0-12", "13-24", "25-36", "36+"),
                      right = TRUE)

#Create Tables for Duration and Credit Class
duration_class_table <- table(AData$duration, AData$class)
duration_class_table

#Fisher's Exact Test
fisher_test_duration_sim <- fisher.test(duration_class_table, simulate.p.value = TRUE)
fisher_test_duration_sim

#Chi-Square Test
duration_chisq_sim <- chisq.test(duration_class_table, simulate.p.value = TRUE)
duration_chisq_sim

#Conversion
AData$duration <- as.factor(AData$duration)
AData$class <- as.factor(AData$class)

#Bar Chart
ggplot(AData, aes(x = duration, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Credit History Length and Credit Risk",
       x = "Duration",
       y = "Count",
       fill = "Credit Class") +
  theme_minimal()

#Calculate Percentages for Duration and Class
duration_class_df <- as.data.frame(duration_class_table)
colnames(duration_class_df) <- c("Duration", "Class", "Count")

duration_class_df <- duration_class_df %>%
  group_by(Duration) %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Bar Chart with Percentages
ggplot(duration_class_df, aes(x = Duration, y = Count, fill = Class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5))+
  labs(title = "Credit History Length and Risk Classification",
       x = "Credit History Length (Duration)",
       y = "Count",
       fill = "Credit  Class") +
  theme_minimal()


#----------

#analysis 4-4: What are the external factors that interact with past credit history (e.g., income, employment status) to influence credit risk classification?
#Conversion
AData$class <- as.factor(AData$class)
AData$credit_history <- as.factor(AData$credit_history)
AData$employment <- as.factor(AData$employment)


#4.4.1 
#Create Logistic model with Interaction between Credit History and Employment
history_employ_interaction <- glm(class ~ credit_history + employment + credit_history:employment, data = AData, family = binomial)
summary(history_employ_interaction)

#Predict Probabilities using Interaction Model
AData$predicted_prob <- predict(history_employ_interaction, type = "response")

#Summary of Predicted Probabilities
interaction_summary <- AData %>%
  group_by(credit_history, employment) %>%
  summarise(mean_prob = mean(predicted_prob))
interaction_summary

#Bar Plot between Credit History and Employment Status on Predicted Probabilities
ggplot(interaction_summary, aes(x = credit_history, y = mean_prob, fill = employment)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(mean_prob, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Interaction Effect between Credit History and Employment on Credit Risk",
       x = "Credit History",
       y = "Mean Predicted Probability",
       fill = "Employment") +
  theme_minimal()


#4.4.2
#Interaction between Age Group and Credit Class
AData <- AData %>%
  mutate(age_group = case_when(
    age <= 30 ~ "< 30",
    age > 30 & age <= 50 ~ "30-50",
    age > 50 ~ "50 more"
  ))

#Create Tables for Age Group and Credit Class
age_class_table <- table(AData$age_group, AData$class)
age_class_table

#Chi-Square Test
age_chisq <- chisq.test(age_class_table)
age_chisq

#Bar Plot between Age Group and Credit Class
ggplot(AData, aes(x = age_group, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Age Group and Credit Risk Class", 
       x = "Age Group", 
       y = "Count") +
  theme_minimal()


#4.4.3
#Relationship between Saving Status and Employment
# Grouping savings_status
AData$savings_status <- recode(AData$savings_status,
                               "<100" = "<500",
                               "100<=X<500" = "<500",
                               "500<=X<10000" = ">=500",
                               ">=1000" = ">=500",
                               "no known savings" = "no known savings")

# Grouping employment
AData$employment <- recode(AData$employment,
                           "<1" = "short_term",
                           "1<=X<4" = "mid_term",
                           "4<=X<7" = "long_term",
                           ">=7" = "long_term",
                           "unemployed" = "unemployed")

#Create Tables for Saving Status and Employment
savings_employment_table <- table(AData$savings_status, AData$employment)

#Chi-Square Test
saving_employment_chisq <- chisq.test(savings_employment_table)
saving_employment_chisq

#Bar Plot between Savings Status and Employment
ggplot(AData, aes(x = savings_status, fill = employment)) +
  geom_bar(position = "dodge") +
  labs(title = "Savings Status and Employment", 
       X = "Savings Status", 
       y = "Count") +
  theme_minimal()


#4.4.4
#Boxplot between Credit Amount and Credit Class
ggplot(AData, aes(x = class, y = credit_amount, fill = class)) +
  geom_boxplot() +
  labs(title = "Credit Amount by Credit Risk Class",
       x = "Credit Class",
       y = "Credit Amount") +
  theme_minimal()

#Density Plot for Credit Amount
ggplot(AData, aes(x = credit_amount)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot for Credit Amount by Credit Class", 
       x = "Credit Amount",
       y = "Density") +
  theme_minimal()


#----------

#analysis 4-5: Is there any relationship between the purpose of having the loan and the credit risk classification?
#Conversion
AData$class <- as.factor(AData$purpose)
AData$class <- as.factor(AData$class)

#Create tables for Purpose of Loan and Credit Class 
purpose_class_table <- table(AData$purpose, AData$class)
purpose_class_table

#Fisher's Exact Test to Handle Larger Tables
fisher_test_pur_class_sim <- fisher.test(purpose_class_table, simulate.p.value = TRUE)
fisher_test_pur_class_sim

#Chi-Square Test
purpose_class_chisq_sim <- chisq.test(purpose_class_table, simulate.p.value = TRUE)
purpose_class_chisq_sim

#Calculate Percentages
purpose_class_df <- as.data.frame(purpose_class_table)
colnames(purpose_class_df) <- c("Purpose", "Class", "Count")

purpose_class_df <- purpose_class_df %>%
  group_by(Purpose) %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Bar Chart
ggplot(purpose_class_df, aes(x = Purpose, y = Count, fill = Class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size =3.5) +
  labs(title = "Loan Purpose and Risk Classification",
       x = "Loan Purpose",
       y = "Count",
       fill = "Credit Risk Class") +
  theme_minimal()

#----------

#analysis 4-6: Is there any relationship between the categorical variables (e.g., credit history, purpose, and employment status)?
#Cross-tabulation for Credit History and Purpose
credit_history_purpose_table <- table(AData$credit_history, AData$purpose)
credit_history_purpose_table

#Cross-tabulation for Credit History and Employment Status
credit_history_employment_table <- table(AData$credit_history, AData$employment)
credit_history_employment_table

#Fisher's Exact Test to Handle Larger Tables
fisher_test_purpose_sim <- fisher.test(credit_history_purpose_table, simulate.p.value = TRUE)
fisher_test_purpose_sim

#Chi-square test for Credit History and Purpose
chisq_credit_history_purpose_sim <- chisq.test(credit_history_purpose_table, simulate.p.value = TRUE)
chisq_credit_history_purpose_sim

#Chi-square test for Credit History and Employment Status
chisq_credit_history_employment <- chisq.test(credit_history_employment_table)
chisq_credit_history_employment

#Stacked Bar Plot for Credit History and Purpose
ggplot(AData, aes(x = credit_history, fill = purpose)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Proportions of Credit Purpose by Credit History",
       x = "Credit History",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#Combine Both Tables into A List to Create A Bipartite Graph
edges <- rbind(
  cbind(rep("credit_history", nrow(credit_history_purpose_table)), rownames(credit_history_purpose_table), as.vector(credit_history_purpose_table)),
  cbind(rep("credit_history", nrow(credit_history_employment_table)), rownames(credit_history_employment_table), as.vector(credit_history_employment_table))
)

#Create the igraph
credit_network <- graph_from_edgelist(edges[, 1:2], directed = FALSE)

#Plot and Add weights to the edges
plot(credit_network,
     vertex.size = 10,
     vertex.color = "pink",
     vertex.label.cex = 0.8,
     layout = layout_in_circle,
     edge.width = E(credit_network)$weight / 10,
     main = "Network Graph: Relationships between Credit History, Purpose and Employment Status")