setwd("C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_statistics_hdd/_final_project/wholesales")
#remove.packages("rlang")
#install.packages("rlang")
################################################################################
# Data setup
################################################################################
#install.packages("tidyverse")
#install.packages("magrittr")

library(tidyverse)
library(magrittr) # https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

data<-read_csv("wholesale_customers_data.csv")
names(data) %<>% tolower
data <- data %>%
  rename(class = channel,
         delicatessen = delicassen)

data <- data %>%
  mutate(class = as.factor(data$class),
         region = as.factor(data$region))

levels(data$class) <- list("Horeca"=1, "Retail"=2)
levels(data$region) <- list("Lisbon"=1, "Oporto"=2, "Other"=3 )

################################################################################
# Some visualizations and descriptive statistics
################################################################################

#------ descriptive statistics:

library(pastecs)
stat.desc(data)

#------ frecuency on factor variables:

#install.packages('epiDisplay')
library(epiDisplay)

table(data$class)
tab1(data$class, sort.group = "decreasing", cum.percent = TRUE, graph = FALSE)
tab1(data$region, sort.group = "decreasing", cum.percent = TRUE, graph = FALSE)

class <- data %>% ggplot(aes(x=class)) +
  geom_bar()

region <- data %>% ggplot(aes(x=region)) +
  geom_bar()

library(gridExtra)
grid.arrange(class, region)

#------ frecuency of covariates:

library(scales)

suffix <- "k"
scale <- 1e-3

fresh <- ggplot(data, aes(x=fresh)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

milk <- ggplot(data, aes(x=milk)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

grocery <- ggplot(data, aes(x=grocery)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

frozen <- ggplot(data, aes(x=frozen)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

detergents_paper <- ggplot(data, aes(x=detergents_paper)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

delicatessen <- ggplot(data, aes(x=delicatessen)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

grid.arrange(fresh, milk, grocery, frozen, detergents_paper, delicatessen)


#------ density plot (overlapped plots):
# The successor to reshape2 is tidyr. The equivalent of melt() and dcast() are gather() and spread() respectively.

data %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen) %>% 
  gather() %>% 
  #head()
  ggplot(aes(x=value, colour=key)) +
  geom_density() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))


################################################################################
# Testing assumptions (before transforming)
################################################################################

#------ homoskedasticity:

# Bartlett's test for equal variance across samples for individual variables:

#install.packages("stargazer")
#install.packages("olsrr")
#install.packages("xtable")
library(stargazer)
library(olsrr)
library(xtable)

temp_data <- data

bart_fresh <- ols_test_bartlett(temp_data, "fresh", group_var = "class")
bart_milk <- ols_test_bartlett(temp_data, "milk", group_var = "class")
bart_grocery <- ols_test_bartlett(temp_data, "grocery", group_var = "class")
bart_frozen <- ols_test_bartlett(temp_data, "frozen", group_var = "class")
bart_detergents_paper <- ols_test_bartlett(temp_data, "detergents_paper", group_var = "class")
bart_delicatessen <- ols_test_bartlett(temp_data, "delicatessen", group_var = "class")

bart_names <- temp_data %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen) %>%
  names()
bart_names

decimals <- 3

bart_pvalues <- c(
  round(bart_fresh[[2]], decimals),
  round(bart_milk[[2]], decimals),
  round(bart_grocery[[2]], decimals),
  round(bart_frozen[[2]], decimals),
  round(bart_detergents_paper[[2]], decimals),
  round(bart_detergents_paper[[2]], decimals)
)

bart_std <- tibble(bart_names, bart_pvalues)
bart_std

stargazer(bart_std, summary=FALSE, rownames=FALSE, out="bart_std_raw.tex")

# Cai TT, Ma Z (2013) two sample test of equality of covariance matrices:
# https://cran.r-project.org/web/packages/CovTools/CovTools.pdf
#install.packages("CovTools")
library(CovTools)

class_1 <- data %>%
  dplyr::filter(class == "Horeca") %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen)

class_2 <- data %>%
  dplyr::filter(class == "Retail") %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen)

CovTest2.2013Cai(as.matrix(class_1), as.matrix(class_2))
stargazer(bart_std, summary=FALSE, rownames=FALSE, out="bart_std_raw.tex")


#------ normality:

# Shaphiro-Wilk test of normality for individual covariates:

temp_data <- data

shap_fresh <- shapiro.test(temp_data$fresh)
shap_milk <- shapiro.test(temp_data$milk)
shap_grocery <- shapiro.test(temp_data$grocery)
shap_frozen <- shapiro.test(temp_data$frozen)
shap_detergents_paper <- shapiro.test(temp_data$detergents_paper)
shap_delicatessen <- shapiro.test(temp_data$delicatessen)

names(shap_fresh)

shap_names <- temp_data %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen) %>% 
  names()
shap_names

decimals <- 3

shap_pvalues <- c(
  round(shap_fresh[[2]], decimals),
  round(shap_milk[[2]], decimals),
  round(shap_grocery[[2]], decimals),
  round(shap_frozen[[2]], decimals),
  round(shap_detergents_paper[[2]], decimals),
  round(shap_detergents_paper[[2]], decimals)
)

shap_std <- tibble(shap_names, shap_pvalues)
shap_std

stargazer(shap_std, summary=FALSE, rownames=FALSE, out="shap_std_raw.tex")


# Generalized Shapiro-Wilk test for Multivariate Normality:

#install.packages("QuantPsyc")
#install.packages("mvShapiroTest")
#install.packages("mvnormtest")
library(QuantPsyc)
library(mvShapiroTest)
library(mvnormtest)

temp_data <- data %>% 
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen)

temp_data %>%
  data.matrix() %>%
  mvShapiro.Test()

mvnormtest::mshapiro.test(t(temp_data))

mult.norm(temp_data)$mult.test

################################################################################
# Data transformation and standardization.
################################################################################

ln_fresh<-scale(log(data$fresh+1))
ln_milk<-scale(log(data$milk+1))
ln_grocery<-scale(log(data$grocery+1))
ln_frozen<-scale(log(data$frozen+1))
ln_detergents_paper<-scale(log(data$detergents_paper+1))
ln_delicatessen<-scale(log(data$delicatessen+1))

data <- cbind(data, ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

data %>%
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen) %>%
  stat.desc()

################################################################################
# Some visualizations and descriptive statistics on the transformed data
################################################################################

# Distribution of the transformed covariates:

suffix <- ""
scale <- 1

ln_fresh <- ggplot(data, aes(x=ln_fresh)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

ln_milk <- ggplot(data, aes(x=ln_milk)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

ln_grocery <- ggplot(data, aes(x=ln_grocery)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

ln_frozen <- ggplot(data, aes(x=ln_frozen)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

ln_detergents_paper <- ggplot(data, aes(x=ln_detergents_paper)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

ln_delicatessen <- ggplot(data, aes(x=ln_delicatessen)) +
  geom_histogram() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

grid.arrange(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

#------ density plot (overlapped plots) on transformed data:

temp_data <- data %>% 
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

temp_data %>%
  gather() %>% 
  #head()
  ggplot(aes(x=value, colour=key)) +
  geom_density() +
  scale_x_continuous(labels = label_number(suffix = suffix, scale = scale))

################################################################################
# Testing assumptions  on the transformed data.
################################################################################

#------ homoskedasticity:

# Bartlett's test for equal variance across samples for individual variables:

#install.packages("olsrr")
library(olsrr)

temp_data <- data %>%
  dplyr::select(class, ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

bart_fresh <- ols_test_bartlett(temp_data, "ln_fresh", group_var = "class")
bart_milk <- ols_test_bartlett(temp_data, "ln_milk", group_var = "class")
bart_grocery <- ols_test_bartlett(temp_data, "ln_grocery", group_var = "class")
bart_frozen <- ols_test_bartlett(temp_data, "ln_frozen", group_var = "class")
bart_detergents_paper <- ols_test_bartlett(temp_data, "ln_detergents_paper", group_var = "class")
bart_delicatessen <- ols_test_bartlett(temp_data, "ln_delicatessen", group_var = "class")

bart_names <- temp_data %>%
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen) %>%
  names()
bart_names

decimals <- 3

bart_pvalues <- c(
  round(bart_fresh[[2]], decimals),
  round(bart_milk[[2]], decimals),
  round(bart_grocery[[2]], decimals),
  round(bart_frozen[[2]], decimals),
  round(bart_detergents_paper[[2]], decimals),
  round(bart_detergents_paper[[2]], decimals)
)

bart_std <- tibble(bart_names, bart_pvalues)
bart_std

stargazer(bart_std, summary=FALSE, rownames=FALSE, out="bart_std_tra.tex")

# Cai TT, Ma Z (2013) two sample test of equality of covariance matrices:
# https://cran.r-project.org/web/packages/CovTools/CovTools.pdf
#install.packages("CovTools")
library(CovTools)

class_1 <- data %>%
  dplyr::filter(class == "Horeca") %>%
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)
class_1

class_2 <- data %>%
  dplyr::filter(class == "Retail") %>%
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)
class_2

CovTest2.2013Cai(as.matrix(class_1), as.matrix(class_2))
names(CovTest2.2013Cai(as.matrix(class_1), as.matrix(class_2)))

#------ normality:

# Shaphiro-Wilk test of normality for individual covariates:

temp_data <- data %>% 
  dplyr::select(class, ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

shap_fresh <- shapiro.test(temp_data$ln_fresh)
shap_milk <- shapiro.test(temp_data$ln_milk)
shap_grocery <- shapiro.test(temp_data$ln_grocery)
shap_frozen <- shapiro.test(temp_data$ln_frozen)
shap_detergents_paper <- shapiro.test(temp_data$ln_detergents_paper)
shap_delicatessen <- shapiro.test(temp_data$ln_delicatessen)

names(shap_fresh)

shap_names <- temp_data %>%
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen) %>% 
  names()
shap_names

decimals <- 3

shap_pvalues <- c(
  round(shap_fresh[[2]], decimals),
  round(shap_milk[[2]], decimals),
  round(shap_grocery[[2]], decimals),
  round(shap_frozen[[2]], decimals),
  round(shap_detergents_paper[[2]], decimals),
  round(shap_detergents_paper[[2]], decimals)
)

shap_std <- tibble(shap_names, shap_pvalues)
shap_std

stargazer(shap_std, summary=FALSE, rownames=FALSE, out="shap_std_tra.tex")


# Generalized Shapiro-Wilk test for Multivariate Normality:

#install.packages("QuantPsyc")
#install.packages("mvShapiroTest")
#install.packages("mvnormtest")
library(QuantPsyc)
library(mvShapiroTest)
library(mvnormtest)

temp_data <- data %>% 
  dplyr::select(ln_fresh, ln_milk, ln_grocery, ln_frozen, ln_detergents_paper, ln_delicatessen)

temp_data %>%
  data.matrix() %>%
  mvShapiro.Test()

mvnormtest::mshapiro.test(t(temp_data))

mult.norm(temp_data)$mult.test

################################################################################
################################################################################
# Discriminant Analisys
################################################################################

# [https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/]
#install.packages("MASS")
library(MASS)

#==============================================================================
# Splitting the sample
#==============================================================================

#install.packages("caTools")
library(caTools)
set.seed(33)

split<-sample.split(data$class,SplitRatio=0.65)

training<-subset(data,split==TRUE)
testing<-subset(data,split==FALSE)

n_complete <- table(data$class)
prop_complete <- table(data$class)/length(data$class)

n_training <- table(training$class)
prop_training <- table(training$class)/length(training$class)

n_testing <- table(testing$class)
prop_testing <- table(testing$class)/length(testing$class)

names(table(testing$class)/length(testing$class))
prop_complete[[1]]

prop_names <- c(
  "Complete dataset",
  "Training dataset",
  "Testing dataset"
)

prop_values_1 <- c(
  prop_complete[[1]],
  prop_training[[1]],
  prop_testing[[1]]
)

n_values_1 <- c(
  n_complete[[1]],
  n_training[[1]],
  n_testing[[1]]
)

prop_values_2 <- c(
  prop_complete[[2]],
  prop_training[[2]],
  prop_testing[[2]]
)

n_values_2 <- c(
  n_complete[[2]],
  n_training[[2]],
  n_testing[[2]]
)

total <- c(
  n_values_1[1] + n_values_2[1],
  n_values_1[2] + n_values_2[2],
  n_values_1[3] + n_values_2[3]
)
prop_table <- tibble(prop_names,n_values_1, format(prop_values_1, digits = 3), n_values_2, format(prop_values_2, digits = 3), total)
prop_table
stargazer(prop_table, summary=FALSE, rownames=FALSE, out="prop.tex")


################################################################################
# Training sets
################################################################################

#==============================================================================
# LDA on the transformed data (training set)
#==============================================================================
temp_data <- training

model<-lda(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model
names(model)
lda_function <- model[4]

temp_data %>% mutate(regionOporto = 0)
temp_data %<>% mutate(regionOporto = ifelse(region == "Oporto",1,0))
temp_data %>% mutate(regionOther = 0)
temp_data %<>% mutate(regionOther = ifelse(region == "Other",1,0))

#install.packages("collapse")
#install.packages("doBy")
library(collapse)
library(doBy)

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

db <- tibble(temp_data, pred)

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_LDA_train <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_names <- c("Accuracy: (TP+TN)/total",
               "Misclassification Rate: (FP+FN)/total",
               "Sensitivity, True Positive Rate: TP/(True yes)",
               "False Positive Rate: FP/(True no)",
               "Specificity, True Negative Rate: TN/(True no)",
               "Precision: TP/(predicted yes)",
               "Prevalence: (True yes)/total")


fit_table <- tibble(fit_names, fit_scores_LDA_train)
fit_table

#==============================================================================
# QDA on the transformed data (training set)
#==============================================================================

temp_data <- training

model<-qda(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_QDA_train <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names, fit_scores_LDA_train, fit_scores_QDA_train)
fit_table

#==============================================================================
# Naive Bayes on the transformed data (training set)
#==============================================================================
#https://rpubs.com/Subhalaxmi/742119
#install.packages("naivebayes")
library(naivebayes)

temp_data <- training

model<-naive_bayes(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)

confmat<-table(Predicted = pred, Actual = temp_data$class)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_BAYES_train <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names,
                    format(fit_scores_LDA_train, digits = 2),
                    format(fit_scores_QDA_train, digits = 2),
                    format(fit_scores_BAYES_train, digits = 2))
fit_table
stargazer(fit_table, summary=FALSE, rownames=FALSE, out="acc_train.tex")


################################################################################
# Testing sets
################################################################################

#==============================================================================
# LDA on the transformed data (testing set)
#==============================================================================
temp_data <- testing

model<-lda(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_LDA_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_names <- c("Accuracy: (TP+TN)/total",
               "Misclassification Rate: (FP+FN)/total",
               "Sensitivity, True Positive Rate: TP/(True yes)",
               "False Positive Rate: FP/(True no)",
               "Specificity, True Negative Rate: TN/(True no)",
               "Precision: TP/(predicted yes)",
               "Prevalence: (True yes)/total")


fit_table <- tibble(fit_names, fit_scores_LDA_test)
fit_table

#==============================================================================
# QDA on the transformed data (testing set)
#==============================================================================

temp_data <- testing

model<-qda(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_QDA_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names, fit_scores_LDA_test, fit_scores_QDA_test)
fit_table

#==============================================================================
# NAIVE BAYES on the transformed data (testing set)
#==============================================================================
#https://rpubs.com/Subhalaxmi/742119
#install.packages("naivebayes")
library(naivebayes)

temp_data <- testing

model<-naive_bayes(formula=class~region+ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)

confmat<-table(Predicted = pred, Actual = temp_data$class)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_BAYES_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names,
                    format(fit_scores_LDA_test, digits = 2),
                    format(fit_scores_QDA_test, digits = 2),
                    format(fit_scores_BAYES_test, digits = 2))
fit_table
stargazer(fit_table, summary=FALSE, rownames=FALSE, out="acc_test.tex")

#==============================================================================
# Visualization of partition rules
#==============================================================================
#install.packages("klaR")
library(klaR)

partimat(class~ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen, data = testing, method = "lda",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)


partimat(class~ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen, data = testing, method = "qda",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)

partimat(class~ln_fresh+ln_milk+ln_grocery+ln_frozen+ln_detergents_paper+ln_delicatessen, data = testing, method = "naiveBayes",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)




jpeg("lda.jpeg") 
lda <- partimat(class~ln_milk+ln_detergents_paper, data = testing, method = "lda",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)
dev.off()

jpeg("qda.jpeg") 
qda <- partimat(class~ln_milk+ln_detergents_paper, data = testing, method = "qda",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)
dev.off()

jpeg("bayes.jpeg") 
bayes <- partimat(class~ln_milk+ln_detergents_paper, data = testing, method = "naiveBayes",
         nplots.hor = 3,
         col.mean = "black",
         col.correct="blue",
         col.wrong="firebrick3",
)
dev.off()

################################################################################
# Testing sets [untransformed data]
################################################################################

#==============================================================================
# LDA on the untransformed data (testing set)
#==============================================================================
temp_data <- testing

model<-lda(formula=class~region+fresh+milk+grocery+frozen+detergents_paper+delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_LDA_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_names <- c("Accuracy: (TP+TN)/total",
               "Misclassification Rate: (FP+FN)/total",
               "Sensitivity, True Positive Rate: TP/(True yes)",
               "False Positive Rate: FP/(True no)",
               "Specificity, True Negative Rate: TN/(True no)",
               "Precision: TP/(predicted yes)",
               "Prevalence: (True yes)/total")


fit_table <- tibble(fit_names, fit_scores_LDA_test)
fit_table

#==============================================================================
# QDA on the untransformed data (testing set)
#==============================================================================

temp_data <- testing

model<-qda(formula=class~region+fresh+milk+grocery+frozen+detergents_paper+delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)$class

confmat<-confusionmatrix(temp_data$class, pred)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_QDA_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names, fit_scores_LDA_test, fit_scores_QDA_test)
fit_table

#==============================================================================
# NAIVE BAYES on the untransformed data (testing set)
#==============================================================================
#https://rpubs.com/Subhalaxmi/742119
#install.packages("naivebayes")
library(naivebayes)

temp_data <- testing

model<-naive_bayes(formula=class~region+fresh+milk+grocery+frozen+detergents_paper+delicatessen,data=temp_data)
model

#install.packages("biotools")
library(biotools)
pred<-predict(model,data=temp_data)

confmat<-table(Predicted = pred, Actual = temp_data$class)
confmat

#Accuracy
accuracy <- sum(diag(prop.table(confmat)))

#Misclassification Rate:(FP+FN)/total
total<-sum(confmat)
misclassification_rate <- (confmat[1,2]+confmat[2,1])/total

#Sensitivity, True Positive Rate:TP/(True yes)
true_yes<-confmat[2,1]+confmat[2,2]
sensitivity <- confmat[2,2]/true_yes

#False Positive Rate: FP/(True no).
true_no<-confmat[1,1]+confmat[1,2]
fpr <- confmat[1,2]/true_no

#Specificity, True Negative Rate: TN/(True no).
specificity <- confmat[1,1]/true_no

#Precision: TP/(predicted yes).
predicted_yes<-confmat[1,2]+confmat[2,2]
precision <-confmat[2,2]/predicted_yes

#Prevalence: (True yes)/total.
prevalence <- true_yes/total

fit_scores_BAYES_test <- c(accuracy, misclassification_rate, sensitivity, fpr, specificity, precision, prevalence)

#------ building up the table:

fit_table <- tibble(fit_names,
                    format(fit_scores_LDA_test, digits = 2),
                    format(fit_scores_QDA_test, digits = 2),
                    format(fit_scores_BAYES_test, digits = 2))
fit_table
stargazer(fit_table, summary=FALSE, rownames=FALSE, out="acc_test_raw.tex")



# https://rpubs.com/ZheWangDataAnalytics/DecisionBoundary
# https://www.geeksforgeeks.org/quadratic-discriminant-analysis/?ref=rp

################################################################################
# ANNEX: Helpful links and code to keep
################################################################################

#https://rpubs.com/ZheWangDataAnalytics/DecisionBoundary
#https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/

#------ COrrelation plots:

# https://plotly.com/ggplot2/splom/
#install.packages("plotly")
#install.packages("GGally")
library(plotly)
library(GGally)

data %>%
  dplyr::select(fresh, milk, grocery, frozen, detergents_paper, delicatessen) %>%
  ggpairs(ggplot2::aes(colour=data$class)) %>%
  ggplotly()




