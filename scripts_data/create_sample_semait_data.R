# We create a sample dataset for SEMAIT Analysis using the datasets
# generated with download_data.sh and process_data.R, and using baseline R Machine Learning
# Systems with a sampling of metrics
library(rpart)
library(e1071)
library(interpret)
library(xgboost)

# Need to install mlr3
requireNamespace("mlr3measures")

# Change this to be the data directory
in_data_dir <- Sys.getenv("MIG_ANALYZER_INTERNAL_DATA_DIR")
data_dir <- Sys.getenv("MIG_ANALYZER_DATA_DIR")
titanic_dir <- normalizePath(file.path(in_data_dir, "processed", "titanic_openml_40945"))
adult_dir <- normalizePath(file.path(in_data_dir, "processed", "adult_uci"))
hmda_dir <- normalizePath(file.path(in_data_dir, "processed", "hmda_2021"))

titanic_train_df <- read.csv(normalizePath(file.path(titanic_dir, "titanic_40945_data_train.csv")))
titanic_test_df <- read.csv(normalizePath(file.path(titanic_dir, "titanic_40945_data_test.csv")))
titanic_train_df$pclass <- as.factor(titanic_train_df$pclass)
titanic_train_df$survived <- as.factor(titanic_train_df$survived)
titanic_train_df$sex <- as.factor(titanic_train_df$sex)
titanic_test_df$pclass <- as.factor(titanic_test_df$pclass)
titanic_test_df$survived <- as.factor(titanic_test_df$survived)
titanic_test_df$sex <- as.factor(titanic_test_df$sex)
titanic_score_df <- titanic_test_df[, c("record_id", "sex", "survived")]

adult_train_df <- read.csv(normalizePath(file.path(adult_dir, "adult_data_train.csv")))
adult_test_df <- read.csv(normalizePath(file.path(adult_dir, "adult_data_test.csv")))
adult_train_df$incomeg50 <- as.integer(as.logical(as.character(adult_train_df$income) == " >50K"))
adult_test_df$incomeg50 <- as.integer(as.logical(as.character(adult_test_df$income) == " >50K"))
adult_score_df <- adult_test_df[, c("record_id", "gender", "incomeg50")]

hmda_train_df <- read.csv(normalizePath(file.path(hmda_dir, "hmda_data_train.csv")))
hmda_test_df <- read.csv(normalizePath(file.path(hmda_dir, "hmda_data_test.csv")))
hmda_train_df$derived_dwelling_category <- as.factor(hmda_train_df$derived_dwelling_category)
hmda_train_df$derived_sex <- as.factor(hmda_train_df$derived_sex)
hmda_train_df$derived_race <- as.factor(hmda_train_df$derived_race)
hmda_train_df$loan_type <- as.factor(hmda_train_df$loan_type)
hmda_train_df$loan_term <- as.numeric(hmda_train_df$loan_type)
hmda_train_df$interest_ge_3 <- as.integer(as.logical(hmda_train_df$interest_ge_3))
hmda_test_df$derived_dwelling_category <- as.factor(hmda_test_df$derived_dwelling_category)
hmda_test_df$derived_sex <- as.factor(hmda_test_df$derived_sex)
hmda_test_df$derived_race <- as.factor(hmda_test_df$derived_race)
hmda_test_df$loan_type <- as.factor(hmda_test_df$loan_type)
hmda_test_df$loan_term <- as.numeric(hmda_test_df$loan_type)
hmda_test_df$interest_ge_3 <- as.integer(as.logical(hmda_test_df$interest_ge_3))
hmda_score_df <- hmda_test_df[, c("record_id", "derived_sex", "interest_ge_3")]

titanic_rpart <- rpart(survived ~ pclass + sex + age + fare + embarked + sibsp + parch, 
                      data = titanic_train_df, method = "class")
titanic_lr <- glm(survived ~ pclass + sex + age + fare + embarked + sibsp + parch, 
                  data = titanic_train_df, family = binomial)
titanic_plr <- glm(survived ~ poly(pclass,2) + sex + age + I(fare^2) +
                     embarked + poly(sibsp,2) + poly(parch,2) +
                     sex*age + fare*pclass, 
                  data = titanic_train_df, family = binomial)

titanic_score_df$rpart_probs <- predict(titanic_rpart, titanic_test_df, type = "prob")[,2]
titanic_score_df$rpart_class <- predict(titanic_rpart, titanic_test_df, type = "class")
titanic_score_df$lr_probs <- predict(titanic_lr, titanic_test_df, type = "response")
titanic_score_df$lr_class <- as.integer(as.logical((!is.na(predict(titanic_lr, titanic_test_df, 
                                                                   type = "response"))) & 
  (predict(titanic_lr, titanic_test_df, type = "response") > 0.5)))
if (nrow(titanic_score_df[is.na(titanic_score_df$lr_probs),]) > 0) {
  titanic_score_df[is.na(titanic_score_df$lr_probs),]$lr_probs <- 0
}
titanic_score_df$plr_probs <- predict(titanic_plr, titanic_test_df, type = "response")
titanic_score_df$plr_class <- as.integer(as.logical((!is.na(predict(titanic_plr, titanic_test_df, 
                                                                   type = "response"))) & 
                                                     (predict(titanic_plr, titanic_test_df, type = "response") > 0.5)))
if (nrow(titanic_score_df[is.na(titanic_score_df$plr_probs),]) > 0) {
  titanic_score_df[is.na(titanic_score_df$plr_probs),]$plr_probs <- 0
}

adult_rpart <- rpart(incomeg50 ~ age + workclass + fnlwgt + education + education_num +
                       marital_status + occupation + relationship + race + gender +
                       capital_gain + capital_loss + hours_per_week + native_country, 
                     data = adult_train_df, method = "class")

adult_lr <- glm(incomeg50 ~ age + workclass + fnlwgt + education + education_num +
                  marital_status + occupation + relationship + race + gender +
                  capital_gain + capital_loss + hours_per_week + native_country, 
                  data = adult_train_df, family = binomial)
adult_plr <- glm(incomeg50 ~ poly(age,2) + workclass + poly(fnlwgt,3) + education + education_num +
                   marital_status + occupation + relationship + race + gender +
                   capital_gain + capital_loss + poly(hours_per_week,2) + native_country + age*occupation +
                   workclass*education, 
                   data = adult_train_df, family = binomial)

adult_score_df$rpart_probs <- predict(adult_rpart, adult_test_df, type = "prob")[,2]
adult_score_df$rpart_class <- predict(adult_rpart, adult_test_df, type = "class")
adult_score_df$lr_probs <- predict(adult_lr, adult_test_df, type = "response")
adult_score_df$lr_class <- as.integer(as.logical((!is.na(predict(adult_lr, adult_test_df, 
                                                                   type = "response"))) & 
                                                     (predict(adult_lr, adult_test_df, type = "response") > 0.5)))
if (nrow(adult_score_df[is.na(adult_score_df$lr_probs),]) > 0) {
  adult_score_df[is.na(adult_score_df$lr_probs),]$lr_probs <- 0
}
adult_score_df$plr_probs <- predict(adult_plr, adult_test_df, type = "response")
adult_score_df$plr_class <- as.integer(as.logical((!is.na(predict(adult_plr, adult_test_df, 
                                                                    type = "response"))) & 
                                                      (predict(adult_plr, adult_test_df, type = "response") > 0.5)))
if (nrow(adult_score_df[is.na(adult_score_df$plr_probs),]) > 0) {
  adult_score_df[is.na(adult_score_df$plr_probs),]$plr_probs <- 0
}



hmda_rpart <- rpart(interest_ge_3 ~ derived_dwelling_category + derived_sex + 
                      derived_race + loan_type + loan_term + loan_amount,
                       data = hmda_train_df, method = "class")
hmda_lr <- glm(interest_ge_3 ~ derived_dwelling_category + derived_sex + 
                 derived_race + loan_type + loan_term + loan_amount, 
                  data = hmda_train_df, family = binomial)
hmda_plr <- glm(interest_ge_3 ~ derived_dwelling_category + derived_sex + 
                  derived_race + loan_type + poly(loan_term,3) + poly(loan_amount,2) + 
                  loan_amount*loan_term + loan_type*loan_term + derived_sex*derived_race, 
                   data = hmda_train_df, family = binomial)

hmda_score_df$rpart_probs <- predict(hmda_rpart, hmda_test_df, type = "prob")[,2]
hmda_score_df$rpart_class <- predict(hmda_rpart, hmda_test_df, type = "class")
hmda_score_df$lr_probs <- predict(hmda_lr, hmda_test_df, type = "response")
hmda_score_df$lr_class <- as.integer(as.logical((!is.na(predict(hmda_lr, hmda_test_df, 
                                                                   type = "response"))) & 
                                                     (predict(hmda_lr, hmda_test_df, type = "response") > 0.5)))
if (nrow(hmda_score_df[is.na(hmda_score_df$lr_probs),]) > 0) {
  hmda_score_df[is.na(hmda_score_df$lr_probs),]$lr_probs <- 0
}
hmda_score_df$plr_probs <- predict(hmda_plr, hmda_test_df, type = "response")
hmda_score_df$plr_class <- as.integer(as.logical((!is.na(predict(hmda_plr, hmda_test_df, 
                                                                    type = "response"))) & 
                                                      (predict(hmda_plr, hmda_test_df, type = "response") > 0.5)))
if (nrow(hmda_score_df[is.na(hmda_score_df$plr_probs),]) > 0) {
  hmda_score_df[is.na(hmda_score_df$plr_probs),]$plr_probs <- 0
}

# Now that we have the trained models and the predictions on the test sets, create our
# data frame for SEMAIT Analysis
exp_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(exp_df) <- c("system", "dataset", "metric", "score")

# acc metric
curr_score <- mlr3measures::acc(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$rpart_class))
curr_row <- c("dt", "titanic", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
colnames(exp_df) <- c("model", "dataset", "metric", "score")
curr_score <- mlr3measures::acc(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$lr_class))
curr_row <- c("lr", "titanic", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::acc(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$plr_class))
curr_row <- c("plr", "titanic", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::acc(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$rpart_class))
curr_row <- c("dt", "adult", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::acc(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$lr_class))
curr_row <- c("lr", "adult", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::acc(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$plr_class))
curr_row <- c("plr", "adult", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::acc(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$rpart_class))
curr_row <- c("dt", "hmda", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::acc(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$lr_class))
curr_row <- c("lr", "hmda", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::acc(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$plr_class))
curr_row <- c("plr", "hmda", "acc", curr_score)
exp_df <- rbind(exp_df, curr_row)

# precision metric
curr_score <- mlr3measures::precision(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$rpart_class), "1")
curr_row <- c("dt", "titanic", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
colnames(exp_df) <- c("model", "dataset", "metric", "score")
curr_score <- mlr3measures::precision(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$lr_class), "1")
curr_row <- c("lr", "titanic", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::precision(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$plr_class), "1")
curr_row <- c("plr", "titanic", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::precision(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$rpart_class), "1")
curr_row <- c("dt", "adult", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::precision(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$lr_class), "1")
curr_row <- c("lr", "adult", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::precision(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$plr_class), "1")
curr_row <- c("plr", "adult", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::precision(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$rpart_class), "1")
curr_row <- c("dt", "hmda", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::precision(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$lr_class), "1")
curr_row <- c("lr", "hmda", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::precision(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$plr_class), "1")
curr_row <- c("plr", "hmda", "precision", curr_score)
exp_df <- rbind(exp_df, curr_row)

# recall metric
curr_score <- mlr3measures::recall(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$rpart_class), "1")
curr_row <- c("dt", "titanic", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
colnames(exp_df) <- c("model", "dataset", "metric", "score")
curr_score <- mlr3measures::recall(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$lr_class), "1")
curr_row <- c("lr", "titanic", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::recall(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$plr_class), "1")
curr_row <- c("plr", "titanic", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::recall(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$rpart_class), "1")
curr_row <- c("dt", "adult", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::recall(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$lr_class), "1")
curr_row <- c("lr", "adult", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::recall(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$plr_class), "1")
curr_row <- c("plr", "adult", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::recall(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$rpart_class), "1")
curr_row <- c("dt", "hmda", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::recall(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$lr_class), "1")
curr_row <- c("lr", "hmda", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::recall(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$plr_class), "1")
curr_row <- c("plr", "hmda", "recall", curr_score)
exp_df <- rbind(exp_df, curr_row)

# f1 metric
curr_score <- mlr3measures::fbeta(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$rpart_class), "1", beta = 1)
curr_row <- c("dt", "titanic", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
colnames(exp_df) <- c("model", "dataset", "metric", "score")
curr_score <- mlr3measures::fbeta(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$lr_class), "1", beta = 1)
curr_row <- c("lr", "titanic", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::fbeta(as.factor(titanic_score_df$survived), as.factor(titanic_score_df$plr_class), "1", beta = 1)
curr_row <- c("plr", "titanic", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::fbeta(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$rpart_class), "1", beta = 1)
curr_row <- c("dt", "adult", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::fbeta(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$lr_class), "1", beta = 1)
curr_row <- c("lr", "adult", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::fbeta(as.factor(adult_score_df$incomeg50), as.factor(adult_score_df$plr_class), "1", beta = 1)
curr_row <- c("plr", "adult", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::fbeta(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$rpart_class), "1", beta = 1)
curr_row <- c("dt", "hmda", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::fbeta(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$lr_class), "1", beta = 1)
curr_row <- c("lr", "hmda", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::fbeta(as.factor(hmda_score_df$interest_ge_3), as.factor(hmda_score_df$plr_class), "1", beta = 1)
curr_row <- c("plr", "hmda", "f1", curr_score)
exp_df <- rbind(exp_df, curr_row)

# auc metric
curr_score <- mlr3measures::auc(as.factor(titanic_score_df$survived), titanic_score_df$rpart_probs, "1")
curr_row <- c("dt", "titanic", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(titanic_score_df$survived), titanic_score_df$lr_probs, "1")
curr_row <- c("lr", "titanic", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(titanic_score_df$survived), titanic_score_df$plr_probs, "1")
curr_row <- c("plr", "titanic", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::auc(as.factor(adult_score_df$incomeg50), adult_score_df$rpart_probs, "1")
curr_row <- c("dt", "adult", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(adult_score_df$incomeg50), adult_score_df$lr_probs, "1")
curr_row <- c("lr", "adult", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(adult_score_df$incomeg50), adult_score_df$plr_probs, "1")
curr_row <- c("plr", "adult", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)

curr_score <- mlr3measures::auc(as.factor(hmda_score_df$interest_ge_3), hmda_score_df$rpart_probs, "1")
curr_row <- c("dt", "hmda", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(hmda_score_df$interest_ge_3), hmda_score_df$lr_probs, "1")
curr_row <- c("lr", "hmda", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)
curr_score <- mlr3measures::auc(as.factor(hmda_score_df$interest_ge_3), hmda_score_df$plr_probs, "1")
curr_row <- c("plr", "hmda", "auc", curr_score)
exp_df <- rbind(exp_df, curr_row)

# Demographic parity by gender

# Titanic only has "female" and male
titanic_parity_ratio <- table(titanic_score_df[titanic_score_df$rpart_class == "1", ]$sex, useNA="ifany")/table(titanic_score_df$sex, useNA="ifany")
curr_score <- titanic_parity_ratio[2]/titanic_parity_ratio[1]
curr_row <- c("dt", "titanic", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
titanic_parity_ratio <- table(titanic_score_df[titanic_score_df$lr_class == "1", ]$sex, useNA="ifany")/table(titanic_score_df$sex, useNA="ifany")
curr_score <- titanic_parity_ratio[2]/titanic_parity_ratio[1]
curr_row <- c("lr", "titanic", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
titanic_parity_ratio <- table(titanic_score_df[titanic_score_df$plr_class == "1", ]$sex, useNA="ifany")/table(titanic_score_df$sex, useNA="ifany")
curr_score <- titanic_parity_ratio[2]/titanic_parity_ratio[1]
curr_row <- c("plr", "titanic", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)

adult_parity_ratio <- table(adult_score_df[adult_score_df$rpart_class == "1", ]$gender, useNA="ifany")/table(adult_score_df$gender, useNA="ifany")
curr_score <- adult_parity_ratio[1]/adult_parity_ratio[2]
curr_row <- c("dt", "adult", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
adult_parity_ratio <- table(adult_score_df[adult_score_df$lr_class == "1", ]$gender, useNA="ifany")/table(adult_score_df$gender, useNA="ifany")
curr_score <- adult_parity_ratio[1]/adult_parity_ratio[2]
curr_row <- c("lr", "adult", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
adult_parity_ratio <- table(adult_score_df[adult_score_df$plr_class == "1", ]$gender, useNA="ifany")/table(adult_score_df$gender, useNA="ifany")
curr_score <- adult_parity_ratio[1]/adult_parity_ratio[2]
curr_row <- c("plr", "adult", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)

hmda_parity_ratio <- table(hmda_score_df[hmda_score_df$rpart_class == "1", ]$derived_sex, useNA="ifany")/table(hmda_score_df$derived_sex, useNA="ifany")
curr_score <- hmda_parity_ratio[3]/hmda_parity_ratio[1]
curr_row <- c("dt", "hmda", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
hmda_parity_ratio <- table(hmda_score_df[hmda_score_df$lr_class == "1", ]$derived_sex, useNA="ifany")/table(hmda_score_df$derived_sex, useNA="ifany")
curr_score <- hmda_parity_ratio[3]/hmda_parity_ratio[1]
curr_row <- c("lr", "hmda", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)
hmda_parity_ratio <- table(hmda_score_df[hmda_score_df$plr_class == "1", ]$derived_sex, useNA="ifany")/table(hmda_score_df$derived_sex, useNA="ifany")
curr_score <- hmda_parity_ratio[1]/hmda_parity_ratio[3]
curr_row <- c("plr", "hmda", "dpr", curr_score)
exp_df <- rbind(exp_df, curr_row)

colnames(exp_df) <- c("system", "dataset", "metric", "score")

output_fp = normalizePath(file.path(data_dir, "raw", "semait_sdms_1.csv"), mustWork = FALSE)
write.csv(exp_df, output_fp, row.names = FALSE)