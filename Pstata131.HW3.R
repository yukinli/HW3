library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
library(ISLR)
library(ISLR2) 
library(discrim)
library(poissonreg)
library(corrr)
library(klaR) 
tidymodels_prefer()
#1
set.seed(3435)
mydb<-dbConnect(RSQLite::SQLite(),"my-db.sqlite")
gender_submission<-read.csv("/Users/yukinli/Documents/gender_submission.csv")
train<-read.csv("/Users/yukinli/Documents/train.csv")
test<-read.csv("/Users/yukinli/Documents/test.csv")
dbWriteTable(mydb, "gender_submission", gender_submission)
dbWriteTable(mydb, "train", train)
dbWriteTable(mydb, "test", test)
view(gender_submission)
view(train)
view(test)
# Sampling provides better population coverage because researchers can control for subgroups to ensure that all subgroups are represented in the sampling.


#2
view(train$Survived)
summary(train$Survived)
sum(train$Survived == "1")
sum(train$Survived =="0")
# 342 passengers have survived and 549 passengers have not survived.

#3
cor_Titanic <- train %>%
  select(-Survived) %>%
  correlate()
rplot(cor_Titanic)

# I think Pclass and Fare are correlated.

#4
titanic_recipe <- recipe(Survived ~ ., data = train) %>% 
  step_dummy(all_nominal_predictors())


#5
log_reg <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")
log_wkflow <- workflow() %>% 
  add_model(log_reg) %>% 
  add_recipe(titanic_recipe)

log_fit <- fit(log_wkflow, train)

#6
lda_mod <- discrim_linear() %>% 
  set_mode("classification") %>% 
  set_engine("MASS")

lda_wkflow <- workflow() %>% 
  add_model(lda_mod) %>% 
  add_recipe(titanic_recipe)

lda_fit <- fit(lda_wkflow,train)

#7
qda_mod <- discrim_quad() %>% 
  set_mode("classification") %>% 
  set_engine("MASS")

qda_wkflow <- workflow() %>% 
  add_model(qda_mod) %>% 
  add_recipe(titanic_recipe)

qda_fit <- fit(qda_wkflow, train)

#8
nb_mod <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR") %>% 
  set_args(usekernel = FALSE) 

nb_wkflow <- workflow() %>% 
  add_model(nb_mod) %>% 
  add_recipe(titanic_recipe)

nb_fit <- fit(nb_wkflow, train)

#9
titanic_train_res1<- predict(log_fit, new_data = train, type = "prob")
titanic_train_res1<- bind_cols(titanic_train_res1, train %>% select(Survived))
log_reg_acc <- augment(log_fit, new_data = train) %>%
  accuracy(truth = Direction, estimate = .pred_class)
log_reg_acc

titanic_train_res2<- predict(lda_fit, new_data = train, type = "prob")
titanic_train_res2<- bind_cols(titanic_train_res2, train %>% select(Survived))
lda_acc <- augment(log_fit, new_data = train) %>%
  accuracy(truth = Direction, estimate = .pred_class)
lda_acc

titanic_train_res3<- predict(qda_fit, new_data = train, type = "prob")
titanic_train_res3<- bind_cols(titanictrain_res3, train %>% select(Survived))
qda_acc <- augment(log_fit, new_data = train) %>%
  accuracy(truth = Direction, estimate = .pred_class)
qda_acc

titanic_train_res4<- predict(nb_fit, new_data = train, type = "prob")
titanic_train_res4<- bind_cols(titanic_train_res4, train %>% select(Survived))
nb_acc <- augment(log_fit, new_data = train) %>%
  accuracy(truth = Direction, estimate = .pred_class)
nb_acc

accuracies <- c(log_reg_acc$.estimate, lda_acc$.estimate, 
                nb_acc$.estimate, qda_acc$.estimate)
models <- c("Logistic Regression", "LDA", "Naive Bayes", "QDA")
results <- tibble(accuracies = accuracies, models = models)
results %>% 
  arrange(-accuracies)

# the Naive Bayes mode is better

#10
predict(nb_fit, new_data = test, type = "prob")
augment(nb_fit, new_data = test) %>%
  conf_mat(truth = Survived, estimate = .pred_class) 
augment(nb_fit, new_data = test) %>%
  roc_curve(Survived, .pred_Down) %>%
  autoplot()


