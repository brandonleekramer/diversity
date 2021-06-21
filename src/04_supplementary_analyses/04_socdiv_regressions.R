
# run 01_aggregated_ids (except the psql bit) for dataframes  

library("tidyverse")
library("tidytext")
library("RPostgreSQL")
library("naniar")
library("psych")
library("ggcorrplot")
library("aod")
library("caret")
library('pscl')
library("ROCR")

# connect to postgresql to get our data
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
all_human_abstracts <- dbGetQuery(conn, "select fk_pmid, year, human_study, animal_study, publication
                                         from pubmed_2021.biomedical_human_abstracts;")
dbDisconnect(conn)
rm(h1_ids, h2_ids, h3_ids, conn)

# combined_ids comes from 01_aggregated_ids and includes all of the vars we want 
combined_ids <- combined_ids %>% select(-year)

# add that to the full dataset 
all_human_abstracts <- all_human_abstracts %>% 
  left_join(combined_ids, by = 'fk_pmid') %>%
  mutate_all(funs(replace_na(.,0)))

# bind in journal topics 
journals_wtopics <- read_csv("~/git/diversity/data/journal_rankings/top_250_journals_wtopics.csv")

all_human_abstracts <- all_human_abstracts %>% 
  left_join(journals_wtopics, by = "publication") %>% 
  select(-count, -include, -overall_rank)

setwd("~/git/diversity/data/final_data/")
write_rds(all_human_abstracts, "socdiv_regression_data.rds")

# check the missingness - no missingness  
all_human_abstracts %>% 
  summarise_all(funs(sum(is.na(.))))

# check to make sure we only have counts - true  
all_human_abstracts %>% 
  select(-publication, -domain) %>% 
  summarise_all(sum)
describe(all_human_abstracts %>% select(-publication, -domain))

# convert all of the variables to binary indicator vars 
to_binary <- function(x, na.rm = FALSE) (if_else(x > 0, 1, 0))
# convert the outcome to binary 
soc_div_binary_df <- all_human_abstracts %>% 
  select(-fk_pmid, -diversity, -publication, -domain) %>% 
  mutate_at(vars(soc_diversity), to_binary) 
describe(soc_div_binary_df)
soc_div_cors <- cor(soc_div_binary_df)
ggcorrplot(soc_div_cors)

# logistic regression for social_diversity outcome 

# first, we will split the dataset 
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(soc_div_binary_df), replace = T, prob = c(0.6,0.4))
train <- soc_div_binary_df[sample, ]
test <- soc_div_binary_df[!sample, ]

# lets visualize the relationship between soc div and racial terms 
soc_div_binary_df %>%
  mutate(prob = soc_diversity) %>%
  ggplot(aes(racial, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Racial") +
  ylab("Probability of Using Social Diversity")

# let's run a simple test model with only racial terms included 
test_model <- glm(soc_diversity ~ racial, family = "binomial", data = train)

# to see the model outputs 
summary(test_model)
# in a tidy format 
tidy(test_model)
# to interpret the log odds as factors 
exp(coef(test_model))
# this can be interpreted as for every 1 time that race is used 
# the probability of social diversity will be used increases 
# by a logs odd of 0.0107 or by a factor of 1.5 

# to get the confidence intervals 
confint(test_model)

# now we can predict whether soc div will be used based on using racial terms once or five times 
# jumps from 1% to 7% 
predict(test_model, data.frame(racial = c(1, 5)), type = "response")


# testing how H1 cateogoris predict social_diversity 
model1 <- glm(soc_diversity ~ year + cultural + disability + equity + lifecourse + migration + 
               minority + race_ethnicity + sex_gender + sexuality + social_class, 
               data = train, family = "binomial")
tidy(model1) # AIC: 214919

model2 <- glm(soc_diversity ~ year + asian + black + ethnic + hispanic + 
               native_american + pacific_islander + racial + white, 
               data = train, family = "binomial")
tidy(model2) # AIC: 220744
anova(model1, model2, test = "Chisq")

model3 <- glm(soc_diversity ~ year + continental + directional + national + 
                 omb_uscensus + race_ethnicity + subcontinental + subnational, 
               data = train, family = "binomial")
tidy(model3) # AIC: 217176
anova(model1, model3, test = "Chisq")
# model 3 is better than model 1 
anova(model2, model3, test = "Chisq")
# model 3 is not better than model 2 

model4 <- glm(soc_diversity ~ year + cultural + disability + equity + lifecourse + migration + 
                 minority + minority + sex_gender + sexuality + social_class +
                 asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white, 
               data = train, family = "binomial")
tidy(model44) # AIC: 214397
anova(model1, model4, test = "Chisq")
anova(model2, model4, test = "Chisq")
anova(model3, model4, test = "Chisq")
# model 4 is better than everything else 

model5 <- glm(soc_diversity ~ year + cultural + disability + equity + lifecourse + migration + 
                 minority + minority + race_ethnicity + sex_gender + sexuality + social_class +
                 continental + directional + national + omb_uscensus + race_ethnicity + subcontinental + subnational, 
               data = train, family = "binomial")
tidy(model5) # AIC: 212023
anova(model1, model5, test = "Chisq")
anova(model2, model5, test = "Chisq")
anova(model3, model5, test = "Chisq")
anova(model4, model5, test = "Chisq")
# model5 is better than m1-3 but not better than m4 

model6 <- glm(soc_diversity ~ year + cultural + disability + equity + lifecourse + 
                 migration + minority + sex_gender + sexuality + social_class +
                 asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white +
                 continental + directional + national + subcontinental + subnational, 
               data = train, family = "binomial")
summary(model6) # AIC: 211664
anova(model4, model6, test = "Chisq")
anova(model5, model6, test = "Chisq")
# not surprisingly, model6 is the best of them all

model7 <- glm(soc_diversity ~ year + human_study + animal_study + cultural + disability + equity + lifecourse + 
                migration + minority + sex_gender + sexuality + social_class +
                asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white +
                continental + directional + national + subcontinental + subnational, 
              data = train, family = "binomial")
summary(model7) # AIC: 210865
anova(model6, model7, test = "Chisq")

# adding in the journal information 
model8 <- glm(soc_diversity ~ year + human_study + animal_study + cultural + disability + equity + lifecourse + 
                migration + minority + sex_gender + sexuality + social_class +
                asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white +
                continental + directional + national + subcontinental + subnational +
                nature + annual_reviews + lancet + trends_in, 
              data = train, family = "binomial")
summary(model8) # AIC: 210623
anova(model7, model8, test = "Chisq")

# adding in the journal information 
# left out other as the baseline 
model9 <- glm(soc_diversity ~ year + human_study + animal_study + cultural + disability + equity + lifecourse + 
                migration + minority + sex_gender + sexuality + social_class +
                asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white +
                continental + directional + national + subcontinental + subnational +
                bio_tech + bio_sciences + brain_sciences + cancer_research + general_sciences + genomic_sciences + 
                healthcare + immuno_infect + internal_medicine + pharma_sciences + public_health, 
              data = train, family = "binomial")
summary(model9) # AIC: 207835
anova(model7, model9, test = "Chisq")  
anova(model8, model9, test = "Chisq")

model10 <- glm(soc_diversity ~ year + human_study + animal_study + cultural + disability + equity + lifecourse + 
                migration + minority + sex_gender + sexuality + social_class +
                asian + black + ethnic + hispanic + native_american + pacific_islander + racial + white +
                continental + directional + national + subcontinental + subnational +
                nature + annual_reviews + lancet + trends_in + 
                brain_sciences + cancer_research + general_sciences + genomic_sciences + public_health, 
              data = train, family = "binomial")
summary(model10) # AIC: 208081
anova(model9, model10, test = "Chisq") 


alt_df <- all_human_abstracts %>% select()

# we can also predict outcomes given a specific set of conditions 
# new.df <- tibble(var1 = 1500, var2 = 40, var3 = c("Yes", "No"))
# predict(model3, new.df, type = "response")

# testing importance of variables in a model 
caret::varImp(model6)

# assessing model fit ########################

# using ratio likelihood test 
# the goal is to increase the log likelihood and reduce the model deviance
anova(model1, model2, test = "Chisq")

# using pseudo R-squared (i.e. mcfadden's R-squared)
# the higher the R-squared the better 
list(model1 = pscl::pR2(model1)["McFadden"], #0.06194243
     model2 = pscl::pR2(model2)["McFadden"], #0.03649587
     model3 = pscl::pR2(model3)["McFadden"], #0.05206379
     model4 = pscl::pR2(model4)["McFadden"], #0.06428183
     model5 = pscl::pR2(model5)["McFadden"], #0.07463675
     model6 = pscl::pR2(model6)["McFadden"], #0.07625731
     model7 = pscl::pR2(model7)["McFadden"]) #0.07976081 

# residual assessment ################################################
# we want to look at the data to see if outliers might bias the result 
model7_data <- augment(model7) %>% 
  mutate(index = 1:n())

ggplot(model6_data, aes(index, .std.resid, color = soc_diversity)) + 
  geom_point(alpha = .5) 

# for example those with a SD over 3 
model6_data %>% 
  filter(abs(.std.resid) > 3)
# or you can use the cook's distance 
plot(model6, which = 4, id.n = 5)
model6_data %>% top_n(5, .cooksd)

# Validation of Predicted Values
test_predicted_m1 <- predict(model1, newdata = test, type = "response")
test_predicted_m2 <- predict(model2, newdata = test, type = "response")
test_predicted_m3 <- predict(model3, newdata = test, type = "response")
test_predicted_m4 <- predict(model4, newdata = test, type = "response")
test_predicted_m5 <- predict(model5, newdata = test, type = "response")
test_predicted_m6 <- predict(model6, newdata = test, type = "response")
test_predicted_m7 <- predict(model7, newdata = test, type = "response")

list(
  model1 = table(test$soc_diversity, test_predicted_m1 > 0.5) %>% prop.table() %>% round(3),
  model2 = table(test$soc_diversity, test_predicted_m2 > 0.5) %>% prop.table() %>% round(3),
  model3 = table(test$soc_diversity, test_predicted_m3 > 0.5) %>% prop.table() %>% round(3),
  model4 = table(test$soc_diversity, test_predicted_m4 > 0.5) %>% prop.table() %>% round(3),
  model5 = table(test$soc_diversity, test_predicted_m5 > 0.5) %>% prop.table() %>% round(3),
  model6 = table(test$soc_diversity, test_predicted_m6 > 0.5) %>% prop.table() %>% round(3),
  model7 = table(test$soc_diversity, test_predicted_m7 > 0.5) %>% prop.table() %>% round(3)
)

# $model6
# FALSE  TRUE
# 0 0.992 0.000
# 1 0.008 0.000
# 99% are true negatives 
# Type II error less than 0.8%

# error rate 
test %>%
  mutate(m1_pred = ifelse(test_predicted_m1 > 0.5, 1, 0),
         m2_pred = ifelse(test_predicted_m2 > 0.5, 1, 0),
         m3_pred = ifelse(test_predicted_m3 > 0.5, 1, 0),
         m4_pred = ifelse(test_predicted_m4 > 0.5, 1, 0),
         m5_pred = ifelse(test_predicted_m5 > 0.5, 1, 0),
         m6_pred = ifelse(test_predicted_m6 > 0.5, 1, 0),
         m7_pred = ifelse(test_predicted_m7 > 0.5, 1, 0)) %>%
  summarise(m1_error = mean(soc_diversity != m1_pred),
            m2_error = mean(soc_diversity != m2_pred),
            m3_error = mean(soc_diversity != m3_pred),
            m4_error = mean(soc_diversity != m4_pred),
            m5_error = mean(soc_diversity != m5_pred),
            m6_error = mean(soc_diversity != m6_pred),
            m7_error = mean(soc_diversity != m7_pred))
# similar results across models 

# we can now take a look at the raw confusion matrix 
table(test$soc_diversity, test_predicted_m6 > 0.5)
# FALSE   TRUE
# 0 995274    238
# 1   7692     48
# 7692/(7692+48) = 0.9937984
# this means that the precision is terrible 
# 1-238/(995274+238) = 0.9997609
# the sensitivity is pretty good 
# but that's likely bc there are so many non soc_divs in the data 

table(test$soc_diversity, test_predicted_m7 > 0.5)

par(mfrow=c(1, 2))

prediction(test_predicted_m1, test$soc_diversity) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test_predicted_m6, test$soc_diversity) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test_predicted_m7, test$soc_diversity) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test_predicted_m1, test$soc_diversity) %>%
  performance(measure = "auc") %>%
  .@y.values 
# 0.7526162

prediction(test_predicted_m6, test$soc_diversity) %>%
  performance(measure = "auc") %>%
  .@y.values
# 0.7770629

prediction(test_predicted_m7, test$soc_diversity) %>%
  performance(measure = "auc") %>%
  .@y.values
# 0.7773885

# We can continue to “tune” our models to improve these classification rates. 
# If you can improve your AUC and ROC curves (which means you are improving 
# the classification accuracy rates) you are creating “lift”, meaning you are lifting the classification accuracy.

# wald tests for combined effects of h1, h2 and h3 
wald.test(b = coef(model_6), Sigma = vcov(model_6), Terms = 2:10)
wald.test(b = coef(model_6), Sigma = vcov(model_6), Terms = 11:18)
wald.test(b = coef(model_6), Sigma = vcov(model_6), Terms = 19:23)


# references 
# https://uc-r.github.io/logistic_regression 




