rm(list = ls())
library("tidyverse")
library("tidyr")
setwd("~/git/diversity/data/sensitivity_checks/")
after_anna_and_erin <- read_csv("diversity_labeled_full_072821.csv")
first_row <- bind_cols('version' = 'baseline', 'recall' = 0.89, 'precision' = 0.486, 
                       'accuracy' = 0.703, 'f1_score' = 0.621)

after_anna_and_erin %>% 
  group_by(soclabel_both) %>% 
  count()

version_param <- "9"

chks <- after_anna_and_erin %>% 
  select(fk_pmid, starts_with("socdiv_"), soclabel_both, uncertain_both) %>% 
  rename(socdiv_comp = str_c("socdiv_v", version_param)) %>% 
  mutate(validation = ifelse(socdiv_comp == 0 & soclabel_both == 0, "true negative", ""),
         validation = ifelse(socdiv_comp == 1 & soclabel_both == 1, "true positive", validation),
         validation = ifelse(socdiv_comp == 1 & soclabel_both == 0, "false positive", validation),
         validation = ifelse(socdiv_comp == 0 & soclabel_both == 1, "false negative", validation)) %>% 
  count(validation) 

false_negatives <- chks$n[1]
false_positives <- chks$n[2]
true_negatives <- chks$n[3]
true_positives <- chks$n[4]

version = str_c("version ", version_param)
recall = true_positives / (true_positives + false_negatives)
precision = true_positives / (true_positives + false_positives)
accuracy = (true_positives + true_negatives) / (true_positives + true_negatives + false_positives + false_negatives)
f1_score = (2 * true_positives) / ((2 * true_positives) + false_positives + false_negatives)
all_together = bind_cols('version' = version, 'recall' = recall, 'precision' = precision, 
                         'accuracy' = accuracy, 'f1_score' = f1_score)
#final_stats <- first_row %>% bind_rows(all_together)
final_stats <- final_stats %>% bind_rows(all_together)

###########

final_stats_output <- final_stats %>% 
  mutate(recall = round(recall, 3),
         precision = round(precision, 3),
         accuracy = round(accuracy, 3),
         f1_score = round(f1_score, 3)) %>% 
  pivot_longer(!version, names_to = "category", values_to = "measure")

setwd("~/git/diversity/data/sensitivity_checks/")
write_csv(final_stats_output, "socdiv_version_comps_073021.csv")

final_stats_output %>% 
  mutate(version = str_replace(version, "baseline", "Baseline"),
         version = str_replace(version, "version", "Version"),
         category = str_replace(category, "f1_score", "F1 Score"),
         category = str_replace(category, "accuracy", "Accuracy"),
         category = str_replace(category, "recall", "Recall"),
         category = str_replace(category, "precision", "Precision")
         ) %>% 
  ggplot(aes(x=version, y=measure, group=category)) +
  geom_line(aes(linetype=category))+
  geom_point() +
  theme_minimal() +
  labs(title = "Comparison of Social Diversity Classification Algorithms",
       linetype = "Measurements") + 
  theme(legend.position="bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

###################################################################

source("~/git/diversity/scripts/diversitizeR.R")
setwd("~/git/diversity/data/sensitivity_checks/")
after_anna_and_erin <- read_csv("diversity_labeled_full_072821.csv")
chk <- read_csv("socdiv_version_comps_073021.csv")

pubmed_v10 <- after_anna_and_erin %>%
  mutate(human_old = human, nonhuman_old = nonhuman) %>% 
  mutate(abstract_raw = abstract, 
         abstract = tolower(abstract)) %>% 
  humanizeR(fk_pmid, abstract) %>% 
  compoundR(abstract) %>% 
  filter(human > 0 | nonhuman == 0) %>% 
  detect_social_diversity_v3(fk_pmid, abstract_raw, 
                             remove_polysemes = FALSE) %>% 
  rename(socdiv_v10 = soc_diversity) %>% 
  select(fk_pmid, socdiv_v10)

after_anna_and_erin <- after_anna_and_erin %>% 
  left_join(pubmed_v10, by = "fk_pmid") %>% 
  mutate(socdiv_v10 = replace_na.(socdiv_v10, 0))

after_anna_and_erin <- after_anna_and_erin %>%
  select(starts_with("socdiv_v10"))


