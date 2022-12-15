# MOV data analysis for Nigeria for dpt
# Author: Caroline Nondin
# Objective: Understand if high performing countries such as Nigeria 
#successfully addressed “missed opportunities” for dpt and identify which factors 
#significantly affect MOV
# date: Last modified 12/04/22

#Libraries:
library(aod)
library(dplyr)
library(ggplot2)
library(tibble)

#libraries for visualizations 
library(broom)
library("gridExtra")
library(tidyverse)
library(pixiedust)
library(kableExtra)

#setting up team one drive and code repo 
g_drive  <- '/Users/cnondin/Library/CloudStorage/OneDrive-SharedLibraries-UW/og_phi_global_vaccination_improvement_project - General/'
code_dir <- 'C:/Documents/PHI TA job/aim 1/git_repo/uw-phi-vax/global_vac_index/'
prepped_data_dir <- paste0(g_drive,"Data/prepped_data/") # location of prepped data

#adding a results and visuals directory 
resDir <- paste0(g_drive, "Results/aim_1/missed_opportunities/MOV_analysis/") # location of  any result outputs
visDir <- paste0(g_drive,"Visualizations/aim_1/missed_opportunities/MOV_analysis/") # location where visualizations are saved

#Opening relevant data files (04_prepped_dhs_for_MOV):
prep_data_Nig <- paste0(prepped_data_dir, "aim_1/04_prepped_dhs_for_mov.RDS")

#Loading data 
dt_Nig <- as_tibble(readRDS(prep_data_Nig))

#selecting the variables relevant to MOV analysis: 
#these include: dpt_missed_opportunity (outcome), wom_agecat, edu, female_head, literate, total_children_born, wom_occ, marital, hhsize, assets, urban, strata
dt_Nig <- select(dt_Nig, dpt_missed_opportunity, wom_agecat, edu, female_head,
                 literate, total_children_born, wom_occ, marital, hhsize, assets, urban, strata)

#logistic regression Nigeria: 
mylogit_Nig <- glm(dpt_missed_opportunity ~ wom_agecat + edu + female_head + literate +
                     total_children_born + wom_occ + marital + hhsize + assets + 
                     urban + strata, data = dt_Nig, family = "binomial")
summary(mylogit_Nig)

#making a table with the results of the regression: 
tidy(mylogit_Nig)

table_results <- dust(mylogit_Nig) %>%
  sprinkle_print_method("console") %>%
  kable() %>%
  kable_styling()

#calculating CIs using the standard error (can also do using log-likelihood if better):
confint.default(mylogit_Nig)

#wald test: 
#for age of mother:
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 2:3)
#for education level: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 4:5)
#for sex of head of house: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 6)
#Literacy levels: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 7)
#Total nb of children born: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 8:10)
#respondents occupation: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 11)
#maritial status: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 12:14)
#household size: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 15)
#Wealth index: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 16:19)
#urban vs. rural: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 20)
#year: 
wald.test(b = coef(mylogit_Nig), Sigma = vcov(mylogit_Nig), Terms = 21)

#calculating the odds ratios and the 95% CI: 
OR_CI_data_Nig = exp(cbind(OR = coef(mylogit_Nig), confint(mylogit_Nig)))
OR_CI_data_Nig = as.data.frame(OR_CI_data_Nig)
OR_Nig = OR_CI_data_Nig$OR
CI_Nig_2.5 = OR_CI_data_Nig$`2.5 %`
CI_Nig_97.5 = OR_CI_data_Nig$`97.5 %`

#making a forest plot with the ORs: 
dat_FP_Nig <- data.frame(
  Index = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), ## This provides an order to the data
  label = c("Intercept", "Current age in yrs (15-19 vs. 20-34)", "Current age in yrs (15-19 vs. 35-49)", "Education (None vs. Primary)", "Education (None vs. Secondary or higher)",
            "Sex head houshold (male vs. female)", "Literacy (Iliterate vs. Literate)", "nb children (1 vs. 2-3)", "nb children (1 vs. 4-5)", "nb children (1 vs. 6+)", 
            "Unemployed vs. Employed", "Marital Status (Single vs. Married)", "Marital Status (Single vs. Union)", "Marital Status (Single vs. Divorced, seperated, widowed, or other)", 
            "Household Size", "Household Assets (quintile 1 vs. 2)", "Household Assets (quintile 1 vs. 3)", "Household Assets (quintile 1 vs. 4)", 
            "Household Assets (quintile 1 vs. 5)", "Rural vs. Urban Residence", "DHS Year (2013 vs. 2018)"),
  OR = OR_Nig,
  CI_Lower = CI_Nig_2.5,
  CI_Upper = CI_Nig_97.5
)
dat_FP_Nig$CI_Lower_round <- round(dat_FP_Nig$CI_Lower ,digit=2)
dat_FP_Nig$CI_Upper_round <- round(dat_FP_Nig$CI_Upper ,digit=2)
dat_FP_Nig$CI <- paste(dat_FP_Nig$CI_Lower_round, dat_FP_Nig$CI_Upper_round)
dat_FP_Nig

FP_Nig <- ggplot(dat_FP_Nig, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.25) +
  geom_vline(xintercept = 1, color = "purple", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:21, labels = dat_FP_Nig$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
FP_Nig

#adding CI and OR values to graph: 
table_base <- ggplot(dat_FP_Nig, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(OR, digits = 1))), size = 4) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")

#Combining the plot with the tables: 
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(FP_Nig, tab1, tab2, layout_matrix = lay)

#calculating the p-values w/ data stratified by year: 
#for 2013: 
df_2013 = dt_Nig[dt_Nig$strata == '2013',]

mylogit_Nig_2013 <- glm(dpt_missed_opportunity ~ wom_agecat + edu + female_head + literate +
                     total_children_born + wom_occ + marital + hhsize + assets + 
                     urban, data = df_2013, family = "binomial")
summary(mylogit_Nig_2013)

#making a table with the results of the regression: 
tidy(mylogit_Nig_2013)

dust(mylogit_Nig_2013) %>%
  sprinkle_print_method("console") %>%
  kable() %>%
  kable_styling()

#log reg on stratified data, 2018
df_2018 = dt_Nig[dt_Nig$strata == '2018',]

mylogit_Nig_2018 <- glm(dpt_missed_opportunity ~ wom_agecat + edu + female_head + literate +
                          total_children_born + marital + hhsize + assets + 
                          urban, data = df_2018, family = "binomial")
#removed wom_occ category since there was only one value ("employed") in the column 
summary(mylogit_Nig_2018)

#making a table with the results of the regression: 
tidy(mylogit_Nig_2018)

dust(mylogit_Nig_2018) %>%
  sprinkle_print_method("console") %>%
  kable() %>%
  kable_styling()

#STEP 2: Making predictions based on year: 
#we're keeping all the other variables except for strata constant. 

#finding the most frequent value for certain variables: 
tail(names(sort(table(dt_Nig$wom_agecat))), 1) #age category 
tail(names(sort(table(dt_Nig$edu))), 1) #level of education 
tail(names(sort(table(dt_Nig$literate))), 1) #literacy level 
tail(names(sort(table(dt_Nig$total_children_born))), 1) #nb children in household   
tail(names(sort(table(dt_Nig$wom_occ))), 1) #occupation 
tail(names(sort(table(dt_Nig$assets))), 1) #asset quintile 
tail(names(sort(table(dt_Nig$urban))), 1) #urban vs. rural 

#making a dataset with the baseline values for the variables: 
pred_prob_data_Nig <- with(dt_Nig, data.frame(wom_agecat = "20-34", edu = "No education",  female_head = "Female", literate = "Iliterate",
                                              total_children_born = "2-3 children", wom_occ = "Employed", marital = "Married", hhsize = mean(hhsize), 
                                              assets = "Quintile 2", urban = "Rural household", strata = c("2013", "2018")))
pred_prob_data_Nig
pred_prob_data_Nig$strataP <- predict(mylogit_Nig, newdata = pred_prob_data_Nig, type = "response")
pred_prob_data_Nig

#making a graph to see the predicted probabilities with education as the independent variable 
#creating 100 values for each variable 
pred_prob_edu <- with(dt_Nig, data.frame(edu = factor(rep(unique(dt_Nig$edu), length.out= 100)),
                                         wom_agecat = factor(rep(c("20-34"), each = 100)), female_head = factor(rep(c("Female"), each =100)),
                                         literate = factor(rep(c("Iliterate"), each = 100)), total_children_born = factor(rep(c("2-3 children"), each = 100)),
                                         wom_occ = factor(rep(c("Employed"), each =100)), marital = factor(rep(c("Married"), each = 100)), hhsize = mean(hhsize), 
                                         assets = factor(rep(c("Quintile 2"), each = 100)), urban = factor(rep(c("Rural household"), each = 100)),
                                         strata = factor(rep(unique(dt_Nig$strata), ech = 50, length.out = 100))))

pred_prob_lit <- with(dt_Nig, data.frame(edu = factor(rep(c("No education"), each = 100)),
                                         wom_agecat = factor(rep(c("20-34"), each = 100)), female_head = factor(rep(c("Female"), each =100)),
                                         literate = factor(rep(c("Iliterate", "Literate"), length.out = 100)), total_children_born = factor(rep(c("2-3 children"), each = 100)),
                                         wom_occ = factor(rep(c("Employed"), each =100)), marital = factor(rep(c("Married"), each = 100)), hhsize = mean(hhsize), 
                                         assets = factor(rep(c("Quintile 2"), each = 100)), urban = factor(rep(c("Rural household"), each = 100)),
                                         strata = factor(rep(unique(dt_Nig$strata), each = 50, length.out = 100))))

pred_prob_assets <- with(dt_Nig, data.frame(edu = factor(rep(c("No education"), each = 100)),
                                         wom_agecat = factor(rep(c("20-34"), each = 100)), female_head = factor(rep(c("Female"), each =100)),
                                         literate = factor(rep(c("Iliterate"), each = 100)), total_children_born = factor(rep(c("2-3 children"), each = 100)),
                                         wom_occ = factor(rep(c("Employed"), each =100)), marital = factor(rep(c("Married"), each = 100)), hhsize = mean(hhsize), 
                                         assets = factor(rep(unique(dt_Nig$assets), length.out = 100)), urban = factor(rep(c("Rural household"), each = 100)),
                                         strata = factor(rep(unique(dt_Nig$strata), each = 50, length.out = 100))))

#calculating the predicted probs and the standard errors: 
pred_prob_edu <- cbind(pred_prob_edu, predict(mylogit_Nig, newdata = pred_prob_edu, type = "link",
                                              se = TRUE))
pred_prob_edu <- within(pred_prob_edu, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(pred_prob_edu)

pred_prob_lit <- cbind(pred_prob_lit, predict(mylogit_Nig, newdata = pred_prob_lit, type = "link",
                                              se = TRUE))
pred_prob_lit <- within(pred_prob_lit, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(pred_prob_lit)

pred_prob_assets <- cbind(pred_prob_assets, predict(mylogit_Nig, newdata = pred_prob_assets, type = "link",
                                              se = TRUE))
pred_prob_assets <- within(pred_prob_assets, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(pred_prob_assets)

#making a graph with the predictions: 
p_edu <- ggplot(pred_prob_edu, aes(x=edu, y=PredictedProb, fill=strata)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(.9))

p_edu + scale_fill_brewer(palette="Paired") + theme_minimal()

p_lit <- ggplot(pred_prob_lit, aes(x=literate, y=PredictedProb, fill=strata)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(.9))

p_lit + scale_fill_brewer(palette="Paired") + theme_minimal()

p_assets <- ggplot(pred_prob_assets, aes(x=assets, y=PredictedProb, fill=strata)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.2,
                position=position_dodge(.9))

p_assets + scale_fill_brewer(palette="Paired") + theme_minimal()

# measuring how well our logistic reg model fit (test statistic) 
with(mylogit_Nig, null.deviance - deviance)

#The degrees of freedom for the difference between the two models (our model vs. null): 
with(mylogit_Nig, df.null - df.residual)

#finding the p_value 
with(mylogit_Nig, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# our model fits significantly better than a null model 

