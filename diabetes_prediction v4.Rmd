---
title: "Diabetes prediction"
date: "May 2017"
author: "Simon Hendrie"
output:
  word_document:
    reference_docx: word-styles-reference-01.docx
#    fig_captions: true
    
---
  
***

```{r global_options, include = FALSE, cache = FALSE}
# put in cache true here, captions - check these work

### Libraries

library(doParallel)
library(pryr)

library(rio)
library(foreign)

library(magrittr)
library(stringr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(lubridate)
library(knitr)
library(tabplot)
library(broom)

library(pander)

library(mlbench)
library(FSelector)
library(caret)
library(car)
library(dummy)
library(glmnet)
library(ROCR)
library(Matrix)
# library(elasticnet)
# library(stargazer)
library(pscl)
library(effects)
library(biglm)

# library(CHAID)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(partykit)

library(vcd)
library(directlabels)
library(ggrepel)
library(shiny)

library(colorspace)
library(RColorBrewer)

library(scales)
library(dplyr) # last so it can override conflicts

# detach("package:plyr", unload=TRUE) 

# Careful with this one... There's a conflict in the function 
#'performance' with ROCR
# library(portfolio)
# This may conflict with ROCR
# library(pROC)
# May need:
# detach("package:portfolio", unload=TRUE)
# detach("package:pROC", unload=TRUE)

### Set environment

# turn off scientific notation except for big numbers
options(scipen = 9)

### Check cores
detectCores()
registerDoParallel(cores = 6)
# Midi 8
# Terminator 40

setwd("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes")
# rm(list= ls())
knitr::opts_chunk$set(fig.width = 9, fig.height = 5, echo = FALSE, message = FALSE, error = FALSE, dpi = 300)
```


```{r import_full_pop_data}

### Import smaller sample for quick fits
popf <- read.csv("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes/all_data_vF.csv", header = TRUE, stringsAsFactors = FALSE, nrows = 7000000) 
# Half the sample size reduces MacFadden from 0.17 to 0.165. Coefficients quite volatile but directionally similar. Confusion matrix still disastrous

### Import full version
# popf.sample <- read.csv("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes/all_data_vF.csv", header = TRUE, nrows = 50000, stringsAsFactors = FALSE) 
# (tried 100 000 sample - picks up the problem fields)

# popf.colclass <- sapply(popf.sample, class)
# system.time(popf <- tbl_df(read.csv("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes/all_data_vF.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = popf.colclass, comment.char = "")))

# ~6 minute load time
# system.time(popf <- tbl_df(read.csv("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes/all_data_vF.csv", stringsAsFactors = FALSE, header = TRUE)))

mem_used()
object_size(popf)

# setwd("C:/Users/simon9/Dropbox (Discovery Limited)/000 Simon H/DSU/Diabetes")
plancodz <- tbl_df(read.csv("PLAN_GROUPINGS_OH.csv", header = TRUE, stringsAsFactors = FALSE)) # weirdly, need to run this line twice to get it to work
# plancodz <- tbl_df(read.csv("C://Users//simon9//Dropbox (Discovery Limited)//000 Simon H//DSU//Diabetes//PLAN_GROUPINGS_OH.csv", header = TRUE, stringsAsFactors = FALSE))

```


```{r pop_data_checks, eval = FALSE}
# Entity checks
# 1000009132- diabetic benefit since 2002, first claim 2008
# 1000162188 - normal dude

### Standard checks

# Count number of NAs per column
NAcols <- sapply(popf, function(x) sum(is.na(x)))
NAcols[NAcols > 0]
# NAs per row
NAs <- which(is.na(popf), arr.ind=TRUE)
NArows <- popf[NAs[,1],]
NArows

# Count number of nulls per column
nullcols <- sapply(p1, function(x) sum(is.null(x)))
nullcols[nullcols > 0]
# p1$Master_Effective_To <- NULL

# Incomplete cases
incomplete <- p1[!complete.cases(p1),]
# sum(!complete.cases(p1))
incomplete


str(popf) # check path
# n_distinct(popf$ENTITY_NO)

check.p <- popf %>%
  filter(ENTITY_NO == 1002261053)

# 1000009132
# 1002261053 - hypertension duration

extre_pop <- popf %>%
   filter(popf$TOTAL_PAID_HCC > 1000000) %>%
   arrange(desc(TOTAL_PAID_HCC))
 # write.csv(extre_pop, "population_claims_extreme_values.csv")
 
summary(popf)
 
tabbo <- popf %>%
   group_by(MONTH_KEY_START) %>%
   summarise(popn = n_distinct(ENTITY_NO), poppo = n())
 
condo <- popf %>%
  group_by(CONDITION_CONCAT_LESS_COMB_ADJ, MONTH_KEY_START) %>%
  summarise(counto = n()) %>%
  spread(MONTH_KEY_START, counto)

condy <- popf %>%
  group_by(CONDITION_CONCAT_LESS_COMB_ADJ_NEXT, MONTH_KEY_START) %>%
  summarise(counto = n()) %>%
  spread(MONTH_KEY_START, counto)
  
# %>%
#  arrange(desc(counto)) 

# p <- tablePrepare(popf)
# tableplot(p, select = c("HYPERTENSION_DURATION_DEG", "CHRONIC_RENAL_DISEASE_DURATION_DEG"))
# "CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG", "HYPERLIPIDAEMIA_DURATION_DEG", "DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG"

# outz <- head(popf[order(-popf$HYPERTENSION_DURATION_DEG), ], 100)
quantile(popf$HYPERTENSION_DURATION, probs = seq(0, 1, 0.05))
outz <- head(popf[order(-popf$HYPERTENSION_DURATION), ], 100)
outz <- head(popf[order(-popf$HYPERTENSION_STAGE), ], 100)
quantile(popf$HYPERTENSION_STAGE, probs = seq(0, 1, 0.05))

# summary(popf$HYPERTENSION_STAGE, popf$CHRONIC_RENAL_DISEASE_STAGE, popf$CHRONIC_RENAL_DISEASE_END_STAGE_STAGE, popf$HYPERLIPIDAEMIA_STAGE,popf$DIABETES_MELLITUS_TYPES_1_2_STAGE)

quantile(popf$AGE, probs = seq(0, 1, 0.05))

quantile(popf$CREATINE_TEST_RESULT, probs = seq(0, 1, 0.05))
outz <- head(popf[order(-popf$CREATINE_TEST_RESULT), ], 100)

outz <- head(popf[order(-popf$CHOLESTROL_TEST_RESULT), ], 100)

planz <- popf %>%
  group_by(PLAN_CODE) %>%
  summarise(count = n())

biolevl <- popf %>%
  group_by(BP_SYSTOLIC) %>%
  summarise(count = n())
```


```{r extract_people_not_reg_but_with_open_DEG_in_2016, eval = FALSE}
sum(grepl("Diabetes", popf$CONDITION_CONCAT_LESS)) / dim(popf)[1]
sum(grepl("Diabetes", popf$CONDITION_CONCAT_LESS_DEG)) / dim(popf)[1]
sum(grepl("Diabetes", popf$CONDITION_CONCAT_LESS_COMB)) / dim(popf)[1]
sum(grepl("Diabetes", popf$CONDITION_CONCAT_LESS_COMB_ADJ)) / dim(popf)[1]

# popf$DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG_ACTIVE
# popf$DIABETES_DRUGS
# popf$DIABETES_MELLITUS_TYPES_1_2_DURATION
# popf$DIABETES_MELLITUS_TYPES_1_2_STAGE
# popf$DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG

# not_reg_but_with_deg <- popf[grepl("Diabetes", popf$CONDITION_CONCAT_LESS) == FALSE & grepl("Diabetes", popf$CONDITION_CONCAT_LESS_DEG) == TRUE & popf$MONTH_KEY_START == 201601, ]

should_reg <- popf %>%
  filter(MONTH_KEY_START == 201601, grepl("Diabetes", CONDITION_CONCAT_LESS) == FALSE) %>%
  filter(grepl("Diabetes", CONDITION_CONCAT_LESS_DEG) == TRUE | DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG_ACTIVE > 0 |  DIABETES_DRUGS > 0 | DIABETES_MELLITUS_TYPES_1_2_DURATION > 0 | DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG > 0) %>%
  arrange(desc(DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG), desc(DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG_ACTIVE), desc(DIABETES_DRUGS))
# write.csv(should_reg, "should_reg.csv")
```

```{r pop_data_edit}
# popf.sample$year = as.numeric(substr(popf.sample$MONTH_KEY_START, 1, 4))
# popf.sample$MONTH_KEY_START <- NULL
popf$year = as.numeric(substr(popf$MONTH_KEY_START, 1, 4))
popf$MONTH_KEY_START <- NULL

### Take out non-useful columns
popf$MONTH_KEY_END <- NULL
popf$YM_START_DATE <- NULL
popf$YM_END_DATE <- NULL
# use durations for flags rather
popf$CONDITION_CONCAT_LESS <- NULL
popf$CONDITION_CONCAT_LESS_DEG <- NULL
popf$CONDITION_CONCAT_LESS_COMB <- NULL
# popf$CONDITION_CONCAT_LESS_COMB_ADJ <- NULL
popf$CONDITION_CONCAT_LESS_NEXT <- NULL
popf$CONDITION_CONCAT_LESS_DEG_NEXT <- NULL
# popf$CONDITION_CONCAT_LESS_COMB_ADJ_NEXT <- NULL

popf$HYPERTENSION_DURATION_DEG <- NULL                        
popf$CHRONIC_RENAL_DISEASE_DURATION_DEG <- NULL               
popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG <- NULL     
popf$HYPERLIPIDAEMIA_DURATION_DEG <- NULL                 
popf$DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG <- NULL

# take out staging - all ones
popf$HYPERTENSION_STAGE <- NULL                        
popf$CHRONIC_RENAL_DISEASE_STAGE <- NULL               
popf$CHRONIC_RENAL_DISEASE_END_STAGE_STAGE <- NULL     
popf$HYPERLIPIDAEMIA_STAGE <- NULL                 
popf$DIABETES_MELLITUS_TYPES_1_2_STAGE <- NULL

# take out ACG key
popf$ACG_KEY <- NULL

# take out language
popf$PRIMARY_LANGUAGE <- NULL

### Make more useful ones

### Try without dummies first

# Cut age
# popf$age_band <- cut(popf$age, breaks = seq(0, 110, 10))

# Get OH benefit category
# or look at separate models by OH richness
popf <- popf %>%
  left_join(plancodz, by = c("PLAN_CODE"))
popf$PLAN_CODE <- NULL

popf$OH_BENEFIT_LEVEL[is.na(popf$OH_BENEFIT_LEVEL)] <- "Unclassified"

# R glm for logistic regression should handle dummying of variables

# Dummy OH benefit category
# OH_level <- dummy(as.data.frame(popf$OH_BENEFIT_LEVEL), int = TRUE)
# names(OH_level) <- sub("popf.", "", names(OH_level))
# popf <- cbind(popf, OH_level)

# Dummy gender
# gender <- dummy(as.data.frame(popf$GENDER), int = TRUE)
# names(gender) <- sub("popf.", "", names(gender))
# popf$GENDER <- NULL
# popf <- cbind(popf, gender)

# # Dummy BMI
# bmid <- dummy(as.data.frame(popf$BMI), int = TRUE)
# names(bmid) <- sub("popf.", "", names(bmid))
# popf$BMI <- NULL
# popf <- cbind(popf, bmid)
# 
# # Dummy BP_SYSTOLIC
# bpsd <- dummy(as.data.frame(popf$BP_SYSTOLIC), int = TRUE)
# names(bpsd) <- sub("popf.", "", names(bpsd))
# popf$BP_SYSTOLIC <- NULL
# popf <- cbind(popf, bpsd)
# 
# # Dummy BP_DIASTOLIC
# bpdd <- dummy(as.data.frame(popf$BP_DIASTOLIC), int = TRUE)
# names(bpdd) <- sub("popf.", "", names(bpdd))
# popf$BP_DIASTOLIC <- NULL
# popf <- cbind(popf, bpdd)
# 
# # Dummy BP_DIASTOLIC
# chold <- dummy(as.data.frame(popf$CHOLESTEROL), int = TRUE)
# names(chold) <- sub("popf.", "", names(chold))
# popf$CHOLESTEROL <- NULL
# popf <- cbind(popf, chold)

# Target = Registered for diabetic benefit or should be (DEG opened)

# popf$diabt0 <- 0 
# popf$diabt0[popf$DIABETES_MELLITUS_TYPES_1_2_DURATION > 0] <- 1 
# sum(popf$diabt0)

popf$diabt0 <- "N"
popf$diabt0[grepl("Diabetes", popf$CONDITION_CONCAT_LESS_COMB_ADJ)] <- "Y" 
#sum(popf$diabt0)
popf$diabt0 <- as.factor(popf$diabt0)

popf$diabt1 <- "N" 
popf$diabt1[grepl("Diabetes", popf$CONDITION_CONCAT_LESS_COMB_ADJ_NEXT)] <- "Y" 
#sum(popf$diabt1)
popf$diabt1 <- as.factor(popf$diabt1)

# popf$diabt0 <- as.character(popf$diabt0)
# popf$diabt0[popf$diabt0 == 1] <- "Y"
# popf$diabt0[popf$diabt0 == 0] <- "N"
# 
# popf$diabt1 <- as.character(popf$diabt1)
# popf$diabt1[popf$diabt1 == 1] <- "Y"
# popf$diabt1[popf$diabt1 == 0] <- "N"

### Take out existing diabetic indicators - most should be taken out in the definition of training and test sets - what's left effectively exposes a gap in the DEG. And get picked up in the first list of people to look at for registration
popf$DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG_ACTIVE <- NULL
popf$DIABETES_DRUGS <- NULL
popf$DIABETES_MELLITUS_TYPES_1_2_DURATION <- NULL
popf$DIABETES_MELLITUS_TYPES_1_2_STAGE <- NULL
popf$DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG <- NULL

### Remove deaths in base year
popf <- subset(popf, !grepl("Death", popf$CONDITION_CONCAT_LESS_COMB_ADJ)) 

### remove columns don't need anymore
popf$CONDITION_CONCAT_LESS_COMB_ADJ <- NULL
popf$CONDITION_CONCAT_LESS_COMB_ADJ_NEXT <- NULL

### Dummy the categoricals - don't need to do this for RF
### Remove the outliers - don't seem to be any major problems
### Recode to make easier to understand
popf$CHRONIC_INDICATOR_KEY <- as.character(popf$CHRONIC_INDICATOR_KEY)
popf$CHRONIC_INDICATOR_KEY[popf$CHRONIC_INDICATOR_KEY == 2] <- "N"
popf$CHRONIC_INDICATOR_KEY[popf$CHRONIC_INDICATOR_KEY == 1] <- "Y"

### Make factors out of characters
popf$GENDER <- as.factor(popf$GENDER)
popf$BMI <- as.factor(popf$BMI)
popf$BP_SYSTOLIC <- as.factor(popf$BP_SYSTOLIC)
popf$BP_DIASTOLIC <- as.factor(popf$BP_DIASTOLIC)
popf$CHOLESTEROL <- as.factor(popf$CHOLESTEROL)
popf$OH_BENEFIT_LEVEL <- as.factor(popf$OH_BENEFIT_LEVEL)
popf$NEURO_HISTORY <- as.factor(popf$NEURO_HISTORY)

popf$HYPERTENSION_DURATION_DEG_ACTIVE[popf$HYPERTENSION_DURATION_DEG_ACTIVE == -1] <- "N"
popf$HYPERTENSION_DURATION_DEG_ACTIVE[popf$HYPERTENSION_DURATION_DEG_ACTIVE == 1] <- "Y"

popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == -1] <- "N"
popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == 1] <- "Y"

popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE[popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE == -1] <- "N"
popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE[popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE == 1] <- "Y"

popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE[popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE == -1] <- "N"
popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE[popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE == 1] <- "Y"

popf$HYPERTENSION_DURATION_DEG_ACTIVE <- as.factor(popf$HYPERTENSION_DURATION_DEG_ACTIVE)
popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- as.factor(popf$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE)
popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE <- as.factor(popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE)
popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE <- as.factor(popf$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE)

### Make path missing flags
popf$CREATINE_TEST_RESULT_ACTIVE <- "N"
popf$CREATINE_TEST_RESULT_ACTIVE[popf$CREATINE_TEST_RESULT != -1] <- "Y"
popf$CREATINE_TEST_RESULT_ACTIVE <- as.factor(popf$CREATINE_TEST_RESULT_ACTIVE)

popf$GFR_TEST_RESULT_ACTIVE <- "N"
popf$GFR_TEST_RESULT_ACTIVE[popf$GFR_TEST_RESULT != -1] <- "Y"
popf$GFR_TEST_RESULT_ACTIVE <- as.factor(popf$GFR_TEST_RESULT_ACTIVE)

popf$UREA_TEST_RESULT_ACTIVE <- "N"
popf$UREA_TEST_RESULT_ACTIVE[popf$UREA_TEST_RESULT != -1] <- "Y"
popf$UREA_TEST_RESULT_ACTIVE <- as.factor(popf$UREA_TEST_RESULT_ACTIVE)

popf$POTASSIUM_TEST_RESULT_ACTIVE <- "N"
popf$POTASSIUM_TEST_RESULT_ACTIVE[popf$POTASSIUM_TEST_RESULT != -1] <- "Y"
popf$POTASSIUM_TEST_RESULT_ACTIVE <- as.factor(popf$POTASSIUM_TEST_RESULT_ACTIVE)

popf$ALBUMIN_TEST_RESULT_ACTIVE <- "N"
popf$ALBUMIN_TEST_RESULT_ACTIVE[popf$ALBUMIN_TEST_RESULT != -1] <- "Y"
popf$ALBUMIN_TEST_RESULT_ACTIVE <- as.factor(popf$ALBUMIN_TEST_RESULT_ACTIVE)

popf$PROTIEN_TEST_RESULT_ACTIVE <- "N"
popf$PROTIEN_TEST_RESULT_ACTIVE[popf$PROTIEN_TEST_RESULT != -1] <- "Y"
popf$PROTIEN_TEST_RESULT_ACTIVE <- as.factor(popf$PROTIEN_TEST_RESULT_ACTIVE)

popf$CHOLESTROL_TEST_RESULT_ACTIVE <- "N"
popf$CHOLESTROL_TEST_RESULT_ACTIVE[popf$CHOLESTROL_TEST_RESULT != -1] <- "Y"
popf$CHOLESTROL_TEST_RESULT_ACTIVE <- as.factor(popf$CHOLESTROL_TEST_RESULT_ACTIVE)

popf$LDL_CHOLESTROL_TEST_RESULT_ACTIVE <- "N"
popf$LDL_CHOLESTROL_TEST_RESULT_ACTIVE[popf$LDL_CHOLESTROL_TEST_RESULT != -1] <- "Y"
popf$LDL_CHOLESTROL_TEST_RESULT_ACTIVE <- as.factor(popf$LDL_CHOLESTROL_TEST_RESULT_ACTIVE)

popf$NON_LDL_CHOLESTROL_TEST_RESULT_ACTIVE <- "N"
popf$NON_LDL_CHOLESTROL_TEST_RESULT_ACTIVE[popf$NON_LDL_CHOLESTROL_TEST_RESULT != -1] <- "Y"
popf$NON_LDL_CHOLESTROL_TEST_RESULT_ACTIVE <- as.factor(popf$NON_LDL_CHOLESTROL_TEST_RESULT_ACTIVE)

popf$HBA1C_TEST_RESULT_ACTIVE <- "N"
popf$HBA1C_TEST_RESULT_ACTIVE[popf$HBA1C_TEST_RESULT != -1] <- "Y"
popf$HBA1C_TEST_RESULT_ACTIVE <- as.factor(popf$HBA1C_TEST_RESULT_ACTIVE)

### Make factors
popf$CHRONIC_INDICATOR_KEY <- as.factor(popf$CHRONIC_INDICATOR_KEY)
popf$SMOKING_STATUS <- as.factor(popf$SMOKING_STATUS)
popf$CARDIAC_HISTORY <- as.factor(popf$CARDIAC_HISTORY)
popf$ENDOCRINE_HISTORY <- as.factor(popf$ENDOCRINE_HISTORY)
popf$MENTAL_HISTORY <- as.factor(popf$MENTAL_HISTORY)
popf$MUSCULOSKELETAL_HISTORY <- as.factor(popf$MUSCULOSKELETAL_HISTORY)
popf$RENAL_HISTORY <- as.factor(popf$RENAL_HISTORY)
popf$RESPIRATORY_HISTORY <- as.factor(popf$RESPIRATORY_HISTORY)
popf$year <- as.factor(popf$year)
popf$OH_BENEFIT_LEVEL <- as.factor(popf$OH_BENEFIT_LEVEL)

```

* ACG key lookup
* blanks
* fix up years and exact timing of starts, when joined, when diabetes started, etc

```{r data_vis, eval = FALSE}
tp <- tablePrepare(popf)
tableplot(dat = tp, select = c(AGE, CHRONIC_INDICATOR_KEY, GENDER), scales = "lin", numMode = "mb")

tableplot(dat = tp, select = c(HYPERTENSION_DURATION_DEG_ACTIVE, HYPERTENSION_DURATION, HYPERTENSION_DRUGS), scales = "lin", numMode = "mb", sortCol = HYPERTENSION_DURATION)
          
tableplot(dat = tp, select = c(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE, CHRONIC_RENAL_DISEASE_DURATION), scales = "lin", numMode = "mb", sortCol = CHRONIC_RENAL_DISEASE_DURATION)

tableplot(dat = tp, select = c(CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE, CHRONIC_RENAL_DISEASE_END_STAGE_DURATION), scales = "lin", numMode = "mb", sortCol = CHRONIC_RENAL_DISEASE_END_STAGE_DURATION)

tableplot(dat = tp, select = c(HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, HYPERLIPIDAEMIA_DURATION, HYPERLIPIDAEMIA_DRUGS), scales = "lin", numMode = "mb", sortCol = HYPERLIPIDAEMIA_DURATION)

tableplot(dat = tp, select = c(TOTAL_PAID_HCC, AMT_CLAIMED, PHARMACY_PAID_HCC), scales = "lin", numMode = "mb", sortCol = AMT_CLAIMED)

tableplot(dat = tp, select = c(GP_VISITS, SPECIALIST_VISITS, EMERGENCY_VISITS, EVENT_COUNT), scales = "lin", numMode = "mb", sortCol = GP_VISITS)

tableplot(dat = tp, select = c(CREATINE_TEST_RESULT, GFR_TEST_RESULT, UREA_TEST_RESULT), scales = "lin", numMode = "mb", sortCol = CREATINE_TEST_RESULT)

tableplot(dat = tp, select = c(POTASSIUM_TEST_RESULT, PROTIEN_TEST_RESULT, ALBUMIN_TEST_RESULT), scales = "lin", numMode = "mb", sortCol = POTASSIUM_TEST_RESULT)

tableplot(dat = tp, select = c(CHOLESTROL_TEST_RESULT, LDL_CHOLESTROL_TEST_RESULT, CHOLESTEROL), scales = "lin", numMode = "mb", sortCol = LDL_CHOLESTROL_TEST_RESULT)

tableplot(dat = tp, select = c(HBA1C_TEST_RESULT, diabt0, diabt1), scales = "lin", numMode = "mb", sortCol = HBA1C_TEST_RESULT)

tableplot(dat = tp, select = c(VEM_SCORE, BMI, BP_SYSTOLIC, BP_DIASTOLIC), scales = "lin", numMode = "mb", sortCol = VEM_SCORE)

tableplot(dat = tp, select = c(SMOKING_STATUS, EDUCATION_LEVEL), scales = "lin", numMode = "mb", sortCol = EDUCATION_LEVEL)

tableplot(dat = tp, select = c(CARDIAC_HISTORY, ENDOCRINE_HISTORY, RENAL_HISTORY), scales = "lin", numMode = "mb", sortCol = ENDOCRINE_HISTORY)

tableplot(dat = tp, select = c(MENTAL_HISTORY, NEURO_HISTORY, MUSCULOSKELETAL_HISTORY, RESPIRATORY_HISTORY), scales = "lin", numMode = "mb", sortCol = MENTAL_HISTORY)

tableplot(dat = tp, select = c(year, OH_BENEFIT_LEVEL), scales = "lin", numMode = "mb", sortCol = year)
```

```{r extract_sets}
### Remove existing diabetics

# popfexis <- subset(popf, diabt0 == "Y")

popf <- subset(popf, diabt0 == "N")

### Change scale on numerics
popf$HYPERTENSION_DURATION <- popf$HYPERTENSION_DURATION / 365.25
popf$HYPERLIPIDAEMIA_DURATION <- popf$HYPERLIPIDAEMIA_DURATION / 365.25
# popf$CHRONIC_RENAL_DISEASE_DURATION <- popf$CHRONIC_RENAL_DISEASE_DURATION / 365.25
# popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION <- popf$CHRONIC_RENAL_DISEASE_END_STAGE_DURATION / 365.25
popf$TOTAL_PAID_HCC <- popf$TOTAL_PAID_HCC / 1000
popf$AMT_CLAIMED <- popf$AMT_CLAIMED / 1000
popf$HYPERTENSION_DRUGS <- popf$HYPERTENSION_DRUGS / 1000
popf$HYPERLIPIDAEMIA_DRUGS <- popf$HYPERLIPIDAEMIA_DRUGS / 1000
popf$VEM_SCORE <- popf$VEM_SCORE / 1000

### For 2016 clinical review in the wild

# popf16 <- popf[popf$year == 2016, ]

popf$year <- as.numeric(as.character(popf$year))
popf <- subset(popf, year < 2016)
```


```{r extract_training_and_test_sets}
# (Reduce data set size to be able to run the model) rather do this in original sample
# scal <- 1

### For training and test sets
# popf$ENTITY_NO <- NULL

### Make set flag
# train_index <- createDataPartition(popf$diabt1, p = 0.6, list = FALSE, times = 1)
# randy <- sample(1:dim(train_index)[1], round(dim(train_index)[1] * scal, digits = 0))
# train_index <- train_index[randy]

train_index <- createDataPartition(popf$diabt1, p = 0.7, list = FALSE)
popf$set[train_index] <- "Training"
popf$set[-train_index] <- "Testing"

# popf$set <- NA
# rando <- sample(1:dim(popf)[1], round(dim(popf)[1] * scal, digits = 0))
# trrando <- sample(1:length(rando), round(length(rando) * trshare, digits = 0))
# terando <- rando[-trrando]
# 
# popf$set[trrando] <- "Training"
# popf$set[terando] <- "Testing"

popftr <- subset(popf, set == "Training") %>%
  select(diabt1, CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE, AGE, HYPERTENSION_DURATION_DEG_ACTIVE, AMT_CLAIMED, HYPERTENSION_DRUGS, HYPERTENSION_DURATION, TOTAL_PAID_HCC, CHRONIC_INDICATOR_KEY, HYPERLIPIDAEMIA_DURATION, GP_VISITS, VEM_SCORE, HYPERLIPIDAEMIA_DRUGS, HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, BMI)

popfte <- subset(popf, set == "Testing") %>%
  select(diabt1, CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE, AGE, HYPERTENSION_DURATION_DEG_ACTIVE, AMT_CLAIMED, HYPERTENSION_DRUGS, HYPERTENSION_DURATION, TOTAL_PAID_HCC, CHRONIC_INDICATOR_KEY, HYPERLIPIDAEMIA_DURATION, GP_VISITS, VEM_SCORE, HYPERLIPIDAEMIA_DRUGS, HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, BMI)
```


```{r pre-process, eval = FALSE}
### Fine to remove these only on the training set. As running the trained model on the test set will only pick up the variables called by the model

### NZV - takes a long time... and need to keep these in
ptm <- proc.time()
nzv <- nearZeroVar(popftr, saveMetrics= TRUE)
proc.time() - ptm

nzvdf <- cbind(as.data.frame(nzv), row.names(nzv)) %>%
  arrange(-freqRatio)
zerovars <- which(nzv$zeroVar == TRUE)

# popftr <- popftr[ , -zerovars]

### High correlations

#Convert data.frame to a matrix with a convenient structure
l <- lapply(popftr, function(X) as.numeric(factor(X, levels=unique(X))))
m <- as.matrix(data.frame(l))

ptm <- proc.time()
descrCor <- cor(m, m)
summary(descrCor[upper.tri(popftr)])
proc.time() - ptm

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.5)
highlyCorDescr
names(popftr)[highlyCorDescr]
# popftr <- popftr[, -highlyCorDescr]

### Linear combos
# comboInfo <- findLinearCombos(popftr)
# comboInfo

```


```{r feature_selection, eval = FALSE}
### Chi-squared of outcome against all inputs
#Calculate the chi square statistics - works best when >5 in each cell
weights <- chi.squared(diabt1 ~ ., popftr) 
weights <- cbind(weights, rownames(weights)) %>%
  arrange(-attr_importance)
weights
# Somewhat different to RF

### Or just rely on the RF
```



# Random Forest

```{r rf}
### Note - change the sample size depending on size of the class you're trying to predict. Gives better results! 

# RF run challenges -- options:
# - H2O
# - bigrf
# - data.table
# - sub RFs then combine
# - gc to clean memory

# run specs 
# training  ntree nodesize  fit_rf size Metric  Strat                   Run_time    OOB error Conf matrix
# 80%       500   1         32Gb    
# 1%        100   100       <1Gb?     
# 10%       200   100       200Mb
# 20%       300   50        400Mb                                                   17.6%
# 30%       300   25        620Mb                                                   15.7%
# 30%       300   200       444Mb       Kappa   strata, ss(125k, 12.5k) 500         0.63%     better

# Full data set
# 10%       100   500       156Mb       Kappa   none                     45min       0.51%                incl diab indics
# 15%       200   50                    K       9.8k, 9.8k                3min                            excl diab indics

# keep tree?
# nodeside default?
# boruta

#             
# set samp size (manually with number!) as function of prevalence of target variable in the training set
summary(popftr$diabt1)[2]

threshold <- 0.95

### Memory management before fitting model
#rm(list = c("NArows", "NAs", "plancodz", "popf", "popf.sample", "NAcols", "popf.colclass", "rando", "rando2", #"popf.sample", "should_reg"))
mem_used()
gc()

#### Check
# http://www.listendata.com/2014/11/random-forest-with-r.html

ptm <- proc.time()
fit_rf <- randomForest(popftr$diabt1 ~ .,
                      data = popftr,
                      metric = "Kappa", # useful for imbalanced classes
                      method = "class", 
                      do.trace = 20,
                      ntree = 100,
#                     replace = TRUE,
                      strata = popftr$diabt1,
                      sampsize = c(3000, 420), # leads to more chance of overfit but useful for imbalanced classes
                      norm.votes = TRUE,
                      nodesize = 25,
#                     xtest = training_input_c,
#                     ytest = training_output,
                      keep.forest = TRUE,
                      cutoff = c(1 - threshold, threshold) # class 0 then 1 format
#                     mtry = 9#, # usually it's ~ root p
#                     classwt = c(0.01, 0.99) # not working yet in package
)
proc.time() - ptm

print(fit_rf)
plot(fit_rf)
rf_var_imp <- importance(fit_rf)
rf_var_imp <- rf_var_imp[order(-rf_var_imp[,1]),]
head(rf_var_imp, 15)
varImpPlot(fit_rf, cex = 1, col = "black", pch = 19, n.var = 15,
           main = "Random Forest Variable Importance")
```


# Model checks

```{r predict_line_test, eval = FALSE}
### TEST SET

### Single line test
# Possible line inputs
# line_test_input <- test_input[1:10, ]
# line_test_input <- data.frame(TOTAL_PAID_HCC = 0, AMT_CLAIMED = 0, PHARMACY_PAID_HCC = 0,
#                              HYPERTENSION_DRUGS = 0, HIV_DRUGS = 0, HYPERLIPIDAEMIA_DRUGS = 0,
#                              DIABETES_DRUGS = 0, GP_VISITS = 0, SPECIALIST_VISITS = 0,
#                              EMERGENCY_VISITS = 0, EVENT_COUNT = 0)
# line_test_input <- data.frame(TOTAL_PAID_HCC = mean(test_input$TOTAL_PAID_HCC), 
#                               AMT_CLAIMED = mean(test_input$AMT_CLAIMED), 
#                               PHARMACY_PAID_HCC = mean(test_input$PHARMACY_PAID_HCC), 
#                               HYPERTENSION_DRUGS = mean(test_input$HYPERTENSION_DRUGS), 
#                               HIV_DRUGS = mean(test_input$HIV_DRUGS), 
#                               HYPERLIPIDAEMIA_DRUGS = mean(test_input$HYPERLIPIDAEMIA_DRUGS), 
#                               DIABETES_DRUGS = mean(test_input$DIABETES_DRUGS), 
#                               GP_VISITS = mean(test_input$GP_VISITS), 
#                               SPECIALIST_VISITS = mean(test_input$SPECIALIST_VISITS), 
#                               EMERGENCY_VISITS = mean(test_input$EMERGENCY_VISITS), 
#                               EVENT_COUNT = mean(test_input$EVENT_COUNT))
# 
# line_test_input <- test_input[1, ]
# line_test_input$AGE = 40
# line_test_input$CHRONIC_INDICATOR_KEY = "N"
# line_test_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = -1
# line_test_input$HYPERTENSION_DURATION_DEG_ACTIVE = -1
# line_test_input$HYPERTENSION_DURATION = -1
# line_test_input$HYPERTENSION_DRUGS = -1
# line_test_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = -1
# line_test_input$HYPERLIPIDAEMIA_DURATION = -1
# line_test_input$HYPERLIPIDAEMIA_DRUGS = -1
# line_test_input$BMI = "Unknown"
# line_test_input$VEM_SCORE = -1
# line_test_input$GP_VISITS = -1
# line_test_input$AMT_CLAIMED = -1
# line_test_input$TOTAL_PAID_HCC = -1

line_test_input <- data.frame(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = -1,
      AGE = 40, 
      HYPERTENSION_DURATION_DEG_ACTIVE = -1, 
      AMT_CLAIMED = 0 * 1000, 
      HYPERTENSION_DRUGS = 0 * 1000,
      HYPERTENSION_DURATION = max(-1, -1 * 365.25), 
      TOTAL_PAID_HCC = 0 * 1000,
      CHRONIC_INDICATOR_KEY = "N",
      HYPERLIPIDAEMIA_DURATION = max(-1, -1 * 365.25),                  
      GP_VISITS = 0, 
      VEM_SCORE = 0 * 1000, 
      HYPERLIPIDAEMIA_DRUGS = 0 * 1000,
      HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = -1,
      BMI = "Unknown")

line_test_input$CHRONIC_INDICATOR_KEY <- factor(line_test_input$CHRONIC_INDICATOR_KEY, levels = c("N", "Y"))
line_test_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- factor(line_test_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE,
                                                                    levels = c("-1", "1"))
line_test_input$HYPERTENSION_DURATION_DEG_ACTIVE <- factor(line_test_input$HYPERTENSION_DURATION_DEG_ACTIVE,
                                                                    levels = c("-1", "1"))
line_test_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE <- factor(line_test_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE,
                                                                    levels = c("-1", "1"))
line_test_input$BMI <- factor(line_test_input$BMI, levels = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"))

# line_test_input <- data.frame(AGE = 40,  
#                               CHRONIC_INDICATOR_KEY = 1, 
#                               CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = 1,
#                               HYPERTENSION_DURATION_DEG_ACTIVE = 1,
#                               HYPERTENSION_DURATION = 1, 
#                               HYPERTENSION_DRUGS = 1,
#                               HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = 1,
#                               HYPERLIPIDAEMIA_DURATION = 1, 
#                               HYPERLIPIDAEMIA_DRUGS = 1,
#                               BMI = 7, 
#                               VEM_SCORE = 1, 
#                               GP_VISITS = 1, 
#                               AMT_CLAIMED = 1, 
#                               TOTAL_PAID_HCC = 1)
# 
# line_test_input <- data.frame(AGE = 40,  
#                               CHRONIC_INDICATOR_KEY = factor("N", levels = c("N", "Y"), 
#                               CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = as.factor(-1, levels = c(-1, 1), labels = c(-1, 1)),
#                               HYPERTENSION_DURATION_DEG_ACTIVE = as.factor(-1, levels = c(-1, 1), labels = c(-1, 1)),
#                               HYPERTENSION_DURATION = -1, 
#                               HYPERTENSION_DRUGS = -1,
#                               HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = as.factor(-1, levels = c(-1, 1), labels = c(-1, 1)),
#                               HYPERLIPIDAEMIA_DURATION = -1, 
#                               HYPERLIPIDAEMIA_DRUGS = -1,
#                               BMI = as.factor("Unknown", levels = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"), 
#                                               labels = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown")), 
#                               VEM_SCORE = -1, 
#                               GP_VISITS = -1, 
#                               AMT_CLAIMED = -1, 
#                               TOTAL_PAID_HCC = -1)

line_test_pred <- predict(fit_rf, line_test_input, type = "prob", predict.all = TRUE)
# line_test_pred <- predict(fit_rf, line_test_input, type = "response")
line_test_pred

dim(line_test_pred)
preds <- as.matrix(line_test_pred$aggregate, ncol = 2)
class(preds) <- "numeric"

### Get a confidence interval for the line input - note assumption of normality of underlying distributions
all_trees <- line_test_pred$individual
all_trees <- as.matrix(line_test_pred$individual, nrow = dim(line_test_pred$aggregate)[1])
class(all_trees) <- "numeric"
apply(all_trees, 1, mean)
sds <- apply(all_trees, 1, sd)
preds_ci <- as.data.frame(cbind(preds[, 2], pmax(0, preds[, 2] - 2*sds), pmin(1, preds[, 2] + 2*sds)))
names(preds_ci) <- c("mean", "lower", "upper")
    #paste(percent(round(preds_ci$lower, digits = 2)), percent(round(preds_ci$mean, digits = 2)), percent(round(preds_ci$upper, digits = 2)),sep = " - ")
```



# Model tests

```{r predict_whole_test_set}
### Predict on the whole test set
fitted_classes <- predict(fit_rf, popfte, type = "response")
confusion_test <- confusionMatrix(fitted_classes, popfte$diabt1, positive = "Y")
confusion_test
# Positive predictive value
confusion_test$byClass[3]

conf_perc_test <- round(confusion_test$table/sum(confusion_test$table),4)*100
conf_perc_test

# mosaic(confusion_test$table,
#        #       gp = gpar(fill = fill_colors, col = 0),
#        #       direction = "v", 
#        shade = TRUE,
#        color = TRUE,
#        legend = FALSE,
#        main = "Diabetes test set",
#        pop = FALSE)
# labeling_cells(text = conf_perc_test)(confusion_test$table)


## Y/N Predictions from test set
# fitted_classes_t <- predict(fit_rf, test_input_c, type = "response")
# comp_t <- as.data.frame(cbind(fitted_classes, test_output))


# Probability curve plot from test set
prob_t = as.data.frame(predict(fit_rf, popfte, type = "prob"))

names(prob_t) <- c("Probnot", "Prob")
prob_t$Entity <- row.names(prob_t)
avp <- as.data.frame(cbind(as.data.frame(popfte$diabt1), round(prob_t$Prob, digits = 2), prob_t$Entity))
names(avp) <- c("Actual", "Probability", "Entity")
avp <- avp[order(avp$Probability), ]
avp$Actual <- as.numeric(avp$Actual)-1
avp$Entity <- as.factor(row.names(avp))
avp$Entity <- factor(avp$Entity, levels = avp$Entity[order(avp$Probability, avp$Actual, decreasing = FALSE)])
rand_avp <- sample(1:dim(avp)[1], round(dim(avp)[1]*0.2, digits = 0))
samp_avp <- avp[rand_avp, ]
samp_avp <- samp_avp %>%
   gather(key, value, Probability, Actual)

ggplot(samp_avp, aes(x = Entity, y = value, colour = key)) +
  geom_point(data = subset(samp_avp, samp_avp$key == "Actual"), aes(x = Entity, y = value), color = "blue", alpha = 0.03, size = 0.5, 
             position = position_jitter(height = 0.01, width = 0.5)) +
  geom_point(data = subset(samp_avp, samp_avp$key == "Probability"), aes(x = Entity, y = value), color = "red", alpha = 0.04, size = 0.5) +
  labs(x = "Entity", y = "Probability of developing diabetes") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_hline(yintercept = threshold, color = "grey") 

# position = position_jitter(height = 0.02, width = 0.03)

# Violin and scatter plot from test set - good for more balanced classes
# 
# v <- rep(NA, nrow(samp_avp))
#   v <- ifelse(samp_avp$Probability >= threshold & samp_avp$Actual == 1, "TP", v)
#   v <- ifelse(samp_avp$Probability >= threshold & samp_avp$Actual == 0, "FP", v)
#   v <- ifelse(samp_avp$Probability < threshold & samp_avp$Actual == 1, "FN", v)
#   v <- ifelse(samp_avp$Probability < threshold & samp_avp$Actual == 0, "TN", v)
# samp_avp$pred_type <- v
# 
# ggplot(data = samp_avp, aes(x = Actual, y = Probability)) + 
#   geom_violin(fill = rgb(1, 1, 1, alpha = 0.8), color = NA) + 
#   geom_jitter(aes(color = pred_type), alpha = 0.3) +
#   geom_hline(yintercept = threshold, color="red", alpha = 0.6) +
#   scale_color_discrete(name = "type") +
#   labs(title = sprintf("Threshold at %.2f", threshold))

```



```{r AUC_ROC_on_training}
predictions = as.vector(fit_rf$votes[, 2])
pred = prediction(predictions, training_output)

perf_AUC = performance(pred, "auc") #Calculate the AUC value
AUC = perf_AUC@y.values[[1]]
AUC

perf_ROC = performance(pred, "tpr", "fpr") #plot the actual ROC curve

plot(perf_ROC, main = "ROC plot")
text(0.5, 0.5, paste("AUC = ", format(AUC, digits = 5, scientific = FALSE)))
```


```{r AUC_ROC_on_test}
pred = prediction(prob_t[ ,2], labels = as.numeric(as.vector(test_output)))

perf_AUC = performance(pred, "auc") #Calculate the AUC value
AUC = perf_AUC@y.values[[1]]
AUC

# perf_ROC = performance(pred,"tpr","fpr") #plot the actual ROC curve
# plot(perf_ROC, main = "ROC plot")
# text(0.5, 0.5,paste("AUC = ", format(AUC, digits = 5, scientific = FALSE)))

calculate_roc <- function(df, cost_of_fp, cost_of_fn, n = 100) {
  tpr <- function(df, threshold) {
    sum(df$Probability >= threshold & df$Actual == 1) / sum(df$Actual == 1)
  }
  fpr <- function(df, threshold) {
    sum(df$Probability >= threshold & df$Actual == 0) / sum(df$Actual == 0)
  }
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$Probability >= threshold & df$Actual == 0) * cost_of_fp + 
      sum(df$Probability < threshold & df$Actual == 1) * cost_of_fn
  }

  roc <- data.frame(threshold = seq(0,1,length.out = n), tpr = NA, fpr = NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
  }

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v)) / diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold - threshold))
  
  col_ramp <- colorRampPalette(c("green", "orange", "red", "black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99) + 1]
  p_roc <- ggplot(roc, aes(fpr, tpr)) + 
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) +
    geom_point(color = col_by_cost, size = 1, alpha = 0.5) +
    coord_fixed() +
    geom_line(aes(threshold, threshold), color = rgb(0, 0, 1, alpha = 0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept = roc[idx_threshold, "tpr"], alpha = 0.5, linetype = "dashed") +
    geom_vline(xintercept = roc[idx_threshold, "fpr"], alpha = 0.5, linetype = "dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) +
    geom_point(color = col_by_cost, size = 1, alpha = 0.5) +
    labs(title = sprintf("Cost function")) +
    xlab("Threshold") + ylab("Cost") +
    geom_vline(xintercept = threshold, alpha = 0.5, linetype = "dashed") +
    scale_y_continuous(labels = scales::comma_format())
  
# + coord_cartesian(xlim = c(0.8, 1), ylim = c(20000, 30000)
  
  sub_title <- sprintf("Threshold at %.2f - Cost of FP = %d, Cost of FN = %d", threshold, cost_of_fp, cost_of_fn)

  grid.arrange(p_roc, p_cost, ncol = 2, sub = textGrob(sub_title, gp = gpar(cex = 1), just = "bottom"))
}
roc <- calculate_roc(avp, 1, 10)
plot_roc(roc, threshold, 1, 10)
# try with cost of FN four times higher than cost of FP, and zoom in at threshold to show upturn in cost function corresponding to where precision is about 30%
```

```{r predict_on_2016_to_get_top_100_at_risk, eval = FALSE}
# Remove existing diabetics from popf16
# prosp <- popf16 %>%
#   filter(grepl("Diabetes", CONDITION_CONCAT_LESS_COMB_ADJ) == FALSE) %>%
#   filter(grepl("Diabetes", CONDITION_CONCAT_LESS_COMB_ADJ_NEXT) == FALSE & #DIABETES_MELLITUS_TYPES_1_2_DURATION_DEG_ACTIVE <= 0 &  DIABETES_DRUGS <= 0 & DIABETES_MELLITUS_TYPES_1_2_DURATION # <= 0) 
# prosp_set <- popf16[, 1:48]
preds <- predict(fit_rf, popf16[, c(2:46,48)], type = "prob")

preds <- as.data.frame(preds)
names(preds) <- c("pred_not", "pred_diab")
popf16 <- cbind(popf16, preds[2])
names(popf16) <- c(names(popf16)[1:50], "pred_diab")
popf16risk <- popf16 %>%
  arrange(desc(pred_diab)) %>%
  filter(pred_diab >= 0.95)

# write.csv(popf16risk, "popf16risk.csv")
```


```{r precision_recall_curve}
perf_pr <- performance(pred, "prec", "rec")
plot(perf_pr)

precs <- as.vector(perf_pr@y.values[[1]])
recs <- as.vector(perf_pr@x.values[[1]])
pr <- merge(as.data.frame(precs), as.data.frame(recs), by = 0)

ggplot(pr, aes(x = recs, y = precs)) + 
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) +
    geom_point(size = 1, alpha = 0.5) +  # color = col_by_cost, 
    coord_fixed() +
#    geom_line(aes(threshold, threshold), color = rgb(0, 0, 1, alpha = 0.5)) +
    labs(title = sprintf("PR curve")) + xlab("Recall/TPR") + ylab("Precision") 
# +
#     geom_hline(yintercept = roc[idx_threshold, "tpr"], alpha = 0.5, linetype = "dashed") +
#     geom_vline(xintercept = roc[idx_threshold, "fpr"], alpha = 0.5, linetype = "dashed")

```

``` {r f_score}
fscore <- 2 * precs * recs / (precs + recs)
plot(fscore)
maxf <- which(fscore == max(fscore, na.rm = TRUE))
precs[maxf]
recs[maxf]

```
















# Logistic regression

Overall, logistic regression fits this problem poorly. The target variable is very rare so the model tends to underweight the probability of recognising diabetes, creating effectively a dumb classifier that nearly always predicts the negative case.The effects of the predictor variables are highly interactive, and not individually linear, so it is difficult to improve on this situation. Random Forest gives a better fit.

The logistic regression results have some use though in detecting the directional effect of the different variables, in a way the random forest cannot. 


```{r model_control}

# Probably need to include interaction terms...
# try biglm

ptm <- proc.time()
fit_logit <- glm(popftr$diabt1 ~ CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE + AGE + HYPERTENSION_DURATION_DEG_ACTIVE + HYPERTENSION_DURATION + HYPERTENSION_DRUGS + AMT_CLAIMED + TOTAL_PAID_HCC + HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE + HYPERLIPIDAEMIA_DURATION + HYPERLIPIDAEMIA_DRUGS + GP_VISITS + CHRONIC_INDICATOR_KEY * VEM_SCORE + BMI,
                data = popftr,
                family = binomial(link = "logit"),
                y = FALSE,
                model = FALSE)
proc.time() - ptm

print(fit_logit)
#  takes about 3 mins on full data set. Don't make the plot though
# plot(fit_logit)
```

Look at r2CU = Nagelkerke
Or McFadden (where 0.2 - 0.4 is excellent fit)

```{r rsqd}
pander(pR2(fit_logit))
```

```{r coefs}
# fit_logit_vi <- as.data.frame(varImp(fit_logit))
# fit_logit_vi$term <- rownames(fit_logit_vi)
# names(fit_logit_vi) <- c("importance", "term")
# bmi_vi$term <- factor(bmi_vi$term, levels = bmi_vi$term[order(bmi_vi$importance, decreasing = FALSE)])

coef <- tidy(fit_logit) 

# Scale for interpretability - this might mess with the combination and interpretation of the coefficients
# coef_scale <- coef
# coef_scale$estimate[which(coef_scale$term == "AGE")] <- coef_scale$estimate[which(coef_scale$term == "AGE")] * 5
# coef_scale$term[which(coef_scale$term == "AGE")] <- "AGE_5"
# 
# coef_scale$estimate[which(coef_scale$term == "AMT_CLAIMED")] <- coef_scale$estimate[which(coef_scale$term == "AMT_CLAIMED")] * 1000
# coef_scale$term[which(coef_scale$term == "AMT_CLAIMED")] <- "AMT_CLAIMED_000"
# 
# coef_scale$estimate[which(coef_scale$term == "TOTAL_PAID_HCC")] <- coef_scale$estimate[which(coef_scale$term == "TOTAL_PAID_HCC")] * 1000
# coef_scale$term[which(coef_scale$term == "TOTAL_PAID_HCC")] <- "TOTAL_PAID_HCC_000"
# 
# coef_scale$estimate[which(coef_scale$term == "HYPERTENSION_DRUGS")] <- coef_scale$estimate[which(coef_scale$term == "HYPERTENSION_DRUGS")] * 1000
# coef_scale$term[which(coef_scale$term == "HYPERTENSION_DRUGS")] <- "HYPERTENSION_DRUGS_00"
# 
# coef_scale$estimate[which(coef_scale$term == "HYPERLIPIDAEMIA_DRUGS")] <- coef_scale$estimate[which(coef_scale$term == "HYPERLIPIDAEMIA_DRUGS")] * 1000
# coef_scale$term[which(coef_scale$term == "HYPERLIPIDAEMIA_DRUGS")] <- "HYPERLIPIDAEMIA_DRUGS_00"
# 
# coef_scale$estimate[which(coef_scale$term == "HYPERTENSION_DURATION")] <- coef_scale$estimate[which(coef_scale$term == "HYPERTENSION_DURATION")] * 365
# coef_scale$term[which(coef_scale$term == "HYPERTENSION_DURATION")] <- "HYPERTENSION_DURATION_YR"
# 
# coef_scale$estimate[which(coef_scale$term == "HYPERLIPIDAEMIA_DURATION")] <- coef_scale$estimate[which(coef_scale$term == "HYPERLIPIDAEMIA_DURATION")] * 365
# coef_scale$term[which(coef_scale$term == "HYPERLIPIDAEMIA_DURATION")] <- "HYPERLIPIDAEMIA_DURATION_YR"
# 
# coef_scale$estimate[which(coef_scale$term == "VEM_SCORE")] <- coef_scale$estimate[which(coef_scale$term == "VEM_SCORE")] * 1000
# coef_scale$term[which(coef_scale$term == "VEM_SCORE")] <- "VEM_SCORE_000"
# 
# coef_scale$std.error[which(coef_scale$term == "AGE")] <- coef_scale$std.error[which(coef_scale$term == "AGE")] * 5
# coef_scale$std.error[which(coef_scale$term == "AMT_CLAIMED")] <- coef_scale$std.error[which(coef_scale$term == "AMT_CLAIMED")] * 1000
# coef_scale$std.error[which(coef_scale$term == "TOTAL_PAID_HCC")] <- coef_scale$std.error[which(coef_scale$term == "TOTAL_PAID_HCC")] * 1000
# coef_scale$std.error[which(coef_scale$term == "HYPERTENSION_DRUGS")] <- coef_scale$std.error[which(coef_scale$term == "HYPERTENSION_DRUGS")] * 1000
# coef_scale$std.error[which(coef_scale$term == "HYPERLIPIDAEMIA_DRUGS")] <- coef_scale$std.error[which(coef_scale$term == "HYPERLIPIDAEMIA_DRUGS")] * 1000
# coef_scale$std.error[which(coef_scale$term == "HYPERTENSION_DURATION")] <- coef_scale$std.error[which(coef_scale$term == "HYPERTENSION_DURATION")] * 365
# coef_scale$std.error[which(coef_scale$term == "HYPERLIPIDAEMIA_DURATION")] <- coef_scale$std.error[which(coef_scale$term == "HYPERLIPIDAEMIA_DURATION")] * 365
# coef$std.error[which(coef$term == "VEM_SCORE")] <- coef$std.error[which(coef$term == "VEM_SCORE")] * 1000

coef$cat[coef$term == "AMT_CLAIMED"] <- "Per R1000"
coef$cat[coef$term == "TOTAL_PAID_HCC"] <- "Per R1000"
coef$cat[coef$term == "AGE"] <- "Per additional year"
coef$cat[coef$term == "CHRONIC_INDICATOR_KEYY"] <- "If registered"
coef$cat[coef$term == "GP_VISITS"] <- "Per additional visit"
coef$cat[coef$term == "VEM_SCORE"] <- "Per 1000 points"
coef$cat[which(grepl("BMI", coef$term))] <- "BMI"
coef$cat[which(grepl("DURATION", coef$term))] <- "Per year duration"
coef$cat[which(grepl("ACTIVEY", coef$term))] <- "If with condition"
coef$cat[which(grepl("DRUGS", coef$term))] <- "Per R1000"

intercept = coef$estimate[which(coef$term == "(Intercept)")]
bmi_unk = coef$estimate[which(coef$term == "BMIUnknown")]
# base_age = coef$estimate[which(coef$term == "AGE")]

coef$base_est <- intercept + bmi_unk
#+ base_age * mean(popftr$AGE)

coef$single_factor <- coef$base_est + coef$estimate

# Adjust BMI estimate to be relative to base unknown BMI
coef$estimate[which(grepl("BMI", coef$term))] <- coef$estimate[which(grepl("BMI", coef$term))] - rep(bmi_unk, 6)

coef <- coef[-c(which(coef$term == "(Intercept)"), which(coef$term == "BMIUnknown")), ]

coef$base_prob <- 1 / (1 + exp(-coef$base_est))
coef$sf_prob <- 1 / (1 + exp(-coef$single_factor))

# coef$base_prob <- exp(coef$base_est) / (1 + exp(coef$base_est))
# coef$sf_prob <- exp(coef$single_factor) / (1 + exp(coef$single_factor))

coef$impact <- coef$sf_prob - coef$base_prob

# coef <- coef %>%
#   mutate(prob = exp(single_factor) / (1 + exp(single_factor)))
#   mutate(lower_est = estimate - 2 * std.error, upper_est = estimate + 2 * std.error) %>%
#   mutate(prob = odds / (1 + odds), lowerp = lower / (1 + lower), upperp = upper / (1 + upper)) %>%
#   left_join(fit_logit_vi, by = "term") %>%
#   filter(p.value < 0.05)
# 
# coef$cat <- "Other"
# coef$cat[which(grepl("AGE_GROUPED", coef$term))] <- "Age"

c_high <- coef %>%
  dplyr::select(cat, term, impact, p.value) %>%
  arrange(cat, desc(impact))
c_high$impact <- percent(c_high$impact)
c_high$p_value <- " " 
c_high$p_value[c_high$p.value > 0 & c_high$p.value < 0.001] <- "***" 
c_high$p_value[c_high$p.value > 0.001 & c_high$p.value <0.01] <- "**" 
c_high$p_value[c_high$p.value > 0.01 & c_high$p.value <0.05] <- "*" 
c_high$p_value[c_high$p.value > 0.05 & c_high$p.value < 0.1] <- "." 

c_high$p.value <- NULL

pander(c_high)
```

```{r plot_var_imp, eval = FALSE}
ggplot(coef_scale, aes(x = statistic, y = impact)) +
  geom_point(size = 2) +
#  scale_y_log10() +
  ylab("Probability impact relative to base") +
  xlab("Factor importance (by p-value)") +
  geom_text_repel(aes(x = statistic, y = impact, label = term), size = 3) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.3) +
#    geom_hline(yintercept = base_prob, linetype = 2, alpha = 0.3, colour = "blue") +
  scale_y_continuous(labels = scales::percent_format())
```


```{r effects_model}
mem_used()

names(fit_logit)
vsize <- object.size(fit_logit[1])

vsize <- lapply(fit_logit, object_size)
vsize2 <- as.data.frame(cbind(names(vsize), as.character(vsize)))
# Big ones: residuals, fitted.values, effects, qr, linear.predictors, weights, prior.weights, data

logit2prob <- function(l) {exp(l)/(1+ exp(l))}


eff_age <- effect("AGE", fit_logit)
eff_age2 <- logit2prob(eff_age$fit)
eff_age <- as.data.frame(cbind(eff_age$x, eff_age2))
eff_age$eff_age2 <- percent(eff_age$eff_age2)

eff_renal <- effect("CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE", fit_logit)
eff_renal2 <- logit2prob(eff_renal$fit)
eff_renal <- as.data.frame(cbind(eff_renal$x, eff_renal2))
eff_renal$eff_renal2 <- percent(eff_renal$eff_renal2)

# Focus on modifiable variables
eff_VEM <- effect("VEM_SCORE", fit_logit)
eff_VEM2 <- logit2prob(eff_VEM$fit)
eff_VEM <- as.data.frame(cbind(eff_VEM$x, eff_VEM2))
eff_VEM$eff_VEM2 <- percent(eff_VEM$eff_VEM2)

eff_bmi <- effect("BMI", fit_logit)
eff_bmi2 <- logit2prob(eff_bmi$fit)
eff_bmi <- as.data.frame(cbind(eff_bmi$x, eff_bmi2))
eff_bmi$eff_bmi2 <- percent(eff_bmi$eff_bmi2)

eff_hypl <- effect("HYPERLIPIDAEMIA_DRUGS", fit_logit)
eff_hypl2 <- logit2prob(eff_hypl$fit)
eff_hypl <- as.data.frame(cbind(eff_hypl$x, eff_hypl2))
eff_hypl$eff_hypl2 <- percent(eff_hypl$eff_hypl2)

eff_hypt <- effect("HYPERTENSION_DRUGS", fit_logit)
eff_hypt2 <- logit2prob(eff_hypt$fit)
eff_hypt <- as.data.frame(cbind(eff_hypt$x, eff_hypt2))
eff_hypt$eff_hypt2 <- percent(eff_hypt$eff_hypt2)

# Not enough data for this
# eff_VEM_bmi <- effect("VEM_SCORE:BMI", fit_logit)
# eff_VEM_bmi2 <- logit2prob(eff_VEM_bmi$fit)
# eff_VEM_bmi <- as.data.frame(cbind(eff_VEM_bmi$x, eff_VEM_bmi2))
# eff_VEM_bmi$eff_VEM_bmi2 <- percent(eff_VEM_bmi$eff_VEM_bmi2)
# eff_VEM_bmi <- eff_VEM_bmi %>%
#   spread(VEM_SCORE, eff_VEM_bmi2)

eff_VEM_chron <- effect("CHRONIC_INDICATOR_KEY:VEM_SCORE", fit_logit)
eff_VEM_chron2 <- logit2prob(eff_VEM_chron$fit)
eff_VEM_chron <- as.data.frame(cbind(eff_VEM_chron$x, eff_VEM_chron2))
eff_VEM_chron$eff_VEM_chron2 <- percent(eff_VEM_chron$eff_VEM_chron2)
eff_VEM_chron <- eff_VEM_chron %>%
  spread(VEM_SCORE, eff_VEM_chron2)

# allEffects(fit_logit)
# plot(allEffects(fit_logit))
```

```{r predict_lm_on_test}
fitted_classes <- predict(fit_logit, popfte, type = "response")
#confusion matrix
pred <- as.vector(ifelse(fitted_classes > 0.5, "Y", "N"))

confusion_test <- confusionMatrix(pred, popfte$diabt1, positive = "Y")
confusion_test

# cm <- as.data.frame(cbind(pred, popfte$diabt1))
# names(cm) <- c("Predicted", "Actual")
# cm$Actual <- cm$Actual - 1 
# conf <- as.data.frame(xtabs(~ Actual + Predicted, data = cm))
# 
# TN <- conf$Freq[conf$Actual == 0 & conf$Predicted == 0]
# FN <- conf$Freq[conf$Actual == 1 & conf$Predicted == 0]
# FP <- conf$Freq[conf$Actual == 0 & conf$Predicted == 1]
# TP <- conf$Freq[conf$Actual == 1 & conf$Predicted == 1]
# P <- TP + FN
# N <- TN + FP
# Accuracy <- (TP + TN) / (P + N)
# Sensitivity = TP / P
# Specificity = TN / N
# Precision = TP / (TP + FP)
# F1 = (2 * Sensitivity * Precision) / (Sensitivity + Precision)
# 
# table(popfte$diabt1, fitted_classes > 0.5)
# Precision
# Sensitivity
# F1 

```

























# Caret Rpart

```{r model_control}
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, 
                     classProbs = TRUE, 
                     summaryFunction = defaultSummary)
# Don't need to dummy variables explicitly if training with the formula input
# if preprocess in the train, then it's contained in the prediction model and you can predict directly 

# CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE + AGE + HYPERTENSION_DURATION_DEG_ACTIVE + AMT_CLAIMED + HYPERTENSION_DRUGS + HYPERTENSION_DURATION + TOTAL_PAID_HCC + CHRONIC_INDICATOR_KEY + HYPERLIPIDAEMIA_DURATION + GP_VISITS + VEM_SCORE + HYPERLIPIDAEMIA_DRUGS + HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE + BMI
```


```{r rpart_fit}
ptm <- proc.time()
rpart_fit <- train(diabt1 ~ ., data = popftr, method = "rpart", tuneLength = 10, trControl = ctrl, metric = "Kappa", preProcess = c("center", "scale"))
proc.time() - ptm

## Try trainNWS to harness parallel processing

# 600k x 14 rpart = 21 mins
# 600k x 14 rpart = 7 mins
# 1.7m x 14 rpart = 3 hours
rpart_fit
plot(rpart_fit)
rpart_var_imp <- varImp(rpart_fit)
rpart_var_imp
plot(rpart_var_imp)
```

# Caret RF

```{r rpart_fit}
ptm <- proc.time()
crf_fit <- train(diabt1 ~ ., data = popftr, method = "rf", tuneLength = 10, trControl = ctrl, metric = "Kappa")
proc.time() - ptm

## Try trainNWS to harness parallel processing

# 600k x 14 rpart = 21 mins
# 600k x 14 rpart = 7 mins
# 1.7m x 14 rpart = 3 hours

# rf - 100k x 14 : >2 hours terminated

crf_fit
plot(crf_fit)
crf_var_imp <- varImp(crf_fit)
crf_var_imp
plot(crf_var_imp)
```








```{r lasso_fit}
ptm <- proc.time()
# rpart_fit <- train(diabt1 ~ ., data = popftr, method = "lasso", tuneLength = 10, trControl = ctrl, metric = "Rsquared", preProcess = c("center", "scale"))
lasso_fit <- train(diabt1 ~ ., data = popftr, method = "glmnet", tuneLength = 10, trControl = ctrl, metric = "Kappa", preProcess = c("center", "scale"), family = "binomial")
proc.time() - ptm

# 1.7m x 14 = crash
# 900k x 14 = 1h20

## Try trainNWS to harness parallel processing
# or H2O
lasso_fit
plot(lasso_fit)
lasso_var_imp <- varImp(lasso_fit)
lasso_var_imp
plot(lasso_var_imp)

## Produce table of coefficients for given lambda and alpha 
# (would usually do this for the CV optimised model)
fin_mod <- lasso_fit$finalModel
tune_mod <- fin_mod$tuneValue
coef <- coef(lasso_fit$finalModel, s = as.numeric(tune_mod[2]), alpha = as.numeric(tune_mod[1])) # can change lambda manually to get coefficient output 
coef <- as.matrix(coef)
names(coef) <- "Coef"

## Select non-zero coefficients - glmnet returns coefficients on original scale
Nonzero_coef <- coef[coef != 0,]
Nonzero_coef <- sort(Nonzero_coef, decreasing = TRUE)
Nonzero_coef <- as.data.frame(Nonzero_coef)
names(Nonzero_coef) <- "Coefficient"
Nonzero_coef$Variable <- row.names(Nonzero_coef)
#    write.csv(Nonzero_coef,"Nonzero_coef DA.csv")
Nonzero_coef$Variable <- factor(Nonzero_coef$Variable, levels = Nonzero_coef$Variable[order(Nonzero_coef$Coefficient, decreasing = FALSE)])
ggplot(data = Nonzero_coef, aes(x = Variable, y = Coefficient)) + 
  geom_bar(stat = "identity") + coord_flip() +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.text.y = element_text(size = 7))  

```

```{r check_model_function}
# write.csv(coef, "coef.csv")
line_test_input <- data.frame(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = "N",
      AGE = 41, 
      HYPERTENSION_DURATION_DEG_ACTIVE = "N", 
      AMT_CLAIMED = 7329.97, 
      HYPERTENSION_DRUGS = 0,
      HYPERTENSION_DURATION = -1, 
      TOTAL_PAID_HCC = 300,
      CHRONIC_INDICATOR_KEY = "N",
      HYPERLIPIDAEMIA_DURATION = -1,                  
      GP_VISITS = 2, 
      VEM_SCORE = 13475, 
      HYPERLIPIDAEMIA_DRUGS = 0,
      HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE = "N",
      BMI = "35+")
line_test_input$CHRONIC_INDICATOR_KEY <- factor(line_test_input$CHRONIC_INDICATOR_KEY, levels = c("N", "Y"))
line_test_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- factor(line_test_input$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE,
                                                                    levels = c("N", "Y"))
line_test_input$HYPERTENSION_DURATION_DEG_ACTIVE <- factor(line_test_input$HYPERTENSION_DURATION_DEG_ACTIVE,
                                                                    levels = c("N", "Y"))
line_test_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE <- factor(line_test_input$HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE,
                                                                    levels = c("N", "Y"))
line_test_input$BMI <- factor(line_test_input$BMI, levels = c("15-", "15-19", "20-24", "25-29", "30-34", "35+", "Unknown"))
preddy <- predict(lasso_fit, line_test_input, type = "prob", predict.all = TRUE)

preddy2 <- predict(lasso_fit, popftr[3,], type = "prob", predict.all = TRUE, s = 'lambda.min')
```

```{r testing}
fitted_classes <- predict(lasso_fit, newdata = popfte, type = "raw")
# Don't need to exclude the outcome variable from the test set! 

confusion_test1 <- confusionMatrix(data = fitted_classes, reference = popfte$diabt1, positive = "Y", mode = "sens_spec")
confusion_test1

confusion_test2 <- confusionMatrix(data = fitted_classes, reference = popfte$diabt1, positive = "Y", mode = "prec_recall")
confusion_test2
```


```{r glmnet}




```





```{r glmnet_toy_test}
tr <- popftr[1:5000, 1:3]
# tr$diabt1[2] <- "Y"
# tr$diabt1[12] <- "Y"
# tr$diabt1[17] <- "Y"
# tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[2] <- "Y"
# tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[19] <- "Y"

te <- popftr[5001:7000, 1:3]
# te$diabt1[3] <- "Y"
# te$diabt1[4] <- "Y"
# te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[4] <- "Y"

# toy_lasso <- train(diabt1 ~ ., data = tr, method = "glmnet", tuneLength = 10, trControl = ctrl, metric = "Kappa", family = "binomial")
# toy_lasso <- glm(diabt1 ~ ., data = tr, family = "binomial") # this can get to match

# glmnet
tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- as.character(tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE)
tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == "Y"] <- 1
tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == "N"] <- 0
tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- as.numeric(tr$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE)

te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- as.character(te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE)
te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == "Y"] <- 1
te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE[te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE == "N"] <- 0
te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE <- as.numeric(te$CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE)

toy_lasso <- glmnet(x= as.matrix(tr[, 2:3]), y = tr$diabt1, alpha=1, family="binomial")

# caret
toy_fit_class <- predict(toy_lasso, newdata = te, type = "raw")
# glmnet
predso <- predict(toy_lasso, newx = as.matrix(te), type = "response", s = 0.0001553884)

lti <- data.frame(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE = 0,
      AGE = 67)

# caret
# lti_pred <- predict(toy_lasso, newdata = lti, type = "prob")
fin_mod_toy <- toy_lasso$finalModel
tune_mod_toy <- fin_mod$tuneValue
coef <- coef(toy_lasso$finalModel, s = as.numeric(tune_mod_toy[2]), alpha = as.numeric(tune_mod_toy[1])) # can change lambda manually to get coefficient output 
coef <- as.matrix(coef)

# glmnet
te <- te[, 2:3]
lti_pred <- predict(toy_lasso, newx = as.matrix(lti), type = "response", s = 0.0001553884)
coef(toy_lasso, s = 0.0001553884)

```


```{r best_on_resample}
resamps <- resamples(list(rpart = rpart_fit, lasso = lasso_fit))
summary(resamps)

```




```{r predict_classes}
cfitted_classes <- predict(crf, newdata = popfte, type = "raw")
# Don't need to exclude the outcome variable from the test set! 

confusion_test1 <- confusionMatrix(data = cfitted_classes, reference = popfte$diabt1, positive = "Y", mode = "sens_spec")
confusion_test1

confusion_test2 <- confusionMatrix(data = cfitted_classes, reference = popfte$diabt1, positive = "Y", mode = "prec_recall")
confusion_test2

conf_perc_test <- round(confusion_test$table/sum(confusion_test$table),4)*100
conf_perc_test

```

```{r predict_probs_interp?}
cfitted_probs <- predict(crf, newdata = popfte, type = "prob")

cfitted_probs <- data.frame(obs = popfte$diabt1, N = cfitted_probs[ ,1], Y = cfitted_probs[ ,2], pred = cfitted_classes)
twoClassSummary(cfitted_probs, lev = levels(cfitted_probs$obs))
prSummary(cfitted_probs, lev = levels(cfitted_probs$obs))

# ggplot(cfitted_probs, aes(x = Y)) + 
#   geom_histogram(binwidth = .05) + 
#   facet_wrap(~ obs) + 
#   xlab("Probability of diabetes")
```

# Background

Take health system interactions in a year, and use them to predict chances of diabetes development in the next year (either diabetic program registration, or start of diabetic-related DEG)

Merging the years violates independence assumption - need to allow for this?
Merging years -- ideally need an inflation adjustment to older claims

Drop the outlier values - left all in for now, pending review
Handle missing values - RF handles them as factor levels, or -1 for continuous variables

Recast flat file to avoid proliferating objects
Column reordering


```{r data_vis_at_risk, eval = FALSE}
tableplot(dat = popf16risk, select = c(AGE, CHRONIC_INDICATOR_KEY, GENDER, OH_BENEFIT_LEVEL), scales = "lin", numMode = "mb")

tableplot(dat = popf16risk, select = c(HYPERTENSION_DURATION_DEG_ACTIVE, HYPERTENSION_DURATION, HYPERTENSION_DRUGS), scales = "lin", numMode = "mb", sortCol = HYPERTENSION_DURATION)
          
tableplot(dat = popf16risk, select = c(CHRONIC_RENAL_DISEASE_DURATION_DEG_ACTIVE, CHRONIC_RENAL_DISEASE_DURATION), scales = "lin", numMode = "mb", sortCol = CHRONIC_RENAL_DISEASE_DURATION)

tableplot(dat = popf16risk, select = c(CHRONIC_RENAL_DISEASE_END_STAGE_DURATION_DEG_ACTIVE, CHRONIC_RENAL_DISEASE_END_STAGE_DURATION), scales = "lin", numMode = "mb", sortCol = CHRONIC_RENAL_DISEASE_END_STAGE_DURATION)

tableplot(dat = popf16risk, select = c(HYPERLIPIDAEMIA_DURATION_DEG_ACTIVE, HYPERLIPIDAEMIA_DURATION, HYPERLIPIDAEMIA_DRUGS), scales = "lin", numMode = "mb", sortCol = HYPERLIPIDAEMIA_DURATION)

tableplot(dat = popf16risk, select = c(TOTAL_PAID_HCC, AMT_CLAIMED, PHARMACY_PAID_HCC), scales = "lin", numMode = "mb", sortCol = AMT_CLAIMED)

tableplot(dat = popf16risk, select = c(GP_VISITS, SPECIALIST_VISITS, EMERGENCY_VISITS, EVENT_COUNT), scales = "lin", numMode = "mb", sortCol = GP_VISITS)

tableplot(dat = popf16risk, select = c(CREATINE_TEST_RESULT, GFR_TEST_RESULT, UREA_TEST_RESULT), scales = "lin", numMode = "mb", sortCol = CREATINE_TEST_RESULT)

tableplot(dat = popf16risk, select = c(HBA1C_TEST_RESULT, POTASSIUM_TEST_RESULT, PROTIEN_TEST_RESULT, ALBUMIN_TEST_RESULT), scales = "lin", numMode = "mb", sortCol = HBA1C_TEST_RESULT)

tableplot(dat = popf16risk, select = c(CHOLESTROL_TEST_RESULT, LDL_CHOLESTROL_TEST_RESULT, CHOLESTEROL), scales = "lin", numMode = "mb", sortCol = LDL_CHOLESTROL_TEST_RESULT)

tableplot(dat = popf16risk, select = c(VEM_SCORE, BMI, BP_SYSTOLIC, BP_DIASTOLIC), scales = "lin", numMode = "mb", sortCol = VEM_SCORE)

tableplot(dat = popf16risk, select = c(SMOKING_STATUS, EDUCATION_LEVEL), scales = "lin", numMode = "mb", sortCol = EDUCATION_LEVEL)

tableplot(dat = popf16risk, select = c(CARDIAC_HISTORY, ENDOCRINE_HISTORY, RENAL_HISTORY), scales = "lin", numMode = "mb", sortCol = ENDOCRINE_HISTORY)

tableplot(dat = popf16risk, select = c(MENTAL_HISTORY, NEURO_HISTORY, MUSCULOSKELETAL_HISTORY, RESPIRATORY_HISTORY), scales = "lin", numMode = "mb", sortCol = MENTAL_HISTORY)

```


```{r model_for_export, eval = FALSE}
tr_form <- merge(training_input, as.data.frame(training_output), by = 0)
tr_form <- tr_form[ , -1]

# randy <- sample(1:dim(tr_form)[1], round(0.5 * dim(tr_form)[1], digits = 0))
# tr_form <- tr_form[randy, ]

# Count number of NAs per column
# NAcols <- sapply(tr_form, function(x) sum(is.na(x)))
# NAcols[NAcols > 0]
# # NAs per row
# NAs <- which(is.na(tr_form), arr.ind=TRUE)
# NArows <- tr_form[NAs[,1],]
# NArows
# 
# # Count number of nulls per column
# nullcols <- sapply(tr_form, function(x) sum(is.null(x)))
# nullcols[nullcols > 0]
# # p1$Master_Effective_To <- NULL
# 
# # Incomplete cases
# incomplete <- tr_form[!complete.cases(tr_form),]
# # sum(!complete.cases(p1))
# incomplete

summary(tr_form$training_output)

ptm <- proc.time()
fit_rfx <- randomForest(training_output ~ ., 
                        tr_form,
                        metric = "Kappa", # useful for imbalanced classes
                      method = "class", 
                      do.trace = 20,
                      ntree = 100,
#                     replace = TRUE,
#                      strata = training_output,
#                      sampsize = c(45000, 9000), # leads to more chance of overfit but useful for imbalanced classes
                      norm.votes = TRUE,
                      nodesize = 25,
#                     xtest = training_input_c,
#                     ytest = training_output,
                      keep.forest = TRUE,
                      cutoff = c(1 - threshold, threshold) # class 0 then 1 format
#                     mtry = 9#, # usually it's ~ root p
#                     classwt = c(0.01, 0.99) # not working yet in package
)
proc.time() - ptm

print(fit_rfx)

pmml(fit_rfx)

```
