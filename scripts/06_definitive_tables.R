#0. Preparing ----------------------------------------------------------------
source("scripts/01_preparation.R")

pacman::p_load(
  tidyverse,
  ggplot2,
  sjPlot,
  cowplot,
  reshape2,
  nnet,
  foreign,
  VGAM,
  labelled
)

study1_model <- study1|>
  mutate_at(.vars = vars(gender,rm),
            .funs = list(~to_label(.)))|>
  mutate_at(.vars = vars(ppal_ocupation,educ_level),
            .funs = list(~to_numeric(.)))

study2_model <- study2|>
  mutate_at(.vars = vars(gender,rm),
            .funs = list(~to_label(.)))|>
  mutate_at(.vars = vars(ppal_ocupation,educ_level),
            .funs = list(~to_numeric(.)))

var_label(study1_model$age) <- "Age"
var_label(study2_model$age) <- "Age"


var_label(study1_model$rm) <- "Metropolitan region"
var_label(study2_model$rm) <- "Metropolitan region"

#1. First Models ---------------------------------------------------------------------


exposure_a <- lm(exposure ~ 
                   alg_aware+
                   age+gender+rm+educ_level+
                   wsp+ig+fb+yt+tw+tiktok,
                  data = study1_model)

exposure_b <- lm(exposure ~ 
                   alg_aware+
                   age+gender+rm+educ_level+
                   wsp+ig+fb+yt+tw+tiktok,
                  data = study2_model)


sjPlot::tab_model(list(exposure_a,exposure_b), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Study 1","Study 2"),
                  pred.labels = c("(Intercept)",
                                  "Alg. Awareness",
                                  "Age",
                                  "Gender (Women = 1)",
                                  "Region (MR = 1)", 
                                  "Educational level",
                                  "Whatsapp weekly use", 
                                  "Instagram weekly use", 
                                  "Facebook weekly use", 
                                  "Youtube weekly use", 
                                  "Twitter weekly use", 
                                  "TikTok weekly use"),
                  string.pred = "Predictors", 
                  string.est = "β",
                  collapse.se = TRUE,
                  title = "Awareness missinformation exposure")

sharing_a <- glm(sharing~
                   alg_aware+
                   age+gender+rm+educ_level+
                   wsp+ig+fb+yt+tw+tiktok,                  
                   data = study1_model,family = "binomial")

sharing_b <- glm(sharing~
                   alg_aware+
                   age+gender+rm+educ_level+
                   wsp+ig+fb+yt+tw+tiktok,                  
                   data = study2_model,family = "binomial")

sjPlot::tab_model(list(sharing_a,sharing_b), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Study 1","Study 2"),
                  pred.labels = c("(Intercept)",
                                  "Alg. Awareness",
                                  "Age",
                                  "Gender (Women = 1)",
                                  "Metropolitan Region", 
                                  "Educational level",
                                  "Whatsapp weekly use", 
                                  "Instagram weekly use", 
                                  "Facebook weekly use", 
                                  "Youtube weekly use", 
                                  "Twitter weekly use", 
                                  "TikTok weekly use"),
                  string.pred = "Predictors", 
                  collapse.se = TRUE,
                  title = "Awareness missinformation sharing",
                  transform = NULL)

# 2. Study 2: Subjective/ Objective knowledge ----------------------------------

exposure_3b <- lm(exposure ~ alg_aware+counteract_algorithm+
                    age+gender+rm+educ_level+
                    wsp+ig+fb+yt+tw+tiktok,
                data=study2_model)

sharing_3b <- glm(sharing ~ alg_aware+counteract_algorithm+
                    age+gender+rm+educ_level+
                    wsp+ig+fb+yt+tw+tiktok,
                  data=study2_model,
                  family = "binomial")

obj_know_model<-lm(obj_know ~ alg_aware+counteract_algorithm+
                     age+gender+rm+educ_level+
                     wsp+ig+fb+yt+tw+tiktok,
                   data=study2_model)

subj_know_model <- lm(subj_know ~ alg_aware+counteract_algorithm+
                    age+gender+rm+educ_level+
                      wsp+ig+fb+yt+tw+tiktok,
                  data=study2_model)

score_model <- lm(subj_know_def ~ alg_aware+counteract_algorithm+
                        age+gender+rm+educ_level+
                    wsp+ig+fb+yt+tw+tiktok,
                      data=study2_model)

tab_model(exposure_3b,sharing_3b,obj_know_model,subj_know_model,score_model,
          show.ci = FALSE,
          p.style = "stars",
          dv.labels = c("Perceived Misinformation Exposure",
                        "Perceived Misinformation Sharing",
                        "Objective news knowledge",
                        "Subjective news knowledge",
                        "Knowledge overconfidence"),
          pred.labels = c("(Intercept)",
                          "Alg. Awareness",
                          "Alg. counteractions",
                          "Age",
                          "Gender (Women = 1)",
                          "Metropolitan Region", 
                          "Educational level",
                          "Whatsapp weekly use", 
                          "Instagram weekly use", 
                          "Facebook weekly use", 
                          "Youtube weekly use", 
                          "Twitter weekly use", 
                          "TikTok weekly use"),
          collapse.se = TRUE,
          title = "Study 2 exploration")
