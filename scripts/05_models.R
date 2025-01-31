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
  VGAM
)

#1. Models ---------------------------------------------------------------------

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

exposure_0a <- lm(exposure ~ 1,
                  data = study1_model)
exposure_1a <- lm(exposure ~ alg_aware,
                  data = study1_model)
exposure_2a <- lm(exposure ~ alg_aware+age+gender+rm,
                  data = study1_model)
exposure_3a <- lm(exposure ~ alg_aware+age+gender+rm+educ_level,
                  data = study1_model)
exposure_4a <- lm(exposure ~ alg_aware+age+gender+rm+educ_level+ppal_ocupation,
                  data = study1_model)

sjPlot::tab_model(list(exposure_0a, exposure_1a, exposure_2a,exposure_3a,exposure_4a), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Modelo Nulo", "Modelo 1", "Modelo 2", "Modelo 3","Modelo 4"),
                  string.pred = "Predictores", 
                  string.est = "β",
                  collapse.se = TRUE)

exposure_0b <- lm(exposure ~ 1,
                  data = study2_model)
exposure_1b <- lm(exposure ~ alg_aware,
                  data = study2_model)
exposure_2b <- lm(exposure ~ alg_aware+age+gender,
                  data = study2_model)
exposure_3b <- lm(exposure ~ alg_aware+age+gender+rm+educ_level,
                  data = study2_model)
exposure_4b <- lm(exposure ~ alg_aware+age+gender+rm+educ_level+ppal_ocupation,
                  data = study2_model)

sjPlot::tab_model(list(exposure_0b,exposure_1b, exposure_2b,exposure_3b,exposure_4b), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Modelo Nulo","Modelo 1", "Modelo 2", "Modelo 3","Modelo 4"),
                  string.pred = "Predictores", 
                  string.est = "β",
                  collapse.se = TRUE)

sharing_0a <- glm(sharing~1,data = study1_model,family = "binomial")
sharing_1a <- glm(sharing~alg_aware,data = study1_model,family = "binomial")
sharing_2a <- glm(sharing~alg_aware+age+gender,
                  data = study1_model,family = "binomial")
sharing_3a <- glm(sharing~alg_aware+age+gender+rm+educ_level,
                  data = study1_model,family = "binomial")
sharing_4a <- glm(sharing~alg_aware+age+gender+rm+educ_level+ppal_ocupation,
                  data = study1_model,family = "binomial")

sjPlot::tab_model(list(sharing_0a, sharing_1a,sharing_2a,sharing_3a,sharing_4a), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Modelo Nulo", "Modelo 1", "Modelo 2", "Modelo 3","Modelo 4"),
                  string.pred = "Predictores", 
                  string.est = "log",
                  collapse.se = TRUE)


sharing_0b <- glm(sharing~1,data = study2_model,family = "binomial")
sharing_1b <- glm(sharing~alg_aware,data = study2_model,family = "binomial")
sharing_2b <- glm(sharing~alg_aware+age+gender,
                  data = study2_model,family = "binomial")
sharing_3b <- glm(sharing~alg_aware+age+gender+rm+educ_level,
                  data = study2_model,family = "binomial")
sharing_4b <- glm(sharing~alg_aware+age+gender+rm+educ_level+ppal_ocupation,
                  data = study2_model,family = "binomial")

sjPlot::tab_model(list(sharing_0b, sharing_1b,sharing_2b,sharing_3b,sharing_4b), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Modelo Nulo", "Modelo 1", "Modelo 2", "Modelo 3","Modelo 4"),
                  string.pred = "Predictores", 
                  string.est = "log",
                  collapse.se = TRUE)

exposure_1_media <- lm(exposure ~ alg_aware+wsp+ig+fb+yt+tw+tiktok+
                         age+gender+educ_level+rm+ppal_ocupation,
                       data = study1_model)
exposure_2_media <- lm(exposure ~ alg_aware+wsp+ig+fb+yt+tw+tiktok+
                         age+gender+educ_level+rm+ppal_ocupation,
                       data = study2_model)

sjPlot::tab_model(list(exposure_1_media,exposure_2_media), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Estudio 1", "Estudio 2"),
                  string.pred = "Predictores", 
                  string.est = "β",
                  collapse.se = TRUE)

sharing_1_media <- glm(sharing~alg_aware+wsp+ig+fb+yt+tw+tiktok+
                         age+gender+educ_level+rm+ppal_ocupation,
                       data = study1_model)
sharing_2_media <- glm(sharing~alg_aware+wsp+ig+fb+yt+tw+tiktok+
                         age+gender+educ_level+rm+ppal_ocupation,
                       data = study2_model)

sjPlot::tab_model(list(sharing_1_media,sharing_2_media), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Estudio 1", "Estudio 2"),
                  string.pred = "Predictores", 
                  string.est = "log",
                  collapse.se = TRUE)


#2. Visualizations -------------------------------------------------------------

plot_grid(
  
study1|>
  ggplot(aes(x = alg_aware, y = exposure))+
  geom_point()+
  geom_smooth(method = "lm", se=T)+
  scale_y_continuous(limits = c(1,5)),

study2|>
  ggplot(aes(x = alg_aware, y = exposure))+
  geom_point()+
  geom_smooth(method = "lm",se = T)+
  scale_y_continuous(limits = c(1,5)),

  ncol = 2, labels = c("Estudio 1","Estudio 2"))


sjPlot::plot_models(exposure_3a, exposure_3b,
            show.p = TRUE,
            show.values = TRUE,
            show.intercept = TRUE) +
  labs(title = "Perceived Misinformation Exposure",
       subtitle = "Linear Regression Model",
       color = "Study") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("Estudio 2", "Estudio 1"))

plot_grid(

study1|>
  ggplot(aes(x = alg_aware, y = sharing))+
  geom_point()+
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial)),

study2|>
  ggplot(aes(x = alg_aware, y = sharing))+
  geom_point()+
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial)),

ncol = 2, labels = c("Estudio 1","Estudio 2"))



sjPlot::plot_models(sharing_3a, sharing_3b,
                    show.p = TRUE,
                    show.values = TRUE,
                    show.intercept = TRUE) +
  labs(title = "Perceived Misinformation Sharing",
       subtitle = "Logistic Regression Model",
       color = "Study") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("Estudio 2", "Estudio 1"))


#3. Interaction education and algorithmic awareness ----------------------------

exposure_5a <- lm(exposure ~ alg_aware+age+gender+rm+educ_level+
                    educ_level*alg_aware,
                  data = study1_model)
exposure_5b <- lm(exposure ~ alg_aware+age+gender+rm+educ_level+
                    educ_level*alg_aware,
                  data = study2_model)

tab_model(exposure_5a,exposure_5b, 
          show.ci=FALSE, 
          p.style = "stars", 
          dv.labels = c("Estudio 1", "Estudio 2"),
          string.pred = "Predictores", 
          string.est = "β",
          collapse.se = TRUE)

plot_model(exposure_5a, type = "pred", terms = c("alg_aware", "educ_level"))+
  labs(y="Exposure",
       title="Study 1",
       subtitle="Moderation effect education in alg aware (Exposure)")

plot_model(exposure_5b, type = "pred", terms = c("alg_aware", "educ_level"))+
  labs(y="Exposure",
       title="Study 2",
       subtitle="Moderation effect education in alg aware (Exposure)")

sharing_5a <- glm(sharing ~ alg_aware+age+gender+rm+educ_level+
                    educ_level*alg_aware,
                  data=study1_model)
sharing_5b <- glm(sharing ~ alg_aware+age+gender+rm+educ_level+
                    educ_level*alg_aware,
                  data=study2_model)
 
tab_model(sharing_5a,sharing_5b,
          show.ci=FALSE, 
          p.style = "stars", 
          dv.labels = c("Estudio 1", "Estudio 2"),
          string.pred = "Predictores", 
          string.est = "log",
          collapse.se = TRUE)

plot_model(sharing_5a, type = "pred", terms = c("alg_aware", "educ_level"))+
  labs(y="Sharing",
       title="Study 1",
       subtitle="Moderation effect education in alg aware (Sharing)")

plot_model(sharing_5b, type = "pred", terms = c("alg_aware", "educ_level"))+
  labs(y="Sharing",
       title="Study 2",
       subtitle="Moderation effect education in alg aware (Sharing)")

#4. Objective missinformation models -------------------------------------------
study2_model <- study2_model|>
  mutate(info_type=to_label(info_type))|>
  mutate(info_type=relevel(info_type,"Informed")) #Informed as reference category

info_type_model<-multinom(info_type ~ alg_aware+age+gender+rm+educ_level,
                      data=study2_model)

tab_model(info_type_model,
          show.ci = FALSE,
          p.style = "stars",
          collapse.se = TRUE)

stargazer(info_type_model,type = "text",out="test.html")

strategies<- lm(exposure ~ alg_aware+counteract_algorithm+age+gender+rm+educ_level,
                data=study2_model)
obj_know_model<-lm(obj_know ~ alg_aware+age+gender+rm+educ_level,
                          data=study2_model)
obj_know3_model<-lm(obj_know3 ~ alg_aware+age+gender+rm+educ_level,
                         data=study2_model)

obj_know5_model<-lm(obj_know3 ~ alg_aware+counteract_algorithm+age+gender+rm+educ_level,
                    data=study2_model)

tab_model(exposure_3b,strategies,obj_know_model,obj_know3_model,obj_know5_model,
          show.ci = FALSE,
          p.style = "stars",
          dv.labels = c("Perceived Missinformation Exposure","+ Counteract Algorithm Strategies", "-6/6 range Objective Missinformation","0/6 range Objective Missinformation",
                        "+ Counteract Algorithm Stategies"),
          collapse.se = TRUE)

sharing7<-glm(sharing ~ alg_aware+counteract_algorithm+age+gender+rm+educ_level,
              data=study2_model)
