#0. Preparation ----------------------------------------------------------------
pacman::p_load(tidyverse,
               summarytools)

source("scripts/01_preparation.R")

#1. Table demographics comparative ---------------------------------------------

demo1 <- study1|>
  select(age,age_range,gender,
         educ_level,ppal_educ_level,ppal_ocupation,rm)
demo2 <- study2|>
  select(age,age_range,gender,
         educ_level,ppal_educ_level,ppal_ocupation,rm)

demo1|>
  to_label()|>
dfSummary(plain.ascii  = FALSE, 
          style        = "grid", 
          graph.magnif = 0.75, 
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp",
          method="render",
          labels.col = FALSE)|>
  view()

demo2|>
  to_label()|>
dfSummary(plain.ascii  = FALSE, 
          style        = "grid", 
          graph.magnif = 0.75, 
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp",
          method="render",
          labels.col = FALSE)|>
  view()

#2. Algorithmic Awareness by Demographics --------------------------------------

#Age range
rbind(
  study1 %>%
    group_by(age_range) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(age_range) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(age_range)|>
  ggplot(aes(x=age_range,y=mean_alg_aware,color=Estudio,group=Estudio))+
  geom_point()+
  geom_line()+
  labs(title = "Algorithmic Awareness mean by Age Range",
       x = "Age range",
       y = "Algorithmic Awareness mean")+
  scale_y_continuous(limits = c(1,5))

#Level of education
rbind(
  study1 %>%
    group_by(educ_level) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(educ_level) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(educ_level)|>
  ggplot(aes(x=educ_level,y=mean_alg_aware,color=Estudio,group=Estudio))+
  geom_line()+
  geom_point()+
  labs(title = "Algorithmic Awareness mean by Education level",
       x = "Education level",
       y = "Algorithmic Awareness mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(limits = c(1,5))

#Occupation
rbind(
  study1 %>%
    group_by(ppal_ocupation) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(ppal_ocupation) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(ppal_ocupation)|>
  ggplot(aes(x=ppal_ocupation,y=mean_alg_aware,color=Estudio,group=Estudio))+
  geom_line()+
  geom_point()+
  labs(title = "Algorithmic Awareness mean by Principal Occupation",
       x = "Principal Occupation",
       y = "Algorithmic Awareness mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(limits = c(1,5))

#Region
rbind(
study1 %>%
  to_label(rm) %>%
  group_by(rm) %>%
  summarise(score = mean(alg_aware, na.rm = TRUE)) %>%
  mutate(Estudio="Estudio 1"),

study2 %>%
  to_label(rm) %>%
  group_by(rm) %>%
  summarise(score = mean(alg_aware, na.rm = TRUE)) %>%
  mutate(Estudio="Estudio 2")
)|>
  
  ggplot(aes(x=score,y=Estudio))+
  geom_line(aes(group=Estudio),color="#E7E7E7",linewidth=2.0)+
  geom_point(aes(color=rm),size=4)+
  theme_minimal()+
  labs(x="Mean Algoritmic Awareness score",y="",color="")+
  theme(legend.position = "top")+
  scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  labs(title="Alg. Awareness by Region")+
  scale_x_continuous(limits=c(1,5))

#Gender
rbind(
  study1 %>%
    to_label(gender) %>%
    group_by(gender) %>%
    summarise(score = mean(alg_aware, na.rm = TRUE)) %>%
    mutate(Estudio="Estudio 1"),
  
  study2 %>%
    to_label(gender) %>%
    group_by(gender) %>%
    summarise(score = mean(alg_aware, na.rm = TRUE)) %>%
    mutate(Estudio="Estudio 2")
)|>
  
  ggplot(aes(x=score,y=Estudio))+
  geom_line(aes(group=Estudio),color="#E7E7E7",linewidth=2.0)+
  geom_point(aes(color=gender),size=4)+
  theme_minimal()+
  labs(x="Mean Algoritmic Awareness score",y="",color="")+
  theme(legend.position = "top")+
  scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  labs(title="Alg. Awareness by Gender")+
  scale_x_continuous(limits=c(1,5))

#3. Exposure by Demographics --------------------------------------
#Age range
rbind(
  study1 %>%
    group_by(age_range) %>% na.omit() %>%
    summarise(exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(age_range) %>% na.omit() %>%
    summarise(exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(age_range)|>
  ggplot(aes(x=age_range,y=exposure,color=Estudio,group=Estudio))+
  geom_point()+
  geom_line()+
  labs(title = "Exposure by Age Range",
       x = "Age range",
       y = "Perceive missinformation exposure mean")+
  scale_y_continuous(limits = c(1,5))

#Level of education
rbind(
  study1 %>%
    group_by(educ_level) %>% na.omit() %>%
    summarise(mean_exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(educ_level) %>% na.omit() %>%
    summarise(mean_exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(educ_level)|>
  ggplot(aes(x=educ_level,y=mean_exposure,color=Estudio,group=Estudio))+
  geom_line()+
  geom_point()+
  labs(title = "Exposure by Education level",
       x = "Education level",
       y = "Perceived missinformation exposure mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(limits = c(1,5))

#Ppal Occupation
rbind(
  study1 %>%
    group_by(ppal_ocupation) %>% na.omit() %>%
    summarise(mean_exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(ppal_ocupation) %>% na.omit() %>%
    summarise(mean_exposure = mean(exposure, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  to_label(ppal_ocupation)|>
  ggplot(aes(x=ppal_ocupation,y=mean_exposure,color=Estudio,group=Estudio))+
  geom_line()+
  geom_point()+
  labs(title = "Principal Occupation by Education level",
       x = "Principal Occupation",
       y = "Perceived missinformation exposure mean")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_continuous(limits = c(1,5))

#4. Sharing by Demographics ----------------------------------------------------
