# 0. Preparation ---------------------------------------------------------------

source("scripts/01_preparation.R") #Open proccesed data

pacman::p_load(tidyverse,
               ggplot2, #Plots
               summarytools,
               gtsummary, #Descriptive table
               sjmisc, #labelled manipulation data
               haven,
               forcats,
               Hmisc, #Correlation matrix
               corrplot, #Plot correlation
               scales, #Re-scale axis
               ggmosaic
)

#filter urban cases

study1<-study1|>
  filter(tipo_zona==1)

#2. Descriptive plot -----------------------------------------------------------

#Exposure
rbind(
  study1|>
    count(exposure)|>
    mutate(prop=n/sum(n),
           Estudio="1"),
  
  study2|>
    count(exposure)|>
    mutate(prop=n/sum(n),
           Estudio="2")
)|>
  ggplot(aes(x=exposure,y=prop,fill=Estudio))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Proportion Awareness Missinformation Exposure",
    subtitle= "Misma escala, distintas etiquetas",
    x="Escala numérica"
  )+
  theme(
    axis.title.y = element_blank()
  )

#Sharing
rbind(
  study1|>
    select(sharing)|>to_label()|>
    mutate(Estudio="1"),
  study2|>
    select(sharing)|>to_label()|>
    mutate(Estudio="2")
)|>
  drop_na()|>
  ggplot()+
  geom_mosaic(aes(x = product(sharing, Estudio))) + 
  labs(title="Proportion Awareness Missinformation Sharing",
       subtitle = "Recodificado como dummy")+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )+
  coord_flip()

#Algorithmic awareness Cross table ---------------------------------------------
rbind(
  study1 %>%
    group_by(exposure) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="1"),
  
  study2 %>%
    group_by(exposure) %>% na.omit() %>%
    summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))|>
    mutate(Estudio="2")
)|>
  ggplot(aes(x=exposure,y=mean_alg_aware,color=Estudio))+
  geom_line()+
  geom_point()+
  labs(title = "Algorithmic Awareness mean by Awareness Missinformation Exposure Category",
       x = "Exposure numeric scale",
       y = "Algorithmic Awareness mean")

study1 %>%
  group_by(to_label(sharing)) %>% na.omit() %>%
  summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))

study2 %>%
  group_by(to_label(sharing)) %>% na.omit() %>%
  summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))

study1 %>%
  mutate(
    anti_fake_news=round(anti_fake_news,digits = 0),
    anti_fake_news=case_when(
      anti_fake_news == 1 ~ "Nunca o menos de 1 vez al mes",
      anti_fake_news == 2 ~ "Dos o tres veces al mes",
      anti_fake_news == 3 ~ "Al menos una vez a la semana",
      anti_fake_news == 4 ~ "Todos los días",
      anti_fake_news == 5 ~ "Varias veces al día"
    )
  )%>%
  group_by(to_label(anti_fake_news)) %>% na.omit() %>%
  summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))

study2 %>%
  group_by(to_label(vulnerability)) %>% na.omit() %>%
  summarise(mean_alg_aware = mean(alg_aware, na.rm = TRUE))

#Correlation -------------------------------------------------------------------
data_corr_1 <- study1 |>
  select(exposure,sharing,anti_fake_news,
         content_filt,human_int,ethic,
         alg_aware,alg_lit,
         digital_skills,
         age)|>
  mutate_all(~ ifelse(. %in% c(99, 88), NA, .))|>
  as.matrix()|>
  rcorr()

corrplot(data_corr_1$r, method = 'color', type = 'lower', insig='blank',
         tl.col = "black",bg="white",na.label="-",
         addCoef.col ='black', number.cex = 0.8, diag=FALSE,
         sig.level = 0.05)

#Social media & Alg_aware ------------------------------------------------------
calculate_alg_mean_score <- function(data, group_var, media_name, estudio_id) {
  data %>%
    to_label() %>%
    group_by({{ group_var }}) %>%
    summarise(score = mean(alg_aware, na.rm = TRUE)) %>%
    mutate(media = media_name,
           Estudio = estudio_id) %>%
    rename(user = {{ group_var }})
}

rbind(
  calculate_alg_mean_score(study1,wsp,"Whatsapp","Estudio 1"),
  calculate_alg_mean_score(study1,fb,"Facebook","Estudio 1"),
  calculate_alg_mean_score(study1,ig,"Instagram","Estudio 1"),
  calculate_alg_mean_score(study1,yt,"Youtube","Estudio 1"),
  calculate_alg_mean_score(study1,tw,"Twitter","Estudio 1"),
  calculate_alg_mean_score(study1,tiktok,"TikTok","Estudio 1"),
  
  calculate_alg_mean_score(study2,wsp,"Whatsapp","Estudio 2"),
  calculate_alg_mean_score(study2,fb,"Facebook","Estudio 2"),
  calculate_alg_mean_score(study2,ig,"Instagram","Estudio 2"),
  calculate_alg_mean_score(study2,yt,"Youtube","Estudio 2"),
  calculate_alg_mean_score(study2,tw,"Twitter","Estudio 2"),
  calculate_alg_mean_score(study2,tiktok,"TikTok","Estudio 2")
)|>
  drop_na()|>
  
  ggplot(aes(x=score,y=media))+
  geom_line(aes(group=media),color="#E7E7E7",linewidth=2.0)+
  geom_point(aes(color=user),size=4)+
  theme_minimal()+
  labs(x="Mean Algoritmic Awareness score",y="",color="User",
       caption= "Signif. codes: <0.001 = ***, <0.01 = **, <0.05 = *")+
  theme(legend.position = "top")+
  scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  facet_wrap(~Estudio,ncol=1,scales="free_y")+
  labs(title="Alg. Awareness by social media",
       subtitle = "Estudio 1 (Frequency media use) / Estudio 2 (Media news consuming)")+
  guides(shape=guide_legend(title="User"))
