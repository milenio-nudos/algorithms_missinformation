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

#1. Descriptive table ----------------------------------------------------------

study1|>
  to_label()|>
  tbl_summary(
    missing="ifany",
    type = list(
      starts_with("tech") ~ "continuous2",
      digital_skills ~ "continuous2",
      age ~ "continuous2",
      alg_lit ~ "continuous2",
      alg_aware ~ "continuous2",
      all_continuous() ~ "continuous2"
    ),
    statistic = list(
    all_continuous() ~ c("{mean} ({sd})",
                         "{min} / {max}"),
    all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing Values",
    label = list(
      anti_fake_news ~ "Acciones correctivas de Fake news",
      alg_lit ~ "Alfabetización Algorítmica",
      alg_aware ~ "Consciencia Algorítmica",
      tech_operational_skills ~ "Habilidades operativas",
      tech_informative_skills ~ "Habilidades informativas",
      tech_social_skills ~ "Habilidades sociales",
      tech_creative_skills ~ "Habilidades creativas",
      digital_skills ~ "Habilidades Digitales (índice)"
    )
  )

study2|>
  to_label()|>
  tbl_summary(
    missing="ifany",
    type = list(
      age ~ "continuous2",
      alg_lit ~ "continuous2",
      alg_aware ~ "continuous2",
      counteract_algorithm ~ "continuous2",
      all_continuous() ~ "continuous2"
    ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})",
                           "{min} / {max}"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing_text = "Missing Values",
    label = list(
      alg_lit ~ "Alfabetización Algorítmica",
      alg_aware ~ "Consciencia Algorítmica"
    )
  )

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
  to_label(sharing)|>
  count(sharing)|>
  mutate(prop=n/sum(n),
         Estudio="1"),
study2|>
  to_label(sharing)|>
  count(sharing)|>
  mutate(prop=n/sum(n),
         Estudio="2")
)|>
  drop_na() |>
  ggplot(aes(x=sharing,y=prop,fill=Estudio))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Proportion Awareness Missinformation Sharing",
    subtitle= "Recodificadas como dummy"
  )+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

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
  geom_mosaic(aes(x = product(Estudio, sharing))) + 
  labs(title="Proportion Awareness Missinformation Sharing",
       subtitle = "Recodificado como dummy")+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

#Corrective actions (Only studio 1)
rbind(
  study1|> to_label()|>
    count(Variable=c5_4)|>
    mutate(pregunta="He bloqueado o dejado de seguir a personas porque publican información falsa"),
  study1|> to_label()|>
    count(Variable=c5_5)|>
    mutate(pregunta="Cuando he recibido noticias falsas o engañosas, las he desmentido y aclarado que son falsas"),
  study1|> to_label()|>
    count(Variable=c5_6)|>
    mutate(pregunta="He criticado a alguien por difundir información falsa o engañosa")
)|>drop_na()|>
  ggplot(aes(x=n,y=pregunta,fill=Variable))+
  geom_bar(position="fill", stat="identity")+
  scale_x_continuous(labels = label_percent()) +
  labs(title="Missinformation Corrective Actions items distribution",
       subtitle="Only studio 1")+
  scale_y_discrete(labels = function(x) str_wrap(x, width = 35))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()
  )

#Perceibed vulnerability (Only studio 2)
study2|>
  to_label()|>
  count(vulnerability)|>
  mutate(prop=n/sum(n))|>
  drop_na()|>
  ggplot(aes(x=vulnerability,y=prop))+
  geom_bar(stat = "identity",fill="blue",color="black")+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )+
  coord_flip()+
  scale_y_continuous(labels = label_percent())+
  labs(title="Soy capaz de identificar informaciones incorrectas, engañosas o falsas",
       subtitle="¿Cuán de acuerdo o en desacuerdo está usted con las siguientes afirmaciones?")

#alg_aware_1
rbind(
  study1|>
    mutate(content_filt=ifelse(content_filt==99,NA,content_filt),
           content_filt=as.numeric(content_filt))|>
    count(content_filt)|>
    mutate(prop=n/sum(n),
           Estudio="1"),
  
  study2|>
    mutate(content_filt=ifelse(content_filt==99,NA,content_filt),
           content_filt=as.numeric(content_filt))|>
    count(content_filt)|>
    mutate(prop=n/sum(n),
           Estudio="2")
)|>
  ggplot(aes(x=content_filt,y=prop,fill=Estudio))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "Proportion Content filtration (Alg. Aware item)",
    subtitle= "Los algoritmos se usan para recomendarme contenido en redes sociales y plataformas de streaming",
    x="Escala numérica"
  )+
  theme(
    axis.title.y = element_blank()
  )

#alg_aware_2
rbind(
  study1|>
    mutate(human_int=ifelse(human_int==99,NA,human_int),
           human_int=as.numeric(human_int))|>
    count(human_int)|>
    mutate(prop=n/sum(n),
           Estudio="1"),
  
  study2|>
    mutate(human_int=ifelse(human_int==99,NA,human_int),
           human_int=as.numeric(human_int))|>
    count(human_int)|>
    mutate(prop=n/sum(n),
           Estudio="2")
)|>
  ggplot(aes(x=human_int,y=prop,fill=Estudio))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = label_percent()) +
  theme(axis.title.y = element_blank())+
  labs(
    title="Proportion Human Interpelation",
    subtitle="El contenido que me recomiendan los algoritmos depende del comportamiento que tengo en redes sociales y plataformas de streaming",
    x="Escala numérica"
  )
#alg_aware_3
rbind(
  study1|>
    mutate(ethic=ifelse(ethic==99,NA,ethic),
           ethic=as.numeric(ethic))|>
    count(ethic)|>
    mutate(prop=n/sum(n),
           Estudio="1"),
  
  study2|>
    mutate(ethic=ifelse(ethic==99,NA,ethic),
           ethic=as.numeric(ethic))|>
    count(ethic)|>
    mutate(prop=n/sum(n),
           Estudio="2")
)|>
  ggplot(aes(x=ethic,y=prop,fill=Estudio))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = label_percent()) +
  theme(axis.title.y = element_blank())+
  labs(
    title="Proportion Ethic considerations",
    subtitle="El uso de mis datos personales en los algoritmos tiene consecuencias en mi privacidad en línea",
    x="Escala numérica"
  )


#3. Crossing variables ---------------------------------------------------------

#new_labels_1 <- c("Nunca + Casi nunca" = 1, 
 #               "Nunca + Casi nunca" = 2, 
  #              "A veces" = 3, 
   #             "Casi siempre + Siempre" = 4, 
    #            "Casi siempre + Siempre" = 5)


#val_labels(study1$exposure) <- new_labels_1
#val_labels(study2$exposure) <- new_labels
#val_labels(study1$sharing) <- new_labels_1
#val_labels(study2$sharing) <- new_labels
#val_labels(study1$anti_fake_news) <- new_labels_1
#val_labels(study2$vulnerability) <- new_labels


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
         age,gender)|>
  mutate_all(~ ifelse(. %in% c(99, 88), NA, .))|>
  as.matrix()|>
  rcorr()

corrplot(data_corr_1$r, method = 'color', type = 'lower', insig='blank',
         tl.col = "black",bg="white",na.label="-",
         addCoef.col ='black', number.cex = 0.8, diag=FALSE,
         sig.level = 0.05)

data_corr_2 <- study2 |>
  select(exposure,sharing,vulnerability,
         content_filt,human_int,ethic,
         alg_aware,alg_lit,counteract_algorithm,
         age,gender)|>
  mutate_all(~ ifelse(. %in% c(99, 88), NA, .))|>
  as.matrix()|>
  rcorr()

corrplot(data_corr_2$r, method = 'color', type = 'lower', insig='blank',
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

# Social media & missinformation -----------------------------------------------

#Exposure
calculate_exposure_mean_score <- function(data, group_var, media_name, estudio_id) {
  data %>%
    to_label() %>%
    na.omit({{ group_var }}) %>%
    group_by({{ group_var }}) %>%
    summarise(score = mean(to_numeric(exposure), na.rm = TRUE)) %>%
    mutate(media = media_name,
           Estudio = estudio_id) %>%
    rename(user = {{ group_var }})
}

rbind(
  calculate_exposure_mean_score(study1,wsp,"Whatsapp","Estudio 1"),
  calculate_exposure_mean_score(study1,fb,"Facebook","Estudio 1"),
  calculate_exposure_mean_score(study1,ig,"Instagram","Estudio 1"),
  calculate_exposure_mean_score(study1,yt,"Youtube","Estudio 1"),
  calculate_exposure_mean_score(study1,tw,"Twitter","Estudio 1"),
  calculate_exposure_mean_score(study1,tiktok,"TikTok","Estudio 1"),
  
  calculate_exposure_mean_score(study2,wsp,"Whatsapp","Estudio 2"),
  calculate_exposure_mean_score(study2,fb,"Facebook","Estudio 2"),
  calculate_exposure_mean_score(study2,ig,"Instagram","Estudio 2"),
  calculate_exposure_mean_score(study2,yt,"Youtube","Estudio 2"),
  calculate_exposure_mean_score(study2,tw,"Twitter","Estudio 2"),
  calculate_exposure_mean_score(study2,tiktok,"TikTok","Estudio 2")
)|>
  
  ggplot(aes(x=score,y=media))+
  geom_line(aes(group=media),color="#E7E7E7",linewidth=2.0)+
  geom_point(aes(color=user),size=4)+
  theme_minimal()+
  labs(x="Mean Awareness Missinformation Exposure (Same scale, diff labels)",y="",color="User")+
  theme(legend.position = "top")+
  scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  facet_wrap(~Estudio,ncol=1,scales="free_y")+
  labs(title="Awareness Missinformation Exposure by social media",
       subtitle = "Estudio 1 (Frequency media use) / Estudio 2 (Media news consuming)")+
  guides(shape=guide_legend(title="User"))

# Sharing
count_and_rename <- function(data, column) {
  data %>%
    na.omit({{ column }}) %>%
    group_by({{ column }}) %>%
    to_label() %>%
    summarise(n=sum(to_numeric(sharing),na.rm = T)) %>%
    mutate(prop=n/sum(n))%>%
    rename(variable := {{ column }})
}

bind_rows(
  count_and_rename(study1, wsp) %>% mutate(Estudio = "Estudio 1", media = "Whatsapp"),
  count_and_rename(study1, fb) %>% mutate(Estudio = "Estudio 1", media = "Facebook"),
  count_and_rename(study1, ig) %>% mutate(Estudio = "Estudio 1", media = "Instagram"),
  count_and_rename(study1, yt) %>% mutate(Estudio = "Estudio 1", media = "Youtube"),
  count_and_rename(study1, tw) %>% mutate(Estudio = "Estudio 1", media = "Twitter"),
  count_and_rename(study1, tiktok) %>% mutate(Estudio = "Estudio 1", media = "Tiktok"),
  count_and_rename(study2, wsp) %>% mutate(Estudio = "Estudio 2", media = "Whatsapp"),
  count_and_rename(study2, fb) %>% mutate(Estudio = "Estudio 2", media = "Facebook"),
  count_and_rename(study2, ig) %>% mutate(Estudio = "Estudio 2", media = "Instagram"),
  count_and_rename(study2, yt) %>% mutate(Estudio = "Estudio 2", media = "Youtube"),
  count_and_rename(study2, tw) %>% mutate(Estudio = "Estudio 2", media = "Twitter"),
  count_and_rename(study2, tiktok) %>% mutate(Estudio = "Estudio 2", media = "Tiktok")
)|>
  
  ggplot(aes(x=prop,y=media))+
  geom_line(aes(group=media),color="#E7E7E7",linewidth=2.0)+
  geom_point(aes(color=variable),size=4)+
  theme_minimal()+
  labs(color="Shared?")+
  theme(legend.position = "top",
        axis.title.y = element_blank())+
  scale_y_discrete(labels=function(x) str_wrap(x,width = 50))+
  facet_wrap(~Estudio,ncol=2,scales="free_y")+
  labs(title="Odd of Awareness Sharing Missinformation by social media",
       subtitle = "Estudio 1 (Frequency media use) / Estudio 2 (Media news consuming)",
       x="Odd")+
  guides(shape=guide_legend(title="User"))



#Exposure

#Sharing
