# 0. Preparation ----
pacman::p_load(tidyverse, #Data manipulation
               haven, #.Sav Data manipulation
               janitor, #clean_names
               sjlabelled, #label manipulation
               labelled
               ) 

load("data/study_1.RData")
study1<-data
study2<-read_sav("data/study_2.sav")
load("data/NewsKnow.RData")

# 1. Select data ---------------------------------------------------------------
study1<-study1|>
  select(
    #algorithmic_awareness
    d2_1_1,d2_2_1,d2_3_1,
    #algorithmic literacy
    alg_lit=d1,
    #missinformation
    exposure=c5_1, #exposure
    sharing=c5_3, #sharing
    c5_4,c5_5,c5_6, #corrective actions
    #frequency use of social platforms
    b12_1,#Youtube
    b12_2,#Facebook
    b12_3,#Twitter
    b12_4,#Instagram
    b12_5,#Whatsapp
    b12_6,#TikTok
    #Digital skills
    tech_operational_skills,tech_informative_skills,tech_social_skills,
    tech_creative_skills, digital_skills, #total sumative index
    g5, #Filter principal
    g6,#Principal level of education
    educ_level=g7,#Personal level of education
    g8,#Principal labor occupation
    g9,#Principal labor occupation
    age=a2,#Age
    gender=a1,#Gender
    region
  )

study2<-study2|>
  clean_names()|>
  select(
    #algorithmic awareness
    p57,p58,p59,
    #algorithmic literacy
    alg_lit=p56,
    #awareness missinformation
    exposure=p51,#exposure
    sharing=p52,#sharing
    vulnerability=p53,#perceived vulnerability
    #Behaviour to counteract algorithm filtering
    p60,p61,p62,p63,p64,
    #Last week media consume in social platforms
    fb=p22,#facebook
    ig=p23,#instagram
    wsp=p24,#whatsapp
    yt=p25,#youtube
    tiktok=p26,#tiktok
    tw=p27,#twitter
    #Demographics
    age=p2,
    gender=p3,
    educ_level=p4,#Personal level of education
    ppal_educ_level=p80,#Principal level of education
    ppal_ocupation=p81,#Principal labour occupation
    region
  )

#Add objective missinformation scales
study2<-study2|>
  mutate(info_type=NewsKnow$info_type,
         obj_know=NewsKnow$obj_know,
         obj_know3=NewsKnow$obj_know3)


# 2. Proccess variables --------------------------------------------------------

#Study 1

#Recode demographics
study1 <- study1 %>%
  mutate(
    gender = ifelse(gender == 2, 1, 0), # Dichotomize gender
    rm = ifelse(region == 13, 1, 0), # Dichotomic RM/Region
    
    ppal_educ_level = case_when( 
      g5 == 1 ~ educ_level, # If participant is principal
      g5 == 2 ~ g6, # If participant is not principal
      g5 == 99 ~ NA_real_ # NA if g5 is 99
    ),
    
    ppal_ocupation = case_when( 
      g5 == 1 ~ g9, # If participant is principal
      g5 == 2 ~ g8, # If participant is not principal
      g5 == 99 ~ NA_real_ # NA if g5 is 99
    ),
    
    educ_level = case_when(
      educ_level %in% c(1, 2, 3) ~ 1, # Básica completa/incompleta
      educ_level == 4 ~ 2, # Media incompleta
      educ_level == 5 ~ 3, # Media completa
      educ_level %in% c(6, 7) ~ 4, # Técnico superior incompleto/completo
      educ_level %in% c(8, 9, 10) ~ 5, # Universitario completo/incompleto + Posgrado
      educ_level %in% c(88, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ppal_educ_level = case_when(
      ppal_educ_level %in% c(1, 2, 3) ~ 1, # Básica completa/incompleta
      ppal_educ_level == 4 ~ 2, # Media incompleta
      ppal_educ_level == 5 ~ 3, # Media completa
      ppal_educ_level %in% c(6, 7) ~ 4, # Técnico superior incompleto/completo
      ppal_educ_level %in% c(8, 9, 10) ~ 5, # Universitario completo/incompleto + Posgrado
      ppal_educ_level %in% c(88, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ppal_ocupation = case_when(
      ppal_ocupation == 1 ~ 5, # Altos Ejecutivos
      ppal_ocupation == 2  ~ 4, # Profesionales
      ppal_ocupation == 3  ~ 3, # Técnicos profesionales
      ppal_ocupation %in% c(4, 5) ~ 2, # Trabajadores de oficina, servicio, comercio
      ppal_ocupation %in% c(6, 7, 8, 9) ~ 1, # Agricultores, operarios, operadores, trabajadores no calificados  
      ppal_ocupation %in% c(10, 88, 99) ~ NA_real_, # Otros como NA
      TRUE ~ NA_real_
    ),
    age_range = case_when(
      age <= 25  ~ 1,
      age > 25 & age <= 40 ~ 2,
      age > 40 & age <= 60 ~ 3,
      age > 60 ~ 4,
      TRUE ~ NA_real_
    )
  )


#Create algoritmic awareness index
study1$alg_aware<-study1|>
  mutate_at(.vars=vars(starts_with("d2_")),
            .funs=list(~ifelse(.%in%c(99,88),NA,.)))|>
  select(starts_with("d2"))|>
  rowMeans(na.rm = TRUE)|>
  round(2)

#Create Missinformation corrective actions scale
study1$anti_fake_news <- 
  study1|> 
  mutate_at(.vars=vars(starts_with("c5_")),
            .funs=list(~ifelse(.==99,NA,.)))|>
  dplyr::select(c5_4,c5_5,c5_6)|>
  mutate_all(as.numeric)|>
  rowMeans(na.rm=T)|>
  round(2)

study1<-study1|>
  #99 to NA in not proccesed variables
  mutate_at(.vars=vars(starts_with("c5_"),alg_lit,exposure,sharing),
          .funs=list(~set_na(.,na=c("NS-NR"=99),as.tag=TRUE)))|>
  #Dichotomize users social media
  mutate_at(.vars=vars(starts_with("b12_")),
            .funs=list(~ifelse(.%in%c(3,4,5),1,0)))|>
  #Recode algoritmich literacy
  mutate(alg_lit=case_when(
    alg_lit %in% c(1,2) ~ 0,
    alg_lit %in% c(3,4,5) ~ 1,
    TRUE ~ NA_integer_
  ))|>
  rename(
    yt=b12_1,
    fb=b12_2,
    tw=b12_3,
    ig=b12_4,
    wsp=b12_5,
    tiktok=b12_6,
    content_filt=d2_1_1,
    human_int=d2_2_1,
    ethic=d2_3_1
  )|>
  #Recode Awareness missinformation sharing as dummy
  mutate(sharing=case_when(
    sharing == 1 ~ 0,
    sharing %in% c(2,3,4,5) ~ 1,
    TRUE ~ NA_integer_
  ))


#Study 2

study2 <- study2|>
  mutate(
    gender = ifelse(gender==2,1,0), #Dichotomize gender
    rm = ifelse(region == 13, 1, 0), #Dichotomic RM/Region
    educ_level = case_when(
      educ_level %in% c(1,2,3) ~ 1, #Básica completa/incompleta
      educ_level == 4 ~ 2, #Media incompleta
      educ_level == 5 ~ 3, #Media completa
      educ_level %in% c(6,7) ~ 4, #Técnico superior incompleto/completo
      educ_level %in% c(8,9,10) ~ 5, #Universitario completo/incompleto + Posgrado
      educ_level %in% c(88,99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ppal_educ_level = case_when(
      ppal_educ_level %in% c(1,2,3) ~ 1, #Básica completa/incompleta
      ppal_educ_level == 4 ~ 2, #Media incompleta
      ppal_educ_level == 5 ~ 3, #Media completa
      ppal_educ_level %in% c(6,7) ~ 4, #Técnico superior incompleto/completo
      ppal_educ_level %in% c(8,9,10) ~ 5, #Universitario completo/incompleto + Posgrado
      ppal_educ_level %in% c(88,99) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    ppal_ocupation = case_when(
      ppal_ocupation == 1 ~ 5, #Altos Ejecutivos
      ppal_ocupation == 2  ~ 4, #Profesionales
      ppal_ocupation == 3  ~ 3, #Técnicos profesionales
      ppal_ocupation %in% c(4,5) ~ 2, #Trabajadores de oficina, servicio, comercio
      ppal_ocupation %in% c(6,7,8,9) ~ 1, #Agricultores, operarios, operadores, trabajadores no calificados  
      ppal_ocupation %in% c(10,88,99) ~ NA_real_, #Otros como NA
      TRUE ~ NA_real_
    ),
    age_range = case_when(
      age <= 25  ~ 1,
      age > 25 & age <= 40 ~ 2,
      age > 40 & age <= 60 ~ 3,
      age > 60 ~ 4,
      TRUE ~ NA_real_
    )
  )

#Create algoritmic awareness index
study2$alg_aware<-study2|>
  mutate_at(.vars=vars(p57,p58,p59),
            .funs=list(~ifelse(.%in%c(88,99),NA,.)))|>
  select(p57,p58,p59)|>
  rowMeans(na.rm = TRUE)|>
  round(2)  

#Create Counteract algorithm index
study2$counteract_algorithm <- study2|>
  mutate_at(.vars=vars(p60,p61,p62,p63,p64),
            .funs=list(~case_when(
              . == 1 ~ 1,
              . == 0 ~ 2,
              . %in% c(88,99) ~ NA_real_)
              )
            )|>
  select(p60,p61,p62,p63,p64)|>
  rowSums(na.rm = TRUE)|>
  as.numeric()

#88 and 99 to NA, and drop items
study2<-study2|>
  mutate(across(-age, ~set_na(.,na=c("NS-NR"=99),as.tag=TRUE)))|>
  mutate(across(-age, ~set_na(.,na=c("NS-NR"=88),as.tag=TRUE)))|>
  rename(content_filt=p57,
         human_int=p58,
         ethic=p59)

study2 <- study2|>
  #Recode algorithmic literacy
  mutate(alg_lit=case_when(
    alg_lit== 2 ~ 0,
    alg_lit== 1 ~ 1,
    T ~ NA_integer_
    ))|>
  #Recode awareness missinformation sharing as dummy
  mutate(sharing=case_when(
    sharing == 1 ~ 0,
    sharing %in% c(2,3) ~ 1,
    T ~ NA_integer_
  ))|>
  #Recode social media information as dummy
  mutate_at(
    .vars = vars(fb,ig,wsp,yt,tiktok,tw),
    .funs = list(~case_when(
      . == 1 ~ 1,
      . == 2 ~ 0,
      T ~ NA_integer_
    ))
  )

#Revise labels
lapply(study1,class)
lapply(study2,class)

#Labels manipulation -----------------------------------------------------------

#RM labbel
val_labels(study1$rm)<-c("Otras Regiones"=0,
                         "Región Metropolitana"=1)
val_labels(study2$rm)<-c("Otras Regiones"=0,
                         "Región Metropolitana"=1)

#Demographics
education <- c("Basic school incomplete/complete"=1,
               "Middle school incomplete"=2,
               "Middle school complete"=3,
               "Higher technical incomplete/complete"=4,
               "University + Postgraduate incomplete/complete"=5)

ocuppations<-c(
  "Top executives, managers, Officials, and administrators"=5,
  "Scientific and intellectual professionals"=4,
  "Technicians, mid-level professionals, and sub-officials"=3,
  "Traders and office workers"=2,
  "Agricultural workers, operatives, and unskilled workers"=1)

age_bracket<-c("25 years old or less"=1,
               "Between 26 y 40 years old"=2,
               "Between 41 y 60 years old"=3,
               "60+ years old"=4)

val_labels(study1$age_range)<-age_bracket
val_labels(study2$age_range)<-age_bracket
val_labels(study1$educ_level)<-education
val_labels(study2$educ_level)<-education
val_labels(study1$ppal_educ_level)<-education
val_labels(study2$ppal_educ_level)<-education
val_labels(study1$ppal_ocupation)<-ocuppations
val_labels(study2$ppal_ocupation)<-ocuppations



study1<-study1|>
  mutate_at(
    .vars=vars(alg_lit,alg_aware,starts_with("tech"),digital_skills),
    .funs=list(~as.numeric(.))
  )

study2<-study2|>
  mutate_at(
    .vars=vars(starts_with("alg")),
    .funs=list(~as.numeric(.))
  )

# Sharing relabel
val_labels(study1$sharing)<-c("Never shared missinformation"=0,
                              "Have shared missinformation"=1)
val_labels(study2$sharing)<-c("Never shared missinformation"=0,
                              "Have shared missinformation"=1)
var_label(study1$sharing)<-"Awareness Sharing Missinformation (Recoded)"
var_label(study1$sharing)<-"Awareness Sharing Missinformation (Recoded)"

# Social media relabel
## For study1
val_labels(study1$gender)<-c("Women"=1, "Male"=0)
user_media <- c("Weekly user"=1, "No Weekly user"=0)

val_labels(study1$yt) <- user_media
val_labels(study1$fb) <- user_media
val_labels(study1$ig) <- user_media
val_labels(study1$wsp) <- user_media
val_labels(study1$tw) <- user_media
val_labels(study1$tiktok) <- user_media

var_label(study1$yt) <- "Usuario semanal de Youtube"
var_label(study1$fb) <- "Usuario semanal de Facebook"
var_label(study1$ig) <- "Usuario semanal de Instagram"
var_label(study1$wsp) <- "Usuario semanal de Whatsapp"
var_label(study1$tw) <- "Usuario semanal de Twitter"
var_label(study1$tiktok) <- "Usuario semanal de Tiktok"

# For study2
val_labels(study2$gender)<-c("Women"=1, "Male"=0)
news_info_source <- c("Weekly news information source"=1, "No Weekly news information source"=0)

val_labels(study2$yt) <- news_info_source
val_labels(study2$fb) <- news_info_source
val_labels(study2$ig) <- news_info_source
val_labels(study2$wsp) <- news_info_source
val_labels(study2$tw) <- news_info_source
val_labels(study2$tiktok) <- news_info_source

var_label(study2$yt) <- "Usuario semanal de Youtube para informarse de noticias"
var_label(study2$fb) <- "Usuario semanal de Facebook para informarse de noticias"
var_label(study2$ig) <- "Usuario semanal de Instagram para informarse de noticias"
var_label(study2$wsp) <- "Usuario semanal de Whatsapp para informarse de noticias"
var_label(study2$tw) <- "Usuario semanal de Twitter para informarse de noticias"
var_label(study2$tiktok) <- "Usuario semanal de Tiktok para informarse de noticias"

#Order database ----------------------------------------------------------------
study1<-study1|>
  select(
    exposure, sharing, anti_fake_news,
    content_filt,human_int,ethic,
    alg_lit, alg_aware,
    fb, ig, wsp, yt, tw, tiktok,
    age,age_range,gender,educ_level,ppal_educ_level,ppal_ocupation,rm,
    starts_with("tech"),
    digital_skills,
    c5_4,c5_5,c5_6
  )

study2<-study2|>
  select(
    exposure, sharing, vulnerability,
    info_type,obj_know,obj_know3,
    content_filt,human_int,ethic,
    alg_lit, alg_aware,
    counteract_algorithm,
    fb, ig, wsp, yt, tw, tiktok,
    p60,p61,p62,p63,p64,
    age,age_range,gender,educ_level,ppal_educ_level,ppal_ocupation,rm
    )
  
