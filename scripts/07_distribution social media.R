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
  dplyr::select(
    #algorithmic_awareness
    d2_1_1,d2_2_1,d2_3_1,
    #algorithmic literacy
    alg_lit=d1,
    #missinformation
    exposure=c5_1, #exposure
    sharing=c5_3, #sharing
    c5_4,c5_5,c5_6, #corrective actions
    #frequency use of social platforms
    yt=b12_1,#Youtube
    fb=b12_2,#Facebook
    tw=b12_3,#Twitter
    ig=b12_4,#Instagram
    wsp=b12_5,#Whatsapp
    tiktok=b12_6,#TikTok
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
    region,
    tipo_zona
  )

vars_redes <- c("fb", "ig", "wsp", "yt", "tiktok", "tw")

study1_long <- study1 %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "platform",
    values_to = "frequency"
  ) %>%
  filter(!frequency %in% c(88, 99), !is.na(frequency)) %>%
  mutate(
    freq_label = case_when(
      frequency == 2 ~ "Weekly",
      TRUE ~ as.character(frequency)
    ),
    weekly_plus = ifelse(frequency >= 2, "Weekly or more", "Less than weekly")
  )

study1_prop <- study1_long %>%
  count(platform, freq_label, weekly_plus) %>%
  group_by(platform) %>%
  mutate(prop = n / sum(n))

ggplot(
  study1_prop,
  aes(
    x = factor(freq_label, levels = c("1", "Weekly", "3", "4", "5")),
    y = prop,
    fill = weekly_plus
  )
) +
  geom_col() +
  facet_wrap(~ platform, nrow = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Less than weekly" = "grey70",
      "Weekly or more" = "red3"
    )
  ) +
  labs(
    x = "Frequency of use",
    y = "Proportion of respondents",
    fill = "",
    title = "Study 1: Distribution of social media use variables (weekly threshold highlighted)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )


