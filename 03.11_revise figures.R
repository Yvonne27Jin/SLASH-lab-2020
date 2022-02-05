
# 0311 revise figures

library(foreign)
library(haven)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(lavaan)
library(apaTables)

df_labelled <- read.spss("merged_Survey on Sleep Health+followedup.sav",use.value.labels = T,to.data.frame = T)

parti_lablled <- df_labelled %>% filter(Eli1 == "Yes" & Eli2 == "Yes" & agree == "Yes")

## additional Qs for sleep problems - graphs


#  combine before 2019 + S1 + S2; 
add <- parti_lablled %>% select(starts_with("add")) %>% 
  mutate(adds1_ = case_when(
    adds1 == "Before 2019" ~ "Before 2019 S3",
    adds1 == "BetweenJan 01 2019 and March 31 2019" ~ "Before 2019 S3",
    adds1 == "Between April 01 2019 and June 30 2019" ~ "Before 2019 S3",
    adds1 == "Between July 01 2019 and September 30 2019" ~ "2019 S3",
    adds1 == "Between October 01 2019 and Dec 31 2019 " ~ "2019 S4",
    adds1 == "Between Jan 01 2020 and March 31 2020" ~ "2020 S1",
    adds1 == "After April 01 2020" ~ "After 2020 S1"))



##------------------
# title = "Onset time of Sleep problems"
# value = c(186, 110, 70, 189, 38) , total = 593
# legend.title = element_text("Time of Onset")

insomnia_onset <- data.frame(Time = c("Before 2019 S3", "2019 S3", "2019 S4", "2020 S1", "After 2020 S1"),
                             Value = c(186, 110, 70, 189, 38),
                             Perc = c("31.4%", "18.5%", "11.8%", "31.9%", "6.4%"))

#fix order of categories
insomnia_onset$Time <- factor(insomnia_onset$Time, ordered = T, 
                              levels = as.character(insomnia_onset$Time))


head(insomnia_onset)

# changed number into percentage
# removed outer numbers
# changed color palette
# Changed order of legend and pie slices
### Check this website for color palettes
### https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
### play around with legend
### Rename legend labels 

library(viridis)

figure_1 <- ggplot(insomnia_onset, aes(x = "", y=Value, fill=Time)) +
  geom_bar(width = 1,stat="identity") +
  coord_polar("y",start=0,direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line  = element_blank(),
        panel.grid  = element_blank()) +
  geom_text(aes(label = Perc),
            vjust = 0.2,
            position = position_stack(vjust = 0.5),
            size = 6.5) +
  labs(x= "", y = "", title = "Figure 1.1" ) +
  scale_fill_brewer(name = "Time of Onset", palette = "Set3" )

figure_1

# -----------------
# another way of showing numbers: undone.
library(ggforce)
library(scales)
library(ggrepel)
ggplot(insomnia_onset, aes(x = "", y=Value, fill=Time)) +
  geom_bar(width = 1,stat="identity") +
  coord_polar("y",start=0,direction = -1) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line  = element_blank(),
        panel.grid  = element_blank()) +
        geom_label_repel(aes(label = Perc,y = Value), size=3, show.legend = F, nudge_x = 1)+
        guides(fill = guide_legend(title = "Code"))+
  labs(x= "", y = "" ) +
  scale_fill_brewer(palette = "Set3")



?theme
?theme_set
?position_stack
?scale_fill_brewer
?coord_polar
??geom_text # check_overlap = T

#---------------------------------------------

##adds2 & adds3 [discarded]

insomnia_cause <- parti_lablled %>% select(starts_with("adds2"), starts_with("adds3")) %>%
  mutate(Cause = case_when(adds2 == "yes" & adds3 == "Yes" ~ "Both",
                           adds2 == "yes" & adds3 == "No" ~ "Protests",
                           adds2 == "no" & adds3 == "Yes" ~ "COVID_19 Outbreak",
                           adds2 == "no" & adds3 == "No" ~ "Neither")
  ) %>% 
  group_by(Cause)  %>%
  count()

insomnia_cause <- drop_na(insomnia_cause)


ggplot(insomnia_cause, aes(x = "", y=n, fill=Cause)) +
  geom_bar(width = 1,stat="identity") +
  coord_polar("y",start=0) +
  fill_palette(palette =  "uchicago") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 4.7) +
  labs(title = "Cause of Sleep problems",
       y = "" )

#--------------------------------------------------------
# Figure 2: add4, count number of response for each cause

insomnia_cause <- parti_lablled %>% select(starts_with("adds4"))
summary(insomnia_cause)


cause_insomnia <- data.frame(Cause = c("Political discord", "Worry about COVID-19","Psychological disturbances", "Sleep habits", "Sleep environment", "Daytime activities"),
                              Number = c(327, 305, 470, 261, 200,201))
#Note: psych distrubances  such as depression, anxiety, or high stress

cause_insomnia



figure_2 <- ggplot(data = cause_insomnia, aes(x=Cause, y=Number)) +
  geom_bar(stat="identity",width=0.5,fill="steelblue") + 
  geom_text(aes(label=Number), hjust=1.4, vjust = 0.3, size=3.5, color = "white") +
  theme(text = element_text(size=20)) +
  theme_minimal() + 
  labs(title = "Figure 1.2") +
  coord_flip() # Horizontal

figure_2

# --------------------------------------------

#Figure_3 help seeking
## variable seek2 If you wish to seek help for your sleep problems, which of the following sources of help will you seek? (Check all that apply)

Seek_help <- parti_lablled %>% select(starts_with("seek2"))
summary(Seek_help)


seek_help_n <- data.frame(Source = c("Online information", "Family doctor","Psychiatrist", " Psychologist", "Social worker", "Friends/family"),
                             Number = c(402, 97, 73, 100, 75,317))
#Note: psych distrubances  such as depression, anxiety, or high stress

seek_help_n



figure_3 <- ggplot(data = seek_help_n, aes(x=Source, y=Number)) +
  geom_bar(stat="identity",width=0.5,fill="steelblue") + 
  geom_text(aes(label=Number), hjust=1.4, vjust = 0.3, size=3.5, color = "white") +
  labs(title = "Figure 1.3") +
  theme_minimal() +
  coord_flip() # Horizontal

figure_3

# -----------------------------


#Figure_4 help seeking
# varable seek 3 Which of the following methods do you think can improve your sleep?(Choose all that apply)


Cure_perception <- parti_lablled %>% select(starts_with("seek3"))
summary(Cure_perception)


Cure_n <- data.frame(Source = c("Medications/Western Medicine", "Chinese medicine","Herbal/food remedies", " Stress management", "Exercise", " Cognitive Behavioral Therapy"),
                          Number = c(181, 145, 164, 462, 395,125))


Cure_n



figure_4 <- ggplot(data = Cure_n, aes(x=Source, y=Number)) +
  geom_bar(stat="identity",width=0.5,fill="steelblue") + 
  geom_text(aes(label=Number), hjust=1.4, vjust = 0.3, size=3.5, color = "white") +
  labs(title = "Figure 1.4") +
  theme_minimal() +
  coord_flip() # Horizontal

figure_4

#---------------------

#combine 4 figures in one graph
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
#install.packages("ggpubr")
library(ggpubr)

ggarrange(bxp, dp, bp + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)