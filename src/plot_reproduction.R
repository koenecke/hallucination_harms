usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("tidyverse")
usePackage("ggplot2")
usePackage("readxl")
usePackage("MatchIt")
usePackage("data.table")
usePackage("stargazer")

wd <- '/'
data_dir <- paste0(wd,'data/')
plot_dir <- paste0(wd,'plots/')

snippets <- read_csv(paste0(data_dir,'transcriptions.csv')) 

##########################################################################################
########################### Count hallucinations by subgroups ############################
##########################################################################################

# % ground truth snippets that result in hallucinations (either April or May run)
100*sum(snippets$hallucination)/nrow(snippets)

# % ground truth snippets that result in hallucinations (either April or May run), 
# grouped by aphasia/control
df <- snippets %>%
  group_by(dataset) %>%
  summarize(hallucinate = sum(hallucination),
            total = n()) %>%
  mutate(not_hallucinated = total-hallucinate,
         hallucinate_percent = hallucinate/total,
         dataset_clean = ifelse(dataset=="aphasia","Aphasia","Control")
  )

print(df)

prop.test(x=c(df$hallucinate[1],df$hallucinate[2]), 
          n=c(df$total[1],df$total[2]), 
          p = NULL, alternative = "two.sided",
          correct = TRUE)

fig1b_perc <- ggplot(df,aes(dataset_clean,hallucinate_percent))+
  geom_bar(stat="identity")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "% Hallucinated Transcriptions",x="")

ggsave(paste0(plot_dir,"fig1b_perc.png"),width=4,height=4)

##########################################################################################
########################### Count types of hallucinations ################################
##########################################################################################

total_num_hallucinated <- sum(snippets$old_hallucinate) + sum(snippets$new_hallucinate)

# number of hallucinated snippets with violence, innuendo, or stereotype
total_num_harm <- sum(snippets$old_harm) + sum(snippets$new_harm)

# number of hallucinated snippets with made up names and health statuses
total_num_ID <- sum(snippets$old_ID) + sum(snippets$new_ID)

# number of hallucinated snippets with youtube/video language, thanking language, or website links
total_num_links <- sum(snippets$old_links) + sum(snippets$new_links)

labels <- c("Violence, Innuendo, \nor Stereotypes",
            "Made-up Names, Relationships, \nor Health Statuses",
            "'Youtuber' Language, Thank You's, \nor Website Links")
percs <- c(total_num_harm/total_num_hallucinated,
           total_num_ID/total_num_hallucinated,
           total_num_links/total_num_hallucinated)
fig1a_df <- data.frame(cbind(labels,percs)) %>%
  mutate(percs = as.numeric(percs))

ggplot(fig1a_df,aes(labels,percs))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "Share of Hallucinated Text having Corresponding Harms",x="")

ggsave(paste0(plot_dir,"fig1a_harms.png"),width=10,height=4)


##########################################################################################
############################## Matching ##################################################
##########################################################################################

snippets <- snippets %>%
  mutate(race_categories = ifelse(Race=="WH","White",ifelse(Race=="AA","AfricanAmerican","OtherRace")),
         race_fac = factor(race_categories),
         employ_fac = factor(`Employment  Status`),
         speaker_id = gsub('(.*)_\\w+', '\\1', segment_name),
         nonvocal_seconds = nonvocal_duration/1000,
         num_words = lengths(strsplit(gsub(' .',' ',ground_truth), ' ')),
         total_duration = nonvocal_duration/(1000*share_nonvocal),
         age_sq = age*age,
         wordspeed = num_words/total_duration,
         vision = ifelse(`Adequate Vision`=="Y",1,0),
         hearing = ifelse(`Adequate Hearing`=="Y",1,0))

# Calculate mean values 

mean(snippets[snippets['dataset']=='aphasia',]$num_words)
mean(snippets[snippets['dataset']=='control',]$num_words)

mean(snippets[snippets['dataset']=='aphasia',]$total_duration)
mean(snippets[snippets['dataset']=='control',]$total_duration)

overlap_min <- max(min(snippets[snippets['dataset']=='aphasia',]$total_duration),
    min(snippets[snippets['dataset']=='control',]$total_duration))
overlap_max <- min(max(snippets[snippets['dataset']=='aphasia',]$total_duration),
                   max(snippets[snippets['dataset']=='control',]$total_duration))

overlapduration_snippets <- snippets %>%
  filter(total_duration > overlap_min) %>%
  filter(total_duration < overlap_max)

mean(overlapduration_snippets[overlapduration_snippets['dataset']=='aphasia',]$total_duration)
mean(overlapduration_snippets[overlapduration_snippets['dataset']=='control',]$total_duration)

# Match on speaker demographics

snippets_nona <- snippets %>% filter(!if_any(c(age, edu_years, Race, english_firstlang, Gender), is.na)) 

m.out1 <- matchit(is_aphasia ~ age + edu_years + race_fac + english_firstlang + 
                   is_female, data = snippets_nona,
                 distance = "glm",
                 caliper=.2,
                 mahvars = ~ age + is_female + race_fac
)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)
plot(summary(m.out1, interactions = TRUE),
     var.order = "unmatched")

# subset to matched snippets & re-run comparisons
subset_snippets1 <- match.data(m.out1) 

df <- subset_snippets1 %>%
  group_by(dataset) %>%
  summarize(hallucinate = sum(hallucination),
            total = n()) %>%
  mutate(not_hallucinated = total-hallucinate,
         hallucinate_percent = hallucinate/total,
         dataset_clean = ifelse(dataset=="aphasia","Aphasia","Control")
  )

prop.test(x=c(df$hallucinate[1],df$hallucinate[2]), 
          n=c(df$total[1],df$total[2]), 
          p = NULL, alternative = "two.sided",
          correct = TRUE)

# Match on speaker demographics and snippet characteristics

m.out2 <- matchit(is_aphasia ~ age + edu_years + race_fac + english_firstlang + 
                    is_female + vision + hearing + share_nonvocal + wordspeed, data = snippets_nona,
                  distance = "glm",
                  caliper=.15,
                  mahvars = ~ age + is_female + race_fac + share_nonvocal + wordspeed
)
summary(m.out2)
plot(m.out2, type = "jitter", interactive = FALSE)
plot(summary(m.out2, interactions = TRUE),
     var.order = "unmatched")

# subset to matched snippets & re-run comparisons
subset_snippets2 <- match.data(m.out2) 

df <- subset_snippets2 %>%
  group_by(dataset) %>%
  summarize(hallucinate = sum(hallucination),
            total = n()) %>%
  mutate(not_hallucinated = total-hallucinate,
         hallucinate_percent = hallucinate/total,
         dataset_clean = ifelse(dataset=="aphasia","Aphasia","Control")
  )

prop.test(x=c(df$hallucinate[1],df$hallucinate[2]), 
          n=c(df$total[1],df$total[2]), 
          p = NULL, alternative = "two.sided",
          correct = TRUE)

##########################################################################################
############################## Regressions ###############################################
##########################################################################################

reg1 <- "hallucination~is_aphasia+nonvocal_seconds+wordspeed+is_female+age+age_sq+relevel(race_fac,ref=3)+edu_years+hearing"
rega <- "hallucination~is_aphasia+nonvocal_seconds+num_words+is_female+age+age_sq+relevel(race_fac,ref=3)+edu_years+hearing"
regb <- "hallucination~is_aphasia+share_nonvocal+num_words+is_female+age+age_sq+relevel(race_fac,ref=3)+edu_years+hearing"

# original df
logit1 <- glm(paste0(reg1,"+vision+english_firstlang"), data=snippets_nona, family="binomial")
logit1a <- glm(paste0(rega,"+vision+english_firstlang"), data=snippets_nona, family="binomial")
logit1b <- glm(paste0(regb,"+vision+english_firstlang"), data=snippets_nona, family="binomial")

# df matched on speaker attributes
logit2 <- glm(paste0(reg1,"+vision"), data=subset_snippets1, family="binomial")
logit2a <- glm(paste0(rega,"+vision"), data=subset_snippets1, family="binomial")
logit2b <- glm(paste0(regb,"+vision"), data=subset_snippets1, family="binomial")

# df matched on speaker and audio attributes
logit3 <- glm(reg1, data=subset_snippets2, family="binomial")
logit3a <- glm(rega, data=subset_snippets2, family="binomial")
logit3b <- glm(regb, data=subset_snippets2, family="binomial")

# print tables
stargazer(logit1,single.row = TRUE,
          covariate.labels=c("Has Aphasia","Non-vocal Duration (seconds)","Average Word Speed",
                             "Is Female", "Age", "Age Squared",
                             "African American", "Other Race", "Years of Education",
                             "English is First Language", "No Vision Loss",
                             "No Hearing Loss"),
          dep.var.labels = "Hallucination"
          )

stargazer(logit1a, logit1b, logit2, logit2a, logit3, logit3a,
          single.row = TRUE,
          covariate.labels=c("Has Aphasia","Non-vocal Duration (seconds)","Share of Duration Being Non-Vocal",
                              "Number of Words","Average Word Speed",
                             "Is Female", "Age", "Age Squared",
                             "African American", "Other Race", "Years of Education",
                             "No Hearing Loss", "No Vision Loss","English is First Language"),
          dep.var.caption = "Hallucination Indicator",
          dep.var.labels.include = FALSE,
          column.labels = c("Original Data","Matched on Speaker Attributes",
                             "Matched on Speaker and Segment Attributes"),
          column.separate=c(2,2,2)
)

##########################################################################################
####################### Plot Non-vocal Durations by Subgroups ############################
##########################################################################################

final_nonvocalplot_df <- snippets %>%
  mutate(Audio = ifelse(legend=="AphasiaHallucinations","Aphasia Speaker; Whisper Hallucinates",
                        ifelse(legend=="AphasiaNoHallucinations","Aphasia Speaker; No Hallucinations",
                               ifelse(legend=="ControlHallucinations","Control Speaker; Whisper Hallucinates",
                                      "Control Speaker; No Hallucinations")))) %>%
  mutate(silero_share = (nonvocal_silero/1000)/total_duration)

ggplot(final_nonvocalplot_df, aes(x=share_nonvocal, fill=Audio)) +
  geom_density(alpha=0.6, position ='identity') + xlim(0,1) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Non-Vocal Share of Total Audio Duration") + ylab("Density") + theme_classic() + theme(legend.position=c(.7,.7))

ggsave(paste0(plot_dir,"fig2_pyannote_share.png"),width=6,height=4)

# Robustness checks

ggplot(final_nonvocalplot_df, aes(x=silero_share, fill=Audio)) +
  geom_density(alpha=0.6, position ='identity') +xlim(0,1) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Non-Vocal Share of Total Audio Duration") + ylab("Density") + theme_classic() + theme(legend.position=c(.7,.7))

ggsave(paste0(plot_dir,"fig5_silero_share.png"),width=6,height=4)

ggplot(final_nonvocalplot_df, aes(x=nonvocal_duration/1000, fill=Audio)) +
  geom_density(alpha=0.6, position ='identity') + xlim(0,20) +
  xlab("Non-Vocal Duration (seconds)") + ylab("Density") + theme_classic() + theme(legend.position=c(.7,.7))

ggsave(paste0(plot_dir,"fig5_pyannote_duration.png"),width=6,height=4)

# Compare means: AphasiaHallucinations > AphasiaNoHallucinations > ControlHallucinations > ControlNoHallucinations
snippets %>% group_by(legend) %>% summarise(mean(share_nonvocal))
snippets %>% group_by(legend) %>% summarise(mean(nonvocal_duration))
final_nonvocalplot_df %>% group_by(legend) %>% summarise(mean(silero_share))
overlapduration_snippets %>% group_by(legend) %>% summarise(mean(share_nonvocal))
snippets_nona %>% group_by(legend) %>% summarise(mean(share_nonvocal))

ggplot(snippets, aes(x=nonvocal_duration, fill=legend)) +
  geom_density(alpha=0.6, position ='identity') + xlim(0,30000)

ggplot(overlapduration_snippets, aes(x=nonvocal_duration, fill=legend)) +
  geom_density(alpha=0.6, position ='identity') + xlim(0,30000)

# T-test comparisons
t.test(snippets[snippets['dataset']=='control',]$share_nonvocal,
       snippets[snippets['dataset']=='aphasia',]$share_nonvocal)

t.test(snippets[snippets['hallucination']==1,]$share_nonvocal,
       snippets[snippets['hallucination']==0,]$share_nonvocal)

t.test(snippets[snippets['dataset']=='control',]$nonvocal_seconds,
       snippets[snippets['dataset']=='aphasia',]$nonvocal_seconds)

t.test(snippets[snippets['hallucination']==1,]$nonvocal_seconds,
       snippets[snippets['hallucination']==0,]$nonvocal_seconds)
