library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(irr)
library(caret)
library(RcppAlgos)
library(patchwork)

# FORENSIC PRACTITIONERS AND ART STUDENTS #####
# DESCRIPTIONS OF PARTICIPANTS WITH EXPERIENCE IN FACIAL VARIATION
descriptions <- read.table("descriptions_background.txt", header = T, check.names=FALSE)

# Consistency #####
## PAIRWISE INTRA-GROUP CONSISTENCY ####
# Practitioners
eyesP <- descriptions[descriptions$cohort == "anthrop", ] %>% dplyr::select(contains("Eyes"))
noseP <- descriptions[descriptions$cohort == "anthrop", ] %>% dplyr::select(contains("Nose"))
mouthP <- descriptions[descriptions$cohort == "anthrop", ] %>% dplyr::select(contains("Mouth"))
# Artists
eyesA <- descriptions[descriptions$cohort == "artist", ] %>% dplyr::select(contains("Eyes"))
noseA <- descriptions[descriptions$cohort == "artist", ] %>% dplyr::select(contains("Nose"))
mouthA <- descriptions[descriptions$cohort == "artist", ] %>% dplyr::select(contains("Mouth"))


consistency <- function(DFeyes, DFnose, DFmouth){
  # Descriptions
  DFeyes <- t(DFeyes)
  colnames(DFeyes) <- seq(1, dim(DFeyes)[2])
  DFnose <- t(DFnose)
  colnames(DFnose) <- seq(1, dim(DFnose)[2])
  DFmouth <- t(DFmouth)
  colnames(DFmouth) <- seq(1, dim(DFmouth)[2])
  
  a <- seq(1, dim(DFeyes)[2])
  # pairs for comparison, no duplicates (order does not matter)
  pairgrid <- comboGrid(a, a, repetition = F)
  pairgrid <- as.data.frame(pairgrid)
  colnames(pairgrid) <- c("Part1", "Part2") # Participant 1 and 2
  
  pairgrid$Eyes <- NA
  pairgrid$Nose <- NA
  pairgrid$Mouth <- NA
  
  eyes <- list()
  nose <- list()
  mouth <- list()
  # UNWEIGHTED Cohen's Kappa
  for (i in 1:dim(pairgrid)[1]) {
    eyes[[i]] <- kappa2(DFeyes[, c(pairgrid[i,1], pairgrid[i,2])],
                        weight = "unweighted")
    nose[[i]] <- kappa2(DFnose[, c(pairgrid[i,1], pairgrid[i,2])],
                        weight = "unweighted")
    mouth[[i]] <- kappa2(DFmouth[, c(pairgrid[i,1], pairgrid[i,2])],
                         weight = "unweighted")
    
    pairgrid[i,3] <- eyes[[i]]$value
    pairgrid[i,4] <- nose[[i]]$value
    pairgrid[i,5] <- mouth[[i]]$value
    
  }
  return(pairgrid)
}

# Agreement among practitioners
agreement_P <- consistency(eyesP, noseP, mouthP)
agreement_P$discipline <- "Forensic practitioners" 

# Agreement among artists
agreement_A <- consistency(eyesA, noseA, mouthA)
agreement_A$discipline <- "Art students"

# intra-group agreement
agreement_within <- rbind(agreement_P, agreement_A)

# descriptive statistics
agreement_within %>% 
  pivot_longer(cols = Eyes:Mouth, names_to = "trait", values_to = "k") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>% 
  mutate(discipline = factor(discipline, levels = c("Forensic practitioners", "Art students"))) %>% 
  group_by(discipline, trait) %>%
  summarise(mean = mean(k))

## Plot: intra-group consistency ####
a <- agreement_within %>% 
  pivot_longer(cols = Eyes:Mouth, names_to = "trait", values_to = "k") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>%
  mutate(discipline = factor(discipline, levels = c("Forensic practitioners", 
                                                    "Art students")))%>%
  ggplot(aes(x = trait, y = k, color = discipline))+
  geom_boxplot()+
  scale_color_manual(values=c("#8B9EB7","#F49D37")) +
  stat_summary(aes(group = discipline),
               fun=mean, geom="point", shape=20, 
               size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  theme_bw() + 
  labs(x = "Facial feature", y = "Cohen's Kappa", color = "Expertise")+
  ylim(-1, 1)
#

## PAIRWISE INTER-GROUP CONSISTENCY ####
consistency_between <- function(
    DFeyes1, DFnose1, DFmouth1, #First group
    DFeyes2, DFnose2, DFmouth2  #Second group
){
  
  # First group descriptions
  DFeyes1 <- t(DFeyes1)
  colnames(DFeyes1) <- seq(1, dim(DFeyes1)[2])
  DFnose1 <- t(DFnose1)
  colnames(DFnose1) <- seq(1, dim(DFnose1)[2])
  DFmouth1 <- t(DFmouth1)
  colnames(DFmouth1) <- seq(1, dim(DFmouth1)[2])
  # Second group descriptions
  DFeyes2 <- t(DFeyes2)
  colnames(DFeyes2) <- seq(1, dim(DFeyes2)[2])
  DFnose2 <- t(DFnose2)
  colnames(DFnose2) <- seq(1, dim(DFnose2)[2])
  DFmouth2 <- t(DFmouth2)
  colnames(DFmouth2) <- seq(1, dim(DFmouth2)[2])
  
  
  a <- seq(1, dim(DFeyes1)[2])
  b <- seq(1, dim(DFeyes2)[2])
  # pairs for comparison, no duplicates (order does not matter)
  pairgrid <- comboGrid(a, b, repetition = F)
  pairgrid <- as.data.frame(pairgrid)
  colnames(pairgrid) <- c("Part1", "Part2") # Participant 1 and 2
  
  pairgrid$Eyes <- NA
  pairgrid$Nose <- NA
  pairgrid$Mouth <- NA
  
  eyes <- list()
  nose <- list()
  mouth <- list()
  for (i in 1:dim(pairgrid)[1]) {
    e <- cbind(DFeyes1[,pairgrid[i,1]], 
               DFeyes2[,pairgrid[i,2]])
    
    n <- cbind(DFnose1[,pairgrid[i,1]], 
               DFnose2[,pairgrid[i,2]])
    
    m <- cbind(DFmouth1[,pairgrid[i,1]], 
               DFmouth2[,pairgrid[i,2]])
    
    # UNWEIGHTED Cohen's Kappa
    eyes[[i]] <- kappa2(e, weight = "unweighted")
    nose[[i]] <- kappa2(n, weight = "unweighted")
    mouth[[i]] <- kappa2(m, weight = "unweighted")
    
    pairgrid[i,3] <- eyes[[i]]$value
    pairgrid[i,4] <- nose[[i]]$value
    pairgrid[i,5] <- mouth[[i]]$value
    
  }
  return(pairgrid)
}

agreement_between <- consistency_between(eyesP, noseP, mouthP, 
                                         eyesA, noseA, mouthA)


# descriptive statistics
agreement_between %>% 
  pivot_longer(cols = Eyes:Mouth, names_to = "trait", values_to = "k") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>% 
  group_by(trait) %>%
  summarise(mean = mean(k))

## Plot: inter-group consistency ####
b <- agreement_between %>% 
  pivot_longer(cols = Eyes:Mouth, names_to = "trait", values_to = "k") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>%
  ggplot(aes(x = trait, y = k))+
  geom_boxplot(color="#566246")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  theme_bw() + labs(x = "Facial feature", y = "Cohen's Kappa")+
  ylim(-1, 1)


a + b + plot_annotation(tag_levels = 'A')
ggsave("./Fig3_intra-inter_agreement.png", width = 30, height = 16, units = "cm", dpi = 150)


# Accuracy #####
# Categorical GT AND Comparative GT FOR 1SD and 2SD
CA1 <- read.table("CaGT_1SD.txt", header = T)
CA2 <- read.table("CaGT_2SD.txt", header = T)
COM1 <- read.table("CompGT_1SD.txt", header = T)
COM2 <- read.table("CompGT_2SD.txt", header = T)

# Transforming size categories within GT databases into numeric variables 
GT_to_numeric <- function(DF){
  output <- DF%>% 
    # numeric
    mutate_all(list(~str_replace(., "Small","1"))) %>%
    mutate_all(list(~str_replace(., "Large","3"))) %>%
    mutate_all(list(~str_replace(., "Medium","2"))) %>%
    mutate(across(everything(), as.numeric))
  
  return(output)
  
}

CA1 <- GT_to_numeric(CA1)
CA2 <- GT_to_numeric(CA2)
COM1 <- GT_to_numeric(COM1)
COM2 <- GT_to_numeric(COM2)

# Transforming size categories within description databases into numeric variables 
description_TO_numeric <- function(DF){
  output <- as.data.frame(t(DF)) %>% 
    # numeric
    mutate_all(list(~str_replace(., "Small","1"))) %>%
    mutate_all(list(~str_replace(., "Large","3"))) %>%
    mutate_all(list(~str_replace(., "Medium","2"))) %>%
    rename_with(~str_remove(., 'V')) %>%
    mutate(across(everything(), as.numeric)) %>%
    as_tibble(rownames = "Folio_num")%>%
    # row names into column where we keep the numeric part
    # discarding the character part leads to the warning message
    # but it is not a problem 
    separate(Folio_num, into='Folio_num', remove=F) %>%
    mutate(across(Folio_num, as.numeric)) %>%
    # ascending order
    arrange(Folio_num) %>% as.data.frame()
  
  return(output)
}

eyesNumP <- description_TO_numeric(eyesP)
noseNumP <- description_TO_numeric(noseP)
mouthNumP <- description_TO_numeric(mouthP)
eyesNumA <- description_TO_numeric(eyesA)
noseNumA <- description_TO_numeric(noseA)
mouthNumA <- description_TO_numeric(mouthA)

## RELATIONSHIP BETWEEN GT AND QUALITATIVE DESCRIPTIONS ####
accuracy_GT <- function(DFeyes, DFnose, DFmouth,
                        GT1, GT2 # GT 1SD, GT 2SD
){
  # Eliminating Folio column
  DFeyes <- DFeyes[,-1]
  DFnose <- DFnose[,-1]
  DFmouth <- DFmouth[,-1]
  
  a <- seq(1, dim(DFeyes)[2]) # minus Folio
  # It generates a database of unique comparison pairs, ensuring no duplicates regardless of order
  pairgrid <- comboGrid(a, "GT", repetition = F)
  pairgrid <- as.data.frame(pairgrid)
  colnames(pairgrid) <- c("Part1", "GT") # ParticipantS and GT
  # 1 SD
  pairgrid$Eyes1 <- NA
  pairgrid$Nose1 <- NA
  pairgrid$Mouth1 <- NA
  # 2 SD
  pairgrid$Eyes2 <- NA
  pairgrid$Nose2 <- NA
  pairgrid$Mouth2 <- NA
  # 1SD
  eyes1 <- list()
  nose1 <- list()
  mouth1 <- list()
  # 2SD
  eyes2 <- list()
  nose2 <- list()
  mouth2 <- list()
  
  # Spearman correlation
  for (i in 1:dim(DFeyes)[2]) {
    eyes1[[i]] <- cor.test(GT1$eye_sizeC, DFeyes[,i], method="spearman", exact = FALSE)
    nose1[[i]] <- cor.test(GT1$nose_sizeC, DFnose[,i], method="spearman", exact = FALSE)
    mouth1[[i]] <- cor.test(GT1$mouth_sizeC, DFmouth[,i], method="spearman", exact = FALSE)
    
    eyes2[[i]] <- cor.test(GT2$eye_sizeC, DFeyes[,i], method="spearman", exact = FALSE)
    nose2[[i]] <- cor.test(GT2$nose_sizeC, DFnose[,i], method="spearman", exact = FALSE)
    mouth2[[i]] <- cor.test(GT2$mouth_sizeC, DFmouth[,i], method="spearman", exact = FALSE)
    
    pairgrid[i,3] <- as.vector(eyes1[[i]]$estimate)
    pairgrid[i,4] <- as.vector(nose1[[i]]$estimate)
    pairgrid[i,5] <- as.vector(mouth1[[i]]$estimate)
    pairgrid[i,6] <- as.vector(eyes2[[i]]$estimate)
    pairgrid[i,7] <- as.vector(nose2[[i]]$estimate)
    pairgrid[i,8] <- as.vector(mouth2[[i]]$estimate)
  }
  return(pairgrid)
}

# accuracy using categorical and comparative GT, for practitioners
accuracy_categoricalP <- accuracy_GT(eyesNumP, noseNumP, mouthNumP, CA1, CA2)
accuracy_categoricalP$GT <- "CaGT"
accuracy_categoricalP$Discipline <- "Forensic practitioners"

accuracy_comparativeP <- accuracy_GT(eyesNumP, noseNumP, mouthNumP, COM1, COM2)
accuracy_comparativeP$GT <- "CompGT"
accuracy_comparativeP$Discipline <- "Forensic practitioners"

# accuracy using categorical and comparative GT, for artists
accuracy_categoricalA <- accuracy_GT(eyesNumA, noseNumA, mouthNumA, CA1, CA2)
accuracy_categoricalA$GT <- "CaGT"
accuracy_categoricalA$Discipline <- "Art students"

accuracy_comparativeA <- accuracy_GT(eyesNumA, noseNumA, mouthNumA, COM1, COM2)
accuracy_comparativeA$GT <- "CompGT"
accuracy_comparativeA$Discipline <- "Art students"

# proper formatting
accuracies <- rbind(accuracy_categoricalP, accuracy_comparativeP, 
                    accuracy_categoricalA, accuracy_comparativeA) %>% 
  pivot_longer(Eyes1:Mouth2, names_to = "trait", values_to = "rho") %>%
  separate(trait, into = c("trait", "SD"), "(?<=[a-z])(?=[0-9])") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))%>%
  mutate(SD = factor(SD, levels = c("1", "2"))) %>%
  mutate(Discipline = factor(Discipline, levels = c("Forensic practitioners", "Art students"))) %>%
  mutate(GT = factor(GT, levels = c("CaGT", "CompGT")))

## ACCURACY DIFFERENCES BETWEEN 1SD vs 2SD ####
# descriptive statistics
## Table S1 #### 
# 1SD threshold shows better alignment to quali descriptions than 2SD
accuracies %>%
  group_by(Discipline, GT, SD, trait) %>%
  summarise(mean = mean(rho)) %>% print(n = 30)

# checking normality; if p>0.5 normal (t-test), if p<non normal (Mann-Whitney)
# Eyes and nose normal, mouth non-normal
accuracies %>% 
  group_by(Discipline, GT, trait) %>%
  summarise(normal_pValue = shapiro.test(rho)[2]$p.value)%>% print(n = 30)

# t-test between 1SD and 2SD for eyes and Nose that showed normality
## Table S1 #### 
accuracies %>% filter(trait != "Mouth") %>%
  group_by(Discipline, GT, trait) %>%
  summarise(pvalue = t.test(rho ~ SD)$p.value,
            tstatistic = t.test(rho ~ SD)$statistic,
            DF = t.test(rho ~ SD)$parameter)

# wilcoxon test 1SD and 2SD, for mouth that showed non-normality
## Table S1 #### 
accuracies %>% filter(trait == "Mouth") %>%
  group_by(Discipline, GT, trait) %>%
  summarise(pvalue = wilcox.test(rho ~ SD)$p.value,
            tstatistic = wilcox.test(rho ~ SD)$statistic)

## Plot: accuracy differences between 1SD vs 2SD ####
e <- ggplot(accuracies, aes(x = trait, y = rho, color = SD))+
  geom_boxplot() +
  scale_color_manual(values=c("#454851","grey")) +
  stat_summary(aes(group = SD),
               fun=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  ylim(-1, 1) + theme_bw()+
  labs(x = "Facial feature", y = "Spearman's rho", color = "Threshold") 


## Plot supp figure: accuracy differences between 1SD vs 2SD by GT ####
ggplot(accuracies, aes(x = trait, y = rho, color = SD))+
  geom_boxplot() +
  scale_color_manual(values=c("#454851","grey")) +
  stat_summary(aes(group = SD),
               fun=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  ylim(-1, 1) + theme_bw()+ facet_grid(vars(Discipline), vars(GT))+
  labs(x = "Facial feature", y = "Spearman's rho", color = "Threshold")

ggsave("./Fig1sup_GT_SD.png", width = 30, height = 26, units = "cm", dpi = 200)

## ACCURACY DIFFERENCES BETWEEN CAGT vs COMPGT ####
# Keeping 1SD only, because it showed better performance
# Descriptive statistics
## Table S2 ####
accuracies %>%
  filter(SD == 1) %>% 
  group_by(Discipline, GT,trait) %>%
  summarise(mean = mean(rho))

# checking normality; if p>0.5 normal (t-test), if p<non normal (Mann-Whitney)
# Eyes and mouth normal, nose non-normal
accuracies %>%
  filter(SD == 1) %>% 
  group_by(Discipline, GT,trait) %>%
  summarise(normal = shapiro.test(rho)[2]$p.value)

# t-test between CAGT and COMPGT for eyes and mouth that showed normality
## Table S2 ####
accuracies %>%
  filter(SD == 1) %>% 
  filter(trait != "Nose") %>%
  group_by(Discipline, trait) %>%
  summarise(pvalue = t.test(rho ~ GT)$p.value,
            tstatistic = t.test(rho ~ GT)$statistic,
            DF = t.test(rho ~ GT)$parameter)

# wilcoxon test between CAGT and COMPGT for nose that showed non-normality
accuracies %>% 
  filter(SD == 1) %>% 
  filter(trait == "Nose") %>%
  group_by(Discipline) %>%
  summarise(pvalue = wilcox.test(rho ~ GT)$p.value,
            tstatistic = wilcox.test(rho ~ GT)$statistic)

## Plot: accuracy differences between CAGT vs COMPGT by Discipline ####
ann_text  <- data.frame(
  trait = c("Eyes", "Nose", "Mouth", 
            "Eyes", "Nose", "Mouth"),
  Discipline = c("Forensic practitioners", "Forensic practitioners", "Forensic practitioners",
                 "Art students", "Art students", "Art students"),
  rho = c(0.60, 0.75, 0.95,
          0.60, 0.75, 0.95),
  GT = c("CaGT","CaGT", "CaGT",
         "CaGT", "CaGT", "CaGT") )

f <- ggplot(accuracies, aes(x = trait, y = rho, color = GT))+
  geom_boxplot() +
  scale_color_manual(values=c("#454851","#95D7AE")) +
  stat_summary(aes(group = GT),
               fun=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  ylim(-1, 1) + theme_bw()+
  labs(x = "Facial feature", y = "Spearman's rho", color = "Ground truth")+
  geom_text(data = ann_text,label = c("*", "","", "*", "", ""), 
            size = 7, show.legend = FALSE)+
  facet_wrap(~factor(Discipline, c("Forensic practitioners", "Art students")))

e / f + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')
ggsave("./Fig4_groun_truth.png", width = 30, height = 30, units = "cm", dpi = 200)


# AUC #####
# Combining descriptions databases for each trait, because both disciplines
# showed similar results
DFeyes <- cbind(eyesNumP, eyesNumA[,-1])
DFnose <- cbind(noseNumP, noseNumA[,-1])
DFmouth <- cbind(mouthNumP, mouthNumA[,-1])

# Combining descriptions databases with COMPGT 1SD to facilitate Confusion Matrix calculations
eyeCM <- left_join(COM1[,1:2], DFeyes, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"45") %>% 
  mutate(eye_sizeC = factor(eye_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
noseCM <- left_join(COM1[, c(1,3)], DFnose, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"45") %>% 
  mutate(nose_sizeC = factor(nose_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
mouthCM <- left_join(COM1[, c(1,4)], DFmouth, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"45") %>% 
  mutate(mouth_sizeC = factor(mouth_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))

# CONFUSION MATRICES
# sensitivity: true positive rate (TPR) proportion of identified positives
# specificity: true negative rate (NPR) proportion of identified negatives
eCM <- confusionMatrix(eyeCM$eye_sizeC, eyeCM$Classification)
nCM <- confusionMatrix(noseCM$nose_sizeC, noseCM$Classification)
mCM <- confusionMatrix(mouthCM$mouth_sizeC, mouthCM$Classification)

## Plot: confusion matrix, practitioners and artists combined ####
# extracting the CM values as data.frame
CMplot <- function(DF1, DF2, DF3){
  df.table <- rbind(
    as.data.frame(DF1$table), 
    as.data.frame(DF2$table), 
    as.data.frame(DF3$table) )
  df.table$trait <- rep(c("Eyes", "Nose", "Mouth"), each = 9)
  df.table$ID <- seq(1, 27)
  return(df.table)
}

# extracting CM values as percentage of each true category (to be used for coloring)
CMplot_color <- function(DF1, DF2, DF3){
  df.table <- rbind(
    as.data.frame(prop.table(DF1$table, margin = 2)), 
    as.data.frame(prop.table(DF2$table, margin = 2)), 
    as.data.frame(prop.table(DF3$table, margin = 2)) )
  df.table$ID <- seq(1, 27)
  return(df.table)
}

# extracting sensitivity values 
CMplot_sensitivity <- function(DF1, DF2, DF3){
  table_sen <- data.frame(Sensitivity = 
                            c(as.vector(DF1$byClass[,1]), 
                              as.vector(DF2$byClass[,1]), 
                              as.vector(DF3$byClass[,1]) ),
                          GT = rep(1:3, times = 3),
                          Description = "Sensitivity",
                          trait = rep(c("Eyes", "Nose", "Mouth"), each = 3) )
  
  table_sen <- table_sen %>%
    mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))
  return(table_sen)
}


# proper formatting
DFCM <- CMplot(eCM, nCM, mCM)
DFCM_color <- CMplot_color(eCM, nCM, mCM)
DFCM_sens <- CMplot_sensitivity(eCM, nCM, mCM)

df.tables <- left_join(DFCM, DFCM_color[,3:4], by = "ID")
df.tables <- df.tables %>% 
  select(ID, trait, Prediction, Reference, Freq.x, Freq.y) %>%
  dplyr::rename("Freq" = "Freq.x", "Perc" = "Freq.y",
                "Description" = "Prediction", "GT" = "Reference") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))

# actual plot
ggplot() +
  geom_tile(aes(x=Description, y=GT, fill=Perc),
            data=df.tables, linewidth=0.2, color=grey(0.5)) +
  geom_tile(aes(x=Description, y=GT),
            data=df.tables[df.tables$GT==df.tables$Description, ], 
            size=1, color="black", fill = 'transparent') +
  scale_x_discrete(position = "top",  
                   limits = c(unique(df.tables$Description), "Sensitivity")) +
  geom_text(aes(x=Description, y=GT, label=Freq),
            data=df.tables, size=4, colour="black") +
  theme_bw()+
  theme(axis.text = element_text(size = 16),
                   axis.title = element_text(size = 16, face = "bold"))+
  scale_fill_gradient(low="white", high="red",labels = scales::percent,
                      limits = c(0,1), breaks = c(0,0.5,1))+
  facet_grid(~trait)+
  geom_text(data = DFCM_sens, aes(Description, GT, 
                                  label = format(Sensitivity, digits = 2)))

ggsave("./Fig5_confusion_matrix.png", width = 40, height = 18, units = "cm", dpi = 200)

# TREATMENT AND CONTROL GROUPS #####
# DESCRIPTIONS OF PARTICIPANTS WITH NO EXPERIENCE IN FACIAL VARIATION 
descriptionsT <- read.table("descriptions_treatment.txt", header = T, check.names=FALSE)
descriptionsC <- read.table("descriptions_control.txt", header = T, check.names=FALSE)

# Consistency #####
## PAIRWISE INTRA-GROUP CONSISTENCY ####
# Before training
# Treatment
eyes1T <- descriptionsT %>% filter(rep == 1) %>% dplyr::select(contains("Eyes"))
nose1T <- descriptionsT %>% filter(rep == 1) %>% dplyr::select(contains("Nose"))
mouth1T <- descriptionsT %>% filter(rep == 1) %>% dplyr::select(contains("Mouth"))
# Combining datasets
agreement_T_before <- consistency(eyes1T, nose1T, mouth1T)
agreement_T_before$Description <- "Pre" 
agreement_T_before$Type <- "Treatment" 

# Control
eyes1C <- descriptionsC %>% filter(rep == 1) %>% dplyr::select(contains("Eyes"))
nose1C <- descriptionsC %>% filter(rep == 1) %>% dplyr::select(contains("Nose"))
mouth1C <- descriptionsC %>% filter(rep == 1) %>% dplyr::select(contains("Mouth"))
# Combining datasets
agreement_C_before <- consistency(eyes1C, nose1C, mouth1C)
agreement_C_before$Description <- "Pre" 
agreement_C_before$Type <- "Control" 


# After training
# Treatment
eyes2T <- descriptionsT %>% filter(rep == 2) %>% dplyr::select(contains("Eyes"))
nose2T <- descriptionsT %>% filter(rep == 2) %>% dplyr::select(contains("Nose"))
mouth2T <- descriptionsT %>% filter(rep == 2) %>% dplyr::select(contains("Mouth"))
# Combining datasets
agreement_T_after <- consistency(eyes2T, nose2T, mouth2T)
agreement_T_after$Description <- "Post"
agreement_T_after$Type <- "Treatment"

# Control
eyes2C <- descriptionsC %>% filter(rep == 2) %>% dplyr::select(contains("Eyes"))
nose2C <- descriptionsC %>% filter(rep == 2) %>% dplyr::select(contains("Nose"))
mouth2C <- descriptionsC %>% filter(rep == 2) %>% dplyr::select(contains("Mouth"))
# Combining datasets
agreement_C_after <- consistency(eyes2C, nose2C, mouth2C)
agreement_C_after$Description <- "Post"
agreement_C_after$Type <- "Control"

# PAIRWISE COHENS KAPPA pre and post instructions
agreementsTrainig <- rbind(agreement_T_before, agreement_T_after,
                           agreement_C_before, agreement_C_after)

# long format
agreementsTrainigl <- agreementsTrainig %>% 
  pivot_longer(Eyes:Mouth, names_to = "trait", values_to = "k") %>%
  mutate(Description = factor(Description, levels = c("Pre", "Post"))) %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>%
  mutate(Type = factor(Type, levels = c("Treatment", "Control")))


## AGREEMENT DIFFERENCES BETWEEN PRE vs POST TRAINING ####
# descriptive statistics
agreementsTrainigl %>%
  group_by(Type, Description, trait) %>%
  summarise(mean = mean(k)) %>% print(n = 30)

# checking normality; if p>0.5 normal (t-test), if p<non normal (Mann-Whitney)
# Eyes, nose and mouth in Treatment non-normal
agreementsTrainigl %>% 
  group_by(Type, trait) %>%
  summarise(normal_pValue = shapiro.test(k)[2]$p.value)

# wilcoxon test between PRE and POST-Instructions, 
# for all traits in the Treatment group that showed non-normality
agreementsTrainigl %>% filter(Type == "Treatment") %>%
  group_by(trait) %>%
  summarise(pvalue = wilcox.test(k ~ Description)$p.value,
            tstatistic = wilcox.test(k ~ Description)$statistic)

# t-test between PRE and POST-instructions, 
# for all traits in the Control group that showed non-normality
agreementsTrainigl %>% filter(Type == "Control") %>%
  group_by(trait) %>%
  summarise(pvalue = t.test(k ~ Description)$p.value,
            tstatistic = t.test(k ~ Description)$statistic,
            DF = t.test(k ~ Description)$parameter)

## Plot: agreement differences between PRE vs POST by experimental group ####
ann_text  <- data.frame(
  trait = c("Eyes", "Nose", "Mouth", 
            "Eyes", "Nose", "Mouth"),
  Type = c("Treatment", "Treatment", "Treatment",
           "Control", "Control", "Control"),
  k = c(0.65, 0.65, 0.7,
        0.65, 0.65, 0.7),
  Description = c("Pre","Pre", "Pre",
                  "Post", "Post", "Post") )

g <- ggplot(agreementsTrainigl, aes(x = trait, y = k, color = Description))+
  geom_boxplot() +
  scale_color_manual(values=c("#8B9EB7","#F49D37")) +
  stat_summary(aes(group = Description),
               fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75))+  
  ylim(-1, 1) + theme_bw()+
  labs(x = "Facial feature", y = "Cohen's Kappa", color = "Description")+
  geom_text(data = ann_text,label = c("*", "*","", "*", "", ""), 
            size = 7, colour = "black")+
  facet_grid(~as.factor(Type))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Accuracy #####
## RELATIONSHIP BETWEEN GT AND QUALITATIVE DESCRIPTIONS ####
# Transforming size categories in description databases into numeric variables 
# Treatment
eyesNum1T <- description_TO_numeric(eyes1T)
eyesNum2T <- description_TO_numeric(eyes2T)
noseNum1T <- description_TO_numeric(nose1T)
noseNum2T <- description_TO_numeric(nose2T)
mouthNum1T <- description_TO_numeric(mouth1T)
mouthNum2T <- description_TO_numeric(mouth2T)

# Control
eyesNum1C <- description_TO_numeric(eyes1C)
eyesNum2C <- description_TO_numeric(eyes2C)
noseNum1C <- description_TO_numeric(nose1C)
noseNum2C <- description_TO_numeric(nose2C)
mouthNum1C <- description_TO_numeric(mouth1C)
mouthNum2C <- description_TO_numeric(mouth2C)

# accuracy using CompGT 1SD and 2SD, for the Treatment group, before and after training
accuracy_T_before_Comp <- accuracy_GT(eyesNum1T, noseNum1T, mouthNum1T, COM1, COM2)
accuracy_T_before_Comp$Description <- "Pre"
accuracy_T_before_Comp$GT <- "CompGT"
accuracy_T_before_Comp$Type <- "Treatment"

accuracy_T_after_Comp <- accuracy_GT(eyesNum2T, noseNum2T, mouthNum2T, COM1, COM2)
accuracy_T_after_Comp$Description <- "Post"
accuracy_T_after_Comp$GT <- "CompGT"
accuracy_T_after_Comp$Type <- "Treatment"

# accuracy using CompGT 1SD and 2SD, for the Control group, before and after training
accuracy_C_before_Comp <- accuracy_GT(eyesNum1C, noseNum1C, mouthNum1C, COM1, COM2)
accuracy_C_before_Comp$Description <- "Pre"
accuracy_C_before_Comp$GT <- "CompGT"
accuracy_C_before_Comp$Type <- "Control"

accuracy_C_after_Comp <- accuracy_GT(eyesNum2C, noseNum2C, mouthNum2C, COM1, COM2)
accuracy_C_after_Comp$Description <- "Post"
accuracy_C_after_Comp$GT <- "CompGT"
accuracy_C_after_Comp$Type <- "Control"

# long format
accuraciesTrainingl <- rbind(accuracy_T_before_Comp, accuracy_T_after_Comp,
                     accuracy_C_before_Comp, accuracy_C_after_Comp) %>% 
  pivot_longer(Eyes1:Mouth2, names_to = "trait", values_to = "rho") %>%
  separate(trait, into = c("trait", "SD"), "(?<=[a-z])(?=[0-9])") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth"))) %>%
  mutate(SD = factor(SD, levels = c("1", "2"))) %>% 
  mutate(Type = factor(Type, levels = c("Treatment", "Control"))) %>%
  mutate(Description = factor(Description, levels = c("Pre", "Post")))

## ACCURACY DIFFERENCES BETWEEN 1SD vs 2SD ####
# descriptive statistics 
accuraciesTrainingl %>%
  group_by(Type, Description, SD, trait) %>%
  summarise(media = mean(rho)) %>% print(n = 30)

# checking normality, if p>0.5 normal (t-test), if p<non normal (Mann-Whitney)
# Nose normal, eyes and mouth non-normal, 
accuraciesTrainingl %>%
  group_by(Type, Description, trait) %>%
  summarise(normal_pValue = shapiro.test(rho)[2]$p.value) %>% print(n = 30)

# t-test between 1SD and 2SD for Nose that showed normality
accuraciesTrainingl %>% filter(trait == "Nose") %>%
  group_by(Type, Description, trait) %>%
  summarise(pvalue = t.test(rho ~ SD)$p.value,
            tstatistic = t.test(rho ~ SD)$statistic,
            DF = t.test(rho ~ SD)$parameter)

# wilcoxon test 1SD and 2SD, for eyes and mouth that showed non-normality
accuraciesTrainingl %>% filter(trait %in% c("Eyes", "Mouth") ) %>%
  group_by(Type, Description, trait) %>%
  summarise(pvalue = wilcox.test(rho ~ SD)$p.value,
            tstatistic = wilcox.test(rho ~ SD)$statistic)

## Plot supp figure: accuracy differences 1SD vs 2SD by experimental group and PRE-POST-instructions####
ggplot(accuraciesTrainingl, aes(x = trait, y = rho, color = SD))+
  geom_boxplot() +
  scale_color_manual(values=c("#454851","grey")) +  #"#454851","#95D7AE", "#EEB868"
  stat_summary(aes(group = SD),
               fun=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  ylim(-1, 1) + theme_bw()+ facet_grid(vars(Description), vars(Type))+
  labs(x = "Facial feature", y = "Spearman's rho", color = "Threshold")
ggsave("./Fig2sup_training-rho.png", width = 25, height = 18, units = "cm", dpi = 150)

## ACCURACY DIFFERENCES BETWEEN PRE vs POST FOR 1SD ####
# Keeping 1SD only because it showed better performance
accuraciesTrainingl %>% filter(SD == 1) %>%
  group_by(Type, Description, trait) %>%
  summarise(mean = mean(rho))

# checking normality; if p>0.5 normal (t-test), if p<non normal (Mann-Whitney)
# Eyes and nose normal, mouth non-normal
accuraciesTrainingl %>% filter(SD == 1) %>%
  group_by(Type, trait) %>%
  summarise(normal = shapiro.test(rho)[2]$p.value)

# t-test between PRE and POST for eyes and nose that showed normality
accuraciesTrainingl %>% filter(SD == 1) %>%
  filter(trait != "Mouth") %>%
  group_by(Type, trait) %>%
  summarise(pvalue = t.test(rho ~ Description)$p.value,
            tstatistic = t.test(rho ~ Description)$statistic,
            DF = t.test(rho ~ Description)$parameter)

# wilcoxon test between PRE and POST for mouth that showed non-normality
accuraciesTrainingl %>% filter(SD == 1) %>%
  filter(trait == "Mouth") %>%
  group_by(Type) %>%
  summarise(pvalue = wilcox.test(rho ~ Description)$p.value,
            tstatistic = wilcox.test(rho ~ Description)$statistic)

## Plot: accuracy differences PRE and POST by experimental group####
h <- accuraciesTrainingl %>% filter(SD == 1) %>%
  ggplot(aes(x = trait, y = rho, color = Description))+
  geom_boxplot() + ylim(-0.1, 0.8) + 
  scale_color_manual(values=c("#8B9EB7","#F49D37")) +
  stat_summary(aes(group = Description),
               fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black",
               position=position_dodge(width=.75)) +
  ylim(-1, 1) + theme_bw()+ facet_grid(~Type)+
  labs(x = "Facial feature", y = "Spearman's rho", color = "Description")

g / h + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')
ggsave("./Fig6_Training_consistency_accuracy.png", width = 22, height = 18, units = "cm", dpi = 150)


# AUC #####
# The following calculations use CompGT 1SD as ground truth
# Combining descriptions databases with COMPGT 1SD
# Treatment PRE-instructions
eyeCM_1T <- left_join(COM1[, 1:2], eyesNum1T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(eye_sizeC = factor(eye_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
noseCM_1T <- left_join(COM1[, c(1,3)], noseNum1T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(nose_sizeC = factor(nose_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
mouthCM_1T <- left_join(COM1[, c(1,4)], mouthNum1T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(mouth_sizeC = factor(mouth_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
# Treatment POST-instructions
eyeCM_2T <- left_join(COM1[, 1:2], eyesNum2T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(eye_sizeC = factor(eye_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
noseCM_2T <- left_join(COM1[, c(1,3)], noseNum2T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(nose_sizeC = factor(nose_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
mouthCM_2T <- left_join(COM1[, c(1,4)], mouthNum2T, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"35") %>% 
  mutate(mouth_sizeC = factor(mouth_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))

# Controls PRE-instructions
eyeCM_1C <- left_join(COM1[, 1:2], eyesNum1C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(eye_sizeC = factor(eye_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
noseCM_1C <- left_join(COM1[, c(1,3)], noseNum1C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(nose_sizeC = factor(nose_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
mouthCM_1C <- left_join(COM1[, c(1,4)], mouthNum1C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(mouth_sizeC = factor(mouth_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
# Controls POST-instructions
eyeCM_2C <- left_join(COM1[, 1:2], eyesNum2C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(eye_sizeC = factor(eye_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
noseCM_2C <- left_join(COM1[, c(1,3)], noseNum2C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(nose_sizeC = factor(nose_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))
mouthCM_2C <- left_join(COM1[, c(1,4)], mouthNum2C, by = "Folio_num") %>%
  gather("Participant", "Classification", "1":"19") %>% 
  mutate(mouth_sizeC = factor(mouth_sizeC, levels = c("1", "2", "3")),
         Classification = factor(Classification, levels = c("1", "2", "3")))

# CONFUSION MATRICES
# sensitivity: true positive rate (TPR) proportion of identified positives
# specificity: true negative rate (NPR) proportion of identified negatives
# Treatment pre-instructions
eCM1T <- confusionMatrix(eyeCM_1T$eye_sizeC, eyeCM_1T$Classification)
nCM1T <- confusionMatrix(noseCM_1T$nose_sizeC, noseCM_1T$Classification)
mCM1T <- confusionMatrix(mouthCM_1T$mouth_sizeC, mouthCM_1T$Classification)
# Treatment post-instructions
eCM2T <- confusionMatrix(eyeCM_2T$eye_sizeC, eyeCM_2T$Classification)
nCM2T <- confusionMatrix(noseCM_2T$nose_sizeC, noseCM_2T$Classification)
mCM2T <- confusionMatrix(mouthCM_2T$mouth_sizeC, mouthCM_2T$Classification)
# Control pre-instructions
eCM1C <- confusionMatrix(eyeCM_1C$eye_sizeC, eyeCM_1C$Classification)
nCM1C <- confusionMatrix(noseCM_1C$nose_sizeC, noseCM_1C$Classification)
mCM1C <- confusionMatrix(mouthCM_1C$mouth_sizeC, mouthCM_1C$Classification)
# Control post-instructions
eCM2C <- confusionMatrix(eyeCM_2C$eye_sizeC, eyeCM_2C$Classification)
nCM2C <- confusionMatrix(noseCM_2C$nose_sizeC, noseCM_2C$Classification)
mCM2C <- confusionMatrix(mouthCM_2C$mouth_sizeC, mouthCM_2C$Classification)


## Plot: confusion matrices PRE and POST, by experimental group ####
# TREATMENT PRE-instructions
# proper formatting
DFCM <- CMplot(eCM1T, nCM1T, mCM1T)
DFCM_color <- CMplot_color(eCM1T, nCM1T, mCM1T)
DFCM_sens <- CMplot_sensitivity(eCM1T, nCM1T, mCM1T)

df.tables <- left_join(DFCM, DFCM_color[,3:4], by = "ID")%>% 
  select(ID, trait, Prediction, Reference, Freq.x, Freq.y) %>%
  dplyr::rename("Freq" = "Freq.x", "Perc" = "Freq.y",
                "Description" = "Prediction", "GT" = "Reference") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))

k <- ggplot() +
  geom_tile(aes(x=Description, y=GT, fill=Perc),
            data=df.tables, linewidth=0.2, color=grey(0.5)) +
  geom_tile(aes(x=Description, y=GT),
            data=df.tables[df.tables$GT==df.tables$Description, ], 
            size=1, color="black", fill = 'transparent') +
  scale_x_discrete(position = "top",  
                   limits = c(unique(df.tables$Description), "Sensitivity")) +
  geom_text(aes(x=Description, y=GT, label=Freq),
            data=df.tables, size=4, colour="black") +
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"))+
  scale_fill_gradient(low="white", high="red",labels = scales::percent,
                      limits = c(0,1), breaks = c(0,0.5,1))+
  facet_grid(~trait) +
  geom_text(data = DFCM_sens, aes(Description, GT, 
                                  label = format(Sensitivity, digits = 2)))


# TREATMENT POST-instructions
# proper formatting
DFCM <- CMplot(eCM2T, nCM2T, mCM2T)
DFCM_color <- CMplot_color(eCM2T, nCM2T, mCM2T)
DFCM_sens <- CMplot_sensitivity(eCM2T, nCM2T, mCM2T)

df.tables <- left_join(DFCM, DFCM_color[,3:4], by = "ID")%>% 
  select(ID, trait, Prediction, Reference, Freq.x, Freq.y) %>%
  dplyr::rename("Freq" = "Freq.x", "Perc" = "Freq.y",
                "Description" = "Prediction", "GT" = "Reference") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))

l <- ggplot() +
  geom_tile(aes(x=Description, y=GT, fill=Perc),
            data=df.tables, linewidth=0.2, color=grey(0.5)) +
  geom_tile(aes(x=Description, y=GT),
            data=df.tables[df.tables$GT==df.tables$Description, ], 
            size=1, color="black", fill = 'transparent') +
  scale_x_discrete(position = "top",  
                   limits = c(unique(df.tables$Description), "Sensitivity")) +
  geom_text(aes(x=Description, y=GT, label=Freq),
            data=df.tables, size=4, colour="black") +
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.x=element_blank())+
  scale_fill_gradient(low="white", high="red",labels = scales::percent,
                      limits = c(0,1), breaks = c(0,0.5,1))+
  facet_grid(~trait) +
  geom_text(data = DFCM_sens, aes(Description, GT, 
                                  label = format(Sensitivity, digits = 2)))

k / l + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')
ggsave("./Fig3sup_CM_pre_post.png", width = 22, height = 18, units = "cm", dpi = 150)

# CONTROL PRE-instructions
# proper formatting
DFCM <- CMplot(eCM1C, nCM1C, mCM1C)
DFCM_color <- CMplot_color(eCM1C, nCM1C, mCM1C)
DFCM_sens <- CMplot_sensitivity(eCM1C, nCM1C, mCM1C)

df.tables <- left_join(DFCM, DFCM_color[,3:4], by = "ID")%>% 
  select(ID, trait, Prediction, Reference, Freq.x, Freq.y) %>%
  dplyr::rename("Freq" = "Freq.x", "Perc" = "Freq.y",
                "Description" = "Prediction", "GT" = "Reference") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))

m <- ggplot() +
  geom_tile(aes(x=Description, y=GT, fill=Perc),
            data=df.tables, linewidth=0.2, color=grey(0.5)) +
  geom_tile(aes(x=Description, y=GT),
            data=df.tables[df.tables$GT==df.tables$Description, ], 
            size=1, color="black", fill = 'transparent') +
  scale_x_discrete(position = "top",  
                   limits = c(unique(df.tables$Description), "Sensitivity")) +
  geom_text(aes(x=Description, y=GT, label=Freq),
            data=df.tables, size=4, colour="black") +
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"))+
  scale_fill_gradient(low="white", high="red",labels = scales::percent,
                      limits = c(0,1), breaks = c(0,0.5,1))+
  facet_grid(~trait) + 
  geom_text(data = DFCM_sens, aes(Description, GT, 
                                  label = format(Sensitivity, digits = 2)))


# TREATMENT POST-instructions
# proper formatting
DFCM <- CMplot(eCM2C, nCM2C, mCM2C)
DFCM_color <- CMplot_color(eCM2C, nCM2C, mCM2C)
DFCM_sens <- CMplot_sensitivity(eCM2C, nCM2C, mCM2C)

df.tables <- left_join(DFCM, DFCM_color[,3:4], by = "ID")%>% 
  select(ID, trait, Prediction, Reference, Freq.x, Freq.y) %>%
  dplyr::rename("Freq" = "Freq.x", "Perc" = "Freq.y",
                "Description" = "Prediction", "GT" = "Reference") %>%
  mutate(trait = factor(trait, levels = c("Eyes", "Nose", "Mouth")))

n <- ggplot() +
  geom_tile(aes(x=Description, y=GT, fill=Perc),
            data=df.tables, linewidth=0.2, color=grey(0.5)) +
  geom_tile(aes(x=Description, y=GT),
            data=df.tables[df.tables$GT==df.tables$Description, ], 
            size=1, color="black", fill = 'transparent') +
  scale_x_discrete(position = "top",  
                   limits = c(unique(df.tables$Description), "Sensitivity")) +
  geom_text(aes(x=Description, y=GT, label=Freq),
            data=df.tables, size=4, colour="black") +
  theme_bw()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.x=element_blank())+
  scale_fill_gradient(low="white", high="red",labels = scales::percent,
                      limits = c(0,1), breaks = c(0,0.5,1))+
  facet_grid(~trait) +
  geom_text(data = DFCM_sens, aes(Description, GT, 
                                  label = format(Sensitivity, digits = 2)))


m / n + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')
ggsave("./Fig4sup_CM_pre_post.png", width = 22, height = 18, units = "cm", dpi = 150)
