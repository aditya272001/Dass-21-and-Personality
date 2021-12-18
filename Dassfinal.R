##packages##
library(bootnet)
library(tidyverse)
library(qgraph)
library(NetworkComparisonTest)
library(ggplot2)

##DATA_READING##
DATA <- read.csv("D:/_DASS_/dass21.csv", header = T, sep = ",")

##DATA_CLEANING 
DATA <- filter(DATA, VCL3 == "0", VCL6 == "0", VCL12 == "0", age > 18, age < 90, education > 0, urban > 0, gender > 0)
write.csv(DATA, "D:/_DASS_/dass21cleaned.csv")

#Loading_cleaned_data set
DATAcleaned <- read.csv("D:/_DASS_/dass21cleaned.csv")


##Demographic_Characteristics##
table(DATA$gender)
table(DATA$education)
table(DATA$urban)
table(DATA$race)
table(DATA$country)
summary(DATA$age)
sd(DATA$age)
hist(DATA$age)

##MALE_FEMALE_SEPARATE 
MALE <- filter(DATA, gender == "1")
FEMALE <- filter(DATA, gender == "2")
write.csv(MALE, "D:/_DASS_/MALE.csv")
write.csv(FEMALE, "D:/_DASS_/FEMALE.csv")

##CRONBACH-ALPHA##
cronbach.alpha(DATA[ ,c(1, 6, 8, 11, 12, 14, 18)])
cronbach.alpha(DATA[ ,c(2, 4, 7, 9, 15, 19, 20)])
cronbach.alpha(DATA[ ,c(3, 5, 10, 13, 16, 17, 21)])

##Networks 1.1 and 1.2 (Male and Female)##
MALE1 <- read.csv("D:/_DASS_/MALE.csv")
FEMALE1 <- read.csv("D:/_DASS_/FEMALE.csv")

MALENetwork1 <- estimateNetwork(MALE1[,c(2:22, 39:43)], default = c("EBICglasso"), corMethod = c("cor_auto"))
MALENetwork1graph <- MALENetwork1$graph

FEMALENetwork1 <- estimateNetwork(FEMALE1[,c(2:22, 39:43)], default = c("EBICglasso"), corMethod = c("cor_auto"))
FEMALENetwork1graph <- FEMALENetwork1$graph

###In-varirance
NCT1 <- NCT(MALENetwork1, FEMALENetwork1, it = 1000, test.edges = F, edges = "all")
summary(NCT1)
plot(NCT1, c("network"))
write.csv(NCT1, "D:/_DASS_/NCT1.csv")

##exporting_correlation_LASSO_tables_network_1
setwd("D:/_DASS_")
write.csv(MALENetwork1graph, "D:/_DASS_/MaleNetwork1.csv")
write.csv(FEMALENetwork1graph, "D:/_DASS_/FemaleNetwork1.csv")

##Degree-centrality
setwd("D:/_DASS_")
pdf("CNMP.pdf", paper = "USr", height = 12, width = 16)
plotdc <- ggplot(CNMF, aes(x = personality_trait, y = degree, group = network)) + 
  geom_line(aes(color = network), size = 2) + 
  geom_point(aes(color = network), size = 3) + 
  ylim(0.4 , 1.5) + labs(title = "Degree Centrality of Personality traits in male and female network") + 
  ylab("Strength Centrality") + xlab("Personality traits")
dev.off()

##Average_shortest_path_length 
##MALE
MALEASPL <- centrality(MALENetwork1graph)$ShortestPathLengths
CLEANASPLMALE <- MALEASPL[upper.tri(MALEASPL)]
MEANMALEASPL <- mean(CLEANASPLMALE)
MEANMALEASPL

##FEMALE
FEMALEASPL <- centrality(FEMALENetwork1graph)$ShortestPathLengths
CLEANASPLFEMALE <- FEMALEASPL[upper.tri(FEMALEASPL)]
MEANFEMALEASPL <- mean(CLEANASPLFEMALE)
MEANFEMALEASPL

##PLOTTING_NETWORKS
##creating the names## 
short_names <- c("wind_down", "dry_mouth", "ce_positive_f", 
                 "br_diff", "diff_initiative", "over-react",
                 "trembling", "felt_nervous", "worried",
                 "no_look_forw^", "agitated", "diff_relax", "felt_blue",
                 "intolerant", "panic", "no_enthusiasm", "unworthy",
                 "touchy", "heartaware", "scared", "meaningless",
                 "Ext^", "Agr^", "Cons^", "Emo^", "Opn^")


mynames <- c("hard to wind_down", 
             "dryness of mouth",
             "no positive feeling", 
             "experienced_difficulty_breating",
             "difficult to work up initiative", 
             "tend to over-react", 
             "experienced trembling",
             "felt nervous", 
             "worried about situations",
             "nothing to look forward", 
             "found myself agitated", 
             "diffcult to relax", 
             "downhearted and blue", 
             "intolerant of anything", 
             "felt close to_panic", 
             "no_enthusiasm", 
             "felt_worthless",
             "touchy", 
             "heartaware", 
             "felt scared", 
             "life is meaningless", 
             "Extraversion",
             "Agreeableness", 
             "Conscientiousness", 
             "Emotional Stability",
             "Openness to new Experiences") 

groups_analytica <- list( "Stress" = c(1, 6, 8, 11, 12, 14, 18),
                          "Anxiety" = c(2, 4, 7, 9, 15, 19, 20),
                          "Depression" = c(3, 5, 10, 13, 16, 17, 21),
                          "personality" = c(22, 23, 24, 25, 26))

##nice-color-pallete##
group_analytics_colors <- c("#E8ED61", "#FFC237", "#A2CDCD",
                            "#FFE1AF")


#plotting_male_network_1
setwd("D:/_DASS_") 



pdf("MALENetwork1.pdf", width = 16, height = 9, paper = "USr")

plotmalenetwork1 <- qgraph(MALENetwork1graph, layout = "spring",
                           theme = "Hollywood",
                           labels = short_names,
                           nodeNames = mynames,
                           vsize = 6,
                           edge.labels = FALSE,
                           legend = TRUE, 
                           legend.cex = 0.4,
                           groups = groups_analytica,
                           color = group_analytics_colors,
                           cut = 0.05,
                           minimum = 0.05, maximum = 1, details = TRUE, 
                           title = "DASS-21 and Ten Item Personality Inventory Male", curve = .2, 
                           curveAll = T)

dev.off()

##plotting_female_network_1
setwd("D:/_DASS_") 
L <- averageLayout(MALENetwork1graph, FEMALENetwork1graph)


pdf("FEMALENetwork_1.pdf", width = 16, height = 9, paper = "USr")

plotfemalenetwork1 <- qgraph(FEMALENetwork1graph, layout = L,
                             theme = "Hollywood",
                             labels = short_names,
                             nodeNames = mynames,
                             vsize = 6,
                             edge.labels = FALSE,
                             legend = TRUE, 
                             legend.cex = 0.4,
                             groups = groups_analytica,
                             color = group_analytics_colors,
                             cut = 0.05,
                             minimum = 0.05, maximum = 1, details = TRUE, 
                             title = "DASS-21 and Ten Item Personality Inventory Female", curve = .2, 
                             curveAll = T)

dev.off()

##Networks 2.1 and 2.2 (Male and Female)##
names(MALE1)[names(MALE1) == 'ï..Q1A'] <- 'Q1A' 
names(FEMALE1)[names(FEMALE1) == 'ï..Q1A'] <- 'Q1A'

MALE1 <- mutate(MALE1, Stress = Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A,
                Anxiety = Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A,
                Depression = Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A)
FEMALE1 <- mutate(FEMALE1, Stress = Q1A + Q6A + Q8A + Q11A + Q12A + Q14A + Q18A,
                  Anxiety = Q2A + Q4A + Q7A + Q9A + Q15A + Q19A + Q20A,
                  Depression = Q3A + Q5A + Q10A + Q13A + Q16A + Q17A + Q21A)


MALENetwork2 <- estimateNetwork(MALE1[ , c(39:43, 47:49)], default = "EBICglasso", corMethod = c("spearman"))
MALENetwork2graph <- MALENetwork2$graph

FEMALENetwork2 <- estimateNetwork(FEMALE1[ ,c(39:43, 47:49)], default = "EBICglasso", corMethod = c("spearman"))
FEMALENetwork2graph <- FEMALENetwork2$graph 

##Network invariance test##

NCT2 <- NCT(MALENetwork2, FEMALENetwork2, it = 1000, edges = "all")
summary(NCT2)

write.csv(NCT2, "D:/_DASS_/NCT2.csv")

##importing_network_correaltions
write.csv(MALENetwork2graph, "D:/_DASS_/MaleNetwork2.csv")
write.csv(FEMALENetwork2graph, "D:/_DASS_/FemaleNetwork2.csv")

#Short_names_and_plotting_network2 

short_names2 <- c("Ext^", "Agr^", "Cons^", "Emo^", "Opn^",
                  "Stress", "Anxiety", "Dep^")

mynames2 <- c("Extraversion",
              "Agreeableness", 
              "Conscientiousness", 
              "Emotional Stablity",
              "Openness to new Experience",
              "Stress", "Anxiety", "Depression") 

groups_analytica2 <- list( "Personality" = c(1, 2, 3, 4, 5),
                           "DASS-21" = c(6, 7, 8))
##nice-color-pallet##
group_analytics_colors2 <- c("#E8ED61", "#FFC237")

##plotting graphs
L2 <- averageLayout(MALENetwork2graph, FEMALENetwork2graph)
pdf("NETWORK2MALE.pdf", width = 16, height = 9, paper = "USr")

plotnetwork2m <- qgraph(MALENetwork2graph, layout = L2,
                        theme = "Hollywood",
                        labels = short_names2,
                        nodeNames = mynames2,
                        vsize = 6,
                        esize = 25,
                        edge.labels = FALSE,
                        legend = TRUE, 
                        legend.cex = 0.5,
                        groups = groups_analytica2,
                        color = group_analytics_colors2,
                        cut = 0.05,
                        minimum = 0.05, maximum = 1, details = F, 
                        title = "DASS-21 and Ten Item Personality Inventory Male", fade = F, curve = .3, curveAll = T)


dev.off()


pdf("NETWORK2FEMALE.pdf", width = 16, height = 9, paper = "USr")

plotnetwork2f <- qgraph(FEMALENetwork2graph, layout = L2,
                        theme = "Hollywood",
                        labels = short_names2,
                        nodeNames = mynames2,
                        vsize = 6,
                        esize = 25,
                        edge.labels = FALSE,
                        legend = TRUE, 
                        legend.cex = 0.5,
                        groups = groups_analytica2,
                        color = group_analytics_colors2,
                        cut = 0.05,
                        minimum = 0.05, maximum = 1, details = F, 
                        title = "DASS-21 and Ten Item Personality Inventory Female", fade = F, curve = .3, curveAll = T)


dev.off()

#############################################################################
####_BOOTSTRAPPING_ALL_NETWORKS_AND_CENTRALITY_NETWORK_1(Male and female)####
#############################################################################
setwd("D:/_DASS_")
boot_1_network_1_male <- bootnet(MALENetwork1, boots = 1000, nCores = 4, statistics = c("edge", "strength"))
boot_2_network_1_male <- bootnet(MALENetwork1, boots = 1000, type = "case", nCores = 4, statistics = c("strength", "edge"))
corStability(boot_2_network_1_male)

boot_1_network_1_female <- bootnet(FEMALENetwork1, boots = 1000, nCores = 4, statistics = c("strength", "edge"))
boot_2_network_1_female <- bootnet(FEMALENetwork1, boots = 1000, nCores = 4, type = "case", statistics = c("strength", "edge"))
corStability(boot_2_network_1_female)
plot(boot_2_network_1_male, labels = F, order = "sample")

boot_1_network_2_male <- bootnet(MALENetwork2, boots = 1000, nCores = 4, statistics = "edge")
boot_2_network_2_male <- bootnet(MALENetwork2, boots = 1000, nCores = 4, type = "case", statistics = "edge")
corStability(boot_2_network_2_male)
plot(boot_1_network_2_male, labels = F, order = "sample")

boot_1_network_2_female <- bootnet(FEMALENetwork2, boots = 1000, nCores = 4, statistics = "edge")
boot_2_network_2_female <- bootnet(MALENetwork2, boots = 1000, nCores = 4, type = "case", statistics = "edge")
corStability(boot_2_network_2_female)
plot(boot_1_network_2_female)

save(boot_1_network_1_male, file = "b1n1m.Rdata")
save(boot_2_network_1_male, file = "b2n1m.Rdata")
save(boot_1_network_1_female, file = "b1n1f.Rdata")
save(boot_2_network_1_female, file = "b2n1f.Rdata")
save(boot_1_network_2_male, file = "b1n2m.Rdata")
save(boot_2_network_2_male, file = "b2n2m.Rdata")
save(boot_1_network_2_female, file = "b1n2f.Rdata")
save(boot_2_network_2_female, file = "b2n2f.Rdata")  