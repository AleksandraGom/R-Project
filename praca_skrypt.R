#Plik zawiera jedynie kod wykorzystany w analizie.
#Proszę o zapoznanie się z dodatkowymi plikami praca_skrypt.Rmd oraz praca_skrypt.html, które
#zawierają opis outputów i ostateczne podsumowanie.

library("ggplot2")
library("ggpubr")
library("nortest")
library("tidyverse")
library("scatterplot3d")
library("emmeans")
library("effects")
library("lme4")
library("lmerTest")
library("car")
library("openxlsx")
library("devEMF")
library("RColorBrewer")
library("glue")
library("ggtext")
library("dunn.test")
library("FSA")
library("rcompanion")


#Przeprowadzono analizę ekspresji 8 efryn, oznaczonych kolejno: efna1, efna2, efna3, efna4, efna5, efnb1, efnb2, efnb3 oraz 11 receptorów efryn, oznaczonych kolejno epha1, epha2, epha3, epha4, epha5, epha7, ephb1, ephb2, ephb3, ephb4, ephb6.
#Analizę wykonano dla 70 próbek odpowiadających trzem typom stanów chorobowych: 45 czerniaków pierwotnych, 18 łagodnych znamion i 7 normalnych tkanek skóry.
#W celu porównania wyników w różnych grupach (w próbkach odpowiadających różnym stanom chorobowym) wybrano nieparametryczny test statystyczny Kruskala-Wallisa oraz test post-hoc Dunna.


# Efryny - analiza ekspresji ##################################################

## A1 ##################################################


data <- read.xlsx("A1.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")


sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2#
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(text = element_text(size = 14)) +
  labs(x = "", y = "Względny poziom ekspresji EFNA1 (log2)") + 
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("normal\n(n={normal_n})"), glue("benign nevi\n(n={benignNevi_n})"), glue("malignant melanoma\n (n={malignantMelanoma_n})"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot + scale_y_continuous(limits = c(0,11), breaks = c(1,2,3,4,5,6,7,8,9,10))



data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) + 
  labs(x = "", y = "Względny poziom ekspresji  EFNA1") + 
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


hist <- ggplot(data, aes(x = NormLog2_value)) +
  geom_histogram(binwidth=0.7, colour="black", fill="grey") +  
  facet_wrap(~State,scales="fixed")+
  theme_classic()+ xlab("Względny poziom ekspresji")

hist

hist_norm <- ggplot(data_norm, aes(x = NormLog2_value)) +
  geom_histogram(binwidth=0.7, colour="black", fill="grey") + 
  theme_classic()+ xlab("Względny poziom ekspresji")

hist_norm

ggplot(data_norm, aes(x = NormLog2_value)) + geom_histogram(aes(y=..density..), colour="black", fill="skyblue", binwidth = 10) + geom_density(color="red", size=1)

ggplot(data_beni, aes(x = NormLog2_value)) + geom_histogram(aes(y=..density..), colour="black", fill="skyblue", binwidth = 10) + geom_density(color="red", size=1)

ggplot(data_mela, aes(x = NormLog2_value)) + geom_histogram(aes(y=..density..), colour="black", fill="skyblue", binwidth = 10) + geom_density(color="red", size=1)


#Sprawdzenie założeń - wykresy diagnostyczne
  

hist(data_norm$Value)
hist(data_beni$Value)
hist(data_mela$Value)

ggdensity(data_norm$Value)
ggdensity(data_beni$Value)
ggdensity(data_mela$Value)


ggqqplot(data_norm$Value)
ggqqplot(data_beni$Value)
ggqqplot(data_mela$Value)


plot(data_aov,1)
plot(data_aov,2)
plot(data_aov,4)

shapiro.test(data_norm$Value)
shapiro.test(data_beni$Value)
shapiro.test(data_mela$Value)


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa.


kruskal.test(NormLog2_value ~ State, data = data)

kruskal.test(data$NormLog2_value ~ data$State)

dunnTest(data$NormLog2_value, data$State, method = "bonferroni")

dunnTest(data$NormLog2_value, data$State, method = "bh")

pairwise.wilcox.test(data$NormLog2_value, data$State, p.adj = "bonferroni")



## A2 ##################################################


data <- read.xlsx("A2.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNA2") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))



#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)



## A3 ##################################################


data <- read.xlsx("A3.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNA3") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc

dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## A4 ##################################################


data <- read.xlsx("A4.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNA4") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))



#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## A5 ##################################################


data <- read.xlsx("A5.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNA5") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")
 


## B1 ##################################################


data <- read.xlsx("B1.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")


sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNB1") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")


## B2 ##################################################


data <- read.xlsx("B2.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNB2") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")
 


## B3 ##################################################


data <- read.xlsx("B3.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EFNB3") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)



# Receptory - analiza ekspresji ##################################################

## A1 ##################################################


data <- read.xlsx("A1R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA1") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## A2 ##################################################


data <- read.xlsx("A2R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA2") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")


## A3 ##################################################


data <- read.xlsx("A3R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")


sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA3") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## A4 ##################################################


data <- read.xlsx("A4R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")


sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA4") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## A5 ##################################################


data <- read.xlsx("A5R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA5") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)



## A7 ##################################################


data <- read.xlsx("A7R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHA7") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)



## B1 ##################################################


data <- read.xlsx("B1R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")


sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHB1") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")
 


## B2 ##################################################


data <- read.xlsx("B2R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHB2") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## B3 ##################################################


data <- read.xlsx("B3R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHB3") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")
 


## B4 ##################################################


data <- read.xlsx("B4R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHB4") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")



## B6 ##################################################


data <- read.xlsx("B6R.xlsx")

Order <- factor(data$State, levels=c("normal","benign nevi","malignant melanoma"))
color <- c("white", "gray81", "gray52")
Factor <- c("normal", "benign nevi", "malignant")



sample_count <- data %>%
  count(State)

normal_n <- sample_count %>%
  filter(State == "normal") %>%
  pull(n)

benignNevi_n <- sample_count %>%
  filter(State == "benign nevi") %>%
  pull(n)

malignantMelanoma_n <- sample_count %>%
  filter(State== "malignant melanoma") %>%
  pull(n)


#Statystyka graficzna z normalizacją Log2
  

data$NormLog2_value = log2(data$Value)

data_boxplot <- ggplot(data = data, mapping = aes(x = Order, y = NormLog2_value, fill = State)) + theme_classic() + geom_boxplot(alpha = 0.3) + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title.y = element_text(size = 20)) +
  labs(x = "", y = "Względny poziom ekspresji  EPHB6") +
  theme(legend.position="none") + scale_fill_manual(breaks = Factor, values = color) + theme(panel.border = element_rect(color = "black",fill = NA, size = 1)) + scale_x_discrete(breaks=c("normal", "benign nevi", "malignant melanoma"),labels=c(glue("skóra"), glue("znamię"), glue("czerniak"))) + stat_boxplot(geom = "errorbar",width = 0.4) + geom_boxplot(outlier.color = "black", outlier.shape = 1)
data_boxplot


#Statystyka opisowa
  

data %>% group_by(State) %>%
  summarize(mean = mean(NormLog2_value), median = median(NormLog2_value),
            maximum = max(NormLog2_value), minimum = min(NormLog2_value),
            sd = sd(NormLog2_value), N = length(NormLog2_value))


#Test nieparametryczny
#Z uwagi na małą liczbę obserwacji do analizy istotności wyników wykorzystano test nieparametryczny Kruskala-Wallisa .

kruskal.test(data$NormLog2_value ~ data$State)


#Testy post-hoc
  
dunnTest(data$NormLog2_value, data$State, method = "bonferroni")





