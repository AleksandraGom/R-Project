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


