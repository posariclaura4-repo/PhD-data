#working directory
setwd("C:/Users/Laura/DOKTORAT/Norveska/Disertacija/R_project")


# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
library(compositions)
library(ggpubr) #multiply graphs in one sheet
library(plotly)
library(knitr)
library(nortest)
library(energy)  #Rizzo and Szekely
library(corrplot)   #heatmap
library(reshape2)   #heatmap
library(factoextra)
library(FactoMineR)
library(magrittr)
library(RColorBrewer)
library(grDevices)
library(ggdendro)
library(writexl)
library(readr)
library(openxlsx)
library(ggnewscale)
library(ggridges)
library(ggrepel)
library(stringr)
library(ggh4x)

# Colors --------------------------------------------------------------------
library(ggplot2)
library(scales)

SSE_skala <- c(
  "Water soluble fraction"                   = "#BC31A4", 
  "Exchangeable fraction"                    = "#B6282F",
  "Poorly crystalline Fe(III)-oxyhydroxides" = "#EB7547", 
  "Crystalline Fe(III)-oxides"               = "#F0E442", 
  "Organic matter"                           = "#1B9E77", 
  "Primary sulfide fraction"                 = "#00A8C6", 
  "Residual fraction"                        = "#0072B2")

BCR_skala <- c(
  "Exchangeable fraction" = "#C44E52",
  "Reducible fraction"    = "#DD8452",
  "Oxidisable fraction"   = "#55A868",
  "Residual fraction"     = "#4C72B0"
)


Core_scale <- c(
  
  "#D17C2F",  # topla jantarno-narančasta (različita od moccasin)
  "#8B3C5D",  # prigušena vino/crveno-ljubičasta (ne preblizu rosybrown)
  "#2E86C1",  # jasna srednja plava (različita od powderblue/cornflower)
  "#2F7D5C"   # hladna smaragdno-zelena (ali ne limegreen/1B9E77)
  
)


heatmap_scale <- c(
  "#1F2A44", # 1 tamna plavo-siva
  "#2C3E66",
  "#3F5F8A",
  "#4EA8DE",
  "#7FC8F8",
  "#EAEAEA", # neutral
  "#FFD6A5",
  "#FFB703",
  "#FF9F1C",
  "#E36414",
  "#D62828"  # topli maksimum
)



# THEME
theme_geochem <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#D0D0D0", size = 0.3),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "#333333"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

TOC_color <- "#1F3A5F"   # mekša tamna plava (manje oštra od midnightblue)
TN_color  <- "#4C9F70"   # prirodna zelena (zamjena za limegreen)



# Sequential extraction theory vizualization (Filgueiras et al.) ------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(RColorBrewer)

data <- read_excel("DATA/Data_vizual.xlsx")

fraction_colors <- c(
  "Water soluble"           = "#A23B9A",  # smirenija magenta (umjesto oštre #BC31A4)
  "Exchangeable"            = "#B23A3F",  # toplija i mirnija crvena
  "Acid soluble"            = "#C44E52",  # usklađena s BCR
  "Easily reducible"        = "#E07A5F",  # mekša narančasta
  "Moderately reducible"    = "#DD8452",  # BCR-compatible
  "Poorly reducible oxides" = "#D6C64A",  # prigušena žuta (važna korekcija!)
  
  "Easily oxidisable"       = "#3AAFB9",  # manje neon od #00A8C6
  "Oxidisable"              = "#4C9F70",  # usklađeno s TN
  
  "Residual"                = "#4C72B0"   # stabilno plava (usklađeno s BCR)
)

fraction_levels <- c(
  "Water soluble",
  "Exchangeable",
  "Acid soluble",
  "Easily reducible",
  "Easily oxidisable",
  "Moderately reducible",
  "Oxidisable",
  "Poorly reducible oxides",
  "Residual"
)

SEP_levels <- c("Literature (Filgueiras et al., 2002; Rao et al., 2008)", "SSE", "BCR")

step_levels <- as.character(1:7)

data <- data %>%
  mutate(
    Fraction   = factor(`Operationally defined fraction`, levels = fraction_levels),
    Step       = factor(Step, levels = step_levels),
    Extractant = factor(Extractant, levels = unique(Extractant)),
    SEP        = factor(SEP, levels = SEP_levels)
  )

data_alluvial <- data %>%
  count(
    Fraction,
    Extractant,
    Step,
    SEP,
    name = "value"
  )

tok2 <- ggplot(
  data_alluvial,
  aes(
    axis1 = Fraction,
    axis2 = Extractant,
    axis3 = Step,
    axis4 = SEP,
    y = value
  )
) +
  # TOKOVI ? boja po Fraction
  geom_alluvium(
    aes(fill = Fraction),
    alpha = 0.7,
    colour = NA
  ) +
  # STRATUM ? neutralni s linijama
  geom_stratum(
    width = 0.28,
    fill = "grey90",
    colour = "black",
    linewidth = 0.4
  ) +
  # LABELI STRATUMA
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  scale_fill_manual(
    values = fraction_colors,
    name = "Fraction"
  ) +
  scale_x_discrete(
    limits = c("Fraction", "Extractant", "Step", "SEP"),
    expand = c(0.05, 0.05)
  ) +
  labs(
    x = NULL,
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

tok2
pdf("Sekvencijska_vizualizacije.pdf", width = 30, height = 20)
plot(tok2)
dev.off()






# BCR-701 lake sediment XRD -----------------------------------------------
XRD_ST <- read_excel('DATA/Standard.xlsx', sheet = 1)
XRD_ST

XRD_ST <-  data.frame(XRD_ST)
XRD_ST

min_st <- read_excel("DATA/Standard.xlsx", sheet = "Mineral") %>% 
  data.frame()


min_st2 <- min_st %>%
  rowwise() %>%
  mutate(
    Intensity = XRD_ST$Intensity[which.min(abs(XRD_ST$Theta - Position))]
  ) %>%
  ungroup()



library(ggrepel)
library(stringr)

XRD_GRAPH <- ggplot(XRD_ST, aes(x = Theta, y = Intensity)) +
  geom_path(linewidth = 1, colour = "black") +
  geom_ribbon(aes(ymin=0, ymax=Intensity), fill = "darkmagenta", alpha = 0.2)+
  geom_segment(data = min_st2, aes(x = Position, xend = Position,
                                    y = 0, yend = 13000), linewidth = 0.3, color = "gray30") +
  scale_x_continuous(breaks = seq(5, 65, by = 10)) +
  
  geom_label_repel(data = min_st2,
    aes(x = Position, y = 13000, label = str_wrap(Mineral, width = 4)),
    direction = "y", color = "darkmagenta", fontface = "bold", size = 4.5,
    max.overlaps = 15, inherit.aes = FALSE) +
  
  labs(x = expression(paste("Diffraction angle [2", theta, "]")), y = "Intensity [%]") +
  theme_minimal(base_size = 30) +
  theme()

XRD_GRAPH

#pdf("TIFF/Standard_701_XRD.pdf", width = 20, height = 10)
#plot(XRD_GRAPH)
#dev.off()


tiff("TIFF/Standard_701_XRD.tiff", width = 6000, height = 3000, res = 300)
plot(XRD_GRAPH)
dev.off()

# ALTA - LITHOGEOCHEMISTRY - Data -------------------------------------------------------------

anna <- read_excel('DATA/Annaselva.xlsx', sheet = 1)
anna
anna_df <- data.frame(anna)
anna_df
str(anna_df)
rownames(anna_df) <- anna_df[,1]
anna_df <- anna_df[, 2:32]
anna_df
str(anna_df)

mollne <- read_excel('DATA/Mollneselva.xlsx', sheet = 1)
mollne
mollne_df <- data.frame(mollne)
mollne_df
str(mollne_df)
rownames(mollne_df) <- mollne_df[,1]
mollne_df <- mollne_df[, 2:32]
mollne_df
str(mollne_df)

brakk <- read_excel('DATA/Brakkelva.xlsx', sheet = 1)
brakk
brakk_df <- data.frame(brakk)
brakk_df
str(brakk_df)
rownames(brakk_df) <- brakk_df[,1]
brakk_df <- brakk_df[, 2:32]
brakk_df
str(brakk_df)

alta_data <- read_excel('DATA/Alta.xlsx', sheet = 1)                                   
alta_data
alta_df <- data.frame(alta_data)
alta_df
str(alta_df$Element)


### ALTA - Pairwise log rations ----------------------------------------------

anna_acomp <- acomp(anna_df)
ann <- anna_acomp %>% variation %>% as.matrix %>% round(2)


tiff("TIFF/Anna_heatmap2.tiff", res = 450, width = 5000, height = 5000)
melt(ann) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()

pdf("Anna_heatmap2.pdf", width = 10, height = 10)
melt(ann) %>%
 ggplot(aes(Var1, Var2, fill = value)) +
 geom_tile() +
 geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
 labs(x='', y='', fill='Pairwise \n log ratio \n value')+
 theme(axis.text = element_text(size=14),
       axis.text.x = element_text(angle = 45),
       legend.text = element_text(size = 12),
       legend.key.size = unit(1, 'cm'))
dev.off()

print(anna_acomp %>% variation) %>% write.csv("ALTA_CSV/Anna_variation.csv")

brakk_acomp <- acomp(brakk_df)
bra <-  brakk_acomp %>% variation %>% as.matrix %>% round(2)


tiff("TIFF/Brakk_heatmap2.tiff", res = 450, width = 5000, height = 5000)
melt(bra) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()

pdf("Brakk_heatmap2.pdf", width = 10, height = 10)
melt(bra) %>%
 ggplot(aes(Var1, Var2, fill = value)) +
 geom_tile() +
 geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
 labs(x='', y='', fill='Pairwise \n log ratio \n value')+
 theme(axis.text = element_text(size=14),
       axis.text.x = element_text(angle = 45),
       legend.text = element_text(size = 12),
       legend.key.size = unit(1, 'cm'))
dev.off()


print(brakk_acomp %>% variation) %>% write.csv("ALTA_CSV/Brakk_variation.csv")


mollne_acomp <- acomp(mollne_df)
mol <- mollne_acomp %>% variation %>% as.matrix %>% round(2)

mollne_acomp %>% variation %>% heatmap

tiff("TIFF/Mollne_heatmap2.tiff", res = 450, width = 5000, height = 5000)
melt(mol) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()

pdf("Mollne_heatmap2.pdf", width = 10, height = 10)
melt(mol) %>%
 ggplot(aes(Var1, Var2, fill = value)) +
 geom_tile() +
 geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors =  heatmap_scale) +
 labs(x='', y='', fill='Pairwise \n log ratio \n value')+
 theme(axis.text = element_text(size=14),
       axis.text.x = element_text(angle = 45),
       legend.text = element_text(size = 12),
       legend.key.size = unit(1, 'cm'))
dev.off()

print(mollne_acomp %>% variation) %>% write.csv("ALTA_CSV/Mollne_variation.csv")


### ALTA - correlation of data ----------------------------------------------

anna_df
anna_clr <- clr(anna_df) #transformacija u clr podatke
anna_clr 

anna_cor <- cor(anna_clr, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
anna_cor

testResLog <- cor.mtest(anna_cor, conf.level = 0.95)

write.csv(anna_cor, "ALTA_CSV/Annaselva_clr_Pearson_corr.csv")

#Color scales
COL1(sequential = c("Oranges", "Purples", "Reds", "Blues", "Greens", 
                    "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"), n = 200)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)


#creating heatmap
tiff("TIFF/Annaselva_correlation_heatmap_Pearson.tiff") #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(anna_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/Annaselva_correlation_heatmap_Pearson_full.tiff") #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(anna_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  


#Correlation Mollneselva
mollne_df
mollne_clr <- clr(mollne_df) #transformacija u clr podatke
mollne_log 

mollne_cor <- cor(mollne_clr, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
mollne_cor

testResLog <- cor.mtest(mollne_cor, conf.level = 0.95)

write.csv(mollne_cor, "ALTA_CSV/Mollneselva_clr_Pearson_corr.csv")

#Color scales
COL1(sequential = c("Oranges", "Purples", "Reds", "Blues", "Greens", 
                    "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"), n = 200)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)


#creating heatmap
tiff("TIFF/Mollneselva_correlation_heatmap_Pearson.tiff") #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(mollne_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'blank', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo npr. col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/Mollneselva_correlation_heatmap_Pearson_full.tiff") #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(mollne_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off() 


#Correlation Brakkelva
brakk_df
brakk_clr <- clr(brakk_df) #transformacija u clr podatke
brakk_clr 

brakk_cor <- cor(brakk_clr, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
brakk_cor

testResLog <- cor.mtest(brakk_cor, conf.level = 0.95)

write.csv(brakk_cor, "ALTA_CSV/Brakkelva_clr_Pearson_corr.csv")

#Color scales
COL1(sequential = c("Oranges", "Purples", "Reds", "Blues", "Greens", 
                    "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"), n = 200)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)


#creating heatmap
tiff("TIFF/Brakkelva_correlation_heatmap_Pearson.tiff")  #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(brakk_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/Brakkelva_correlation_heatmap_Pearson_full.tiff") #, res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(brakk_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = COL2('BrBG')) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()



### ALTA - Descriptive statistics - Violin plots ------------------------------------------------------------
alta_df1 <- alta_df %>% 
  mutate(log10_Concentration=log10(Concentration))

#podjela na dvije grupe
grupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
grupa2 <- c("Al","Ca", "Fe", "K", "Mg", "Na")
grupa3 <- c("Au", "Ba", "Ga", "La", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")

violins <- function(data) {
  ggplot(filter(alta_df1, Element %in% grupa1),
         aes(x=Stream, y=log10_Concentration, fill=Stream)) +
  geom_violin(alpha=0.5, trim = F) +
  geom_dotplot(binaxis = 'y', binwidth = 0.09, stackdir = 'center', 
               dotsize=0.5, colour ='#1C1C1C')+
  facet_wrap(~Element, scales = "free_y", ncol = 3) +
  labs(x="", 
       y="log10(Concentration)",
       fill="")+
  scale_fill_manual(values = c("moccasin", "rosybrown1", "powderblue"))+
  theme_minimal()+
  geom_boxplot(width=0.1, color='black', alpha = 0.2) +
  theme(legend.position = 'bottom', 
        strip.text = element_text(face="bold"), 
        text = element_text(size = 130),
        axis.text.x = element_blank(), 
        legend.key.size = unit(10, "line"), 
        legend.title = element_text(size = 150), 
        legend.text = element_text(size = 140), 
        legend.key.spacing.x = unit(1.5, "cm")) 
} 
violins  

violins2 <- function(data) {
  ggplot(filter(alta_df1, Element %in% grupa2),
         aes(x=Stream, y=log10_Concentration, fill=Stream)) +
    geom_violin(alpha=0.5, trim = F) +
    geom_dotplot(binaxis = 'y', binwidth = 0.09, stackdir = 'center', 
                 dotsize=0.5, colour ='#1C1C1C')+
    facet_wrap(~Element, scales = "free_y", ncol = 3) +
    labs(x="", 
         y="log10(Concentration)", 
         fill="")+
    scale_fill_manual(values = c("moccasin", "rosybrown1", "powderblue"))+
    theme_minimal()+
    geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_blank(), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 140), 
          legend.key.spacing.x = unit(1.5, "cm")) 
} 

violins3 <- function(data) {
  ggplot(filter(alta_df1, Element %in% grupa3),
         aes(x=Stream, y=log10_Concentration, fill=Stream)) +
    geom_violin(alpha=0.5, trim = F) +
    geom_dotplot(binaxis = 'y', binwidth = 0.09, stackdir = 'center', 
                 dotsize=0.5, colour ='#1C1C1C')+
    facet_wrap(~Element, scales = "free_y", ncol = 4) +
    labs(x="", 
         y="log10(Concentration)", 
         fill="")+
    scale_fill_manual(values = c("moccasin", "rosybrown1", "powderblue"))+
    theme_minimal()+
    geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_blank(), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 140), 
          legend.key.spacing.x = unit(1.5, "cm")) 
} 


plot1 <- violins(filter(alta_df, Element %in% grupa1))

plot2 <- violins2(filter(alta_df, Element %in% grupa2))

plot3 <- violins3(filter(alta_df, Element %in% grupa3))

tiff("TIFF/Descriptive_statistics_semi_total_minor.tiff", width = 5000, height = 6500)
plot(plot1)
dev.off()

tiff("TIFF/Descriptive_statistics_semi_total_major.tiff", width = 5000, height = 6500)
plot(plot2)
dev.off()

tiff("TIFF/Descriptive_statistics_semi_total_append.tiff", width = 5000, height = 6500)
plot(plot3)
dev.off()

### ALTA - PCA analysis -----------------------------------------------------

#ANNASELVA
# PCA Annaselva prema Kassambara, modified after pair wise log ratios
#here data are closed using the column 'Other' which represents all measured and not-measured elements
#not included into the PCA. SUM of Cu, Sr, Ba, Ag, Hg, Fe, Ti, Al, Mn, and Other is 100(%)
anna_pca <- read_excel('DATA/Annaselva.xlsx', sheet = 2)

anna_pca
anna_pca_df <- data.frame(anna_pca)
anna_pca_df
rownames(anna_pca_df) <- anna_pca_df[,1]
anna_pca_df <- anna_pca_df[, 2:10]
anna_pca_df
str(anna_pca_df)
anna_clr <-  acomp(anna_pca_df)

pc <- princomp(anna_clr)                                #PCA compositions
pc                                                                    #same as 'print(pc)'
summary(pc)

eig.val <- get_eigenvalue(pc)
eig.val                                                               #measuring the amount of variance



# PCA Visualization

anna_scree <- fviz_eig(pc, addlabels = T, ylim = c(0,63), barcolor = "black", barfill = "cornflowerblue") 

tiff("TIFF/Anna_Screeplot_Total.tiff")
fviz_eig(pc, addlabels = T, ylim = c(0,63))                           #creates scree plot, 'ylim = c(0:30)' means that y axis goes to 30,
#it's in accordance with the highest value of eigen values
dev.off()

var <- get_pca_var(pc)                                                #list of matrices containing all the results
var

head(var$coord)                                                       #coordinates
#The observations are represented by their projections, 
#but the variables are represented by their correlations

tiff('TIFF/AnnaselvaCorrelations of the variables in PCA analysis.tiff')
fviz_pca_var(pc, geom.var=c('point', 'text'), col.var = 'violetred4', 
             col.circle = 'black', addEllipses=T, 
             title = "A")                                   #variable correlation plot , shows the relationsships between all variables
dev.off()

head(var$cos2, 4)                                                     #quality of representation of the variables on factor map (cos2), squared cosine or sqaured coordinates

corrplot(var$cos2, is.corr = T)

tiff('TIFF/Variability of the data in dimensions.tiff')
fviz_cos2(pc, choice = 'var', axes = 1:2)                             #variability of the first to dimensions
dev.off()

pdf('TIFF/Variantion of the variables in dim 1-2.pdf')
fviz_pca_var(pc, axes = 1:2, col.var = 'cos2',                                    #Visualization of the variables and their correlation in PC1 and PC2, and corolarition based on the quality of representation ih these dimensions
             gradient.cols = c('PRGn'), 
             repel = T                                                #Avoid text overlapping ##PRGn je paleta boja od ljubi?aste do zelene
)
dev.off()


#MOLLNESELVA
# PCA Mollneselva prema Kassambara, modified after pair wise log ratios
#here data are closed using the column 'Other' which represents all measured and not-measured elements
#not included into the PCA. SUM of elements and Other is 100(%)
mollne_pca <- read_excel('DATA/Mollneselva.xlsx', sheet = 2)
mollne_pca

mollne_pca_df <- data.frame(mollne_pca)
mollne_pca_df
str(mollne_pca_df)

rownames(mollne_pca_df) <- mollne_pca_df[,1]
mollne_pca_df <- mollne_pca_df[, 2:10] #active columns
mollne_pca_df
str(mollne_pca_df)

mollne_clr <-  acomp(mollne_pca_df) #placing data in Aitchinson simplex



mpc <- princomp(mollne_clr)                                #PCA compositions
mpc                                                                    #same as 'print(pc)'
summary(mpc)

eig.val <- get_eigenvalue(mpc)
eig.val                                                               #measuring the amount of variance

moll_scree <- fviz_eig(mpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "cornflowerblue")

# PCA Visualization
tiff("TIFF/Screeplot_Total_Mollneselva.tiff")
fviz_eig(mpc, addlabels = T, ylim = c(0,50))                           #creates scree plot, 'ylim = c(0:30)' means that y axis goes to 30,
#it's in accordance with the highest value of eigen values
dev.off()

var <- get_pca_var(mpc)                                                #list of matrices containing all the results
var

head(var$coord)                                                       #coordinates
#The observations are represented by their projections, 
#but the variables are represented by their correlations

tiff('TIFF/Mollneselva_correlations_variables_PCA.tiff')
fviz_pca_var(mpc, geom.var=c('point', 'text'), col.var = 'violetred4', 
             col.circle = 'black', addEllipses=T, 
             title = "B")                                   #variable correlation plot , shows the relationsships between all variables
dev.off()

head(var$cos2, 4)                                                     #quality of representation of the variables on factor map (cos2), squared cosine or sqaured coordinates

corrplot(var$cos2, is.corr = T)
tiff('TIFF/Mollneselva_variability_data_in_dimensions.tiff')
fviz_cos2(mpc, choice = 'var', axes = 1:2)                             #variability of the first to dimensions
dev.off()

tiff('TIFF/Mollneselva_variantion_of_variables_dim_1-2.tiff')
fviz_pca_var(mpc, axes = 1:2, col.var = 'cos2',                                    #Visualization of the variables and their correlation in PC1 and PC2, and corolarition based on the quality of representation ih these dimensions
             gradient.cols = c('PRGn'), 
             repel = T                                                #Avoid text overlapping ##PRGn je paleta boja od ljubi?aste do zelene
)
dev.off()


#BRAKKELVA
# PCA Brakkelva prema Kassambara, modified after pair wise log ratios
#here data are closed using the column 'Other' which represents all measured and not-measured elements
#not included into the PCA. SUM of elements and Other is 100(%)
brakk_pca <- read_excel('DATA/Brakkelva.xlsx', sheet = 2)
brakk_pca

brakk_pca_df <- data.frame(brakk_pca)
brakk_pca_df
str(brakk_pca_df)

rownames(brakk_pca_df) <- brakk_pca_df[,1]
brakk_pca_df <- brakk_pca_df[, 2:10] #active columns
brakk_pca_df
str(brakk_pca_df)

brakk_clr <-  acomp(brakk_pca_df) #placing data in Aitchinson simplex


bpc <- princomp(brakk_clr)                                #PCA compositions
bpc                                                                    #same as 'print(pc)'
summary(bpc)

eig.val <- get_eigenvalue(bpc)
eig.val                                                               #measuring the amount of variance

brakk_scree <- fviz_eig(bpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "cornflowerblue")

# PCA Visualization
tiff("TIFF/Screeplot_Total_Brakkelva.tiff")
fviz_eig(bpc, addlabels = T, ylim = c(0,50))                           #creates scree plot, 'ylim = c(0:30)' means that y axis goes to 30,
#it's in accordance with the highest value of eigen values
dev.off()

var <- get_pca_var(bpc)                                                #list of matrices containing all the results
var

head(var$coord)                                                       #coordinates
#The observations are represented by their projections, 
#but the variables are represented by their correlations

tiff('TIFF/Brakkelva_correlations_variables_PCA.tiff')
fviz_pca_var(bpc, geom.var=c('point', 'text'), col.var = 'violetred4', 
             col.circle = 'black', addEllipses=T, repel=T, 
             title = "C")                                   #variable correlation plot , shows the relationsships between all variables
dev.off()

head(var$cos2, 4)                                                     #quality of representation of the variables on factor map (cos2), squared cosine or sqaured coordinates

corrplot(var$cos2, is.corr = T)

tiff('TIFF/Brakkelva_variability_data_in_dimensions.tiff')
fviz_cos2(bpc, choice = 'var', axes = 1:2)                             #variability of the first to dimensions
dev.off()

tiff('TIFF/Brakkelva_variantion_of_variables_dim_1-2.tiff')
fviz_pca_var(bpc, axes = 1:2, col.var = 'cos2',                                    #Visualization of the variables and their correlation in PC1 and PC2, and corolarition based on the quality of representation ih these dimensions
             gradient.cols = c('PRGn'), 
             repel = T                                            #Avoid text overlapping ##PRGn je paleta boja od ljubi?aste do zelene
)
dev.off()

#PCA Figures

anna_scree <- fviz_eig(pc, addlabels = T, ylim = c(0,63), barcolor = "black", barfill = "cornflowerblue")
apc <- fviz_pca_var(pc, geom.var=c('arrow', 'point', 'text'), col.var = 'cornflowerblue', 
                    col.circle = 'black', addEllipses=T, 
                    title = "A", repel = T)

moll_scree <- fviz_eig(mpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "cornflowerblue")
mpca <- fviz_pca_var(mpc, geom.var=c('arrow', 'point', 'text'), col.var = 'cornflowerblue', 
                     col.circle = 'black', addEllipses=T, 
                     title = "B", repel = T)   

brakk_scree <- fviz_eig(bpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "cornflowerblue")
bpca <- fviz_pca_var(bpc, geom.var=c('arrow', 'point', 'text'), col.var = 'cornflowerblue', 
                     col.circle = 'black', addEllipses=T, 
                     title = "C", repel = T)


PCA_FIG <-ggarrange(anna_scree, apc, moll_scree, mpca, brakk_scree, bpca, ncol = 2, nrow=3)

tiff('TIFF/PCA_ANALYSIS_KAFJORD.tiff', res = 400, width = 4000, height = 6000)
plot(PCA_FIG)
dev.off()

anna_scree2 <- fviz_eig(pc, addlabels = T, ylim = c(0,63), barcolor = "black", barfill = "moccasin", title = "A")+
  theme(plot.title = element_text(size=26))
apc2 <- fviz_pca_var(pc, geom.var=c('arrow', 'point', 'text'), col.var = 'black', 
                    col.circle = 'black', addEllipses=T, 
                    title = "", repel = T, 
                    )+
  theme(plot.title = element_text(size=26))

moll_scree2 <- fviz_eig(mpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "powderblue", title = "B")+ 
  theme(plot.title = element_text(size=26))

mpca2 <- fviz_pca_var(mpc, geom.var=c('arrow', 'point', 'text'), col.var = 'black', 
                     col.circle = 'black', addEllipses=T, 
                     title = "", repel = T, 
                     )   +
  theme(plot.title = element_text(size=26))

brakk_scree2 <- fviz_eig(bpc, addlabels = T, ylim = c(0,50), barcolor = "black", barfill = "rosybrown1", title = "C")+
  theme(plot.title = element_text(size=26))                   
bpca2 <- fviz_pca_var(bpc, geom.var=c('arrow', 'point', 'text'), col.var = 'black', 
                     col.circle = 'black', addEllipses=T, 
                     title = "",repel = T, 
                     )+
  theme(plot.title = element_text(size=26))


PCA_FIG2 <-ggarrange(anna_scree2, apc2, moll_scree2, mpca2, brakk_scree2, bpca2, ncol = 2, nrow=3)

tiff('TIFF/PCA_ANALYSIS_KAFJORD2.tiff', res = 400, width = 4000, height = 6000)
plot(PCA_FIG2)
dev.off()

### ALTA - FAMD ---------------------------------------------------------------
#according to Kassambara
#Same data as PCA just Lithology added as facotr

#ANNASELVA
anna_mfa <- read_excel('DATA/Annaselva.xlsx', sheet = 3)
anna_mfa

write.csv(anna_mfa, file = 'ALTA_CSV_WRK/Anna_data_MFA.csv')                                #creating new csv    
anna_mfa <- read.csv('ALTA_CSV_WRK/Anna_data_MFA.csv')                                   #reading new csv
anna_mfa <- as.data.frame(anna_mfa)                                         #creating data frame
anna_mfa

# Clr transformation of the data 
element <- acomp(anna_mfa, parts = c("Cu", "Sr", "Ba", "Ag", "Hg", "Fe", "Mn", "Pb", "Zn", "Co"
))                         #defining as acomp
element
element_df <- data.frame(element)    
element_df
str(element_df)

Lithology <- factor(anna_mfa$Lithology)                                          #converts litological labels into factors. 
Lithology                                                                         #this way the levels of factors also are visible and the final data frame looks the way its described
#in Kassambara example in the book.
Stream <- factor(anna_mfa$Stream)
Stream
Sample <- as.character(anna_mfa$Sample)
Sample

# Creating single data frame
anna_famd.df <- data.frame(Sample, Lithology, element_df)                           #creating a single data frame with two factors and continuous variables
anna_famd.df
rownames(anna_famd.df) <- anna_famd.df[,1]
anna_famd.df <- anna_famd.df[,2:12]
anna_famd.df
str(anna_famd.df)

pdf("TIFF/MFA_Anna_results.pdf")
ANNA_famd <- FAMD(anna_famd.df)
print (ANNA_famd)
dev.off()

eig.val <- get_eigenvalue(ANNA_famd)
eig.val
print(eig.val)
write.csv(eig.val, file = 'ALTA_CSV/Anna_famd_eigenvalues.csv')

tiff('TIFF/Screeplot of varience in the FA in Annaselva stream.tiff') #, width = 200, height = 200, res = 1000, units = "mm")
fviz_screeplot(ANNA_famd, barfill="paleturquoise3" , barcolor="black", linecolor="navyblue",
               ncp=10, addlabels = T, font.x = c(14, "bold", "black"), 
               font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
               font.tickslab = c(12, "black"))  #screeplot
dev.off()

var <- get_famd_var(ANNA_famd)
var

pdf('TIFF/FA__Anna_Graph of variables.pdf')
fviz_famd_var(ANNA_famd, 'var', repel = T)                                       #plot of variables
dev.off()

#Graphs of contribution to the dimensions
pdf('TIFF/FA Annaselva Graphs of contributions to the dimensions.pdf')
contrib1 <- fviz_contrib(ANNA_famd, 'var', axes = 1)
contrib2 <- fviz_contrib(ANNA_famd, 'var', axes = 2)
contrib3 <- fviz_contrib(ANNA_famd, 'var', axes = 3)
contrib4 <- fviz_contrib(ANNA_famd, 'var', axes = 4)
print(contrib1)
print(contrib2)
print(contrib3)
print(contrib4)
dev.off()

#quantitative variables

quanti.var <- get_famd_var(ANNA_famd, 'quanti.var')
quanti.var
#visualization of quantitavie varialbes
tiff('TIFF/FA Anna graph of quantitative variables.tiff', width= 6000, height=6000, res = 600)
fviz_famd_var(ANNA_famd, 'quanti.var', col.var= "contrib", 
              axes = c(1,2), repel = T,  
              gradient.cols = c("paleturquoise", "mediumturquoise", "darkturquoise"), 
              addlabels = T, font.x = c(14, "bold", "black"), 
              font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
              font.tickslab = c(12, "black"), 
              title = "Quantitative variables - Factor Analysis", 
              legend.title = "Contribution 
to the factors") 
dev.off()


#Mollneselva
#according to Kassambara
#Same data as PCA just Lithology added as facotr

mollne_famd <- read_excel('DATA/Mollneselva.xlsx', sheet = 3)
mollne_famd

write.csv(mollne_famd, file = 'ALTA_CSV_WRK/Mollne_data_FAMD.csv')                                #creating new csv    
mollne_famd <- read.csv('ALTA_CSV_WRK/Mollne_data_FAMD.csv')                                   #reading new csv
mollne_famd <- as.data.frame(mollne_famd)                                         #creating data frame
mollne_famd

# Clr transformation of the data 
elementm <- acomp(mollne_famd, parts = c("Cu", "Zn", "Sr", "Ba", "Bi", "Co", "Sc", 
                                         "Fe", "Al", "Ag", "Ca", "Ni", "Mg", "Cr", "V"
))                         #defining as acomp
elementm

element_df <- data.frame(elementm)    
element_df
str(element_df)

Lithology <- factor(mollne_famd$Lithology)                                          #converts litological labels into factors. 
Lithology                                                                         #this way the levels of factors also are visible and the final data frame looks the way its described
#in Kassambara example in the book.
Stream <- factor(mollne_famd$Stream)
Stream
Sample <- as.character(mollne_famd$Sample)
Sample

# Creating single data frame
mollne_famd.df <- data.frame(Sample, Lithology, element_df)                           #creating a single data frame with two factors and continuous variables
mollne_famd.df
rownames(mollne_famd.df) <- mollne_famd.df[,1]
mollne_famd.df <- mollne_famd.df[,2:15]
mollne_famd.df
str(mollne_famd.df)

pdf("TIFF/MFA_Anna_results.pdf")
Mollne_famd <- FAMD(mollne_famd.df, graph = T)
print (Mollne_famd)
dev.off()

eig.val <- get_eigenvalue(Mollne_famd)
eig.val
print(eig.val)
write.csv(eig.val, file = 'ALTA_CSV/Mollne_famd_eigenvalues.csv')

tiff('TIFF/Screeplot of varience in the FA in Mollneselva stream.tiff') #, width = 200, height = 200, res = 1000, units = "mm")
fviz_screeplot(Mollne_famd, barfill="paleturquoise3" , barcolor="black", linecolor="navyblue",
               ncp=10, addlabels = T, font.x = c(14, "bold", "black"), 
               font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
               font.tickslab = c(12, "black"))  #screeplot
dev.off()

var <- get_famd_var(Mollne_famd)
var

pdf('TIFF/FA_Mollne_Graph of variables.pdf')
fviz_famd_var(Mollne_famd, 'var', repel = T)                                       #plot of variables
dev.off()

#Graphs of contribution to the dimensions
pdf('TIFF/FA Annaselva Graphs of contributions to the dimensions.pdf')
contrib1 <- fviz_contrib(ANNA_famd, 'var', axes = 1)
contrib2 <- fviz_contrib(ANNA_famd, 'var', axes = 2)
contrib3 <- fviz_contrib(ANNA_famd, 'var', axes = 3)
contrib4 <- fviz_contrib(ANNA_famd, 'var', axes = 4)
print(contrib1)
print(contrib2)
print(contrib3)
print(contrib4)
dev.off()

#quantitative variables

quanti.var <- get_famd_var(ANNA_famd, 'quanti.var')
quanti.var
#visualization of quantitavie varialbes
tiff('TIFF/FA_Mollne_graph of quantitative variables.tiff')#, width= 6000, height=6000, res = 600)
fviz_famd_var(Mollne_famd, 'quanti.var', col.var= "contrib", 
              axes = c(1,2), repel = T,  
              gradient.cols = c("paleturquoise", "mediumturquoise", "darkturquoise"), 
              addlabels = T, font.x = c(14, "bold", "black"), 
              font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
              font.tickslab = c(12, "black"), 
              title = "Quantitative variables - Factor Analysis", 
              legend.title = "Contribution 
to the factors") 
dev.off()


#Brakkelva
#according to Kassambara
#Same data as PCA just Lithology added as facotr

brakk_famd <- read_excel('DATA/Brakkelva.xlsx', sheet = 3)
brakk_famd

write.csv(brakk_famd, file = 'ALTA_CSV_WRK/Brakk_data_FAMD.csv')                                #creating new csv    
brakk_famd <- read.csv('ALTA_CSV_WRK/Brakk_data_FAMD.csv')                                   #reading new csv
brakk_famd <- as.data.frame(brakk_famd)                                         #creating data frame
brakk_famd

# Clr transformation of the data 
elementb <- acomp(brakk_famd, parts = c("Cu", "Sc", "Ti", "Fe", "Ni", "Sr", "Hg", "Bi", "Ba", "Mn",
                                        "Zn", "Other"
))                         #defining as acomp
elementb
element_df <- data.frame(elementb)    
element_df
str(element_df)

Lithology <- factor(brakk_famd$Lithology)                                          #converts litological labels into factors. 
Lithology                                                                         #this way the levels of factors also are visible and the final data frame looks the way its described
#in Kassambara example in the book.
Stream <- factor(brakk_famd$Stream)
Stream
Sample <- as.character(brakk_famd$Sample)
Sample

# Creating single data frame
brakk_famd.df <- data.frame(Sample, Lithology, element_df)                           #creating a single data frame with two factors and continuous variables
brakk_famd.df
rownames(brakk_famd.df) <- brakk_famd.df[,1]
brakk_famd.df <- brakk_famd.df[,2:12]
brakk_famd.df
str(brakk_famd.df)

#pdf("MFA_Anna_results.pdf")
Brakk_famd <- FAMD(brakk_famd.df, graph = T)
print (Brakk_famd)
#dev.off()

eig.val <- get_eigenvalue(Brakk_famd)
eig.val
print(eig.val)
write.csv(eig.val, file = 'ALTA_CSV/Brakk_famd_eigenvalues.csv')

tiff('TIFF/Screeplot of varience in the FA in Brakkelva stream.tiff') #, width = 200, height = 200, res = 1000, units = "mm")
fviz_screeplot(Brakk_famd, barfill="paleturquoise3" , barcolor="black", linecolor="navyblue",
               ncp=10, addlabels = T, font.x = c(14, "bold", "black"), 
               font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
               font.tickslab = c(12, "black"))  #screeplot
dev.off()

var <- get_famd_var(Brakk_famd)
var

pdf('TIFF/FA_Brakk_Graph of variables.pdf')
fviz_famd_var(Brakk_famd, 'var', repel = T)                                       #plot of variables
dev.off()

#quantitative variables
quanti.var <- get_famd_var(Brakk_famd, 'quanti.var')
quanti.var
#visualization of quantitavie varialbes
tiff('TIFF/FA_Brakk_graph of quantitative variables.tiff')#, width= 6000, height=6000, res = 600)
fviz_famd_var(Brakk_famd, 'quanti.var', col.var= "contrib", 
              axes = c(1,2), repel = T,  
              gradient.cols = c("paleturquoise", "mediumturquoise", "darkturquoise"), 
              addlabels = T, font.x = c(14, "bold", "black"), 
              font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
              font.tickslab = c(12, "black"), 
              title = "Quantitative variables - Factor Analysis", 
              legend.title = "Contribution 
to the factors") 
dev.off()




### ALTA - SEQUENTIAL EXTRACTION - descriptive statistics ------------------------------------------------------------------
seq_novo <- read_excel('DATA/Seq_an_v2.xlsx', sheet = 'Stacked')
seq_novo <- data.frame(seq_novo)

seq_novo1 <- seq_novo %>% 
  mutate(log10_Concentration=log10(Concentration))

#podjela na dvije grupe
sgrupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")

seq_violins <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa1),
         aes(x=Element, y=log10_Concentration, fill=Step)) +
    geom_boxplot(linewidth = 1, shape  = 21, color = "black", size = 8)  +
    facet_wrap(~factor(Step, c( "Water soluble fraction", "Exchangeable fraction", "Poorly crystalline Fe(III)-oxyhydroxides", 
                                "Crystalline Fe(III)-oxides", "Organic matter", "Primary sulfide fraction","Residual fraction")),
               scales = "free_y", ncol = 2) +
    labs(x="Element", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = SSE_skala)+
    theme_minimal()+
    # geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 110), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 

splot1 <- seq_violins(filter(seq_novo1, Element %in% sgrupa1))

tiff("TIFF/Descriptive_statistics_SSE_MOI.tiff", width = 6500, height = 5000)
plot(splot1)
dev.off()

#Appendix
seq_violins2 <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa2),
         aes(x=Element, y=log10_Concentration, fill=Step)) +
    geom_boxplot(linewidth = 1, shape  = 21, color = "black", size = 8) +
    facet_wrap(~factor(Step, c( "Water soluble fraction", "Exchangeable fraction", "Poorly crystalline Fe(III)-oxyhydroxides", 
                                "Crystalline Fe(III)-oxides", "Organic matter", "Primary sulfide fraction","Residual fraction")),
               scales = "free_y", ncol = 2) +
    labs(x="Element", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = SSE_skala)+
    theme_minimal()+
    # geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 110), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 

splot2 <- seq_violins2(filter(seq_novo1, Element %in% sgrupa2))

tiff("TIFF/Descriptive_statistics_SSE_app.tiff", width = 6500, height = 5000)
plot(splot2)
dev.off()


### ALTA - SEQUENTIAL EXTRACTION trends -------------------------------------
seq_novo <- read_excel('DATA/Seq_an_v2.xlsx', sheet = 'Stacked')
seq_novo <- data.frame(seq_novo)

seq_novo1 <- seq_novo %>% 
  mutate(log10_Concentration=log10(Concentration))

#podjela na dvije grupe
sgrupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")


anna_sample <- c("J003", "J001", "J007", "J010")
mollne_sample <- c("J002", "J018", "J019", "J024", "J033")
brakk_sample <- c("J028", "J039", "J016", "J041", "J043")

#Annaselva
seq_stack_ana <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa1, 
                Sample %in% anna_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, levels = anna_sample),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_ana <- seq_stack_ana(filter(seq_novo1, Element %in% sgrupa1))

tiff("TIFF/Trends_SSE_MOI_ANNA.tiff", width = 4000, height = 5000)
plot(stack_plot_ana)
dev.off()

seq_stack_ana2 <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa2, 
                Sample %in% anna_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, c("J003", "J001", "J007", "J010")),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_ana2 <- seq_stack_ana2(filter(seq_novo1, Element %in% sgrupa2))

tiff("TIFF/Trends_SSE_MOI_ANNA_app.tiff", width = 4000, height = 5000)
plot(stack_plot_ana2)
dev.off()

#Mollneselva

seq_stack_moll <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa1, Sample %in% mollne_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, c("J002", "J018", "J019", "J024", "J033")),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_moll <- seq_stack_moll(filter(seq_novo1, Element %in% sgrupa1))

tiff("TIFF/Trends_SSE_MOI_MOLL.tiff", width = 4000, height = 5000)
plot(stack_plot_moll)
dev.off()

seq_stack_moll2 <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa2, 
                Sample %in% mollne_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, c("J002", "J018", "J019", "J024", "J033")),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_moll2 <- seq_stack_moll2(filter(seq_novo1, Element %in% sgrupa2))

tiff("TIFF/Trends_SSE_MOI_MOLL_app.tiff", width = 4000, height = 5000)
plot(stack_plot_moll2)
dev.off()

#Brakkelva

seq_stack_brakk <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa1, 
                Sample %in% brakk_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, c("J028", "J039", "J016", "J041", "J043")),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_brakk <- seq_stack_brakk(filter(seq_novo1, Element %in% sgrupa1))

tiff("TIFF/Trends_SSE_MOI_BRAKK.tiff", width = 4000, height = 5000)
plot(stack_plot_brakk)
dev.off()

seq_stack_brakk2 <- function(data) {
  ggplot(filter(seq_novo1, Element %in% sgrupa2, 
                Sample %in% brakk_sample),
         aes(x=Element, y=Concentration, fill=reorder(Step, OrderStep))) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~factor(Sample, c("J028", "J039", "J016", "J041", "J043")),
               scales = "free_y", ncol = 2, axes= "all", axis.labels = "all") +
    labs(x="Element", 
         y="Proportion",
         fill="") +
    scale_fill_manual(values = SSE_skala)+
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme_minimal()+
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm")) 
} 


stack_plot_brakk2 <- seq_stack_brakk2(filter(seq_novo1, Element %in% sgrupa2))

tiff("TIFF/Trends_SSE_MOI_BRAKK_app.tiff", width = 4000, height = 5000)
plot(stack_plot_brakk2)
dev.off()



### ALTA - SEQ COMPARISON TO LITERATURE -------------------------------------
elements_law <- c("As", "Cd", "Cu", "Cr", "Ni", "Pb", "Zn")
#Granice

contamination_bound <-  data.frame(
  Element = rep(c("As", "Cd", "Cr","Cu", "Ni", "Pb", "Zn"), each = 5),
  Classification = rep(c("background", "good", "moderate", "bad", "very bad"), times = 7),
  ymin = c(0, 20, 51, 75, 579, #As 
           0, 0.25, 2.5, 15, 140, #Cd
           0, 70, 560, 5900, 59000, #Cr
           0, 35, 51, 55, 220, #Cu
           0, 30, 46, 120, 840, #Ni
           0, 30, 83, 100, 720, #Pb
           0, 150, 360, 590, 4500 #Zn
          ), 
  ymax = c(20, 52, 76, 580, Inf, #As
           0.25, 2.6, 15, 140, Inf, #Cd
           70, 560, 5900, 59000, Inf, #Cr
           35, 51, 55, 220, Inf, #Cu
           30, 46, 120, 840, Inf, #Ni
           30, 83, 100, 720, Inf, #Pb
           150, 360, 590, 4500, Inf #Zn
  )
)


boje_klasa <- c(
  "background" = "#7fbf7b",  # muted green
  "good"       = "#c7c77a",  # soft yellow-green
  "moderate"   = "#e0a96d",  # soft orange
  "bad"        = "#d97b6a",  # muted red
  "very bad"   = "#8a6fa1"   # desaturated purple
)

Orderklasa <- c("background", "good", "moderate", "bad", "very bad")


# pagmatic_max <- 600

seq_quality <- function(data) {
  ggplot() +
    
    geom_rect(data = contamination_bound %>%
                dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
              
      aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Classification),
      alpha = 0.4,
      color = NA
    ) +
    scale_fill_manual(
      values = boje_klasa,
      name = "Sediment quality (Bakke et al., 2010)"
    ) +
        
    ggnewscale::new_scale_fill() +
    
    geom_bar(
      data = seq_novo %>% filter(Element %in% elements_law),
      aes(x=Sample, 
          y=Concentration, 
          fill=factor(Step, levels = unique(Step[order(OrderStep)]))), 
      position = position_dodge(width = 0.8), 
      stat = "identity", 
      width = 0.9) +
    scale_fill_manual(values = SSE_skala, name = "SSE fraction") +
    
    
    #scale_y_log10(expand = expansion(mult = 0.05)) +
    # coord_cartesian(ylim = c(0, pragmatic_max))+ 
    
    facet_wrap(
      ~factor(Element, levels = elements_law),
      scales = "free_y", 
      ncol = 2, 
      ) +
    labs(x="\n Sample", 
         y="Concentration [mg/kg]") +
    
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(
        override.aes = list(linewidth = 3)
      )) +
    
    theme_geochem() +
    theme(
      legend.position = 'bottom',
      legend.box = "vertical",
      strip.text = element_text(face = "bold"),
      text = element_text(size = 130),
      axis.text.x = element_text(size = 100, angle = 45),
      legend.key.size = unit(8, "line"),
      legend.title = element_text(size = 110),
      legend.text = element_text(size = 90),
      legend.key.spacing.x = unit(1.0, "cm"),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(20, 100, 20, 20)) +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0, 50)),   # As
      scale_y_continuous(limits = c(0, 0.6)),   # Cd
      scale_y_continuous(limits = c(0, 400)),  # Cu
      scale_y_continuous(limits = c(0, 150)),  # Cr
      scale_y_continuous(limits = c(0, 70)),  # Ni
      scale_y_continuous(limits = c(0, 40)),  # Pb
      scale_y_continuous(limits = c(0, 200))   # Zn
    )
  )
} 
seq_quality_plot <- seq_quality(seq_novo)

tiff("TIFF/SEQ_QUALITY_SED_dodgeplot.tiff", width = 6000, height = 7500)
plot(seq_quality_plot)
dev.off()

seq_quality2 <- function(data) {
  ggplot() +
    
    geom_rect(data = contamination_bound %>%
                dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
              
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = Classification),
              alpha = 0.4,
              color = NA
    ) +
    scale_fill_manual(
      values = boje_klasa,
      name = "Sediment quality (Bakke et al., 2010)"
    ) +
    
    ggnewscale::new_scale_fill() +
    
    geom_bar(
      data = seq_novo %>% filter(Element %in% elements_law),
      aes(x=Sample, 
          y=Concentration, 
          fill=reorder(Step, OrderStep)), 
      position = "stack", 
      stat = "identity", 
      width = 0.7) +
    scale_fill_manual(values = SSE_skala, name = "SSE fraction") +
    
    
    #scale_y_log10(expand = expansion(mult = 0.05)) +
    # coord_cartesian(ylim = c(0, pragmatic_max))+ 
    
    facet_wrap(
      ~factor(Element, levels = elements_law),
      scales = "free_y", 
      ncol = 2, 
      axes= "all", 
      axis.labels = "all"
    ) +
    labs(x="\n Sample", 
         y="Concentration [mg/kg]") +
    
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(
        override.aes = list(linewidth = 3)
      )) +
    
    theme_geochem() +
    theme(
      legend.position = 'bottom',
      legend.box = "vertical",
      strip.text = element_text(face = "bold"),
      text = element_text(size = 130),
      axis.text.x = element_text(size = 100, angle = 45),
      legend.key.size = unit(8, "line"),
      legend.title = element_text(size = 110),
      legend.text = element_text(size = 90),
      legend.key.spacing.x = unit(1.0, "cm"),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(20, 100, 20, 20)) +
    facetted_pos_scales(
      y = list(
        scale_y_continuous(limits = c(0, 150)),   # As
        scale_y_continuous(limits = c(0, 1.3)),   # Cd
        scale_y_continuous(limits = c(0, 1000)),  # Cu
        scale_y_continuous(limits = c(0, 170)),  # Cr
        scale_y_continuous(limits = c(0, 100)),  # Ni
        scale_y_continuous(limits = c(0, 60)),  # Pb
        scale_y_continuous(limits = c(0, 350))   # Zn
      )
    )
} 


seq_quality_plot2 <- seq_quality2(seq_novo)

tiff("TIFF/SEQ_QUALITY_SED_stack.tiff", width = 6000, height = 7500)
plot(seq_quality_plot2)
dev.off()

### ALTA - XRD ---------------------------------------------------------------------
#Annaselva
annasel <- read_excel("DATA/Anna_disertacija.xlsx", sheet = "Annaselva")
annaselva <- as.data.frame(annasel)
annaselva
min_anna <- read_excel("DATA/Anna_disertacija.xlsx", sheet = 2)
min_anna <- as.data.frame(min_anna)
min_anna

ANNA_XRD <- ggplot(annaselva, aes(x=Position, y=Sample)) +
  geom_ridgeline(aes(height=Intensity), fill="moccasin", alpha=0.5, scale = 0.0007)+
  geom_segment(data = min_anna, aes(x = Position-0.02, xend = Position,
                                  y = 1, yend = 20), linewidth = 0.5, color = "gray30") +
  new_scale_color()+
  geom_label_repel(data= min_anna, aes (x = Position, y = 20, label = str_wrap(Mineral, width = 4)), direction = "y", color = "gray30", fontface="bold",
                   size = 4.5, max.overlaps = 15) +
  theme_ridges() +
  labs( x = expression(paste("Diffraction angle [°2", theta, "]")), y = "Sample") +
  scale_x_continuous(breaks = seq(5, 65, by=10))
ANNA_XRD  #Nakon ovoga je potrebno malo urediti labelse, jer nije ba? 100% to?no, ali je i dalje bolje nego sve ru?no. 

tiff("TIFF/Annaselva_XRD_analysis.tiff", width = 4000, height = 3000, res = 300)
plot(ANNA_XRD)
dev.off()

#Mollneselva

mollnesel <-read_excel("DATA/XRD_Mollneselva.xlsx", sheet = "Mollneselva")
mollneselva <-  as.data.frame(mollnesel)
mollneselva
min_moll <- read_excel("DATA/XRD_Mollneselva.xlsx", sheet = "Minerals")
min_moll <- as.data.frame(min_moll)
min_moll


MOLLNE_XRD <- ggplot(mollneselva, aes(x=Position, y=Sample)) +
  geom_ridgeline(aes(height=Intensity), fill="powderblue", alpha=0.5, scale = 0.0007)+
  geom_segment(data = min_moll, aes(x = Position-0.02, xend = Position,
                                    y = 1, yend = 27), linewidth = 0.5, color = "gray30") +
  new_scale_color()+
  geom_label_repel(data= min_moll, aes (x = Position, y = 27, label = str_wrap(Mineral, width = 4)), direction = "y", color = "gray30", fontface="bold",
                   size = 4.5, max.overlaps = 15) +
  theme_ridges() +
  labs( x = expression(paste("Diffraction angle [°2", theta, "]")), y = "Sample") +
  scale_x_continuous(breaks = seq(5, 65, by=10))
MOLLNE_XRD

tiff("TIFF/Mollneselva_XRD_analysis.tiff", width = 4000, height = 3000, res = 300)
plot(MOLLNE_XRD)
dev.off()


#Brakkelva

brakkel <-read_excel("DATA/XRD_Brakkelva.xlsx", sheet = "Brakkelva")
brakkelva <-  as.data.frame(brakkel)
brakkelva
min_brakk <- read_excel("DATA/XRD_Brakkelva.xlsx", sheet = "Minerals")
min_brakk <- as.data.frame(min_brakk)
min_brakk


BRAKKE_XRD <- ggplot(brakkelva, aes(x=Position, y=Sample)) +
  geom_ridgeline(aes(height=Intensity), fill="rosybrown1", alpha=0.5, scale = 0.0007)+
  geom_segment(data = min_brakk, aes(x = Position-0.02, xend = Position,
                                    y = 1, yend = 25), linewidth = 0.5, color = "gray30") +
  new_scale_color()+
  geom_label_repel(data= min_brakk, aes (x = Position, y = 25, label = str_wrap(Mineral, width = 4)), direction = "y", color = "gray30", fontface="bold",
                   size = 4.5, max.overlaps = 15) +
  theme_ridges() +
  labs( x = expression(paste("Diffraction angle [°2", theta, "]")), y = "Sample") +
  scale_x_continuous(breaks = seq(5, 65, by=10))
BRAKKE_XRD

tiff("TIFF/Brakkelva_XRD_analysis.tiff", width = 4000, height = 3000, res = 300)
plot(BRAKKE_XRD)
dev.off()

# ALTA - SAVING ALL ALTA STATISTICS ----------------------------------------------

setwd("C:/Users/Laura/DOKTORAT/Norveska/Disertacija/R_project/ALTA_CSV")
csv_files <- list.files(pattern = "\\.csv$")
wb <- createWorkbook()
for (file in csv_files) {
  
  data <- read_csv(file, show_col_types = FALSE)
  
  sheet_name <- tools::file_path_sans_ext(basename(file))
  
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, data)
  
}
saveWorkbook(wb, "Alta_all_statistics.xlsx", overwrite = TRUE)


#ALTA - SPATIAL ANALYSIS ---------------------------------------------------------

### 1. LIBRARIES===========

library(sf)
library(ggplot2)
library(dplyr)
library(scales)
library(ggnewscale)
library(patchwork)
library(readr)
library(readxl)
library(ggpubr)
library(tidyverse)
library(extrafont)
library(gstat)
library(stars)
library(terra)


# import fonts - only once
font_import()

# load fonts - every session
loadfonts(device = "win", quiet = TRUE)



##SEMI_TOTALI========================
### 2. RADNI DIREKTORIJ============================

setwd("C:/Users/Laura/DOKTORAT/Norveska/Disertacija/R_project/MAP")


### 3. UČITAVANJE PODATAKA============================

watersheds <- st_read("DATA/Watersh_krig.shp")
geochem    <- st_read("DATA/Geochemistry_data2.shp")

names(geochem)[names(geochem) == "As_"]  <- "As"
names(geochem)[names(geochem) == "Ga_1"] <- "Ga"


### 4. ČIŠĆENJE============================

watersheds <- watersheds[!st_is_empty(watersheds), ] |> st_make_valid()
geochem    <- geochem[!st_is_empty(geochem), ]

coords <- st_coordinates(geochem)
geochem <- geochem[!is.na(coords[,1]) & !is.na(coords[,2]), ]


### 5. GRUPE ============================

group1 <- c("Ag","As","Bi","Cd","Co","Cr","Cu","Hg","Mn","Mo","Ni","Pb","Sb","Sc","Zn")
group1_left  <- group1[1:8]
group1_right <- group1[9:15]

group2 <- c("Al","Ca","Fe","K","Mg","Na")
group2_left  <- group2[1:3]
group2_right <- group2[4:6]

group3 <- c("Au","Ba","Ga","La","P","S","Se","Sr","Te","Th","Ti","Tl","U","V","W")
group3_left  <- group3[1:8]
group3_right <- group3[9:15]

### 6. BOJE PO ELEMENTIMA============================
# 
element_colors <- c(
  Ag="gray10", 
  As="lemonchiffon", 
  Bi="darksalmon", 
  Cd="orchid",
  Co="midnightblue", 
  Cr="darkorchid4", 
  Cu="orange1", 
  Hg="red1",
  Mn="magenta1", 
  Mo="gray80", 
  Ni="pink1", 
  Pb="darkolivegreen4",
  Sb="turquoise3", 
  Sc="burlywood", 
  Zn="plum", 
  
  #Group 2
  Al = "lightblue",
  Ca = "khaki",
  Fe = "firebrick",
  K  = "tan",
  Mg = "lightgreen",
  Na = "skyblue",
  
  # Group 3
  Au = "gold",
  Ba = "lightsteelblue",
  Ga = "lightpink",
  La = "wheat",
  P  = "palegreen",
  S  = "lightyellow",
  Se = "salmon",
  Sr = "lightskyblue",
  Te = "thistle",
  Th = "gray50",
  Ti = "slateblue",
  Tl = "cadetblue",
  U  = "darkseagreen",
  V  = "mediumorchid",
  W  = "darkgoldenrod"
  
)


### 6a. PALETE BOJA (15 NIJANSI PO ELEMENTU)============================

element_palettes <- lapply(element_colors, function(base_col) {
  colorRampPalette(c("white", base_col))(15)
})



### 7. STACKANI SLOJEVI (PSEUDO‑3D)============================

plot_group_stack <- function(elements, layer_spacing = 5000) {
  
  p <- ggplot()
  offset <- 0
  
  base_coords <- as.data.frame(st_coordinates(geochem))
  
  for (el in elements) {
    
    poly_data <- st_join(watersheds, geochem) |>
      group_by(geometry) |>
      summarise(value = mean(.data[[el]], na.rm = TRUE),
                .groups = "drop")
    
    poly_data$geometry <- poly_data$geometry + c(0, offset)
    
    pts <- geochem
    pts$geometry <- pts$geometry + c(0, offset)
    
    segs <- base_coords
    segs$y0 <- base_coords$Y
    segs$y1 <- base_coords$Y + offset
    
    pal <- element_palettes[[el]]
    
    bbox <- st_bbox(poly_data)
    label_x <- bbox$xmin - 0.08 * (bbox$xmax - bbox$xmin)
    label_y <- bbox$ymax
    
    p <- p +
      # linije iza slojeva
      geom_segment(
        data = segs,
        aes(x = X, xend = X,
            y = y0, yend = y1),
        color = "grey40",
        linewidth = 0.1,
        alpha = 0.1
      ) +
      
      # ✅ OVO JE KLJUČNO
      ggnewscale::new_scale_fill() +
      
      geom_sf(
        data = poly_data,
        aes(fill = value),
        color = "grey40",
        linewidth = 0.1
      ) +
      
      scale_fill_gradientn(
        colours  = pal,
        na.value = "grey90",
        guide    = "none"
      ) +
      
      geom_sf(
        data = pts,
        color = "red3",
        size = 0.3
      ) +
      
      annotate(
        "text",
        x = label_x,
        y = min(st_coordinates(st_centroid(poly_data))[,2]) + offset,
        label = el,
        hjust = 1,
        vjust = 0.5,
        size = 14,
        fontface = "bold"
      )
    
    offset <- offset + layer_spacing
  }
  
  p +
    coord_sf(
      expand = FALSE,
      ylim = fixed_y_range
    ) +
    theme_void()
  
}

### 8. GENERIRANJE KARTE - GRUPA 1============================
# 

layer_spacing <- 5200

max_layers <- max(length(group1_left), length(group1_right))

fixed_y_range <- c(
  st_bbox(watersheds)$ymin,
  st_bbox(watersheds)$ymin + max_layers * layer_spacing
)


map_left  <- plot_group_stack(group1_left)
map_right <- plot_group_stack(group1_right)


map_final <- map_left | map_right
map_final




ggsave(
  "MAP/Group1_stacked_2columns_fixed.pdf",
  map_final,
  device = cairo_pdf,   # VAŽNO - omogućuje transparentnost, složene poligone i više fill skala(ggnewscale)
  width = 30,
  height = 40,
  units = "cm"
)

ggsave(
  "MAP/Group1_stacked_2columns_fixed.pdf",
  map_final,
  device = cairo_pdf,   # VAŽNO - omogućuje transparentnost, složene poligone i više fill skala(ggnewscale)
  width = 30,
  height = 40,
  units = "cm"
)


### 9. GENERIRANJE KARTE - GRUPA 2============================


layer_spacing <- 5200

max_layers <- max(length(group2_left), length(group2_right))

fixed_y_range <- c(
  st_bbox(watersheds)$ymin,
  st_bbox(watersheds)$ymin + max_layers * layer_spacing
)


map_left2  <- plot_group_stack(group2_left)
map_right2 <- plot_group_stack(group2_right)


map_final2 <- map_left2 | map_right2
map_final2


ggsave(
  "MAP/Group2_stacked_2columns_fixed.pdf",
  map_final2,
  device = cairo_pdf,   # VAŽNO - omogućuje transparentnost, složene poligone i više fill skala(ggnewscale)
  width = 30,
  height = 30,
  units = "cm"
)



### 10. GENERIRANJE KARTE - GRUPA 3============================
# 

layer_spacing <- 5200

max_layers <- max(length(group3_left), length(group3_right))

fixed_y_range <- c(
  st_bbox(watersheds)$ymin,
  st_bbox(watersheds)$ymin + max_layers * layer_spacing
)


map_left3  <- plot_group_stack(group3_left)
map_right3 <- plot_group_stack(group3_right)


map_final3 <- map_left3 | map_right3
map_final3


ggsave(
  "MAP/Group3_stacked_2columns_fixed.pdf",
  map_final3,
  device = cairo_pdf,   # VAŽNO - omogućuje transparentnost, složene poligone i više fill skala(ggnewscale)
  width = 30,
  height = 40,
  units = "cm"
)

##SEQUENTIAL=====================

### 11. UČITAVANJE PODATAKA============================


watersheds_s <- st_read("MAP/DATA/Watersheds_krig.shp")

geochem_df <- read_excel("MAP/SEQ_SPATIAL_35Na.xlsx")

# ✅ eksplicitno provjeri da postoje X i Y
stopifnot(all(c("X", "Y") %in% names(geochem_df)))

geochem_sf <- st_as_sf(
  geochem_df,
  coords = c("X", "Y"),
  crs = st_crs(watersheds_s)
)

stopifnot(!is.na(st_crs(watersheds_s)))
stopifnot(st_crs(watersheds_s) == st_crs(geochem_sf))

### 12. ČIŠĆENJE ======================================

watersheds_s <- watersheds_s |>
  filter(!st_is_empty(geometry)) |>
  st_make_valid()

geochem_sf <- geochem_sf |>
  filter(!st_is_empty(geometry))

### 12.a PROSTORNI JOIN ================================

joined <- st_join(
  geochem_sf,
  watersheds_s,
  join = st_within
)

# ✅ SIGURNA AGREGACIJA – NE koristiti As1:Zn7
agg <- joined |>
  st_drop_geometry() |>
  group_by(Id) |>
  summarise(
    across(
      matches("^(As|Cd|Co|Cr|Cu|Mn|Mo|Ni|Pb|Sb|Sc|Zn)[1-7]$"),
      ~ mean(as.numeric(.x), na.rm = TRUE)
    ),
    .groups = "drop"
  )

### 12.b POVRATAK NA POLIGONE ==========================

watersheds_final <- watersheds_s |>
  left_join(agg, by = "Id")

# ✅ osiguraj da su koncentracije numeričke
watersheds_final <- watersheds_final |>
  mutate(
    across(
      matches("^(As|Cd|Co|Cr|Cu|Mn|Mo|Ni|Pb|Sb|Sc|Zn)[1-7]$"),
      as.numeric
    )
  )

# ✅ eksplicitno vrati CRS (after left_join)
st_crs(watersheds_final) <- st_crs(watersheds_s)


#### 13. GRUPE ============================
As_grupa <- c("As1", "As2", "As3", "As4", "As5", "As6", "As7")

Cd_grupa <- c("Cd1", "Cd2", "Cd3", "Cd4", "Cd5", "Cd6", "Cd7")

Co_grupa <- c("Co1", "Co2", "Co3", "Co4", "Co5", "Co6", "Co7")

Cr_grupa <- c("Cr1", "Cr2", "Cr3", "Cr4", "Cr5", "Cr6", "Cr7")

Cu_grupa <- c("Cu1", "Cu2", "Cu3", "Cu4", "Cu5", "Cu6", "Cu7")

Mn_grupa <- c("Mn1", "Mn2", "Mn3", "Mn4", "Mn5", "Mn6", "Mn7")

Mo_grupa <- c("Mo1", "Mo2", "Mo3", "Mo4", "Mo5", "Mo6", "Mo7")

Ni_grupa <- c("Ni1", "Ni2", "Ni3", "Ni4", "Ni5", "Ni6", "Ni7")

Pb_grupa <- c("Pb1", "Pb2", "Pb3", "Pb4", "Pb5", "Pb6", "Pb7")

Sb_grupa <- c("Sb1", "Sb2", "Sb3", "Sb4", "Sb5", "Sb6", "Sb7")

Sc_grupa <- c("Sc1", "Sc2", "Sc3", "Sc4", "Sc5", "Sc6", "Sc7")

Zn_grupa <- c("Zn1", "Zn2", "Zn3", "Zn4", "Zn5", "Zn6", "Zn7")

#### 14. BOJE PO ELEMENTIMA============================
library(colorspace)

SSE_fraction_colors <- c(
  "1" = "#BC31A4",  # Water soluble
  "2" = "#B6282F",  # Exchangeable
  "3" = "#EB7547",  # Poorly crystalline Fe
  "4" = "#F0E442",  # Crystalline Fe
  "5" = "#1B9E77",  # Organic matter
  "6" = "#00A8C6",  # Primary sulfides
  "7" = "#0072B2"   # Residual
)

make_frac_palette <- function(step, n = 20) {
  base_col <- SSE_fraction_colors[[step]]
  colorRampPalette(c("white", base_col))(n)
}


#### 14a. ZA LEGENDU======================

#Dummy data za SSE legendu
SSE_legend_df <- data.frame(
  frac = factor(
    c("1","2","3","4","5","6","7"),
    levels = c("1","2","3","4","5","6","7")
  ),
  label = c(
    "Water soluble fraction",
    "Exchangeable fraction",
    "Poorly crystalline Fe-oxyhydroxides fraction",
    "Crystalline Fe-oxides fraction",
    "Organic matter fraction",
    "Primary sulfide fraction",
    "Residual fraction"
  )
)

# Dummy plot koji služi SAMO za legendu
SSE_legend_plot <- ggplot(SSE_legend_df, aes(x = frac, y = 1, fill = frac)) +
  geom_tile() +
  scale_fill_manual(
    name   = "SSE fractions",
    values = SSE_fraction_colors,
    labels = SSE_legend_df$label
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "left",
      nrow = 4, 
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    text=element_text(family = "Corbel Bold"), 
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9)
  )


#### 15. STACKANI SLOJEVI (PSEUDO‑3D)============================

st_crs(watersheds_final)

base_coords <- as.data.frame(st_coordinates(geochem_sf))
base_coords$X0 <- base_coords$X
base_coords$Y0 <- base_coords$Y



plot_stack_by_group <- function(
    data,
    points,
    base_coords,
    elements,
    steps,
    spacing = 7000
) {
  
  stopifnot(!is.na(st_crs(data)))
  stopifnot(st_crs(data) == st_crs(points))
  
  p <- ggplot()
  offset <- 0
  
  for (i in seq_along(elements)) {
    
    el   <- elements[i]
    step <- steps[i]
    
    # ---------- POLIGONI ----------
    poly_data <- data |>
      dplyr::select(value = all_of(el))
    
    geom_offset <- st_geometry(poly_data) + c(0, offset)
    st_geometry(poly_data) <- geom_offset
    st_crs(poly_data) <- st_crs(data)
    
    vals <- poly_data$value
    vals <- vals[is.finite(vals)]
    
    pal <- make_frac_palette(step)
    
    # ---------- TOČKE ----------
    pts <- points
    geom_pts <- st_geometry(pts) + c(0, offset)
    st_geometry(pts) <- geom_pts
    st_crs(pts) <- st_crs(points)
    
    # ---------- SEGMENTI ----------
    segs <- base_coords
    segs$y1 <- base_coords$Y0 + offset
    
    # ---------- CRTANJE ----------
    p <- p +
      # vertikalne linije (IZA svega)
      geom_segment(
        data = segs,
        aes(
          x    = X0, xend = X0,
          y    = Y0, yend = y1
        ),
        colour = "grey40",
        linewidth = 0.15,
        alpha = 0.15
      ) +
      
      # NOVA fill skala ZA OVAJ SLOJ
      ggnewscale::new_scale_fill() +
      
      geom_sf(
        data = poly_data,
        aes(fill = value),
        colour = "grey40",
        linewidth = 0.1
      )
    
    # ---- ROBUSNA SKALA BOJA ----
    if (length(vals) == 0) {
      
      p <- p +
        scale_fill_manual(
          values = NA,
          na.value = NA,
          guide = "none"
        )
      
    } else if (length(unique(vals)) == 1) {
      
      p <- p +
        scale_fill_manual(
          values = pal[length(pal)],
          na.value = NA,
          guide = "none"
        )
      
    } else {
      
      p <- p +
        scale_fill_gradientn(
          colours = pal,
          limits  = range(vals),
          oob     = scales::squish,
          na.value = NA,
          guide = "none"
        )
    }
    
    # TOČKE (NA VRHU)
    p <- p +
      geom_sf(
        data = pts,
        colour = "red3",
        size = 0.35
      )
    
    offset <- offset + spacing
  }
  
  p +
    coord_sf(expand = FALSE) +
    theme_void()
}

###16. GENERIRANJE KARTE PO ELEMENTIMA=====================
As_grupa <- c("As1","As2","As3","As4","As5","As6","As7")
As_steps <- c("1","2","3","4","5","6","7")

#As
plot_As <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = As_grupa,
  steps       = As_steps,
  spacing     = 5000
)

plot_As

#Cd
Cd_steps <- c("1","2","3","4","5","6","7")
plot_Cd <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Cd_grupa,
  steps       = Cd_steps,
  spacing     = 5000
)

plot_Cd

#Co
Co_steps <- c("1","2","3","4","5","6","7")
plot_Co <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Co_grupa,
  steps       = Co_steps,
  spacing     = 5000
)

plot_Co

#Cr
Cr_steps <- c("1","2","3","4","5","6","7")
plot_Cr <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Cr_grupa,
  steps       = Cr_steps,
  spacing     = 5000
)

plot_Cr

#Cu
Cu_steps <- c("1","2","3","4","5","6","7")
plot_Cu <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Cu_grupa,
  steps       = Cu_steps,
  spacing     = 5000
)

plot_Cu

#Mn
Mn_steps <- c("1","2","3","4","5","6","7")
plot_Mn <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Mn_grupa,
  steps       = Mn_steps,
  spacing     = 5000
)

plot_Mn

#Mo
Mo_steps <- c("1","2","3","4","5","6","7")
plot_Mo <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Mo_grupa,
  steps       = Mo_steps,
  spacing     = 5000
)

plot_Mo

#Ni
Ni_steps <- c("1","2","3","4","5","6","7")
plot_Ni <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Ni_grupa,
  steps       = Ni_steps,
  spacing     = 5000
)

plot_Ni


#Pb
Pb_steps <- c("1","2","3","4","5","6","7")
plot_Pb <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Pb_grupa,
  steps       = Pb_steps,
  spacing     = 5000
)

plot_Pb

#Sb
Sb_steps <- c("1","2","3","4","5","6","7")

#Dodavanje mikroskopskog šuma da ne bude “jedna vrijednost”, odnosno da se 
#zaobiđu sve nule i NA vrijednosti. Iako podaci nisu točni, nule se zamijenjuju 
#vrijednostima 1e-12, ta je vrijednost toliko mala da ne mijenja prostornu 
#Varijabilnost kem elementa, ali omogućava generiranje karata
watersheds_Sb <- watersheds_final

for (col in Sb_grupa) {
  v <- watersheds_Sb[[col]]
  if (length(unique(v[is.finite(v)])) == 1) {
    watersheds_Sb[[col]] <- v + rnorm(length(v), 0, 1e-12)
  }
}

plot_Sb <- plot_stack_by_group(
  data        = watersheds_Sb,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Sb_grupa,
  steps       = Sb_steps,
  spacing     = 5000
)

plot_Sb


#Sc
Sc_steps <- c("1","2","3","4","5","6","7")

#Dodavanje mikroskopskog šuma da ne bude “jedna vrijednost”, odnosno da se 
#zaobiđu sve nule i NA vrijednosti. Iako podaci nisu točni, nule se zamijenjuju 
#vrijednostima 1e-12, ta je vrijednost toliko mala da ne mijenja prostornu 
#Varijabilnost kem elementa, ali omogućava generiranje karata
watersheds_Sc <- watersheds_final

for (col in Sc_grupa) {
  v <- watersheds_Sc[[col]]
  if (length(unique(v[is.finite(v)])) == 1) {
    watersheds_Sc[[col]] <- v + rnorm(length(v), 0, 1e-12)
  }
}


plot_Sc <- plot_stack_by_group(
  data        = watersheds_Sc,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Sc_grupa,
  steps       = Sc_steps,
  spacing     = 5000
)

plot_Sc

#Zn
Zn_steps <- c("1","2","3","4","5","6","7")
plot_Zn <- plot_stack_by_group(
  data        = watersheds_final,
  points      = geochem_sf,
  base_coords = base_coords,
  elements    = Zn_grupa,
  steps       = Zn_steps,
  spacing     = 5000
)

plot_Zn


### 17. GENERIRANJE KARTE ============================

names_seq <- c( "Residual fraction", "Primary sulfide fraction", "Organic matter fraction", 
                "Crystalline Fe-oxides fraction", "Poorly crystalline \n Fe-oxyhydroxides fraction", 
                "Exchangeable fraction", "Water soluble fraction"
)


names_df <- data.frame(
  label = factor(
    names_seq,
    levels = rev(names_seq)  # gore → dolje
  )
)

mid <- ggplot(names_df, aes(x = 1, y = label)) +
  geom_text(
    aes(label = label),
    size = 5,
    hjust = 0.5
  ) +
  scale_x_continuous(limits = c(0.9, 1.1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = unit(c(-2, -1, -0.7, -1), "mm")
  )

mid

spacer <- ggplot()+theme_void()


map_final1S <- ggarrange(
  ggarrange(
    plot_As, mid, plot_Cd, 
    spacer, spacer, spacer, 
    plot_Co, mid, plot_Cr,
    ncol = 3,
    nrow = 3,
    heights = c(1, 0.1, 1),   #sređuje razmak između grafova
    widths = c(1, 0.3, 1),
    labels = c("As", "","Cd"," ", "" , "" , "Co", "", "Cr"),
    hjust = -4,
    vjust = 3,
    align = "hv"
  ),
  SSE_legend_plot,
  ncol = 1,
  heights = c(1, 0.06)
)+
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 


map_final1S


tiff("MAP/TIF/As_Cd_Co_Cr.tiff", res=300, width = 3000, height = 6000)
plot(map_final1S)
dev.off()

map_final2S <- ggarrange(
  ggarrange(
    plot_Cu, mid, plot_Mn, 
    spacer, spacer, spacer, 
    plot_Mo, mid, plot_Ni,
    ncol = 3,
    nrow = 3,
    heights = c(1, 0.1, 1),   #sređuje razmak između grafova
    widths = c(1, 0.3, 1),
    labels = c("Cu", "","Mn"," ", "" , "" , "Mo", "", "Ni"),
    hjust = -4,
    vjust = 3,
    align = "hv"
  ),
  SSE_legend_plot,
  ncol = 1,
  heights = c(1, 0.06)
)+
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))


map_final2S


tiff("MAP/TIF/Cu_Mn_Mo_Ni.tiff", res=300, width = 3000, height = 6000)
plot(map_final2S)
dev.off()

map_final3S <- ggarrange(
  ggarrange(
    plot_Pb, mid, plot_Sb, 
    spacer, spacer, spacer, 
    plot_Sc, mid, plot_Zn,
    ncol = 3,
    nrow = 3,
    heights = c(1, 0.1, 1),   #sređuje razmak između grafova
    widths = c(1, 0.3, 1),
    labels = c("Pb", "","Sb"," ", "" , "" , "Sc", "", "Zn"),
    hjust = -4,
    vjust = 3,
    align = "hv"
  ),
  SSE_legend_plot,
  ncol = 1,
  heights = c(1, 0.06)
)+
  theme(
    plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) 


map_final3S


tiff("MAP/TIF/Pb_Sb_Sc_Zn.tiff", res=300, width = 3000, height = 6000)
plot(map_final3S)
dev.off()

# ALTA - QA ---------------------------------------------------------------
seq_qa_alta <- read_excel('DATA/Alta_QA.xlsx', sheet = 'Table1_1')
seq_qa_alta

seq_qa_alta <- data.frame(seq_qa_alta)
seq_qa_alta 

str(seq_qa_alta)
colnames(seq_qa_alta)


Aqa_plots <- function(seq_qa_alta){
  
  library(dplyr)
  library(stringr)
  library(ggplot2)
  
  grupa1 <- c("As", "Cd", "Co", "Cr", "Cu", "Mn")
  
  step_order <- c(
    "Water soluble fraction",
    "Exchangeable fraction",
    "Poorly crystalline Fe(III)-oxyhydroxides",
    "Crystalline Fe(III)-oxides",
    "Organic matter",
    "Primary sulfide fraction",
    "Residual fraction"
  )
  
  data_clean_Aqa <- seq_qa_alta %>%
    filter(Element %in% grupa1) %>%
    filter(!is.na(Element)) %>%
    
    # ✅ Sample stupac
    mutate(
      Sample = .[[grep("sample", names(.), ignore.case = TRUE)[1]]]
    ) %>%
    
    # ✅ Concentration cleanup
    mutate(
      Concentration = as.character(Concentration),
      Concentration = gsub(",", ".", Concentration),
      Concentration = gsub("[^0-9.]", "", Concentration),
      Concentration = as.numeric(Concentration)
    ) %>%
    
    # ✅ Step order
    mutate(
      Step = factor(Step, levels = step_order)
    ) %>%
    
    # ✅ grupiranje
    mutate(
      Sample_group = str_replace(Sample, "[a-zA-Z]$", "")
    ) %>%
    
    # ✅ NOVI FILTER
    filter(
      Sample_group == "J016" | Step == "Residual fraction"
    )
  
  # ✅ statistika
  stats <- data_clean_Aqa %>%
    group_by(Element, Step, Sample_group) %>%
    summarise(
      mean_val = mean(Concentration, na.rm = TRUE),
      median_val = median(Concentration, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ✅ plot
  ggplot(data_clean_Aqa, aes(x = Sample_group, y = Concentration)) +
    
    geom_col(
      aes(fill = Step,
          group = interaction(Sample, Step)),
      # stat = "identity",
      position = position_dodge(width = 0.8),
      alpha = 0.6
    ) +
    
    geom_boxplot(
      aes(group = Sample_group),
      width = 0.5,
      fill = NA,
      color = "black",
      outlier.shape = NA, 
      alpha = 0.7
    ) +
    
    geom_point(
      data = stats,
      aes(x = Sample_group, y = median_val),
      color = "red",
      size = 9
    ) +
    
    geom_point(
      data = stats,
      aes(x = Sample_group, y = mean_val),
      color = "blue",
      shape = 4,
      size = 11
    ) +
    
    facet_grid2(      #ggh4x paket
      rows = vars(Element),
      cols = vars(Step),
      scales = "free",
      independent = "all",
      space = "free_x",
      labeller = labeller(
        Step = label_wrap_gen(width = 20)
      ))+
    
    labs(
      x = "Sample group",
      y = "Concentration [mg/kg]",
      fill = ""
    ) +
    scale_x_discrete(drop = TRUE)+
    
    
    scale_fill_manual(values = SSE_skala) +
    
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      strip.text = element_text(face = "bold"),
      text = element_text(size = 60),
      axis.text.x = element_text(angle = 45),
      legend.key.size = unit(5, "line"),
      legend.title = element_text(size = 65),
      legend.text = element_text(size = 60)
    )
}


polt_Aqa <- Aqa_plots(seq_qa_alta)

tiff("TIFF/QA_ALTA_G1.tiff", width = 4000, height = 5000)
plot(polt_Aqa)
dev.off()


Aqa2_plots <- function(seq_qa_alta){
  
  library(dplyr)
  library(stringr)
  library(ggplot2)
  
  grupa2 <- c("Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
  
  step_order <- c(
    "Water soluble fraction",
    "Exchangeable fraction",
    "Poorly crystalline Fe(III)-oxyhydroxides",
    "Crystalline Fe(III)-oxides",
    "Organic matter",
    "Primary sulfide fraction",
    "Residual fraction"
  )
  
  data_clean_Aqa2 <- seq_qa_alta %>%
    filter(Element %in% grupa2) %>%
    filter(!is.na(Element)) %>%
    
    # ✅ Sample stupac
    mutate(
      Sample = .[[grep("sample", names(.), ignore.case = TRUE)[1]]]
    ) %>%
    
    # ✅ Concentration cleanup
    mutate(
      Concentration = as.character(Concentration),
      Concentration = gsub(",", ".", Concentration),
      Concentration = gsub("[^0-9.]", "", Concentration),
      Concentration = as.numeric(Concentration)
    ) %>%
    
    # ✅ Step order
    mutate(
      Step = factor(Step, levels = step_order)
    ) %>%
    
    # ✅ grupiranje
    mutate(
      Sample_group = str_replace(Sample, "[a-zA-Z]$", "")
    ) %>%
    
    # ✅ NOVI FILTER
    filter(
      Sample_group == "J016" | Step == "Residual fraction"
    )
  
  # ✅ statistika
  stats <- data_clean_Aqa2 %>%
    group_by(Element, Step, Sample_group) %>%
    summarise(
      mean_val = mean(Concentration, na.rm = TRUE),
      median_val = median(Concentration, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ✅ plot
  ggplot(data_clean_Aqa2, aes(x = Sample_group, y = Concentration)) +
    
    geom_bar(
      aes(fill = Step,
          group = interaction(Sample, Step)),
      stat = "identity",
      position = position_dodge(width = 0.8),
      alpha = 0.6
    ) +
    
    geom_boxplot(
      aes(group = Sample_group),
      width = 0.5,
      fill = NA,
      color = "black",
      outlier.shape = NA, 
      alpha = 0.7
    ) +
    
    geom_point(
      data = stats,
      aes(x = Sample_group, y = median_val),
      color = "red",
      size = 9
    ) +
    
    geom_point(
      data = stats,
      aes(x = Sample_group, y = mean_val),
      color = "blue",
      shape = 4,
      size = 11
    ) +
    
    facet_grid2(      #ggh4x paket
      rows = vars(Element),
      cols = vars(Step),
      scales = "free",
      independent = "all",
      space = "free_x",
      labeller = labeller(
        Step = label_wrap_gen(width = 20)
      ))+
    
    labs(
      x = "Sample group",
      y = "Concentration [mg/kg]",
      fill = ""
    ) +
    scale_x_discrete(drop = TRUE)+
    
    scale_fill_manual(values = SSE_skala) +
    
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      strip.text = element_text(face = "bold"),
      text = element_text(size = 60),
      axis.text.x = element_text(angle = 45),
      legend.key.size = unit(5, "line"),
      legend.title = element_text(size = 65),
      legend.text = element_text(size = 60)
    )
}


polt_Aqa2 <- Aqa2_plots(seq_qa_alta)

tiff("TIFF/QA_ALTA_G2.tiff", width = 4000, height = 5000)
plot(polt_Aqa2)
dev.off()

####SUMMED VALUES COMPARISON----------------------------------------------------
summedA <-  read_excel("DATA/Alta_QA.xlsx", sheet = "SUMMED")
summedA
summedA <-  data.frame(summedA)
str(summedA)

sample.levels <- c('J010', 'J033', 'J041')
sample.levels <- as.factor(sample.levels)
str(sample.levels)

summed_plot <- ggplot(summedA, aes(fill=Procedure, y= Concentration, x=Sample.Interval)) +
  geom_bar(position='dodge', stat = 'identity') +
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = c('violetred4', 'cornflowerblue'), name = "Legend")+
  scale_x_discrete(limits=c('J010', 'J033', 'J041'))+
  facet_wrap(~Element, scales = "free_y")+
  theme_minimal(base_size = 14) +
  labs(x="Sample", y="Concentration [mg/kg]")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        plot.title = element_text(size=13),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))


summed_plot

tiff("TIFF/Summed_values_Alta.tiff", res=300, width = 3000, height = 2500)
plot(summed_plot)
dev.off()


# REPPARFJORD - QA --------------------------------------------------------------

seq_by_samples <- read_excel('DATA/Data.xlsx', sheet = 'QA')
seq_by_samples

seq_by_samples <- data.frame(seq_by_samples)
seq_by_samples 

str(seq_by_samples)
# colnames(data_clean_qa)


qa_plots <- function(seq_by_samples){
  
  library(dplyr)
  library(stringr)
  library(ggplot2)
  
  # ✅ elementi interesa
  grupa1 <- c("As", "Cd", "Co", "Cr", "Cu", "Mn")
  
  # ✅ željeni redoslijed Step
  step_order <- c(
    "Water soluble fraction",
    "Exchangeable fraction",
    "Poorly crystalline Fe(III)-oxyhydroxides",
    "Crystalline Fe(III)-oxides",
    "Organic matter",
    "Primary sulfide fraction",
    "Residual fraction"
  )
  
  # ✅ čišćenje i priprema podataka
  data_clean_qa <- seq_by_samples %>%
    filter(Element %in% grupa1) %>%
    filter(!is.na(Element)) %>%
    
    # ✅ automatski pronađi Sample stupac
    mutate(
      Sample = .[[grep("sample", names(.), ignore.case = TRUE)[1]]]
    ) %>%
    
    # ✅ očisti Concentration (tekst → broj)
    mutate(
      Concentration = as.character(Concentration),
      Concentration = gsub(",", ".", Concentration),
      Concentration = gsub("[^0-9.]", "", Concentration),
      Concentration = as.numeric(Concentration)
    ) %>%
    
    # ✅ postavi Step redoslijed
    mutate(
      Step = factor(Step, levels = step_order)
    ) %>%
    
    # ✅ grupiraj uzorke (0-1a → 0-1)
    mutate(
      Sample_group = str_extract(Sample, "^[0-9]+-[0-9]+"), 
      Sample_group = factor(
        Sample_group,
        levels = c("0-1", "8-9", "23-24"))
    )
  
  # ✅ statistika
  stats <- data_clean_qa %>%
    group_by(Element, Step, Sample_group) %>%
    summarise(
      mean_val = mean(Concentration, na.rm = TRUE),
      median_val = median(Concentration, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ✅ plot
  ggplot(data_clean_qa, aes(x = Sample_group, y = Concentration)) +
    
    # BARPLOT
    geom_bar(
      aes(fill = Step,
          group = interaction(Sample, Step)),
      stat = "identity",
      position = position_dodge(width = 0.8),
      alpha = 0.6
    ) +
    
    # BOXPLOT
    geom_boxplot(
      aes(group = Sample_group),
      width = 0.5,
      fill = NA,
      color = "black",
      outlier.shape = NA
    ) +
    
    # MEDIAN (crveno)
    geom_point(
      data = stats,
      aes(x = Sample_group, y = median_val),
      color = "red",
      size = 9
    ) +
    
    # MEAN (plavo)
    geom_point(
      data = stats,
      aes(x = Sample_group, y = mean_val),
      color = "blue",
      shape = 4,
      size = 11
    ) +
    
    facet_grid2(      #ggh4x paket
      rows = vars(Element),
      cols = vars(Step),
      scales = "free",
      independent = "all",
      space = "free_x",
      labeller = labeller(
        Step = label_wrap_gen(width = 20)
      ))+
    
    labs(
      x = "Sample group",
      y = "Concentration [mg/kg]",
      fill = ""
    ) +
    
    scale_fill_manual(values = SSE_skala) +
    
    theme_minimal() +
    theme(
      legend.position = 'bottom',
      strip.text = element_text(face = "bold"),
      text = element_text(size = 60),
      axis.text.x = element_text(angle = 45),
      legend.key.size = unit(5, "line"),
      legend.title = element_text(size = 65),
      legend.text = element_text(size = 60),
      legend.key.spacing.x = unit(1.0, "cm")
    )
}
  
polt_qa <- qa_plots(seq_by_samples)

tiff("TIFF/QA_REPPAR_G1.tiff", width = 4000, height = 5000)
plot(polt_qa)
dev.off()

qa2_plots <- function(seq_by_samples){
  
  library(dplyr)
  library(stringr)
  library(ggplot2)
  
  # ✅ elementi interesa
  grupa2 <- c("Mo", "Ni", "Pb", "Sb", "Sc", "Zn")

# ✅ željeni redoslijed Step
step_order <- c(
  "Water soluble fraction",
  "Exchangeable fraction",
  "Poorly crystalline Fe(III)-oxyhydroxides",
  "Crystalline Fe(III)-oxides",
  "Organic matter",
  "Primary sulfide fraction",
  "Residual fraction"
)

# ✅ čišćenje i priprema podataka
data_clean_qa2 <- seq_by_samples %>%
  filter(Element %in% grupa2) %>%
  filter(!is.na(Element)) %>%
  
  # ✅ automatski pronađi Sample stupac
  mutate(
    Sample = .[[grep("sample", names(.), ignore.case = TRUE)[1]]]
  ) %>%
  
  # ✅ očisti Concentration (tekst → broj)
  mutate(
    Concentration = as.character(Concentration),
    Concentration = gsub(",", ".", Concentration),
    Concentration = gsub("[^0-9.]", "", Concentration),
    Concentration = as.numeric(Concentration)
  ) %>%
  
  # ✅ postavi Step redoslijed
  mutate(
    Step = factor(Step, levels = step_order)
  ) %>%
  
  # ✅ grupiraj uzorke (0-1a → 0-1)
  mutate(
    Sample_group = str_extract(Sample, "^[0-9]+-[0-9]+"), 
    Sample_group = factor(
      Sample_group,
      levels = c("0-1", "8-9", "23-24"))
  )

# ✅ statistika
stats <- data_clean_qa2 %>%
  group_by(Element, Step, Sample_group) %>%
  summarise(
    mean_val = mean(Concentration, na.rm = TRUE),
    median_val = median(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ plot
ggplot(data_clean_qa2, aes(x = Sample_group, y = Concentration)) +
  
  # BARPLOT
  geom_bar(
    aes(fill = Step,
        group = interaction(Sample, Step)),
    stat = "identity",
    position = position_dodge(width = 0.8),
    alpha = 0.6
  ) +
  
  # BOXPLOT
  geom_boxplot(
    aes(group = Sample_group),
    width = 0.5,
    fill = NA,
    color = "black",
    outlier.shape = NA
  ) +
  
  # MEDIAN (crveno)
  geom_point(
    data = stats,
    aes(x = Sample_group, y = median_val),
    color = "red",
    size = 9
  ) +
  
  # MEAN (plavo)
  geom_point(
    data = stats,
    aes(x = Sample_group, y = mean_val),
    color = "blue",
    shape = 4,
    size = 11
  ) +
  
  facet_grid2(      #ggh4x paket
    rows = vars(Element),
    cols = vars(Step),
    scales = "free",
    independent = "all",
    space = "free_x",
    labeller = labeller(
      Step = label_wrap_gen(width = 20)
    ))+
  
  labs(
    x = "Sample group",
    y = "Concentration [mg/kg]",
    fill = ""
  ) +
  
  scale_fill_manual(values = SSE_skala) +
  
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    strip.text = element_text(face = "bold"),
    text = element_text(size = 60),
    axis.text.x = element_text(angle = 45),
    legend.key.size = unit(5, "line"),
    legend.title = element_text(size = 65),
    legend.text = element_text(size = 60),
    legend.key.spacing.x = unit(1.0, "cm")
  )
}

polt_qa2 <- qa2_plots(seq_by_samples)

tiff("TIFF/QA_REPPAR_G2.tiff", width = 4000, height = 5000)
plot(polt_qa2)
dev.off()
  
###SUMMED VALUES COMPARISON  -----------------------------------------------
#Sumiranje sedam koraka SSE-a, i 4 koraka BCR-a (bez petog koraka TOT)

library(dplyr)
library(stringr)
library(readxl)

summed <- read_excel("C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx", sheet = "SEP7_BCR") %>%
  data.frame()

# ✅ KLJUČNO: pretvori u numeric
summed <- summed %>%
  mutate(
    Concentration = str_replace(Concentration, "<", ""),
    Concentration = as.numeric(str_replace(Concentration, ",", "."))
  )


# 1. filtriranje elemenata
elements_keep <- c("As","Cd","Co","Cr","Cu","Mn","Mo","Ni","Pb","Sb","Sc","Zn")

summed <- summed %>%
  filter(Element %in% elements_keep)

# 2. makni a/b/c iz Sample -> dobiješ bazni uzorak
summed <- summed %>%
  mutate(Sample_base = str_remove(Sample, "[abc]$"))

# 3. prosjek duplikata/triplikata
summed <- summed %>%
  group_by(Element, SEP, StepOrder, Step, Sample_base) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")

# 4. zbroj kroz sve korake (1–7 itd.)
summed <- summed %>%
  group_by(Element, SEP, Sample_base) %>%
  summarise(SUM = sum(Concentration, na.rm = TRUE), .groups = "drop")

# 5. standardiziraj ime stupca da ti paše u grafu
summed <- summed %>%
  rename(Sample.Interval = Sample_base)


sample.levels <- c('Standard', '0-1', '8-9', '14-15', '15-16', '16-17',
                  '17-18', '18-19', '19-20', '20-21', '21-22', '23-24')


# write.csv(summed, "Repparfjord_data.csv")
summedR_plot <- ggplot(summed, aes(fill=SEP, y= SUM, x=Sample.Interval)) +
  geom_bar(position='dodge', stat = 'identity') +
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()+
  geom_label(aes (label = round(SUM, 1),  position =  "nudge", 
                  vjust = -1, size = 3))+
  scale_fill_manual(values = c('aquamarine3', 'violetred4'), name = "Legend")+
  scale_x_discrete(limits=c('Standard', '0-1', '8-9', '14-15', '15-16', '16-17', '17-18', '18-19', 
                            '19-20', '20-21', '21-22', '23-24'))+
  facet_wrap(~Element, scales = "free_y")+
  theme_minimal(base_size = 14) +
  labs(x="Sample name (core interval) [cm]", y="Summed value of steps [mg/kg]")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        plot.title = element_text(size=13),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))


summedR_plot

tiff("TIFF/Summed_values_SSE_BCR_BCRTOT.tiff", res=300, width = 10000, height = 2500)
plot(summedR_plot)
dev.off()

#SUmmed bez residual (tot)

summed <- read_excel("C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx", sheet = "SEP7_BCR") %>%
  data.frame()

summed <- summed %>%
  mutate(
    Concentration = str_replace(Concentration, "<", ""),
    Concentration = as.numeric(str_replace(Concentration, ",", "."))
  )


# 1. filtriranje elemenata
elements_keep <- c("As","Cd","Co","Cr","Cu","Mn","Mo","Ni","Pb","Sb","Sc","Zn")

summed <- summed %>%
  filter(Element %in% elements_keep)

# ✅ NOVO: makni residual fraction
summed <- summed %>%
  filter(Step != "Residual fraction (TOT)")

# 2. makni a/b/c iz Sample -> dobiješ bazni uzorak
summed <- summed %>%
  mutate(Sample_base = str_remove(Sample, "[abc]$"))

# 3. prosjek duplikata/triplikata
summed <- summed %>%
  group_by(Element, SEP, StepOrder, Step, Sample_base) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE), .groups = "drop")

# 4. zbroj kroz sve korake (1–7 itd.)
summed <- summed %>%
  group_by(Element, SEP, Sample_base) %>%
  summarise(SUM = sum(Concentration, na.rm = TRUE), .groups = "drop")

# 5. standardiziraj ime stupca da ti paše u grafu
summed <- summed %>%
  rename(Sample.Interval = Sample_base)


sample.levels <- c('Standard', '0-1', '8-9', '14-15', '15-16', '16-17',
                   '17-18', '18-19', '19-20', '20-21', '21-22', '23-24')


# write.csv(summed, "Repparfjord_data.csv")
summedR2_plot <- ggplot(summed, aes(fill=SEP, y= SUM, x=Sample.Interval)) +
  geom_bar(position='dodge', stat = 'identity') +
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = c('aquamarine3', 'violetred4'), name = "Legend")+
  scale_x_discrete(limits=c('Standard', '0-1', '8-9', '14-15', '15-16', '16-17', '17-18', '18-19', 
                            '19-20', '20-21', '21-22', '23-24'))+
  facet_wrap(~Element, scales = "free_y")+
  theme_minimal(base_size = 14) +
  labs(x="Sample name (core interval) [cm]", y="Summed value of steps [mg/kg]")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        plot.title = element_text(size=13),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))


summedR2_plot

tiff("TIFF/Summed_values_SSE_BCR_bezBCRTOT.tiff", res=300, width = 3000, height = 2500)
plot(summedR2_plot)
dev.off()

# Repparfjord - LITHOGEOCHEMISTRY DATA  -----------------------------------
#treba kopirati iz druge skripte

#SEP Comparison ----------------------------------------------------------
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)

#negdje u kodu postoji neki bug ili greška. kad se eksportira izmijenjiva, bude 
# okej, ali čim se eksportira bilo koja sljedeća frakcija, stvara novi file
#i uz to prepisuje preko grafa za exchangeable.

####EXCHANGEABLE FRACTION====================================================

library(dplyr)
library(stringr)
library(readxl)

exchang_trends <- read_excel(
  "C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx",
  sheet = "SEP7_BCR"
) %>%
  data.frame() %>%
  
  # ✅ 1. čišćenje koncentracije
  mutate(
    Concentration = as.character(Concentration),
    Concentration = str_replace_all(Concentration, "<", ""),
    Concentration = str_replace_all(Concentration, ",", "."),
    Concentration = as.numeric(Concentration),
    Sample = as.character(Sample)
  ) %>%
  
  # ✅ 2. filtriranje elemenata
  filter(Element %in% c("As","Cd","Co","Cr","Cu","Mn",
                        "Mo","Ni","Pb","Sb","Sc","Zn")) %>%
  
  # ✅ 3. makni ST
  filter(!str_detect(Sample, "ST")) %>%
  
  # ✅ 5. uzmi samo exchangeable dio (SSE + BCR)
  filter(Step %in% c("Water soluble fraction",
                     "Exchangeable fraction")) %>%
  
  # ✅ 6. ukloni a/b/c
  mutate(Sample_base = str_remove(Sample, "[abc]$")) %>%
  
  # ✅ 4. zadrži samo željene uzorke
  filter(Sample_base %in% c("0-1", "8-9", "14-15", "15-16", "16-17",
                            "17-18", "18-19", "19-20", "20-21",
                            "21-22", "23-24")) %>%
  
  # ✅ 7. prosjek paralelki
  group_by(Element, SEP, Step, Sample_base, Depth) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE),
            .groups = "drop") %>%
  
  # ✅ 8. pretvori Depth u numeric
  mutate(Depth = as.numeric(str_extract(Depth, "^[0-9]+")))

# ✅ SSE: zbroji Water soluble + Exchangeable
SSE_part <- exchang_trends %>%
  filter(SEP == "Seven step sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ BCR: uzmi samo Exchangeable (već filtrirano)
BCR_part <- exchang_trends %>%
  filter(SEP == "BCR sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ spoji (sigurno, bez greške)
exchange_trends <- bind_rows(SSE_part, BCR_part)

exchange_trends <- exchange_trends %>%
  arrange(Element, SEP, Sample_base, Depth)


exchang_trends_mean <- exchang_trends %>%
  group_by(Element, SEP, Depth) %>%
  summarise(Concentration = mean(Concentration), .groups = "drop") %>%
  arrange(Element, SEP, Depth)



exchange_plot2 <- ggplot(exchang_trends_mean,
                       aes(x = Concentration,
                           y = Depth,
                           group = SEP)) +
  
  geom_ribbon(aes(xmin = 0,
                  xmax = Concentration),
              alpha = 0.2,
              fill = '#C44E52') +
  
  geom_path(aes(linetype = SEP),
            color = '#C44E52',
            linewidth = 1) +
  
  scale_linetype_manual(values = c(4, 1),
                        name = "Sequential extraction type") +
  
  scale_y_reverse() +
  
  facet_wrap(~Element, ncol= 6, scales = "free_x",
             labeller = labeller(
             SEP = label_wrap_gen(width = 20)
          )) +
  
  labs(x = "Concentration [mg/kg]",
       y = "Depth [cm]") +
  
  theme_minimal(base_size = 36) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 26, angle = 45),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    panel.spacing = unit(1, "cm"), 
    legend.key.size = unit(1.5, "cm"),
    legend.title.position = "left")

exchange_plot2

tiff("TIFF/Exchangeable_trends.tiff", res=300, width = 6000, height = 5000)
plot(exchange_plot2)
dev.off()

####REDUCIBLE FRACTION=======================================================

reduci_trends <- read_excel(
  "C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx",
  sheet = "SEP7_BCR"
) %>%
  data.frame() %>%
  
  # ✅ 1. čišćenje koncentracije
  mutate(
    Concentration = as.character(Concentration),
    Concentration = str_replace_all(Concentration, "<", ""),
    Concentration = str_replace_all(Concentration, ",", "."),
    Concentration = as.numeric(Concentration),
    Sample = as.character(Sample)
  ) %>%
  
  # ✅ 2. filtriranje elemenata
  filter(Element %in% c("As","Cd","Co","Cr","Cu","Mn",
                        "Mo","Ni","Pb","Sb","Sc","Zn")) %>%
  
  # ✅ 3. makni ST
  filter(!str_detect(Sample, "ST")) %>%
  
  # ✅ 5. uzmi samo exchangeable dio (SSE + BCR)
  filter(Step %in% c("Poorly crystalline Fe(III)-oxyhydroxides",
                     "Crystalline Fe(III)-oxides",
                     "Reducible fraction")) %>%
  
  # ✅ 6. ukloni a/b/c
  mutate(Sample_base = str_remove(Sample, "[abc]$")) %>%
  
  # ✅ 4. zadrži samo željene uzorke
  filter(Sample_base %in% c("0-1", "8-9", "14-15", "15-16", "16-17",
                            "17-18", "18-19", "19-20", "20-21",
                            "21-22", "23-24")) %>%
  
  # ✅ 7. prosjek paralelki
  group_by(Element, SEP, Step, Sample_base, Depth) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE),
            .groups = "drop") %>%
  
  # ✅ 8. pretvori Depth u numeric
  mutate(Depth = as.numeric(str_extract(Depth, "^[0-9]+")))

# ✅ SSE: zbroji Water soluble + Exchangeable
SSER_part <- reduci_trends %>%
  filter(SEP == "Seven step sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ BCR: uzmi samo Exchangeable (već filtrirano)
BCRR_part <- reduci_trends %>%
  filter(SEP == "BCR sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ spoji (sigurno, bez greške)
reducib_trends <- bind_rows(SSER_part, BCRR_part)

reducib_trends <- reducib_trends %>%
  arrange(Element, SEP, Sample_base, Depth)


reducib_trends_mean <- reducib_trends %>%
  group_by(Element, SEP, Depth) %>%
  summarise(Concentration = mean(Concentration), .groups = "drop") %>%
  arrange(Element, SEP, Depth)


reduci_plot2 <- ggplot(reducib_trends_mean,
                       aes(x = Concentration,
                           y = Depth,
                           group = SEP)) +
  
  geom_ribbon(aes(xmin = 0,
                  xmax = Concentration),
              alpha = 0.2,
              fill = '#DD8452') +
  
  geom_path(aes(linetype = SEP),
            color = '#DD8452',
            linewidth = 1) +
  
  scale_linetype_manual(values = c(4, 1),
                        name = "Sequential extraction type") +
  
  scale_y_reverse() +
  
  facet_wrap(~Element, ncol = 6, scales = "free_x",
             labeller = labeller(
               SEP = label_wrap_gen(width = 20)
             )) +
  
  labs(x = "Concentration [mg/kg]",
       y = "Depth [cm]") +
  
  theme_minimal(base_size = 36) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 26, angle = 45),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    panel.spacing = unit(1, "cm"), 
    legend.key.size = unit(1.5, "cm"),
    legend.title.position = "left"
  )

reduci_plot2

tiff("TIFF/Reducible__trends.tiff", res=300, width = 6000, height = 5000)
plot(reduci_plot2)
dev.off()


####OXIDIZABLE FRACTION====================================================
oxidiz_trends <- read_excel(
  "C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx",
  sheet = "SEP7_BCR"
) %>%
  data.frame() %>%
  
  # ✅ 1. čišćenje koncentracije
  mutate(
    Concentration = as.character(Concentration),
    Concentration = str_replace_all(Concentration, "<", ""),
    Concentration = str_replace_all(Concentration, ",", "."),
    Concentration = as.numeric(Concentration),
    Sample = as.character(Sample)
  ) %>%
  
  # ✅ 2. filtriranje elemenata
  filter(Element %in% c("As","Cd","Co","Cr","Cu","Mn",
                        "Mo","Ni","Pb","Sb","Sc","Zn")) %>%
  
  # ✅ 3. makni ST
  filter(!str_detect(Sample, "ST")) %>%
  
  # ✅ 5. uzmi samo exchangeable dio (SSE + BCR)
  filter(Step %in% c("Organic matter",
                     "Primary sulfide fraction",
                     "Oxidizable fraction")) %>%
  
  # ✅ 6. ukloni a/b/c
  mutate(Sample_base = str_remove(Sample, "[abc]$")) %>%
  
  # ✅ 4. zadrži samo željene uzorke
  filter(Sample_base %in% c("0-1", "8-9", "14-15", "15-16", "16-17",
                            "17-18", "18-19", "19-20", "20-21",
                            "21-22", "23-24")) %>%
  
  # ✅ 7. prosjek paralelki
  group_by(Element, SEP, Step, Sample_base, Depth) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE),
            .groups = "drop") %>%
  
  # ✅ 8. pretvori Depth u numeric
  mutate(Depth = as.numeric(str_extract(Depth, "^[0-9]+")))

# ✅ SSE: zbroji Water soluble + Exchangeable
SSEO_part <- oxidiz_trends %>%
  filter(SEP == "Seven step sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ BCR: uzmi samo Exchangeable (već filtrirano)
BCRO_part <- oxidiz_trends %>%
  filter(SEP == "BCR sequential extraction") %>%
  group_by(Element, SEP, Sample_base, Depth) %>%
  summarise(
    Concentration = sum(Concentration, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ spoji (sigurno, bez greške)
oxidizab_trends <- bind_rows(SSEO_part, BCRO_part)

oxidizab_trends <- oxidizab_trends %>%
  arrange(Element, SEP, Sample_base, Depth)


oxidizab_trends_mean <- oxidizab_trends %>%
  group_by(Element, SEP, Depth) %>%
  summarise(Concentration = mean(Concentration), .groups = "drop") %>%
  arrange(Element, SEP, Depth)



oxidiz_plot2 <- ggplot(oxidizab_trends_mean,
                       aes(x = Concentration,
                           y = Depth,
                           group = SEP)) +
  
  geom_ribbon(aes(xmin = 0,
                  xmax = Concentration),
              alpha = 0.2,
              fill = '#55A868') +
  
  geom_path(aes(linetype = SEP),
            color = '#55A868',
            linewidth = 1) +
  
  scale_linetype_manual(values = c(4, 1),
                        name = "Sequential extraction type") +
  
  scale_y_reverse() +
  
  facet_wrap(~Element, ncol=6, scales = "free_x",
             labeller = labeller(
               SEP = label_wrap_gen(width = 20)
             )) +
  
  labs(x = "Concentration [mg/kg]",
       y = "Depth [cm]") +
  
  theme_minimal(base_size = 36) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 26, angle = 45),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    panel.spacing = unit(1, "cm"), 
    legend.key.size = unit(1.5, "cm"),
    legend.title.position = "left"
  )

oxidiz_plot2

tiff("TIFF/Oxidizable__trends.tiff", res=300, width = 6000, height = 5000)
plot(oxidiz_plot2)
dev.off()


####RESIDUAL FRACTION==========================================================

residu_trends <- read_excel(
  "C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx",
  sheet = "SEP7_BCR"
) %>%
  data.frame() %>%
  
  # ✅ 1. čišćenje koncentracije
  mutate(
    Concentration = as.character(Concentration),
    Concentration = str_replace_all(Concentration, "<", ""),
    Concentration = str_replace_all(Concentration, ",", "."),
    Concentration = as.numeric(Concentration),
    Sample = as.character(Sample)
  ) %>%
  
  # ✅ 2. filtriranje elemenata
  filter(Element %in% c("As","Cd","Co","Cr","Cu","Mn",
                        "Mo","Ni","Pb","Sb","Sc","Zn")) %>%
  
  # ✅ 3. makni ST
  filter(!str_detect(Sample, "ST")) %>%
  
  # ✅ 5. uzmi samo exchangeable dio (SSE + BCR)
  filter(Step %in% c("Residual fraction (AR)",
                     "Residual fraction (TOT)",
                     "Residual fraction")) %>%
  
  # ✅ 6. ukloni a/b/c
  mutate(Sample_base = str_remove(Sample, "[abc]$")) %>%
  
  # ✅ 4. zadrži samo željene uzorke
  filter(Sample_base %in% c("0-1", "8-9", "14-15", "15-16", "16-17",
                            "17-18", "18-19", "19-20", "20-21",
                            "21-22", "23-24")) %>%
  
  # ✅ 7. prosjek paralelki
  group_by(Element, SEP, Step, Sample_base, Depth) %>%
  summarise(Concentration = mean(Concentration, na.rm = TRUE),
            .groups = "drop") %>%
  
  # ✅ 8. pretvori Depth u numeric
  mutate(Depth = as.numeric(str_extract(Depth, "^[0-9]+")))


# ✅ 1. AR (samo AR)
AR_part <- residu_trends %>%
  filter(Step == "Residual fraction (AR)") %>%
  group_by(Element, Sample_base, Depth) %>%
  summarise(Concentration = sum(Concentration, na.rm = TRUE),
            Residual_type = "BCR sequential extraction",
            .groups = "drop")

# ✅ 2. AR + TOT
AR_TOT_part <- residu_trends %>%
  filter(Step %in% c("Residual fraction (AR)", "Residual fraction (TOT)")) %>%
  group_by(Element, Sample_base, Depth) %>%
  summarise(Concentration = sum(Concentration, na.rm = TRUE),
            Residual_type = "BCR sequential extraction + TD",
            .groups = "drop")

# ✅ 3. SSE Residual (samo "Residual fraction")
RES_part <- residu_trends %>%
  filter(Step == "Residual fraction") %>%
  group_by(Element, Sample_base, Depth) %>%
  summarise(Concentration = sum(Concentration, na.rm = TRUE),
            Residual_type = "Seven step sequential extraction",
            .groups = "drop")

# ✅ spoj svega
residual_trends <- bind_rows(AR_part, AR_TOT_part, RES_part) %>%
  arrange(Element, Residual_type, Sample_base, Depth)



residual_trends_mean <- residual_trends %>%
  group_by(Element, Residual_type, Depth) %>%
  summarise(Concentration = mean(Concentration), .groups = "drop") %>%
  arrange(Element, Residual_type, Depth)



residu_plot2 <- ggplot(residual_trends_mean,
                       aes(x = Concentration,
                           y = Depth,
                           group = Residual_type)) +
  
  geom_ribbon(aes(xmin = 0,
                  xmax = Concentration),
              alpha = 0.2,
              fill = '#4C72B0') +
  
  geom_path(aes(linetype = Residual_type),
            color = '#4C72B0',
            linewidth = 1) +
  
  scale_linetype_manual(values = c(4, 3, 1),
                        name = "Sequential extraction type") +
  
  scale_y_reverse() +
  
  facet_wrap(~Element, ncol = 6, scales = "free_x",
             labeller = labeller(
               SEP = label_wrap_gen(width = 20)
             )) +
  
  labs(x = "Concentration [mg/kg]",
       y = "Depth [cm]") +
  
  theme_minimal(base_size = 36) +
  
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 26, angle = 45),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 24),
    panel.spacing = unit(1, "cm"), 
    legend.key.size = unit(1.5, "cm"),
    legend.title.position = "top"
  )

residu_plot2

tiff("TIFF/Residual_trends.tiff", res=300, width = 6000, height = 5000)
plot(residu_plot2)
dev.off()


setwd("C:/Users/Laura/Desktop/Repparfjord_stat")

#REPPARFJORD========================================================
#### Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
library(compositions)
library(ggpubr) #multiply graphs in one sheet
library(plotly)
library(knitr)
library(nortest)
library(energy)  #Rizzo and Szekely
library(corrplot)   #heatmap
library(reshape2)   #heatmap
library(factoextra)
library(FactoMineR)
library(magrittr)
library(RColorBrewer)
library(grDevices)
library(ggdendro)
library(writexl)
library(readr)
library(openxlsx)
library(ggnewscale)
library(marquee)

#### Colors --------------------------------------------------------------------

library(ggplot2)
library(scales)

SSE_skala <- c(
  "Water soluble fraction"                   = "#BC31A4", 
  "Exchangeable fraction"                    = "#B6282F",
  "Poorly crystalline Fe(III)-oxyhydroxides" = "#EB7547", 
  "Crystalline Fe(III)-oxides"               = "#D6C64A", 
  "Organic matter"                           = "#1B9E77", 
  "Primary sulfide fraction"                 = "#00A8C6", 
  "Residual fraction"                        = "#0072B2")

BCR_skala <- c(
  "Exchangeable fraction" = "#C44E52",
  "Reducible fraction"    = "#DD8452",
  "Oxidizable fraction"   = "#55A868",
  "Residual fraction (AR)"     = "#4C72B0", 
  "Residual fraction (TD)" = "#1f3a75"
)

Core_scale <- c(
  
  "#D17C2F",  # topla jantarno-narančasta (različita od moccasin)
  "#8B3C5D",  # prigušena vino/crveno-ljubičasta (ne preblizu rosybrown)
  "#2E86C1",  # jasna srednja plava (različita od powderblue/cornflower)
  "#2F7D5C"   # hladna smaragdno-zelena (ali ne limegreen/1B9E77)
  
)


heatmap_scale <- c(
  "#1F2A44", # 1 tamna plavo-siva
  "#2C3E66",
  "#3F5F8A",
  "#4EA8DE",
  "#7FC8F8",
  "#EAEAEA", # neutral
  "#FFD6A5",
  "#FFB703",
  "#FF9F1C",
  "#E36414",
  "#D62828"  # topli maksimum
)



# THEME
theme_geochem <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#D0D0D0", linewidth = 0.3),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "#333333"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14)
    )
}

TOC_color <- "#1F3A5F"   # mekša tamna plava (manje oštra od midnightblue)
TN_color  <- "#4C9F70"   # prirodna zelena (zamjena za limegreen)



#### SAVING ALL REPPARFJORD DATA ---------------------------------------------
setwd("C:/Users/Laura/Desktop/Repparfjord_stat/CSV")
csv_files <- list.files(pattern = "\\.csv$")
wb <- createWorkbook()
for (file in csv_files) {
  
  data <- read_csv(file, show_col_types = FALSE)
  
  sheet_name <- tools::file_path_sans_ext(basename(file))
  
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, data)
  
}
saveWorkbook(wb, "Repparfjord_all_statistics.xlsx", overwrite = TRUE)

#### XRD analysis ------------------------------------------------------------

library(ggridges)
library(ggrepel)

repparfj <- read_excel("DATA/XRD_Repparfjord.xlsx", sheet = "Repparfjord")
repparfjordx <- as.data.frame(repparfj)
repparfjordx
min_repp <- read_excel("DATA/XRD_Repparfjord.xlsx", sheet = "Minerals")
min_repp <- as.data.frame(min_repp)
min_repp

tailing <- data.frame(
  xmin = c (-Inf), 
  xmax = c (Inf), 
  ymin = c (5), 
  ymax = c (13))    

tailing$ymin_rev <- 25 - tailing$ymax
tailing$ymax_rev <- 25 - tailing$ymin

lines2 <- read_excel("DATA/XRD_Repparfjord.xlsx", sheet = "Lines")
lines2 <- as.data.frame(lines2)
lines2

C352_XRD <- ggplot(repparfjordx, aes(x=Position, y=reorder(as.factor(Sample), Depth, decreasing = TRUE))) +
  geom_rect(data = tailing,
            aes(xmin = xmin, xmax = xmax, ymin = ymin_rev, ymax = ymax_rev),
            fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
  geom_ridgeline(aes(height=Intensity), fill="#D7F5D7", alpha=0.5, scale = 0.005)+
  geom_segment(data = lines2, aes(x = Position-0.02, xend = Position,
                                  y = 1, yend = 32), linewidth = 0.5, color = "gray30") +
  new_scale_color()+
  geom_label_repel(data= lines2, aes (x = Position, y = 32, label = str_wrap(Mineral, width = 4)), direction = "y", color = "gray30", fontface = "bold",
                   size = 4, max.overlaps = 19) +
  # scale_y_reverse() + 
  
  labs(x = expression(paste("Diffraction angle [°2", theta, "]")), y = "Sample") +
  scale_x_continuous(breaks = seq(5, 65, by=10)) + 
  theme_geochem() #+
# theme(axis.text.y = element_blank())

C352_XRD  #Nakon ovoga je potrebno malo urediti labelse, jer nije ba? 100% to?no, ali je i dalje bolje nego sve ru?no. 

tiff("TIFF/Repparfjord_XRD_analysis.tiff", width = 4000, height = 3000, res = 300)
plot(C352_XRD)
dev.off()

#### LITHOGEOCHEMISTRY - Data -------------------------------------------------------------

reppar <- read_excel('DATA/Data.xlsx', sheet = "Semi_total_bulk (2)")
reppar
reppar <- data.frame(reppar)
rownames(reppar) <- reppar[,1]
reppar <- data.frame(reppar[,2:41])

reppar_tr <- read_excel('DATA/Data.xlsx', sheet = "Trends_bulk")
reppar_tr <- data.frame(reppar_tr)

Sample <- as.factor(reppar_tr$Sample)
Core <- as.factor(reppar_tr$Core)
Depth <- as.character(reppar_tr$Depth)
Mean_depth <- as.double(reppar_tr$Mean_depth)
Unit <- as.character(reppar_tr$Unit)
LLD <- as.double(reppar_tr$LLD)
Element <- as.factor(reppar_tr$Element)
Concentration <- as.double(reppar_tr$Semi_total)
TN <- as.double(reppar_tr$TN)
TOC <- as.double(reppar_tr$TOC)
TOC_TN <-  as.double(reppar_tr$TOC_TN)

repparfjord <- data.frame(Core, Depth, Mean_depth, Unit, LLD, Element, Concentration, TN, TOC, TOC_TN)
str(repparfjord)


#Ako se radi na svakoj pojedinoj jezgri onda treba izvu?i ove podatke ovdje. 
#po?ev?i s ponovnim u?itavanjem podataka za repparfjord
reppar <- read_excel('DATA/Data.xlsx', sheet = "Semi_total_bulk (2)")
reppar
core352 <- data.frame(reppar[1:24, ])
rownames(core352) <- core352[,1]
core352 <- data.frame(core352[ ,2:40])

core357 <- data.frame(reppar[25:35, ])
rownames(core357) <- core357[,1]
core357 <- data.frame(core357[ ,2:40])

core346 <- data.frame(reppar[36:50, ])
rownames(core346) <- core346[,1]
core346 <- data.frame(core346[ ,2:40])

core334 <- data.frame(reppar[51:91, ])
rownames(core334) <- core334[,1]
core334 <- data.frame(core334[ ,2:40])


tailings <- reppar[1:91, ][-c(25:35), ]
rownames(tailings) <- tailings[,1]
tailings <- data.frame(tailings[ ,2:40])

##### Pairwise log-ratios -----------------------------------------------------

tailings_acomp <- acomp(tailings)
tail <- tailings_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Variation_heatmap_334_346_352.tiff", res=450, width = 5000, height = 5000)
melt(tail) %>% 
  ggplot(aes(Var1, Var2, fill = value))+
  geom_tile()+
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) + 
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(tailings_acomp %>% variation) %>% write.csv("CSV/Tailings_334_346_352_variation.csv")


core352_acomp <- acomp(core352) 
c352 <- core352_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Core_352_pairwaise_logratio.tiff", res = 450, width = 5000, height = 5000)
melt(c352) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) + 
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(core352_acomp %>% variation) %>% write.csv("CSV/Core_352_variation.csv")

pdf("TIFF/Core_352_pairwaise_lr.pdf", width = 10, height = 10)
melt(c352) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) + 
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()


core357_acomp <- acomp(core357)
c357 <- core357_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Core_357_pairwaise_logratio.tiff", res = 450, width = 5000, height = 5000)
melt(c357) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(core357_acomp %>% variation) %>% write.csv("CSV/Core_357_variation.csv")

pdf("TIFF/Core_357_pairwaise_lr.pdf", width = 10, height = 10)
melt(c357) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()


core346_acomp <- acomp(core346)
c346 <- core346_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Core_346_pairwaise_logratio.tiff", res = 450, width = 5000, height = 5000)
melt(c346) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale)+
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(core346_acomp %>% variation) %>% write.csv("CSV/Core_346_variation.csv")

pdf("TIFF/Core_346_pairwaise_lr.pdf", width = 10, height = 10)
melt(c346) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale)+
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()

core334_acomp <- acomp(core334)
c334 <- core334_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Core_334_pairwaise_logratio.tiff", res = 450, width = 5000, height = 5000)
melt(c334) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(core334_acomp %>% variation) %>% write.csv("CSV/Core_334_variation.csv")

pdf("TIFF/Core_334_pairwaise_lr.pdf", width = 10, height = 10)
melt(c334) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()

reppar_acomp <- acomp(reppar)
repp <- reppar_acomp %>% variation %>% as.matrix %>% round(1)
tiff("TIFF/Repparfjord_all_values_pairwaise_logratio.tiff", res = 450, width = 5000, height = 5000)
melt(repp) %>% 
  ggplot(aes(Var1, Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = value), size = 4, angle=45)+
  scale_fill_gradientn(colors = heatmap_scale) +
  labs(x='', y='', fill='Pairwise \n log ratio \n value')+
  theme_geochem()+
  theme(axis.text = element_text(size=14), 
        axis.text.x = element_text(angle = 45), 
        legend.text = element_text(size = 12), 
        legend.key.size = unit(1, 'cm'))
dev.off()
print(reppar_acomp %>% variation) %>% write.csv("CSV/Repparfjord_all_values_variation.csv")



##### Pearson correlation -----------------------------------------------------
#CORE 352
core352_cor <- cor(core352_acomp, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
core352_cor

testResLog <- cor.mtest(core352_cor, conf.level = 0.95)

write.csv(core352_cor, "CSV/CORE_352_clr_Pearson_corr.csv")

#creating heatmap
tiff("TIFF/CORE_352_correlation_heatmap_Pearson.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core352_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/CORE_352_correlation_heatmap_Pearson_full.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core352_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

#CORE 357
core357_cor <- cor(core357_acomp, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
core357_cor

testResLog <- cor.mtest(core357_cor, conf.level = 0.95)

write.csv(core357_cor, "CSV/CORE_357_clr_Pearson_corr.csv")

#creating heatmap
tiff("TIFF/CORE_357_correlation_heatmap_Pearson.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core357_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/CORE_357_correlation_heatmap_Pearson_full.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core357_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  


#CORE 346
core346_cor <- cor(core346_acomp, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
core346_cor

testResLog <- cor.mtest(core346_cor, conf.level = 0.95)

write.csv(core346_cor, "CSV/CORE_346_clr_Pearson_corr.csv")

#creating heatmap
tiff("TIFF/CORE_346_correlation_heatmap_Pearson.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core346_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/CORE_346_correlation_heatmap_Pearson_full.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core346_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

#CORE 334
core334_cor <- cor(core334_acomp, method = 'pearson', robust = getOption("robust")) #Pearsonova korelacija podataka iz Annaselve
core334_cor

testResLog <- cor.mtest(core334_cor, conf.level = 0.95)

write.csv(core334_cor, "CSV/CORE_334_clr_Pearson_corr.csv")

#creating heatmap
tiff("TIFF/CORE_334_correlation_heatmap_Pearson.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core334_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'color', type = "lower", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  

tiff("TIFF/CORE_334_correlation_heatmap_Pearson_full.tiff", res = 600, width = 5000, height = 5000)   #opens file to be created
corrplot(core334_cor, p.mat = testResLog$p, sig.level = 0.95,
         method = 'square', type = "full", tl.col = 'gray28',
         order = 'hclust', hclust.method = 'average', addrect = 5, tl.cex = 0.7,
         tl.srt = 45, insig = 'n', col = heatmap_scale) #ne ?elimo da i?ta napravi s nezna?ajnim vrijednostima, i ?elimo da je u sklai boja od ljubi?aste do zelene, sa kontinuiranom skalom
#da dodamo col=COL2('PRGn', 10), tada bi skalu boja podijelio na 10 boja i odre?eni intervali bili bi odre?ene boje. 
dev.off()  



##### Descriptive statistics --------------------------------------------------
#Repparfjord entire dataset
reppar_ds <- as.data.frame(summary(reppar))

# 2 Makni red "NA's" ako postoji
reppar_ds <- reppar_ds[!rownames(reppar_ds) %in% "NA's", ]

# Kreiraj workbook
wb <- createWorkbook()
addWorksheet(wb, "Summary")

# Upis tablice (po?inje od retka 2 da ostavimo mjesto za naslov)
writeData(wb, "Summary", reppar_ds,
          startRow = 2,
          startCol = 2,
          rowNames = TRUE)

# Dodaj naslov iznad stupaca
writeData(wb, "Summary", "Repparfjord",
          startRow = 1,
          startCol = 2)

# Spoji ?elije za naslov (preko svih elemenata)
mergeCells(wb, "Summary",
           cols = 2:(ncol(reppar_ds)+1),
           rows = 1)

# Centriraj i podebljaj naslov
title_style <- createStyle(textDecoration = "bold",
                           halign = "center",
                           fontSize = 14)

addStyle(wb, "Summary", title_style,
         rows = 1, cols = 2,
         gridExpand = TRUE)

# Automatska ?irina stupaca
setColWidths(wb, "Summary", cols = 1:(ncol(reppar_ds)+1), widths = "auto")

# Spremi
saveWorkbook(wb,
             "CSV/Repparfjord_descriptive_summary.xlsx",
             overwrite = TRUE)

#CORE 352
core352_ds <- as.data.frame(summary(core352))

core352_ds <- core352_ds[!rownames(core352_ds) %in% "NA's", ]
wb <- createWorkbook()
addWorksheet(wb, "Summary")

writeData(wb, "Summary", core352_ds,
          startRow = 2,
          startCol = 2,
          rowNames = TRUE)

writeData(wb, "Summary", "Core 352",
          startRow = 1,
          startCol = 2)

mergeCells(wb, "Summary",
           cols = 2:(ncol(core352_ds)+1),
           rows = 1)

title_style <- createStyle(textDecoration = "bold",
                           halign = "center",
                           fontSize = 14)

addStyle(wb, "Summary", title_style,
         rows = 1, cols = 2,
         gridExpand = TRUE)

setColWidths(wb, "Summary", cols = 1:(ncol(core352_ds)+1), widths = "auto")

saveWorkbook(wb,
             "CSV/Core_352_descriptive_summary.xlsx",
             overwrite = TRUE)

#CORE 357
core357_ds <- as.data.frame(summary(core357))

core357_ds <- core357_ds[!rownames(core357_ds) %in% "NA's", ]
wb <- createWorkbook()
addWorksheet(wb, "Summary")

writeData(wb, "Summary", core357_ds,
          startRow = 2,
          startCol = 2,
          rowNames = TRUE)

writeData(wb, "Summary", "Core 357",
          startRow = 1,
          startCol = 2)

mergeCells(wb, "Summary",
           cols = 2:(ncol(core357_ds)+1),
           rows = 1)

title_style <- createStyle(textDecoration = "bold",
                           halign = "center",
                           fontSize = 14)

addStyle(wb, "Summary", title_style,
         rows = 1, cols = 2,
         gridExpand = TRUE)

setColWidths(wb, "Summary", cols = 1:(ncol(core357_ds)+1), widths = "auto")

saveWorkbook(wb,
             "CSV/Core_357_descriptive_summary.xlsx",
             overwrite = TRUE)

#CORE 346
core346_ds <- as.data.frame(summary(core346))

core346_ds <- core346_ds[!rownames(core346_ds) %in% "NA's", ]
wb <- createWorkbook()
addWorksheet(wb, "Summary")

writeData(wb, "Summary", core346_ds,
          startRow = 2,
          startCol = 2,
          rowNames = TRUE)

writeData(wb, "Summary", "Core 346",
          startRow = 1,
          startCol = 2)

mergeCells(wb, "Summary",
           cols = 2:(ncol(core346_ds)+1),
           rows = 1)

title_style <- createStyle(textDecoration = "bold",
                           halign = "center",
                           fontSize = 14)

addStyle(wb, "Summary", title_style,
         rows = 1, cols = 2,
         gridExpand = TRUE)

setColWidths(wb, "Summary", cols = 1:(ncol(core346_ds)+1), widths = "auto")

saveWorkbook(wb,
             "CSV/Core_346_descriptive_summary.xlsx",
             overwrite = TRUE)


#CORE 334
core334_ds <- as.data.frame(summary(core334))

core334_ds <- core334_ds[!rownames(core334_ds) %in% "NA's", ]
wb <- createWorkbook()
addWorksheet(wb, "Summary")

writeData(wb, "Summary", core334_ds,
          startRow = 2,
          startCol = 2,
          rowNames = TRUE)

writeData(wb, "Summary", "Core 334",
          startRow = 1,
          startCol = 2)

mergeCells(wb, "Summary",
           cols = 2:(ncol(core334_ds)+1),
           rows = 1)

title_style <- createStyle(textDecoration = "bold",
                           halign = "center",
                           fontSize = 14)

addStyle(wb, "Summary", title_style,
         rows = 1, cols = 2,
         gridExpand = TRUE)

setColWidths(wb, "Summary", cols = 1:(ncol(core334_ds)+1), widths = "auto")

saveWorkbook(wb,
             "CSV/Core_334_descriptive_summary.xlsx",
             overwrite = TRUE)



##### Violin plots ------------------------------------------------------------

repparfjord_v <- repparfjord %>% 
  mutate(log10_Concentration=log10(Concentration)) %>% 
  mutate(Core = recode(Core, "CORE-352" = "HH-352-BC-A")) %>%  #zamjenjuje vrijednosti u stupcu 
  mutate(Core = recode(Core, "CORE-334" = "HH-334-BC-B")) %>% 
  mutate(Core = recode(Core, "CORE-346" = "HH-346-BC-A")) %>% 
  mutate(Core = recode(Core, "CORE-357" = "HH-357-BC"))

#podjela na dvije grupe
grupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
grupa2 <- c("Al","Ca", "Fe", "K", "Mg", "Na")
grupa3 <- c("Ba", "Ce", "Hf", "La", "Li", "Nb", "P", "Rb", "Re", "S", "Se", "Sn", 
            "Sr", "Ta", "Th", "Ti", "U", "V", "W", "Y", "Zr")   #In, Te i Tl izba?eni jer nemaju vrijednosti

Rviolins <- function(data) {
  ggplot(filter(repparfjord_v, Element %in% grupa1),
         aes(x=Core, y=log10_Concentration, fill=Core)) +
    geom_violin(alpha=0.5, trim = F) +
    facet_wrap(~Element, scales = "free_y", ncol = 3) +
    labs(x="", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = Core_scale)+
    guides(
      fill = guide_legend(nrow = 1, byrow = TRUE),
    ) +
    theme_minimal()+
    geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_blank(), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 140), 
          legend.key.spacing.x = unit(1.5, "cm")) 
}

Rviolins2 <- function(data) {
  ggplot(filter(repparfjord_v, Element %in% grupa2),
         aes(x=Core, y=log10_Concentration, fill=Core)) +
    geom_violin(alpha=0.5, trim = F) +
    facet_wrap(~Element, scales = "free_y", ncol = 3) +
    labs(x="", 
         y="log10(Concentration)", 
         fill="")+
    scale_fill_manual(values = Core_scale)+
    guides(
      fill = guide_legend(nrow = 1, byrow = TRUE),
    ) +
    theme_minimal()+
    geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_blank(), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 140), 
          legend.key.spacing.x = unit(1.5, "cm")) 
} 

Rviolins3 <- function(data) {
  ggplot(filter(repparfjord_v, Element %in% grupa3),
         aes(x=Core, y=log10_Concentration, fill=Core)) +
    geom_violin(alpha=0.5, trim = F) +
    facet_wrap(~Element, scales = "free_y", ncol = 4) +
    labs(x="", 
         y="log10(Concentration)", 
         fill="")+
    scale_fill_manual(values = Core_scale)+
    guides(
      fill = guide_legend(nrow = 1, byrow = TRUE),
    ) +
    theme_minimal()+
    geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 130),
          axis.text.x = element_blank(), 
          legend.key.size = unit(10, "line"), 
          legend.title = element_text(size = 150), 
          legend.text = element_text(size = 140), 
          legend.key.spacing.x = unit(1.5, "cm")) 
} 


Rplot1 <- Rviolins(filter(repparfjord_v, Element %in% grupa1))

Rplot2 <- Rviolins2(filter(repparfjord_v, Element %in% grupa2))

Rplot3 <- Rviolins3(filter(repparfjord_v, Element %in% grupa3))

tiff("TIFF/Descriptive_statistics_semi_total_minor.tiff", width = 5000, height = 6500)
plot(Rplot1)
dev.off()

tiff("TIFF/Descriptive_statistics_semi_total_major.tiff", width = 5000, height = 6500)
plot(Rplot2)
dev.off()

tiff("TIFF/Descriptive_statistics_semi_total_append.tiff", width = 5000, height = 6500)
plot(Rplot3)
dev.off()



##### Lithogeochemistry trends ------------------------------------------------
repparfjord <- repparfjord %>% 
  mutate(Core = recode(Core, "CORE-352" = "HH-352-BC-A")) %>%  #zamjenjuje vrijednosti u stupcu 
  mutate(Core = recode(Core, "CORE-334" = "HH-334-BC-B")) %>% 
  mutate(Core = recode(Core, "CORE-346" = "HH-346-BC-A")) %>% 
  mutate(Core = recode(Core, "CORE-357" = "HH-357-BC"))

str(repparfjord)
grupa1 <- c("Ag", "As", "Bi",  "Cd", "Co", "Cr", "Cu", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")

data_clean <- repparfjord %>%
  dplyr::filter(Element %in% grupa1) %>%
  dplyr::filter(!is.na(Element)) %>% 
  arrange(Core, Element, Mean_depth)


tailing <- data.frame(
  xmin = c (-Inf), 
  xmax = c (Inf), 
  ymin = c (12), 
  ymax = c (23))

litho_trend <- function(repparfjord) {
  
  ggplot(data_clean, aes(y = Mean_depth)) +
    geom_rect(data = tailing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
    geom_path(data=data_clean,
              aes(x=Concentration, y= Mean_depth, color = Core, 
                  group = interaction(Core, Element)),
              linewidth = 3) +
    scale_color_manual(values = Core_scale) +
    scale_y_reverse() + 
    facet_wrap(~factor(Element, levels = grupa1),
               scales = "free_x", ncol = 3, drop = T) +
    labs(x="Concentration [mg/kg]", 
         y="Depth [cm]") +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 2))) +
    
    theme_minimal()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 100),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 130), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm"), 
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
          panel.spacing = unit(1.2, "lines"),
          plot.margin = margin(20, 20, 20, 20)) 
} 

litho_trend_plot <- litho_trend(filter(data_clean, Element %in% grupa1))

tiff("TIFF/Trends_lithogeochemistry_MOI.tiff", width = 4000, height = 5000)
plot(litho_trend_plot)
dev.off()
jpeg("TIFF/Trends_lithogeochemistry_MOI.jpeg", width = 4000, height = 5000)
plot(litho_trend_plot)
dev.off()

grupa2 <- c("Al", "Ba", "Ca", "Ce", "Fe", "Hf", "K", "La", "Li", "Mg", 
            "Na","Nb", "P", "Rb", "Re", "S", "Se", "Sn", 
            "Sr", "Ta", "Th", "Ti", "U", "V", "W", "Y", "Zr")

data_clean <- repparfjord %>%
  dplyr::filter(Element %in% grupa2) %>%
  dplyr::filter(!is.na(Element)) %>% 
  arrange(Core, Element, Mean_depth)


tailing <- data.frame(
  xmin = c (-Inf), 
  xmax = c (Inf), 
  ymin = c (12), 
  ymax = c (23))


litho_trend2 <- function(repparfjord) {
  
  ggplot(data_clean, aes(y = Mean_depth)) +
    geom_rect(data = tailing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
    geom_path(data=data_clean,
              aes(x=Concentration, y= Mean_depth, color = Core, 
                  group = interaction(Core, Element)),
              linewidth = 3) +
    scale_color_manual(values = Core_scale) +
    scale_y_reverse() + 
    facet_wrap(~factor(Element, levels = grupa2),
               scales = "free_x", ncol = 4, drop = T) +
    labs(x="Concentration [mg/kg]", 
         y="Depth [cm]") +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 2))) +
    
    theme_minimal()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 100),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 130), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm"), 
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
          panel.spacing = unit(1.2, "lines"),
          plot.margin = margin(20, 20, 20, 20)) 
} 

litho_trend_plot2 <- litho_trend2(filter(data_clean, Element %in% grupa2))

tiff("TIFF/Trends_lithogeochemistry_APP.tiff", width = 4000, height = 5000)
plot(litho_trend_plot2)
dev.off()

##### Lithogeochemistry + TOC + TN trends -------------------------------------
repparfjord

grupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")

data_clean <- repparfjord %>%
  dplyr::filter(Element %in% grupa1) %>%
  dplyr::filter(!is.na(Element)) %>% 
  arrange(Core, Element, Mean_depth)


tailing <- data.frame(
  xmin = c (-Inf), 
  xmax = c (Inf), 
  ymin = c (12), 
  ymax = c (23))

litho_trend3 <- function(repparfjord) {
  
  
  ggplot(data_clean, aes(y = Mean_depth)) +
    geom_ribbon(aes(xmin=0, xmax=TOC, y=Mean_depth),
                fill = TOC_color, alpha = 0.3, inherit.aes = FALSE, data = data_clean) +
    geom_ribbon(aes(xmin=0, xmax=TN, y=Mean_depth),
                fill = TN_color, alpha = 0.3, inherit.aes = FALSE, data = data_clean) +
    geom_rect(data = tailing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
    geom_path(data=data_clean,
              aes(x=Concentration, y= Mean_depth, color = Core, 
                  group = interaction(Core, Element)),
              linewidth = 3) +
    scale_color_manual(values = Core_scale) +
    scale_y_reverse() + 
    facet_wrap(~factor(Element, levels = grupa1),
               scales = "free_x", ncol = 3, drop = T) +
    labs(x="Concentration [mg/kg]", 
         y="Depth [cm]") +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 2))) +
    
    theme_minimal()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 100),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 130), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm"), 
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
          panel.spacing = unit(1.2, "lines"),
          plot.margin = margin(20, 20, 20, 20)) 
} 

litho_trend_plot3 <- litho_trend3(filter(data_clean, Element %in% grupa1))

tiff("TIFF/Trends_lithogeochemistry_TOC_TN_MOI.tiff", width = 4000, height = 5000)
plot(litho_trend_plot3)
dev.off()


litho_trend4 <- function(repparfjord) {
  
  grupa2 <- c("Al", "Ba", "Ca", "Ce", "Fe", "Hf", "K", "La", "Li", "Mg", 
              "Na","Nb", "P", "Rb", "Re", "S", "Se", "Sn", 
              "Sr", "Ta", "Th", "Ti", "U", "V", "W", "Y", "Zr")
  
  data_clean <- repparfjord %>%
    dplyr::filter(Element %in% grupa2) %>%
    dplyr::filter(!is.na(Element)) %>% 
    arrange(Core, Element, Mean_depth)
  
  
  tailing <- data.frame(
    xmin = c (-Inf), 
    xmax = c (Inf), 
    ymin = c (12), 
    ymax = c (23))
  
  ggplot(data_clean, aes(y = Mean_depth)) +
    geom_ribbon(aes(xmin=0, xmax=TOC, y=Mean_depth),
                fill = TOC_color, alpha = 0.3, inherit.aes = FALSE, data = data_clean) +
    geom_ribbon(aes(xmin=0, xmax=TN, y=Mean_depth),
                fill = TN_color, alpha = 0.3, inherit.aes = FALSE, data = data_clean) +
    geom_rect(data = tailing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
    geom_path(data=repparfjord,
              aes(x=Concentration, y= Mean_depth, color = Core, 
                  group = interaction(Core, Element)),
              linewidth = 3) +
    scale_color_manual(values = Core_scale) +
    scale_y_reverse() + 
    facet_wrap(~factor(Element, levels = grupa2),
               scales = "free_x", ncol = 4, drop = T) +
    labs(x="Concentration [mg/kg]", 
         y="Depth [cm]") +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 2))) +
    
    theme_minimal()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 100),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 130), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm"), 
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
          panel.spacing = unit(1.2, "lines"),
          plot.margin = margin(20, 20, 20, 20)) 
} 

litho_trend_plot4 <- litho_trend4(filter(repparfjord, Element %in% grupa2))

tiff("TIFF/Trends_lithogeochemistry_TOC_TN_APP.tiff", width = 4000, height = 5000)
plot(litho_trend_plot4)
dev.off()



#### TOC and TN  trends ------------------------------------------------------

repparfjord

data_clean <- repparfjord %>%
  dplyr::filter(Element %in% grupa1) %>%
  dplyr::filter(!is.na(Element))


toc_trends <- function(repparfjord){
  
  tailing <- data.frame(
    xmin = c (-Inf), 
    xmax = c (Inf), 
    ymin = c (12), 
    ymax = c (23))
  
  ggplot(repparfjord, aes(y=Mean_depth))+
    geom_rect(data = tailing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
    geom_ribbon(aes(xmin=0, xmax=TOC, y=Mean_depth),
                fill = TOC_color, alpha = 0.6, inherit.aes = FALSE)+
    geom_point(aes(x=TOC), 
               shape = 18, size = 16, colour=TOC_color, fill=TOC_color) +
    geom_ribbon(aes(xmin=0, xmax=TN, y=Mean_depth),
                fill = TN_color, alpha = 0.6, inherit.aes = FALSE, data = data_clean) +
    geom_point(aes(x=TN, y=Mean_depth), 
               shape = 20, size = 16, colour = TN_color, fill = TN_color)+
    scale_y_reverse() +
    
    facet_wrap(~Core, 
               scales = "free", ncol = 2, drop = T)+
    labs(x = "Concentration [mg/kg]", y = "Depth [cm]") +
    #guides(color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 2))) +
    
    theme_minimal()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 100),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(8, "line"), 
          legend.title = element_text(size = 130), 
          legend.text = element_text(size = 90), 
          legend.key.spacing.x = unit(1.0, "cm"), 
          panel.background = element_rect(fill = "white", colour = NA))
  # panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
  # panel.spacing = unit(1.2, "lines"),
  # plot.margin = margin(20, 20, 20, 20))
}

toc_trends_plot <- toc_trends(repparfjord)

tiff("TIFF/TOC_and_TN_trends_line_plots.tiff", width = 4000, height = 4000)
plot(toc_trends_plot)
dev.off()


#### TOC and TN ratios -------------------------------------------------------
#kako geom_path ne bi spajao prvu i zadnju točku treba grupirati podatke. 
repparfjord <- repparfjord[order(repparfjord$Core, repparfjord$Mean_depth), ]

ratios <- ggplot(repparfjord, aes(x= TOC_TN, y=Mean_depth))+
  geom_rect(data = tailing,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "dimgray", alpha = 0.3, inherit.aes = FALSE) +
  geom_path(aes(group = Core), color= "mediumslateblue", linewidth = 0.5)+   #i ovdje treba dodati grupiranje kako bi ggplot znao da ne treba spajati prvu i zadnju točku
  geom_point(color="gold", fill="gold", shape = 23, size=1)+
  # geom_label(label = TOC_TN, position ="nudge")+
  facet_wrap(~Core, 
             scales = "free", ncol = 2, drop = T)+
  scale_y_reverse() +
  labs(x="TOC/TN", y="Mean depth [cm]")+
  theme_geochem()+
  theme(strip.text = element_text(size =8, face="bold"), 
        axis.text=element_text(size = 7), 
        axis.title = element_text(size=9))

ratios

tiff("TIFF/TOC_TNratios.tiff", res=300, width = 1500, height = 1500)
plot(ratios)
dev.off()


TOC_TN_Corr <-  ggplot(repparfjord, aes (x=TOC, y=TN))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~Core, 
             scales = "free", ncol = 2, drop = T)+
  theme_geochem()+
  theme(strip.text = element_text(size =8, face="bold"), 
        axis.text=element_text(size = 7), 
        axis.title = element_text(size=9))

TOC_TN_Corr

tiff("TIFF/TOC_TNcorr.tiff", res=300, width = 1500, height = 1500)
plot(TOC_TN_Corr)
dev.off()
#### Testing for Compositional Normality using Complete Mulitvariate Test--------
#Based on the energy test of Rizzo and Szekely (2008)

repp_df2 <- reppar %>% 
  select('Mo':'Tl')
repp_df2
comp_norm <- mvnorm.etest(ilr(repp_df2)) #Test is not very sensitive to local deviations from normality
str(comp_norm)
comp_norm2 <- acompNormalGOF.test(repp_df2)
str(comp_norm2)

#Shapiro_Wilks test
element_repp_clr2 <- data.frame(scale(clr(repp_df2), center = T, scale = F)) %>% 
  select("Mo":"Tl")
shp_test <-  function(...)shapiro.test(...)$p.value
shapi_test <-  apply(element_repp_clr2, 2, shp_test)
str(shapi_test)
write.csv(shapi_test, file = 'CSV/Repparfjord_Shapiro-Wilk_test.csv')



#### PCA analysis - CORE 352 -------------------------------------------------

core_pca <- read_excel('DATA/Data.xlsx', sheet = "PCA_CORE_352")

core_pca
core_pca_df <- data.frame(core_pca)
core_pca_df
rownames(core_pca_df) <- core_pca_df[,1]

core_pca_df <- mutate(core_pca_df, Cd=as.numeric(Cd), 
                      As=as.numeric(As))

core_pca_df <- core_pca_df[, 2:19] 
core_pca_df
str(core_pca_df)
core_clr <-  acomp(core_pca_df, BDL = NA)

pc <- princomp(core_clr)                                #PCA compositions
pc                                                                    #same as 'print(pc)'
summary(pc)

eig.val <- get_eigenvalue(pc)
eig.val                                                               #measuring the amount of variance



# PCA Visualization

tiff("TIFF/CORE352_Screeplot_Total.tiff")
fviz_eig(pc, addlabels = T, ylim = c(0,75))                           #creates scree plot, 'ylim = c(0:30)' means that y axis goes to 30,
#it's in accordance with the highest value of eigen values
dev.off()

core_sree <- fviz_eig(pc, addlabels = T, ylim = c(0,75), barcolor = "black", barfill = "#D7F5D7")

var <- get_pca_var(pc)                                                #list of matrices containing all the results
var

head(var$coord)                                                       #coordinates
#The observations are represented by their projections, 
#but the variables are represented by their correlations

tiff('TIFF/CORE352Correlations of the variables in PCA analysis.tiff')
fviz_pca_var(pc, geom.var=c('point', 'text'), col.var = '#D7F5D7', 
             col.circle = 'black', addEllipses=T, 
             title = "A")                                   #variable correlation plot , shows the relationsships between all variables
dev.off()

head(var$cos2, 4)                                                     #quality of representation of the variables on factor map (cos2), squared cosine or sqaured coordinates

corrplot(var$cos2, is.corr = T)

tiff('TIFF/Variability of the data in dimensions.tiff')
fviz_cos2(pc, choice = 'var', axes = 1:2)                             #variability of the first to dimensions
dev.off()

pdf('TIFF/Variantion of the variables in dim 1-2.pdf')
fviz_pca_var(pc, axes = 1:2, col.var = 'cos2',                                    #Visualization of the variables and their correlation in PC1 and PC2, and corolarition based on the quality of representation ih these dimensions
             gradient.cols = heatmap_scale, 
             repel = T                                                #Avoid text overlapping ##PRGn je paleta boja od ljubi?aste do zelene
)
dev.off()

#ne daje nikakve wow rezultate. 

#### MFA/FAMD --------------------------------------------------------------------
library(dplyr)

rep_data2 <- read_excel('DATA/Data.xlsx', sheet = "PCA_CORE_352") %>% 
  data.frame()  #reading data
rep_data2
str(rep_data2)

write.csv(rep_data2, file = 'CSV/Repparfjord_data_MFA.csv')                                #creating new csv    

# rep_data2 <- mutate(rep_data2, As=as.numeric(As))
rep_data2
str(rep_data2)
colnames(rep_data2)
#rep_clr <-  acomp(rep_data2_fa, BDL = NA)


# Clr transformation of the data 
element <- acomp(rep_data2, parts = c("Ag", "Cu", "Ni", "Sc", "Zn", "As", 
                                      "Co", "Cr", "Mn", "Mo", "Pb", "Sb", "Bi",
                                      "Al", "Ca", "K", "Sr", 
                                      "TOC", "TN"
), BDL=NA)    #"Cd" - isključen jer su svi BDL                      #defining as acomp
element
element_df <- data.frame(element)    
element_df
str(element_df)
write.csv(element_df, file='Proba_acomp.csv')

element_df1 <- data.frame(element)     
element_df1 <- clr(element_df1)
write.csv(element_df1, file='Proba_acomp_clr.csv')

# Creating factor data

Type.of.sediment <- factor(rep_data2$Type.of.sediment)                                          #converts litological labels into factors. 
Type.of.sediment                                                                         #this way the levels of factors also are visible and the final data frame looks the way its described
#in Kassambara example in the book.
Sample <- as.character(rep_data2$Sample)
Sample
O2.zone <- factor(rep_data2$O2.zone)

# Creating single data frame
rep_data2 <- data.frame(Sample, Type.of.sediment, O2.zone, element_df1)                           #creating a single data frame with two factors and continuous variables
rep_data2
rownames(rep_data2) <- rep_data2[,1]
rep_data2 <- rep_data2[,2:22]
rep_data2
str(rep_data2)     

#creating groups
#defining the groups and columns that are in each group
#this part below is not used, as it is defined by vectors "groups", "type", and "name.group"
categories <- c(rep_data2$Type.of.sediment, rep_data2$O2.zone)
minor.el.group <- c(rep_data2$Ag, rep_data2$Cu, rep_data2$Ni, rep_data2$Sc, rep_data2$Zn)
illustrative <- c(rep_data2$As, rep_data2$Co, rep_data2$Cr, rep_data2$Mn, rep_data2$Mo, rep_data2$Pb, rep_data2$Sb, rep_data2$Bi)
major.el.group <- c(rep_data2$Al, rep_data2$Ca, rep_data2$K, rep_data2$Sr)
carbon.nitrogen <- c(rep_data2$TOC, rep_data2$TN)

#definig vector, named "group" which contains information about how many variables each group contains
group = c(2, 5, 8, 4, 2)

#defining vector named "type" which containes information about necessity of standardization
type = c ("n", "s", "s", "s")

#defining a vector with group names: 
name.group = c("factors", "minor.elements", "major.elements", "TOC.and.TN")#checking the structure of the data frame, should be factors and numerical.


# Factor analysis

# pdf('FAMD Graph of the quantitative variable.pdf')
#tiff('FAMD Graph of the quantitative variable.tiff', width= 400, height = 450, units = "mm", res = 1000)
rep_mfa <- MFA(
  rep_data2, 
  group = c(2, 5, 8, 4, 2), 
  type = c ("n", "s", "s", "s", "s"), 
  name.group = c("categories", "minor.el.group", "illustrative", "major.el.group", "carbon.nitrogen"),
  num.group.sup = 3,
  graph=TRUE)
print(rep_mfa)
# dev.off()

# Visualization of FAMD data

eig.val <- get_eigenvalue(rep_mfa)
eig.val
print(eig.val)
write.csv(eig.val, file = 'Alta_fa_eigenvalues.csv')

#screeplot
scree_plot <- fviz_screeplot(rep_mfa, barfill="#D7F5D7" , barcolor="black", linecolor="black",
                             ncp=10, addlabels = T, font.x = c(14, "bold", "black"), 
                             font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
                             font.tickslab = c(12, "black")) + #screeplot
  labs(title = "")

group <-  get_mfa_var(rep_mfa, "group")
head(group$coord)
head(group$contrib)

fviz_mfa_var(rep_mfa, "group")

#Graphs of contribution to the dimensions
# pdf('TIFF/MFA Graphs of contributions to the dimensions.pdf')
contrib1 <- fviz_contrib(rep_mfa, 'group', axes = 1)
contrib2 <- fviz_contrib(rep_mfa, 'group', axes = 2)
contrib3 <- fviz_contrib(rep_mfa, 'group', axes = 3)
contrib4 <- fviz_contrib(rep_mfa, 'group', axes = 4)
print(contrib1)
print(contrib2)
print(contrib3)
print(contrib4)
# dev.off()

#quantitative variables

quanti.var <- get_mfa_var(rep_mfa, 'quanti.var')
quanti.var

#visualization of quantitavie varialbes
# tiff('TIFF/MFA graph of quantitative variables.tiff', width= 6000, height=6000, res = 600)
varplot <- fviz_mfa_var(rep_mfa, 'quanti.var', col.var = "black",
                        axes = c(1,2), repel = T,  
                        col.circle ="black", 
                        addlabels = T, font.x = c(14, "bold", "black"), 
                        font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
                        font.tickslab = c(12, "black"), 
                        xlab = "F1 (52.3 %)", ylab= "F2 (29.2 %)", 
                        title = "", 
                        legend.title = "Contribution 
to the factors")
# dev.off()

# tiff('FA graph of quantitative variables3-4.tiff', width= 6000, height=6000, res = 600)
fviz_mfa_var(rep_mfa, 'quanti.var', col.var="contrib", 
             axes = c(3,4), repel = T,  
             gradient.cols = c("orchid2", "magenta4", "darkorchid4"), 
             addlabels = T, font.x = c(14, "bold", "black"), 
             font.y = c(14,"bold", "black"), font.main = c(20, "bold", "black"), 
             font.tickslab = c(12, "black"), 
             xlab = "F3 (11.1 %)", ylab= "F4 (8.32 %)", 
             title = "Quantitative variables - Factor Analysis", 
             legend.title = "Contribution 
to the factors") 
# dev.off()

#Contributions to dimensions: 
fviz_contrib(rep_mfa, choice="quanti.var", axes = 1, top=10, palette="jco")
fviz_contrib(rep_mfa, choice="quanti.var", axes = 2, top=10, palette="jco")


#individuals
ind <- get_mfa_ind(rep_mfa)
ind

idivid <- fviz_mfa_ind(rep_mfa, col.ind = "contrib", 
                       gradient.cols = c("#1f3a75", "#55A868", "#C44E52"),
                       repel = TRUE)

fviz_mfa_ind(rep_mfa,
             habillage = "O2.zone", # color by groups
             palette = c("#1f3a75", "#55A868", "#C44E52"),
             addEllipses = TRUE, ellipse.type = "confidence",
             repel = TRUE # Avoid text overlapping
)

MFA_MAP <- fviz_ellipses(rep_mfa, c("Type.of.sediment", "O2.zone"), 
                         palette = c("#1f3a75", "#4C72B0", "#55A868", "#DD8452", "#C44E52"),
                         addEllipses = TRUE, ellipse.type = "confidence",
                         repel = TRUE)+
  theme_geochem()+
  theme(legend.position = "bottom")


fviz_ellipses(rep_mfa, 1:2, geom="point")

fviz_mfa_ind(rep_mfa, partial="all")
fviz_mfa_axes(rep_mfa)


MFA_plot <-  ggarrange(scree_plot, varplot, ncol=2, nrow=1, heights = c(1,1), widths = c(1,1))+
  theme_geochem()+
  labs(title="")

tiff("TIFF/MFA_plot.tiff", res = 400, width = 6000, height = 3000)
plot(MFA_plot)
dev.off()

tiff("TIFF/MFA_MAP_of_individuals.tiff", res = 400, width = 6000, height = 4000)
plot(MFA_MAP)
dev.off()

#### SEVEN STEP SEQUENTIAL EXTRACTION TRENDS---------------------------------------------------
library(stringr)
library(forcats)
library(dplyr)

sequen7 <- read_excel('DATA/Data.xlsx', sheet = 'SEQ7') %>% 
  data.frame()

#sequen7 <- sequen7 %>% 
#  mutate(log10_Concentration=log10(Concentration))

#podjela na dvije grupe
sgrupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")
sgrupa3 <- c("As", "Cu", "Ni", "Pb", "Zn")


seq_meanMOI <- sequen7 %>%
  filter(Element %in% sgrupa1) %>%
  mutate(
    Sample_base = stringr::str_remove(Sample, "[a-c]$"),
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%
      stringr::str_replace_all("BDL", NA_character_) %>%
      as.numeric()
  ) %>%
  filter(!Sample_base %in% c("ST7", "TD ST")) %>%
  group_by(
    Sample_base,
    Depth2,
    Element,
    Step,
    StepOrder
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  )


write.csv(seq_meanMOI, "CSV/CORE_352_sequential_extr7_no_duplicates_MOI.csv")
seq <- read_csv('CSV/CORE_352_sequential_extr7_no_duplicates_MOI.csv')


seq_plot <- seq %>%
  filter(!is.na(Depth2))

stack_plot_MOI <- ggplot(seq_plot,
                         aes(x = Concentration, y = forcats::fct_rev(
                           forcats::fct_reorder(Sample_base, Depth2)),
                           fill = factor(Step, levels = unique(Step[order(StepOrder)])))) +
  geom_col(position= "fill", stat= "identity", width = 0.9) +
  facet_wrap(~ Element, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = SSE_skala) +
  labs(
    x = "Proportion",
    y = "Sample depth [cm]",
    fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(size = 20),
    legend.key.size = unit(1, "line"), 
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 18), 
    legend.key.spacing.x = unit(0.3, "cm"))


tiff("TIFF/Trends_SSE_MOI.tiff", width = 4500, height = 7000, res=300)
plot(stack_plot_MOI)
dev.off()

#APPENDIX - TREBA popraviti jako jakokooo koood

seq_meanAPP <- sequen7 %>%
  filter(Element %in% sgrupa2) %>%
  mutate(
    Sample_base = stringr::str_remove(Sample, "[a-c]$"),
    
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%        # decimalni zarez
      stringr::str_replace_all("BDL", NA_character_) %>%  # <LOD, <LOQ
      as.numeric()
  ) %>%
  filter(!Sample_base %in% c("ST7", "TD ST")) %>%
  group_by(
    Sample_base,
    Depth,
    Element,
    Step,
    StepOrder
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  )

write.csv(seq_meanAPP, "CSV/CORE_352_sequential_extr7_no_duplicates_APP.csv")
seqa <- read_csv('CSV/CORE_352_sequential_extr7_no_duplicates_APP.csv')

seqa_plot <- seqa %>%
  filter(!is.na(Depth))

stack_plot_APP <- ggplot(seqa_plot,
                         aes(x = Concentration, y = forcats::fct_rev(
                           forcats::fct_reorder(Sample_base, Depth)),
                           fill = factor(Step, levels = unique(Step[order(StepOrder)])))) +
  geom_col(position= "fill", stat= "identity", width = 0.9) +
  facet_wrap(~ Element, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = SSE_skala) +
  labs(
    x = "Proportion",
    y = "Sample depth [cm]",
    fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(size = 20),
    legend.key.size = unit(1, "line"), 
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 18), 
    legend.key.spacing.x = unit(0.3, "cm"))


tiff("TIFF/Trends_SSE_APP.tiff", width = 4500, height = 8500, res=300)
plot(stack_plot_APP)
dev.off()


#### SEVEN STEP SEQUENTIAL EXTRACTION DESCRIPTIVE STATISTICS ----------------------------
#da bi ovaj dio koda radio, potrebno je prvo pokrenuti sve u prethodnoj R sekciji. 

rseq_novo1 <- seq_plot %>% 
  mutate(log10_Concentration=log10(Concentration)) %>% 
  filter(!Step %in% c("Residual fraction"))

labels <- c("A", "B", "C", "D", "E", "F")


seq_violins <- function(data) {
  ggplot(filter(rseq_novo1, Element %in% sgrupa1),
         aes(x=Element, y=log10_Concentration, fill=Step)) +
    geom_boxplot() +
    facet_wrap(~factor(Step, c( "Water soluble fraction", "Exchangeable fraction", "Poorly crystalline Fe(III)-oxyhydroxides", 
                                "Crystalline Fe(III)-oxides", "Organic matter", "Primary sulfide fraction")),
               scales = "free_y", ncol = 2) +
    labs(x="Element", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = SSE_skala)+
    theme_minimal()+
    # geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'none', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 60),
          axis.text.x = element_text(angle = 45), 
          # legend.key.size = unit(10, "line"), 
          # legend.title = element_text(size = 80), 
          # legend.text = element_text(size = 50), 
          # legend.key.spacing.x = unit(1.0, "cm")) 
    )
} 

splot1 <- seq_violins(filter(rseq_novo1, Element %in% sgrupa1))

tiff("TIFF/Descriptive_statistics_SSE_MOI_1_6.tiff", width = 4500, height = 5000)
plot(splot1)
dev.off()

rseq_novoRES <- seq_plot %>% 
  mutate(log10_Concentration=log10(Concentration)) %>% 
  filter(Step %in% c("Residual fraction"))

seq_violinsRES <- function(data) {
  ggplot(filter(rseq_novoRES, Element %in% sgrupa1),
         aes(x=Element, y=log10_Concentration, fill=Step)) +
    geom_boxplot(trim = F) +
    facet_wrap(~factor(Step, c( "Residual fraction")),
               scales = "free_y", ncol = 1) +
    labs(x="Element", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = SSE_skala)+
    theme_minimal()+
    # geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'none', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 60),
          axis.text.x = element_text(angle = 45), 
          # legend.key.size = unit(10, "line"), 
          # legend.title = element_text(size = 80), 
          # legend.text = element_text(size = 50), 
          # legend.key.spacing.x = unit(1.0, "cm")) 
    )
} 

splot1RES <- seq_violinsRES(filter(rseq_novoRES, Element %in% sgrupa1))

tiff("TIFF/Descriptive_statistics_SSE_MOI_7.tiff", width = 2250, height = 1650)
plot(splot1RES)
dev.off()


#APPENDIX

rseqa_novo1 <- seqa_plot %>% 
  mutate(log10_Concentration=log10(Concentration))


seq_violins2 <- function(data) {
  ggplot(filter(rseqa_novo1, Element %in% sgrupa2),
         aes(x=Element, y=log10_Concentration, fill=Step)) +
    geom_boxplot() +
    facet_wrap(~factor(Step, c( "Water soluble fraction", "Exchangeable fraction", "Poorly crystalline Fe(III)-oxyhydroxides", 
                                "Crystalline Fe(III)-oxides", "Organic matter", "Primary sulfide fraction","Residual fraction")),
               scales = "free_y", ncol = 2) +
    labs(x="Element", 
         y="log10(Concentration)",
         fill="")+
    scale_fill_manual(values = SSE_skala)+
    theme_geochem()+
    # geom_boxplot(width=0.1, color='black', alpha = 0.2) +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 20),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(2, "line"), 
          legend.title = element_text(size = 26), 
          legend.text = element_text(size = 18), 
          legend.key.spacing.x = unit(0.2, "cm")) 
} 

splot2 <- seq_violins2(filter(rseqa_novo1, Element %in% sgrupa2))

tiff("TIFF/Descriptive_statistics_SSE_app.tiff", width = 4700, height = 6000, res= 300)
plot(splot2)
dev.off()


##### BCR SEQUENTIAL EXTRACTION DESCRIPTIVE -----------------------------------

rseq_novo_BCR <-  read_excel("DATA/Data.xlsx", sheet = "BCR") 

rseq_novo_BCR13 <- data_frame(rseq_novo_BCR)  %>% 
  mutate(Concentration = as.numeric(Concentration)) %>% 
  mutate(log10_Concentration=log10(Concentration)) %>% 
  filter(!stringr::str_starts(Sample, "ST")) %>% 
  filter(!Step2 %in% c("Residual fraction (AR)", "Residual fraction (TOT)"))

#elementi po grupama 
sgrupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")


BCR_seq_violins <- function(data) {
  
  data %>%
    mutate(
      Step2 = stringr::str_trim(Step2),
      Step2 = factor(
        Step2,
        levels = c("Exchangeable fraction",
                   "Reducible fraction",
                   "Oxidizable fraction")
      )
    ) %>%
    
    filter(
      Element %in% sgrupa1,
      Step2 %in% levels(Step2)
    ) %>%
    
    ggplot(aes(x = Element, y = log10_Concentration, fill = Step2)) +
    
    geom_boxplot(width = 0.6) +
    
    geom_boxplot(width = 0.2, color='black', fill = NA) +
    
    facet_wrap(~Step2,
               scales = "free_y",
               ncol = 1) +
    
    scale_fill_manual(values = BCR_skala) +
    
    labs(x="Element", 
         y="log10(Concentration)",
         fill="") +
    
    theme_minimal() +
    
    theme(
      legend.position = 'none', 
      strip.text = element_text(face="bold"), 
      text = element_text(size = 60),
      axis.text.x = element_text(angle = 45)
    )
}

BCRplot1 <- BCR_seq_violins(filter(rseq_novo_BCR13, Element %in% sgrupa1))

tiff("TIFF/Descriptive_statistics_BCR_MOI_1_3.tiff", width = 2250, height = 5000)
plot(BCRplot1)
dev.off()

rseq_novo_BCR45 <- data_frame(rseq_novo_BCR)  %>% 
  mutate(Concentration = as.numeric(Concentration)) %>% 
  mutate(log10_Concentration=log10(Concentration)) %>% 
  filter(!stringr::str_starts(Sample, "ST")) %>% 
  filter(Step2 %in% c("Residual fraction (AR)", "Residual fraction (TOT)"))

#elementi po grupama 
sgrupa1 <- c("Ag", "As", "Bi", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Cd", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")


BCRRES_seq_violins <- function(data) {
  
  data %>%
    mutate(
      Step2 = stringr::str_trim(Step2),
      Step2 = factor(
        Step2,
        levels = c("Residual fraction (AR)", "Residual fraction (TOT)")
      )
    ) %>%
    
    filter(Element %in% sgrupa1) %>%
    
    ggplot(aes(x = Element, y = log10_Concentration, fill = Step2)) +
    
    geom_boxplot(width = 0.6) +
    geom_boxplot(width = 0.2, color='black', fill = NA) +
    
    facet_wrap(~Step2,
               scales = "free_y",
               ncol = 2) +
    
    scale_fill_manual(values = c(
      "Residual fraction (AR)"     = "#4C72B0", 
      "Residual fraction (TOT)" = "#1f3a75"
    )) +
    
    labs(x="Element", 
         y="log10(Concentration)",
         fill="") +
    
    theme_minimal() +
    
    theme(
      legend.position = 'none', 
      strip.text = element_text(face="bold"), 
      text = element_text(size = 60),
      axis.text.x = element_text(angle = 45)
    )
}
BCRplot1RES <- BCRRES_seq_violins(filter(rseq_novo_BCR45, Element %in% sgrupa1))

tiff("TIFF/Descriptive_statistics_BCR_MOI_4_5.tiff", width = 4500, height = 1650)
plot(BCRplot1RES)
dev.off()

#APPENDIX

rseq_novo_BCR <-  read_excel("DATA/Data.xlsx", sheet = "BCR") 

rseq_novo_BCR <- data_frame(rseq_novo_BCR)  %>% 
  mutate(Concentration = as.numeric(Concentration)) %>% 
  
  mutate(
    log10_Concentration = ifelse(
      Concentration > 0,
      log10(Concentration),
      NA)) %>% 
  filter(!stringr::str_starts(Sample, "ST")) 


#elementi po grupama 
sgrupa1 <- c("Ag", "As", "Bi", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Cd", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")




BCRAPPseq_violins <- function(data) {
  
  data %>%
    mutate(
      Step2 = stringr::str_trim(Step2),
      Step2 = factor(
        Step2,
        levels = c("Exchangeable fraction", 
                   "Reducible fraction", 
                   "Oxidizable fraction", 
                   "Residual fraction (AR)", 
                   "Residual fraction (TOT)")
      )
    ) %>%
    
    filter(Element %in% sgrupa2) %>%
    
    ggplot(aes(x = Element, y = log10_Concentration, fill = Step2)) +
    
    geom_boxplot(width = 0.6) +
    geom_boxplot(width = 0.2, color='black', fill = NA) +
    
    facet_wrap(~Step2,
               scales = "free_y",
               ncol = 2,
               drop = TRUE) +
    
    scale_fill_manual(values = BCR_skala) +
    
    labs(x="Element", 
         y="log10(Concentration)",
         fill="") +
    
    theme_geochem()+
    
    theme(legend.position = 'bottom', 
          strip.text = element_text(face="bold"), 
          text = element_text(size = 20),
          axis.text.x = element_text(angle = 45), 
          legend.key.size = unit(2, "line"), 
          legend.title = element_text(size = 26), 
          legend.text = element_text(size = 18), 
          legend.key.spacing.x = unit(0.2, "cm"))
}

BCRAPPplot1 <- BCRAPPseq_violins(rseq_novo_BCR)

tiff("TIFF/Descriptive_statistics_BCR_MOI_APP.tiff", width = 4700, height = 6000, res= 300)
plot(BCRAPPplot1)
dev.off()

#### BCR vs SSE SEQUENTIAL EXTRACTION DESCRIPTIVE STATISTICS -----------------

mid_des<- ggplot(rseq_novo_BCR)+
  geom_vline(aes(xintercept=1)) +
  ggtitle("")+
  ylab(NULL)+
  scale_y_continuous()+
  theme_minimal(base_size = 16)+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(-7.8,-6,-0.7,-6), "mm"))
mid_des

SSE_legend_df <- data.frame(
  frac = factor(
    c("1","2","3","4","5","6","7"),
    levels = c("1","2","3","4","5","6","7")
  ),
  label = c(
    "Water soluble fraction",
    "Exchangeable fraction",
    "Poorly crystalline Fe-oxyhydroxides fraction",
    "Crystalline Fe-oxides fraction",
    "Organic matter fraction",
    "Primary sulfide fraction",
    "Residual fraction"
  )
)


SSE_fraction_colors <- c(
  "1" = "#BC31A4",  # Water soluble
  "2" = "#B6282F",  # Exchangeable
  "3" = "#EB7547",  # Poorly crystalline Fe
  "4" = "#F0E442",  # Crystalline Fe
  "5" = "#1B9E77",  # Organic matter
  "6" = "#00A8C6",  # Primary sulfides
  "7" = "#0072B2" )  # Residual

# Dummy plot koji služi SAMO za legendu
SSE_legend_plot <- ggplot(SSE_legend_df, aes(x = frac, y = 1, fill = frac)) +
  geom_tile() +
  scale_fill_manual(
    name   = "SSE fractions",
    values = SSE_fraction_colors,
    labels = SSE_legend_df$label
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "left",
      nrow = 3, 
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    text=element_text(family = "Corbel Bold"), 
    legend.position = "bottom",
    legend.title = element_text(size = 80),
    legend.text  = element_text(size = 70)
  )

BCR_legend_df <- data.frame(
  frac = factor(
    c("1","2","3","4","5"),
    levels = c("1","2","3","4","5")
  ),
  label = c(
    "Exchangeable fraction",
    "Reducible fraction",
    "Oxidizable fraction",
    "Residual fraction (AR)",
    "Residual fraction (TOT)"
  )
)

BCR_fraction_colors <- c(
  "1"    = "#C44E52",
  "2"    = "#DD8452",
  "3"    = "#55A868",
  "4"    = "#4C72B0"
)

# Dummy plot koji služi SAMO za legendu
BCR_legend_plot <- ggplot(BCR_legend_df, aes(x = frac, y = 1, fill = frac)) +
  geom_tile() +
  scale_fill_manual(
    name   = "BCR fractions",
    values = BCR_fraction_colors,
    labels = BCR_legend_df$label
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      title.position = "left",
      nrow = 2, 
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    text=element_text(family = "Corbel Bold"), 
    legend.position = "bottom",
    legend.title = element_text(size = 80),
    legend.text  = element_text(size = 70)
  )
spacer <- ggplot()+theme_void()

labile <-  ggarrange(splot1, mid_des, BCRplot1, 
                     nrow=1, widths=c(4/7, 1/7, 2/7), common.legend = F)
residu <- ggarrange(splot1RES, mid_des, BCRplot1RES, 
                    nrow=1, widths=c(2/7, 1/7, 4/7), common.legend = F)

descrip_plot <- ggarrange(labile, residu, nrow=2, ncol = 1, common.legend = F, 
                          heights = c(3/4, 1/4)) 

legend_plots <- ggarrange(SSE_legend_plot, BCR_legend_plot, ncol =1, common.legend = F, 
                          heights = c(0.5, 0.5))

final_plot_descri <- ggarrange(descrip_plot, spacer, SSE_legend_plot, spacer, BCR_legend_plot, ncol =1, common.legend = F, 
                               heights = c(0.9, 0.02 ,0.04, 0.01, 0.02))+
  theme(plot.margin = margin(1,1,1,1, "cm"))


tiff("TIFF/Descriptive_statistics_SSE_BCR_all.tiff", width = 4000, height = 5000)
plot(final_plot_descri)
dev.off()



#### BCR TRENDS --------------------------------------------------------------
library(stringr)
library(forcats)
library(dplyr)

sequenBCR <- read_excel('DATA/Data.xlsx', sheet = 'BCR') %>% 
  data.frame() %>% 
  mutate(Step2 = recode(Step2, "Residual fraction (TOT)" = "Residual fraction (TD)"))


#sequen7 <- sequen7 %>% 
#  mutate(log10_Concentration=log10(Concentration))

#podjela na dvije grupe
sgrupa1 <- c("Ag", "As", "Bi", "Cd", "Co", "Cr", "Cu", "Hg", "Mn", "Mo", "Ni", "Pb", "Sb", "Sc", "Zn")
sgrupa2 <- c("Al", "Au", "Ba", "Ca", "Ga", "Fe", "K", "La", "Mg", "Na", "P", "S", "Se", "Sr", "Te", "Th", "Ti", "Tl", "U", "V", "W")
sgrupa3 <- c("As", "Cu", "Ni", "Pb", "Zn")

seq_meanBCR_MOI <- sequenBCR %>%
  filter(Element %in% sgrupa1) %>%
  mutate(
    Sample_base = stringr::str_remove(Sample, "[a-c]$"),
    
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%        # decimalni zarez
      stringr::str_replace_all("BDL", NA_character_) %>%  # <LOD, <LOQ
      as.numeric()
  ) %>%
  filter(!str_detect(Sample, "ST")) %>% 
  group_by(
    Sample_base,
    Depth,
    Element,
    Step2,
    Step
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  ) %>% 
  filter(!Step2%in% c("Residual fraction (TD)")) #upaliti ili ugasiti ovisno što se želi prikazati

write.csv(seq_meanBCR_MOI, "CSV/CORE_352_sequential_extr4_no_duplicates_MOI.csv") #Ovdje extr4 zamijeniti s extr5 ako je red iznad filter... nije comentiran već je aktivan, ako je pod komentarom onda ostaje extr4. 
seqBCR <- read_csv('CSV/CORE_352_sequential_extr4_no_duplicates_MOI.csv')         #ovdje isto vrijedi zamjena extr4/extr5, #Ovo zapravo ovisi koji se tip grafova želi prikazati, 4stupnja bcr ili bcr+td.


BCRseq_plot <- seqBCR %>%
  filter(!is.na(Depth))

BCRstack_plot_MOI <- 
  ggplot(BCRseq_plot,
         aes(x = Concentration, y = forcats::fct_rev(
           forcats::fct_reorder(Sample_base, Depth)), 
           fill = factor(Step2, levels = unique(Step2[order(Step)])))) +
  geom_col(position= "fill", stat= "identity", width = 0.9) +
  facet_wrap(~ Element, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = BCR_skala) +
  labs(
    x = "Proportion",
    y = "Sample depth [cm]",
    fill = "") +
  theme_geochem() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(size = 20),
    legend.key.size = unit(1, "line"), 
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 18), 
    legend.key.spacing.x = unit(0.3, "cm"))


# tiff("TIFF/Trends_BCR_MOI.tiff", width = 4500, height = 7000, res=300)
tiff("TIFF/Trends_BCR_MOI_4st.tiff", width = 4500, height = 7000, res=300)
plot(BCRstack_plot_MOI)
dev.off()


seq_meanBCR_APP <- sequenBCR %>%
  filter(Element %in% sgrupa2) %>%
  mutate(
    Sample_base = stringr::str_remove(Sample, "[a-c]$"),
    
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%        
      stringr::str_replace_all("BDL", NA_character_) %>%  
      as.numeric(),
    
    Concentration = ifelse(Concentration < 0, NA, Concentration)
  ) %>%
  filter(!str_detect(Sample, "ST")) %>% 
  group_by(
    Sample_base,
    Depth,
    Element,
    Step2,
    Step
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  ) #%>% 
# filter(!Step2%in% c("Residual fraction (TD)"))  #upaliti ili ugasiti ovisno što se prikazuje

write.csv(seq_meanBCR_APP, "CSV/CORE_352_sequential_extr5_no_duplicates_APP.csv")
seqAPPBCR <- read_csv('CSV/CORE_352_sequential_extr5_no_duplicates_APP.csv')


BCRAPPseq_plot <- seqAPPBCR %>%
  filter(!is.na(Depth))

BCRstack_plot_APP <- ggplot(BCRAPPseq_plot,
                            aes(x = Concentration, y = forcats::fct_rev(
                              forcats::fct_reorder(Sample_base, Depth)),
                              fill = factor(Step2, levels = unique(Step2[order(Step)])))) +
  geom_col(position= "fill", stat= "identity", width = 0.9) +
  facet_wrap(~ Element, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = BCR_skala) +
  labs(
    x = "Proportion",
    y = "Sample depth [cm]",
    fill = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    text = element_text(size = 20),
    legend.key.size = unit(1, "line"), 
    legend.title = element_text(size = 22), 
    legend.text = element_text(size = 18), 
    legend.key.spacing.x = unit(0.3, "cm"))

tiff("TIFF/Trends_BCR_APP.tiff", width = 4500, height = 7000, res=300)
# tiff("TIFF/Trends_BCR_APP_4st.tiff", width = 4500, height = 7000, res=300)
plot(BCRstack_plot_APP)
dev.off()



#### SSE SEP comparison to the literature ------------------------------------
sequen7 <- read_excel('DATA/Data.xlsx', sheet = 'SEQ7') %>% 
  data.frame()

#sequen7 <- sequen7 %>% 
#  mutate(log10_Concentration=log10(Concentration))

#podjela na  grupe
elements_law <- c("As", "Cd", "Cu", "Cr", "Ni", "Pb", "Zn")

seq_meanMOI <- sequen7 %>%
  filter(Element %in% elements_law) %>%
  mutate(
    Sample_base = stringr::str_remove(Sample, "[a-c]$"),
    
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%        # decimalni zarez
      stringr::str_replace_all("BDL", NA_character_) %>%  # <LOD, <LOQ
      as.numeric()
  ) %>%
  filter(!str_detect(Sample_base, "ST")) %>% 
  filter(!str_detect(Sample_base, "TD-ST")) %>% 
  group_by(
    Sample_base,
    Depth2,
    Element,
    Step,
    StepOrder
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  )

write.csv(seq_meanMOI, "CSV/CORE_352_sequential_extr7_no_duplicates_MOI.csv")
seq <- read_csv('CSV/CORE_352_sequential_extr7_no_duplicates_MOI.csv')


seq_plot <- seq %>%
  filter(!is.na(Depth2))


#Granice

contamination_bound <-  data.frame(
  Element = rep(c("As", "Cd", "Cr","Cu", "Ni", "Pb", "Zn"), each = 5),
  Classification = rep(c("background", "good", "moderate", "bad", "very bad"), times = 7),
  ymin = c(0, 20, 51, 75, 579, #As 
           0, 0.25, 2.5, 15, 140, #Cd
           0, 70, 560, 5900, 59000, #Cr
           0, 35, 51, 55, 220, #Cu
           0, 30, 46, 120, 840, #Ni
           0, 30, 83, 100, 720, #Pb
           0, 150, 360, 590, 4500 #Zn
  ), 
  ymax = c(20, 52, 76, 580, Inf, #As
           0.25, 2.6, 15, 140, Inf, #Cd
           70, 560, 5900, 59000, Inf, #Cr
           35, 51, 55, 220, Inf, #Cu
           30, 46, 120, 840, Inf, #Ni
           30, 83, 100, 720, Inf, #Pb
           150, 360, 590, 4500, Inf #Zn
  )
)

boje_klasa <- c(
  "background" = "#7fbf7b",  # muted green
  "good"       = "#c7c77a",  # soft yellow-green
  "moderate"   = "#e0a96d",  # soft orange
  "bad"        = "#d97b6a",  # muted red
  "very bad"   = "#8a6fa1"   # desaturated purple
)

Orderklasa <- c("background", "good", "moderate", "bad", "very bad")

pragmatic_max <- 600

RSSEseq_quality <- ggplot() +
  
  
  # BCR / SSE stupci
  
  geom_rect(data = contamination_bound %>%
              dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
            
            aes(ymin = -Inf, ymax = Inf, xmin = ymin, xmax = ymax, fill = Classification),
            alpha = 0.25,
            color = NA
  ) +
  
  scale_fill_manual(
    values = boje_klasa,
    breaks = Orderklasa,
    name = "Sediment quality"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  
  geom_bar(
    data = seq_plot %>% filter(Element %in% elements_law),
    aes(
      x = Concentration, 
      y = fct_rev(fct_reorder(Sample_base, Depth2, .fun = mean)),
      fill = factor(Step, levels = unique(Step[order(StepOrder)]))
    ),
    position = position_dodge(width = 0.8),
    stat = "identity",
    width = 1.5
  )+
  scale_fill_manual(values = SSE_skala, name = "SSE fraction") +
  
  # Osi i facete
  
  #scale_x_log10(expand = expansion(mult = 0.05)) +
  # coord_cartesian(xlim = c(0, pragmatic_max))+  
  facet_wrap(
    ~ factor(Element, levels = elements_law),
    scales = "free_x",
    ncol = 2
  ) +
  
  labs(
    x = "Concentration [mg/kg]",
    y = "Depth interval"
  ) +
  
  guides(
    fill = guide_legend(nrow = 3, byrow = TRUE),
    color = guide_legend(
      override.aes = list(linewidth = 3)
    )
  ) +
  
  theme_geochem() +
  theme(
    legend.position = 'bottom',
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    text = element_text(size = 130),
    axis.text.x = element_text(size = 150, angle = 45),
    legend.key.size = unit(8, "line"),
    legend.title = element_text(size = 130),
    legend.text = element_text(size = 30),
    legend.key.spacing.x = unit(1.0, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(20, 100, 20, 20))



# RSEEseq_quality_plot <- RSSEseq_quality(seq_plot)

tiff("TIFF/RSSESEQ_QUALITY_SED_dodgeplot.tiff", width = 5000, height = 9000)
plot(RSSEseq_quality)
dev.off()

R1SSEseq_quality <- ggplot() +
  
  
  # BCR / SSE stupci
  
  geom_rect(data = contamination_bound %>%
              dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
            
            aes(ymin = -Inf, ymax = Inf, xmin = ymin, xmax = ymax, fill = Classification),
            alpha = 0.25,
            color = NA
  ) +
  
  scale_fill_manual(
    values = boje_klasa,
    breaks = Orderklasa,
    name = "Sediment quality"
  ) +
  
  ggnewscale::new_scale_fill() +
  
  
  geom_bar(
    data = seq_plot %>% filter(Element %in% elements_law),
    aes(
      x = Concentration, 
      y = fct_rev(fct_reorder(Sample_base, Depth2, .fun = mean)),
      fill = factor(Step, levels = unique(Step[order(StepOrder)]))
    ),
    position = "stack",
    stat = "identity",
    width = 0.8
  )+
  scale_fill_manual(values = SSE_skala, name = "SSE fraction") +
  
  
  # Osi i facete
  
  #scale_x_log10(expand = expansion(mult = 0.05)) +
  # coord_cartesian(xlim = c(0, pragmatic_max))+  
  facet_wrap(
    ~ factor(Element, levels = elements_law),
    scales = "free_x",
    ncol = 2
  ) +
  
  labs(
    x = "Concentration [mg/kg]",
    y = "Depth interval"
  ) +
  
  guides(
    fill = guide_legend(nrow = 3, byrow = TRUE),
    color = guide_legend(
      override.aes = list(linewidth = 3)
    )
  ) +
  
  theme_geochem() +
  theme(
    legend.position = 'bottom',
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    text = element_text(size = 130),
    axis.text.x = element_text(size = 100, angle = 45),
    legend.key.size = unit(8, "line"),
    legend.title = element_text(size = 150),
    legend.text = element_text(size = 90),
    legend.key.spacing.x = unit(1.0, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(20, 100, 20, 20))



# RSEEseq_quality_plot <- RSSEseq_quality(seq_plot)

tiff("TIFF/RSSESEQ_QUALITY_SED_stack.tiff", width = 5000, height = 9000)
plot(R1SSEseq_quality)
dev.off()

####BCR SEP comparison to the literature --------------------------------
# DATA
rseq_novo_BCR <-  read_excel("DATA/Data.xlsx", sheet = "BCR") 

#sequen7 <- sequen7 %>% 
#  mutate(log10_Concentration=log10(Concentration))

#podjela na  grupe
elements_law <- c("As", "Cd", "Cu", "Cr", "Ni", "Pb", "Zn")

Rseq_meanMOI <- rseq_novo_BCR %>%
  filter(Element %in% elements_law) %>%
  mutate(
    Concentration = Concentration %>%
      stringr::str_replace(",", ".") %>%        # decimalni zarez
      stringr::str_replace_all("BDL", NA_character_) %>%  # <LOD, <LOQ
      as.numeric()
  ) %>%
  filter(!str_detect(Sample, "ST")) %>% 
  filter(!str_detect(Sample, "ST4")) %>% 
  group_by(
    Sample,
    Depth,
    Element,
    Step,
    Step2
  ) %>%
  summarise(
    Concentration = mean(Concentration, na.rm = TRUE),
    n_rep = sum(!is.na(Concentration)),
    .groups = "drop"
  )

write.csv(Rseq_meanMOI, "CSV/CORE_352_sequential_extrBCR_no_duplicates_MOI.csv")
Rseq <- read_csv('CSV/CORE_352_sequential_extrBCR_no_duplicates_MOI.csv')


RLITBCRseq_plot <- Rseq %>%
  filter(!is.na(Depth))

# PLOT

RBCRseq_quality <- ggplot() +
  
  geom_rect(
    data = contamination_bound %>%
      dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
    
    aes(ymin = -Inf, ymax = Inf, xmin = ymin, xmax = ymax, fill = Classification),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_fill_manual(
    values = boje_klasa,
    breaks = Orderklasa,
    name = "Sediment quality"
  ) +
  
  # ✅ ISPRAVNO
  ggnewscale::new_scale_fill() +
  
  geom_col(
    data = RLITBCRseq_plot,
    aes(
      x = Concentration,
      y = fct_rev(fct_reorder(Sample, Depth, .fun = mean)),
      fill = factor(Step2, levels = names(BCR_skala))
    ),
    position = "stack",
    width = 0.8
  ) +
  
  scale_fill_manual(
    values = BCR_skala,
    breaks = names(BCR_skala),
    name = "BCR fraction"
  ) +
  
  facet_wrap(
    ~ factor(Element, levels = elements_law),
    scales = "free_x",
    ncol = 2
  ) +
  
  labs(
    x = "Concentration [mg/kg]",
    y = "Depth interval"
  ) +
  
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_geochem() +
  theme(
    legend.position = 'bottom',
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    text = element_text(size = 130),
    axis.text.x = element_text(size = 100, angle = 45),
    legend.key.size = unit(8, "line"),
    legend.title = element_text(size = 150),
    legend.text = element_text(size = 90),
    legend.key.spacing.x = unit(1.0, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(20, 100, 20, 20)
  )

# EXPORT
tiff("TIFF/RBCRSEQ_QUALITY_SED_stacked.tiff", width = 5000, height = 9000)
plot(RBCRseq_quality)
dev.off()


R1BCRseq_quality <- ggplot() +
  
  geom_rect(
    data = contamination_bound %>%
      dplyr::mutate(Classification = factor(Classification, levels = Orderklasa)),
    
    aes(ymin = -Inf, ymax = Inf, xmin = ymin, xmax = ymax, fill = Classification),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_fill_manual(
    values = boje_klasa,
    breaks = Orderklasa,
    name = "Sediment quality"
  ) +
  
  # ✅ ISPRAVNO
  ggnewscale::new_scale_fill() +
  
  geom_col(
    data = RLITBCRseq_plot,
    aes(
      x = Concentration,
      y = fct_rev(fct_reorder(Sample, Depth, .fun = mean)),
      fill = factor(Step2, levels = names(BCR_skala))
    ),
    position = "dodge",
    width = 0.8
  ) +
  
  scale_fill_manual(
    values = BCR_skala,
    breaks = names(BCR_skala),
    name = "BCR fraction"
  ) +
  
  facet_wrap(
    ~ factor(Element, levels = elements_law),
    scales = "free_x",
    ncol = 2
  ) +
  
  labs(
    x = "Concentration [mg/kg]",
    y = "Depth interval"
  ) +
  
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(
      override.aes = list(linewidth = 3)
    )
  ) +
  
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    text = element_text(size = 130),
    axis.text.x = element_text(size = 100, angle = 45),
    legend.key.size = unit(8, "line"),
    legend.title = element_text(size = 150),
    legend.text = element_text(size = 90),
    legend.key.spacing.x = unit(1.0, "cm"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(20, 100, 20, 20)
  )

# EXPORT
tiff("TIFF/RBCRSEQ_QUALITY_SED_dodged.tiff", width = 5000, height = 9000)
plot(R1BCRseq_quality)
dev.off()


###### 1. LIBRARIES===========
library(sf)
library(dplyr)
library(readxl)
library(plotly)
library(scales)
library(raster)
library(reticulate)

####SEMI_TOTALI========================
##### 2. RADNI DIREKTORIJ============================

setwd("C:/Users/Laura/Desktop/Repparfjord_stat/MAP")


##### 3. UČITAVANJE PODATAKA ============================

sea <- st_read("SEA_Final.shp")

core_location <- read_excel("Location.xlsx")

reppar_tr <- read_excel(
  "C:/Users/Laura/Desktop/Repparfjord_stat/DATA/Data.xlsx",
  sheet = "Trends_bulk"
) %>% as.data.frame()

# 🔥 BATHYMETRY GRID - ne koristi se zbog prenagalešne dubine pa ne izgleda dobro, a dubina je prenaglašena zbog jezgri
# bathy <- raster("C:/Users/Laura/Desktop/Repparfjord_stat/MAP/bathymetry")

##### 4. clean core names===============
clean_core <- function(x) {
  x %>%
    trimws() %>%
    toupper() %>%
    gsub("_", "-", .) %>%
    gsub("\\s+", "", .)
}

core_location$`Original core no.` <- clean_core(core_location$`Original core no.`)
reppar_tr$Core <- clean_core(reppar_tr$Core)

##### 5. Core coordinate ==================

coords_df <- core_location %>%
  rename(X = Longitude, Y = Latitude, Core = `Original core no.`)


##### 6. GEOKEMIJA==========================


repparfjord <- data.frame(
  Core = reppar_tr$Core,
  Mean_depth = as.numeric(reppar_tr$Mean_depth),
  Element = reppar_tr$Element,
  Concentration = as.numeric(reppar_tr$Semi_total)
) %>%
  mutate(Core = recode(Core,
                       "CORE-352" = "HH-352-BC-A",
                       "CORE-334" = "HH-334-MC-B",
                       "CORE-346" = "HH-346-BC-A",
                       "CORE-357" = "HH-357-BC"
  )) %>%
  mutate(Core = clean_core(Core)) %>%
  left_join(coords_df, by = "Core")

#provjera: 
repparfjord %>% filter(is.na(X)) %>% distinct(Core)

##### 7. normalizacija za boju ====================
repparfjord <- repparfjord %>%
  group_by(Element) %>%
  mutate(norm = rescale(Concentration)) %>%
  ungroup()

##### 8. boja po elementu==================

element_colors <- c(
  Ag="gray10", As="darkgoldenrod", Bi="darksalmon", Cd="orchid",
  Co="midnightblue", Cr="darkorchid4", Cu="orange1",
  Mn="magenta1", Mo="gray80", Ni="pink1", Pb="darkolivegreen4",
  Sb="turquoise3", Sc="burlywood", Zn="plum"
)

# ✅ funkcija za plotly colorscale
make_colorscale <- function(col) {
  list(
    c(0, "gray95"),
    c(1, col)
  )
}

library(grDevices)

get_color_vector <- function(base_color, norm_vals) {
  pal <- colorRampPalette(c("gray95", base_color))
  pal(100)[as.numeric(cut(norm_vals, breaks = 100))]
}


##### 9. jednostavni 3D model==========================

plot_simple_3D <- function(element_name) {
  
  df <- repparfjord %>% filter(Element == element_name)
  
  p <- plot_ly()
  # =========================
  # ✅ CLEAN SEA OUTLINE (BEZ ARTEFAKTA)
  # =========================
  
  sea_single <- st_union(sea)
  sea_boundary <- st_boundary(sea_single)
  sea_lines <- st_cast(sea_boundary, "LINESTRING")
  
  for (i in seq_along(sea_lines)) {
    
    coords <- st_coordinates(sea_lines[i])
    
    p <- p %>%
      add_trace(
        x = coords[,1],
        y = coords[,2],
        z = 0,
        type = "scatter3d",
        mode = "lines",
        line = list(color = "cornflowerblue", width = 6),
        showlegend = FALSE
      )
  }
  # ✅ core lokacije (gore)
  p <- p %>%
    add_trace(
      x = coords_df$X,
      y = coords_df$Y,
      z = 0,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 6, color = "red"),
      name = "Cores"
    )
  
  # ✅ vertikalni stupovi
  for (core_id in unique(df$Core)) {
    
    core_df <- df %>% filter(Core == core_id)
    
    if (nrow(core_df) < 2) next
    
    x0 <- core_df$X[1]
    y0 <- core_df$Y[1]
    
    p <- p %>%
      add_trace(
        x = rep(x0, nrow(core_df)),
        y = rep(y0, nrow(core_df)),
        z = -core_df$Mean_depth, #* 20,   # skaliranje dubine
        type = "scatter3d",
        mode = "lines+markers",
        marker = list(
          size = 14,
          color = get_color_vector(
            element_colors[element_name],
            core_df$norm), 
          showscale = FALSE),
        line = list(
          width = 10,
          color = get_color_vector(
            element_colors[element_name],
            core_df$norm)),
        showlegend = FALSE
      )
  }
  
  # =========================
  # ✅ NORTH ARROW (linija)
  # =========================
  
  # odaberi poziciju (gornji lijevi kut fjorda)
  x0 <- min(coords_df$X)+1000
  y0 <- max(coords_df$Y)+2200
  
  arrow_length <- 1000  # prilagodi razmjeru
  
  p <- p %>%
    add_trace(
      x = c(x0, x0),
      y = c(y0, y0 + arrow_length),
      z = c(0, 0),
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black", width = 6),
      showlegend = FALSE
    )
  
  # =========================
  # ✅ NORTH LABEL
  # =========================
  
  p <- p %>%
    add_trace(
      x = c(x0, x0 - 100, x0, x0 + 100),
      y = c(y0 + arrow_length, y0 + arrow_length - 200, y0 + arrow_length, y0 + arrow_length - 200),
      z = c(0, 0, 0, 0),
      type = "scatter3d",
      mode = "lines+text",
      line = list(color = "black", width = 3),
      textfont = list(size = 16, color = "black"),
      
      showlegend = FALSE
    )
  
  p %>%
    layout(
      title = paste("3D Core Profiles —", element_name),
      scene = list(
        zaxis = list(title = "Depth (cm)"),
        aspectratio = list(x = 1, y = 1, z = 2)
      )
    )
}

ag <- plot_simple_3D("Ag")

png("Ag.png", res = 450, height = 1500, width = 3000)
plot(plot_simple_3D("Ag"))
dev.off()
plot_simple_3D("As")
plot_simple_3D("Bi")
plot_simple_3D("Cd")
plot_simple_3D("Co")
plot_simple_3D("Cr")
plot_simple_3D("Cu")
plot_simple_3D("Mn")
plot_simple_3D("Mo")
plot_simple_3D("Ni")
plot_simple_3D("Pb")
plot_simple_3D("Sb")
plot_simple_3D("Sc")
plot_simple_3D("Zn")

plots <- lapply(names(element_colors), plot_simple_3D)

##### 10. EXPORT===========================================

library(htmlwidgets)

# ✅ napravi folder (ako ne postoji)
dir.create("html", recursive = TRUE, showWarnings = FALSE)

for (el in names(element_colors)) {
  
  p <- plot_simple_3D(el)
  
  htmlwidgets::saveWidget(
    p,
    file = paste0("MAP/html/", el, "_3D.html"),
    selfcontained = TRUE
  )
}

install.packages("writexl")  # samo prvi put
library(writexl)
write_xlsx(repparfjord, "Repparfjord.xlsx")
##Ovo dolje ne koristiti jer ne radi 100% i treba doradu
##### 11. PROBA=====================================

plot_combined <- function(element_name) {
  
  df <- repparfjord %>% filter(Element == element_name)
  
  # =========================
  # 🔵 1. MAP (2D)
  # =========================
  
  
  sea_poly <- st_union(sea)   # jedinstveni poligon
  sea_coords <- st_coordinates(sea_poly)
  
  bbox <- st_bbox(sea_poly)
  
  p_map <- plot_ly() %>%
    
    # ✅ FILLED SEA
    add_trace(
      x = sea_coords[,1],
      y = sea_coords[,2],
      type = "scatter",
      mode = "lines",
      fill = "toself",              # 🔥 OVO POPUNJAVA POLIGON
      fillcolor = "lightblue",
      line = list(color = "lightblue", width = 1),
      showlegend = FALSE
    ) %>%
    
    # ✅ CORE POINTS
    add_trace(
      x = coords_df$X,
      y = coords_df$Y,
      type = "scatter",
      mode = "markers",
      marker = list(size = 8, color = "red"),
      name = "Cores"
    ) %>%
    
    layout(
      title = "Core locations",
      xaxis = list(range = c(bbox$xmin, bbox$xmax)),   # ✅ OGRANIČENJE
      yaxis = list(range = c(bbox$ymin, bbox$ymax)),
      showlegend = FALSE
    )
  
  
  # =========================
  # 🔴 2. 3D PROFILI
  # =========================
  
  
  surface_df <- df %>%
    group_by(Core) %>%
    slice_min(Mean_depth) %>%   # uzmi najplići uzorak (0–1 cm)
    ungroup()
  
  p_3D <- p_3D %>%
    add_trace(
      x = surface_df$X,
      y = surface_df$Y,
      z = 0,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 10,
        color = surface_df$norm,
        colorscale = "OrRd",
        showscale = TRUE,
        colorbar = list(title = paste(element_name, "surface"))
      ),
      name = "Surface concentration"
    )
  
  # profile
  for (core_id in unique(df$Core)) {
    
    core_df <- df %>% filter(Core == core_id)
    if (nrow(core_df) < 2) next
    
    x0 <- core_df$X[1]
    y0 <- core_df$Y[1]
    
    p_3D <- p_3D %>%
      add_trace(
        x = rep(x0, nrow(core_df)),
        y = rep(y0, nrow(core_df)),
        z = -core_df$Mean_depth * 20,
        type = "scatter3d",
        mode = "lines",
        line = list(
          width = 12,
          color = core_df$norm,
          colorscale = "OrRd",
          showscale = TRUE
        ),
        showlegend = FALSE
      )
  }
  
  p_3D <- p_3D %>%
    layout(
      scene = list(
        zaxis = list(title = "Depth (cm)"),
        aspectratio = list(x = 1, y = 1, z = 2)
      )
    )
  
  # =========================
  # ✅ KOMBINACIJA
  # =========================
  
  subplot(
    p_map, 
    p_3D,
    nrows = 2,
    heights = c(0.35, 0.65),
    titleY = TRUE,
    shareX = FALSE,
    shareY = FALSE
  )
}

plot_combined("Cu")



# TABLICA KORIŠTENIH PAKETA -----------------------------------------------


# instaliraj ako treba
# install.packages(c("stringr", "dplyr", "purrr", "writexl"))

library(stringr)
library(dplyr)
library(purrr)
library(writexl)

# ---- 1. učitaj R skriptu 
file_path <- "C:/Users/Laura/DOKTORAT/Norveska/Disertacija/R_project/Disertacija.R"   # ← promijeni putanju
lines <- readLines(file_path, warn = FALSE)

# ---- 2. izdvoji pakete 
packages <- lines %>%
  str_extract_all("(?<=library\\()([^\\)]+)|(?<=require\\()([^\\)]+)") %>%
  unlist() %>%
  str_replace_all("\"|'", "") %>%
  str_trim() %>%
  unique()

packages <- packages[packages != ""]

# ---- 3. funkcija za dohvat metapodataka 
get_pkg_info <- function(pkg) {
  desc <- tryCatch(packageDescription(pkg), error = function(e) NULL)
  cit <- tryCatch(citation(pkg), error = function(e) NULL)
  
  citation_text <- if (!is.null(cit)) {
    paste(capture.output(print(cit)), collapse = " ")
  } else {
    NA
  }
  
  if (!is.null(desc)) {
    tibble(
      Package = pkg,
      Title = desc$Title %||% NA,
      Version = desc$Version %||% NA,
      Author = desc$Author %||% NA,
      Year = desc$Date %||% NA,
      Citation = citation_text
    )
  } else {
    tibble(
      Package = pkg,
      Title = NA,
      Version = NA,
      Author = NA,
      Year = NA,
      Citation = NA
    )
  }
}

# helper za NULL
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- 4. napravi tablicu 
pkg_table <- map_dfr(packages, get_pkg_info)

# ---- 5. spremi u Excel 
write_xlsx(pkg_table, "paketi_citation.xlsx")
