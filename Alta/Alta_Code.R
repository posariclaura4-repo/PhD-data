#working directory
setwd("#####/R_project") #Change ##### with your working directory


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
  "Water-soluble fraction"                   = "#BC31A4", 
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
    facet_wrap(~factor(Step, c( "Water-soluble fraction", "Exchangeable fraction", "Poorly crystalline Fe(III)-oxyhydroxides", 
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

