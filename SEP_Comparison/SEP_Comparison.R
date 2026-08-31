setwd("#######/SEP_comparison") ##Edit your working directory. 


library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)

####EXCHANGEABLE FRACTION====================================================

library(dplyr)
library(stringr)
library(readxl)

exchang_trends <- read_excel(
  "DATA/Data.xlsx",
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

# Standard - trends -------------------------------------------------------
seq_by_samples <- read_excel('SEQ_ANA2.xlsx', sheet = 'Final_mg_kg_7')
seq_by_samples

seq_by_samples <- data.frame(seq_by_samples)
seq_by_samples 

str(seq_by_samples)

Element <- as.factor(seq_by_samples$Element)
Element

Step <- as.factor(seq_by_samples$Step)
Step

LP_ST_7 <- as.double(seq_by_samples$LP_ST_7)
LP_ST_7
LP_0_1 <- as.double(seq_by_samples$LP_1)
LP_0_1
#LP_0_1a <- as.double(seq_by_samples$LP_1a)
#LP_0_1a
#LP_0_1b <- as.double(seq_by_samples$LP_1b)
#LP_0_1b
#P_0_1c <- as.double(seq_by_samples$LP_1c)
#LP_0_1c
LP_1_2 <- as.double(seq_by_samples$LP_2)
LP_1_2
LP_2_3 <- as.double(seq_by_samples$LP_3)
LP_2_3
LP_3_4 <- as.double(seq_by_samples$LP_4)
LP_3_4
LP_4_5 <- as.double(seq_by_samples$LP_5)
LP_4_5
LP_5_6 <- as.double(seq_by_samples$LP_6)
LP_5_6
LP_6_7 <- as.double(seq_by_samples$LP_7)
LP_6_7
LP_7_8 <- as.double(seq_by_samples$LP_8)
LP_7_8
LP_8_9 <- as.double(seq_by_samples$LP_9)
LP_8_9
# LP_8_9a <- as.double(seq_by_samples$LP_9a)
# LP_8_9a
# LP_8_9b <- as.double(seq_by_samples$LP_9b)
# LP_8_9a
LP_9_10 <- as.double(seq_by_samples$LP_10)
LP_9_10
LP_10_11 <- as.double(seq_by_samples$LP_11)
LP_10_11
LP_11_12 <- as.double(seq_by_samples$LP_12)
LP_11_12
LP_12_13 <- as.double(seq_by_samples$LP_13)
LP_12_13
LP_13_14 <- as.double(seq_by_samples$LP_14)
LP_13_14
LP_14_15 <- as.double(seq_by_samples$LP_15)
LP_14_15
LP_15_16 <- as.double(seq_by_samples$LP_16)
LP_15_16
LP_16_17 <- as.double(seq_by_samples$LP_17)
LP_16_17
LP_17_18 <- as.double(seq_by_samples$LP_18)
LP_17_18
LP_18_19 <- as.double(seq_by_samples$LP_19)
LP_18_19
LP_19_20 <- as.double(seq_by_samples$LP_20)
LP_19_20
LP_20_21 <- as.double(seq_by_samples$LP_21)
LP_20_21
LP_21_22 <- as.double(seq_by_samples$LP_22)
LP_21_22
LP_22_23 <- as.double(seq_by_samples$LP_23)
LP_22_23
# LP_23_24a <- as.double(seq_by_samples$LP_24a)
# LP_23_24a
# LP_23_24b <- as.double(seq_by_samples$LP_24b)
# LP_23_24b
LP_23_24 <- as.double(seq_by_samples$LP_24)
LP_23_24

StepOrder <- as.double(seq_by_samples$OrderStep)


seq_samples_df <- data.frame(Element, StepOrder, Step, LP_ST_7, LP_0_1, LP_1_2, 
                             LP_2_3, LP_3_4, LP_4_5, LP_5_6, LP_6_7, LP_7_8, LP_8_9, LP_9_10, 
                             LP_10_11, LP_11_12, LP_12_13, LP_13_14, LP_14_15, LP_15_16, LP_16_17,
                             LP_17_18, LP_18_19, LP_19_20, LP_20_21, LP_21_22, LP_22_23, LP_23_24)
seq_samples_df



#povu?i podatke iz sekcije: "Seven step extraction data visualization"
PLOT_LP_ST7ad <- ggplot(seq_samples_df, aes(fill= reorder(Step, StepOrder), y=LP_ST_7, x=Element)) + 
  geom_col(position = 'fill', stat = 'identity') + 
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = SSE_skala, name = "Legend") +
  labs(x="", y="Proportion") +
  theme_minimal(base_size = 10)+
  theme(axis.text.x = element_blank(), 
        legend.position = "right", 
        axis.text.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_text(size = 18), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(2,-1,2,0), "mm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  # coord_flip() +
  scale_y_reverse()

PLOT_LP_ST7ad


seq_by_samples2 <- read_excel('SEQ_ANA2.xlsx', sheet = 'Final_mg_kg_BCR')
seq_by_samples2

seq_by_samples2 <- data.frame(seq_by_samples2)
seq_by_samples2 

str(seq_by_samples2)

Element2 <- as.factor(seq_by_samples2$Element)
Element2

Step2 <- as.factor(seq_by_samples2$Step)
Step2

LP_BCR_ST <- as.double(seq_by_samples2$LP_BCR1_ST)
LP_BCR_ST
LP_BCR_0_1 <- as.double(seq_by_samples2$LP_BCR1_1)
LP_BCR_0_1
LP_BCR_8_9 <- as.double(seq_by_samples2$LP_BCR1_9)
LP_BCR_8_9
LP_BCR_14_15 <- as.double(seq_by_samples2$LP_BCR1_15)
LP_BCR_14_15
LP_BCR_15_16 <- as.double(seq_by_samples2$LP_BCR1_16)
LP_BCR_15_16
LP_BCR_16_17 <- as.double(seq_by_samples2$LP_BCR1_17)
LP_BCR_16_17
LP_BCR_17_18 <- as.double(seq_by_samples2$LP_BCR1_18)
LP_BCR_17_18
LP_BCR_18_19 <- as.double(seq_by_samples2$LP_BCR1_19)
LP_BCR_18_19
LP_BCR_19_20 <- as.double(seq_by_samples2$LP_BCR1_20)
LP_BCR_19_20
LP_BCR_20_21 <- as.double(seq_by_samples2$LP_BCR1_21)
LP_BCR_20_21
LP_BCR_21_22 <- as.double(seq_by_samples2$LP_BCR1_22)
LP_BCR_21_22
LP_BCR_23_24 <- as.double(seq_by_samples2$LP_BCR1_24)
LP_BCR_23_24

StepOrder2 <- as.double(seq_by_samples2$StepOrder)


seq_samples_df2 <- data.frame(Element2, StepOrder2, Step2, LP_BCR_ST, LP_BCR_0_1, LP_BCR_8_9, 
                              LP_BCR_14_15, LP_BCR_15_16, LP_BCR_16_17, LP_BCR_17_18, 
                              LP_BCR_18_19, LP_BCR_19_20, LP_BCR_20_21, LP_BCR_21_22, LP_BCR_23_24)
seq_samples_df2
str(seq_samples_df2)

#povu?i podatke iz sekcije: "BCR extraction data visualization"
PLOT_BCR_STad <- ggplot(seq_samples_df2, aes(fill= reorder(Step2, StepOrder2), y=LP_BCR_ST, x=Element2)) + 
  geom_col(position = 'fill', stat = 'identity') + 
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = BCR_skala, name = "Legend") +
  labs(x="", y="Proportion") +
  theme_minimal(base_size = 10)+
  theme(axis.text.x = element_blank(), legend.position = "right", 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(size = 18), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        plot.margin = unit(c(2,-1,2,-1), "mm"), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
# coord_flip()

PLOT_BCR_STad

PLOT_BCR_STadt <- ggplot(seq_samples_df2t, aes(fill= reorder(Step2t, StepOrder2t), y=LP_BCR_STt, x=Element2t)) + 
  geom_bar(position = 'fill', stat = 'identity') + 
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = BCR_skala, name = "Legend") +
  labs(x="Element", y="Proportion") +
  theme_minimal(base_size = 10)+
  theme(axis.text.x = element_blank(), legend.position = "right", 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.title = element_text(size = 18), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12), 
        plot.margin = unit(c(2,-1,2,-1), "mm"), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  # coord_flip()+
  scale_y_reverse()

PLOT_BCR_STadt

mid <- ggplot(seq_samples_df, aes(x=1, y=Element))+
  geom_text(aes(label=Element))+
  geom_segment(aes(x=0.94,xend=0.96,yend=Element))+
  geom_segment(aes(x=1.04,xend=1.065,yend=Element))+
  ggtitle("")+
  ylab(NULL)+
  coord_flip()+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme_minimal(base_size = 16)+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(-7.8,-1,-0.7,-1), "mm"))
mid

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
      title.position = "top",
      nrow = 7, 
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    text=element_text(family = "Corbel Bold"), 
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text  = element_text(size = 20), 
    legend.justification = "left",
    legend.box.just = "left",
    
    plot.title = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 0)
    
    
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
  "4"    = "#4C72B0", 
  "5"    = "#1f3a75"
)

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
      title.position = "top",
      nrow = 6, 
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    text=element_text(family = "Corbel Bold"), 
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text  = element_text(size = 20), 
    legend.justification = "left",
    legend.box.just = "left",
    
    plot.title = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 0)
    
  )
spacer <- ggplot()+theme_void()


stand_plot <- ggarrange(PLOT_LP_ST7ad, mid, PLOT_BCR_STad, mid, PLOT_BCR_STadt,
                        ncol=1, heights =c(3/11, 1/11, 3/11, 1/11, 3/11),common.legend = T, legend = "none") 
stand_plot

legend <- ggarrange(SSE_legend_plot, BCR_legend_plot, ncol=2, 
                    heights = c(1/25))

library(cowplot)

left <- function(p) {
  ggdraw() + draw_plot(p, x = 0, width = 1)
}

final_plot_descriST <- ggarrange(
  left(stand_plot),
  spacer,
  left(SSE_legend_plot),
  spacer,
  left(BCR_legend_plot),
  ncol = 1,
  heights = c(0.6, 0.008, 0.05, 0.02, 0.05)
)

final_plot_descriST

pdf("Standard_SE_plots.pdf", width=10, height = 25)
plot(stand_plot)
dev.off()



# SUMMED VALUES  ----------------------------------------------------------


summed <-  read_excel("SEQ_ANA2.xlsx", sheet = "SUMMED VALUES")
summed
summed <-  data.frame(summed)
str(summed)

sample.levels <- c('Standard', '0-1', '8-9', '14-15', '15-16', '16-17', '17-18', '18-19', 
                   '19-20', '20-21', '21-22', '23-24')
sample.levels <- as.factor(sample.levels)
str(sample.levels)

summed_plot <- ggplot(summed, aes(fill=SEP, y= SUM, x=Sample.Interval)) +
  geom_bar(position='dodge', stat = 'identity') +
  #stat omogu?uje da se uo?ava koliko ?ega ima. Za njega treba definirati x i y u aes()
  scale_fill_manual(values = c('darkseagreen', 'peachpuff2'), name = "Legend")+
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


summed_plot

tiff("Summed_values_all_samples.tiff", res=300, width = 3000, height = 2500)
plot(summed_plot)
dev.off()
