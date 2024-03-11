library(ggplot2)
library(dplyr)
library(rcompanion) 
library(purrr)


dati <- read.csv2("./data.csv", stringsAsFactors = FALSE)

convert_to_numeric <- function(column, col_name) {
  numeric_column <- as.numeric(gsub(",", ".", column))
  return(numeric_column)
}

numeric_columns <- c("LOAD", "LCP", "FID", "INP", "CLS", "EXTRA_TEMPERATURE", "EXTRA_VOLTAGE", "BATTERY_PROPERTY_CURRENT_NOW", "Energy_trapz_J", "Energy_simple_J", "Avg_power_W")

for(col in numeric_columns) {
  dati[[col]] <- convert_to_numeric(dati[[col]], col)
}


dati$DEVICE <- as.factor(dati$DEVICE)
dati$BANDWIDTH <- as.factor(dati$BANDWIDTH)

unique_combinations <- expand.grid(DEVICE = unique(dati$DEVICE), BANDWIDTH = unique(dati$BANDWIDTH))

file_name <- paste0("./CLS_Distribution.png")
png(file_name, width = 1600, height = 1200)
par(mfrow=c(3, 2))
for(i in 1:nrow(unique_combinations)) {
  filtered_data <- dati %>%
    filter(DEVICE == unique_combinations$DEVICE[i],
           BANDWIDTH == unique_combinations$BANDWIDTH[i])

  plot_title <- paste("CLS Distribution for", unique_combinations$DEVICE[i], "on", unique_combinations$BANDWIDTH[i])
  plotNormalHistogram(filtered_data$CLS, prob = FALSE, breaks = 100, col="yellow", border="black",
                      main = plot_title, length=1000, linecol="black", lwd=3, xlab = "CLS")

}
dev.off()

file_name <- paste0("./FID_Distribution.png")
png(file_name, width = 1600, height = 1200)
par(mfrow=c(3, 2))
for(i in 1:nrow(unique_combinations)) {
  filtered_data <- dati %>%
    filter(DEVICE == unique_combinations$DEVICE[i],
           BANDWIDTH == unique_combinations$BANDWIDTH[i])

  plot_title <- paste("FID Distribution for", unique_combinations$DEVICE[i], "on", unique_combinations$BANDWIDTH[i])
  plotNormalHistogram(filtered_data$FID, prob = FALSE, breaks = 100, col="yellow", border="black",
                      main = plot_title, length=1000, linecol="black", lwd=3, xlab = "FID")

}
dev.off()


file_name <- paste0("./INP_Distribution.png")
png(file_name, width = 1600, height = 1200)
par(mfrow=c(3, 2))
for(i in 1:nrow(unique_combinations)) {
  filtered_data <- dati %>%
    filter(DEVICE == unique_combinations$DEVICE[i],
           BANDWIDTH == unique_combinations$BANDWIDTH[i])

  plot_title <- paste("INP Distribution for", unique_combinations$DEVICE[i], "on", unique_combinations$BANDWIDTH[i])
  plotNormalHistogram(filtered_data$INP, prob = FALSE, breaks = 100, col="yellow", border="black",
                      main = plot_title, length=1000, linecol="black", lwd=3, xlab = "INP")

}
dev.off()


file_name <- paste0("./LCP_Distribution.png")
png(file_name, width = 1600, height = 1200)
par(mfrow=c(3, 2))
for(i in 1:nrow(unique_combinations)) {
  filtered_data <- dati %>%
    filter(DEVICE == unique_combinations$DEVICE[i],
           BANDWIDTH == unique_combinations$BANDWIDTH[i])

  plot_title <- paste("LCP Distribution for", unique_combinations$DEVICE[i], "on", unique_combinations$BANDWIDTH[i])
  plotNormalHistogram(filtered_data$LCP, prob = FALSE, breaks = 100, col="yellow", border="black",
                      main = plot_title, length=1000, linecol="black", lwd=3, xlab = "LCP")

}
dev.off()

############################### BOX PLOT COMBINATI #####################################

color_map <- setNames(c("red", "green", "blue"), levels(dati$BANDWIDTH))


library(patchwork)

plot_LCP <- ggplot(dati, aes(x = interaction(DEVICE, BANDWIDTH), y = LCP, fill = BANDWIDTH)) +
  geom_boxplot() +
  scale_fill_manual(values = color_map) +
  labs( x = "Bandwidth and Device", y = "LCP") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30), 
    axis.text.y = element_text(size = 30), 
    axis.title.x = element_text(size = 40), 
    axis.title.y = element_text(size = 40), 
  )+
  guides(fill = FALSE)

plot_FID <- ggplot(dati, aes(x = interaction(DEVICE, BANDWIDTH), y = FID, fill = BANDWIDTH)) +
  geom_boxplot() +
  scale_fill_manual(values = color_map) +
  labs(x = "Bandwidth and Device", y = "FID") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30), 
    axis.text.y = element_text(size = 30), 
    axis.title.x = element_text(size = 40), 
    axis.title.y = element_text(size = 40), 
  )+
  guides(fill = FALSE)

plot_INP <- ggplot(dati, aes(x = interaction(DEVICE, BANDWIDTH), y = INP, fill = BANDWIDTH)) +
  geom_boxplot() +
  scale_fill_manual(values = color_map) +
  labs( x = "Bandwidth and Device", y = "INP") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30), 
    axis.text.y = element_text(size = 30), 
    axis.title.x = element_text(size = 40), 
    axis.title.y = element_text(size = 40), 
  )+
  guides(fill = FALSE)

plot_CLS <- ggplot(dati, aes(x = interaction(DEVICE, BANDWIDTH), y = CLS, fill = BANDWIDTH)) +
  geom_boxplot() +
  scale_fill_manual(values = color_map) +
  labs(x = "Bandwidth and Device", y = "CLS") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30), 
    axis.text.y = element_text(size = 30), 
    axis.title.x = element_text(size = 40), 
    axis.title.y = element_text(size = 40), 
  )+
  guides(fill = FALSE)

  plot_Energy <- ggplot(dati, aes(x = interaction(DEVICE, BANDWIDTH), y = Energy_trapz_J, fill = BANDWIDTH)) +
  geom_boxplot() +
  scale_fill_manual(values = color_map) +
  labs(x = "Bandwidth and Device", y = "Energy_trapz_J") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30), 
    axis.text.y = element_text(size = 30), 
    axis.title.x = element_text(size = 40), 
    axis.title.y = element_text(size = 40), 
  )+
  guides(fill = FALSE)


combined_plot <- (plot_LCP | plot_FID) / 
                 (plot_INP | plot_CLS) /
                 (plot_Energy)+
                 plot_layout(guides = "collect") 

ggsave("./combined_plots.png", combined_plot, width = 25, height = 15) 

ggsave("./plot_LCP.png", plot_LCP, width = 25, height = 15) 

ggsave("./plot_CLS.png", plot_CLS, width = 25, height = 15) 

ggsave("./plot_INP.png", plot_INP, width = 25, height = 15) 

ggsave("./plot_FID.png", plot_FID, width = 25, height = 15) 

ggsave("./plot_Energy.png", plot_Energy, width = 25, height = 15) 



########################################


#######à Violini  ##################
metriche <- c("LCP", "FID", "INP", "CLS")

for (metrica in metriche) {
  ggplot(dati, aes(x = BANDWIDTH, y = get(metrica), fill = DEVICE)) +
  geom_violin() +
  labs(title = paste("Violin Plot of", metrica, "by DEVICE and BANDWIDTH"),
         x = "Bandwidth",
         y = metrica) +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~DEVICE) +
  scale_y_log10(limits = c(1e-4, 1e4))
  
  ggsave(paste("./violin_plot_", metrica, ".png", sep = ""))
}

######################################


################## Shaphiro test ############################

risultati_shapiro <- dati_clean %>%
  group_by(BANDWIDTH, DEVICE) %>%
  summarise(
    test_shapiro_LCP = list(shapiro.test(LCP)),
    .groups = 'drop'
  )

risultati_shapiro <- risultati_shapiro %>%
  mutate(
    w_value = map_dbl(test_shapiro_LCP, ~ .x$statistic),
    p_value = map_dbl(test_shapiro_LCP, ~ .x$p.value)
  ) %>%
  select(BANDWIDTH, DEVICE, w_value, p_value)

print(risultati_shapiro)

risultati_shapiro <- dati_clean %>%
  group_by(BANDWIDTH, DEVICE) %>%
  summarise(
    test_shapiro_FID = list(shapiro.test(FID)),
    .groups = 'drop' 
  )

risultati_shapiro <- risultati_shapiro %>%
  mutate(
    w_value = map_dbl(test_shapiro_FID, ~ .x$statistic),
    p_value = map_dbl(test_shapiro_FID, ~ .x$p.value)
  ) %>%
  select(BANDWIDTH, DEVICE, w_value, p_value)

print(risultati_shapiro)

risultati_shapiro <- dati_clean %>%
  group_by(BANDWIDTH, DEVICE) %>%
  summarise(
    test_shapiro_INP = list(shapiro.test(INP)),
    .groups = 'drop' 
  )

risultati_shapiro <- risultati_shapiro %>%
  mutate(
    w_value = map_dbl(test_shapiro_INP, ~ .x$statistic),
    p_value = map_dbl(test_shapiro_INP, ~ .x$p.value)
  ) %>%
  select(BANDWIDTH, DEVICE, w_value, p_value)

print(risultati_shapiro)

risultati_shapiro <- dati_clean %>%
  group_by(BANDWIDTH, DEVICE) %>%
  summarise(
    test_shapiro_CLS = list(shapiro.test(CLS)),
    .groups = 'drop'
  )

risultati_shapiro <- risultati_shapiro %>%
  mutate(
    w_value = map_dbl(test_shapiro_CLS, ~ .x$statistic),
    p_value = map_dbl(test_shapiro_CLS, ~ .x$p.value)
  ) %>%
  select(BANDWIDTH, DEVICE, w_value, p_value)

print(risultati_shapiro)

risultati_shapiro <- dati_clean %>%
  group_by(BANDWIDTH, DEVICE) %>%
  summarise(
    test_shapiro_ENERGY = list(shapiro.test(Energy_trapz_J)),
    .groups = 'drop'
  )

risultati_shapiro <- risultati_shapiro %>%
  mutate(
    w_value = map_dbl(test_shapiro_ENERGY, ~ .x$statistic),
    p_value = map_dbl(test_shapiro_ENERGY, ~ .x$p.value)
  ) %>%
  select(BANDWIDTH, DEVICE, w_value, p_value)

print(risultati_shapiro)


##########################################################


################ QQ-plots ########################################


qq_plot <- ggplot(dati_clean, aes(sample = LCP)) + 
  geom_qq() + 
  geom_qq_line(colour = "red") + 
  facet_wrap(~DEVICE + BANDWIDTH) + 
  theme_minimal() 
ggsave("./qq_plot_LCP_per_BANDWIDTH.png", plot = qq_plot, width = 8, height = 6, dpi = 300)
#print(qq_plot)

qq_plot <- ggplot(dati_clean, aes(sample = FID)) + 
  geom_qq() + 
  geom_qq_line(colour = "red") + 
  facet_wrap(~DEVICE + BANDWIDTH) + 
  theme_minimal() 
ggsave("./qq_plot_FID_per_BANDWIDTH.png", plot = qq_plot, width = 8, height = 6, dpi = 300)
#print(qq_plot)


qq_plot <- ggplot(dati_clean, aes(sample = INP)) + 
  geom_qq() + 
  geom_qq_line(colour = "red") + 
  facet_wrap(~DEVICE + BANDWIDTH) + 
  theme_minimal() 
ggsave("./qq_plot_INP_per_BANDWIDTH.png", plot = qq_plot, width = 8, height = 6, dpi = 300)
#print(qq_plot)



qq_plot <- ggplot(dati_clean, aes(sample = CLS)) + 
  geom_qq() + 
  geom_qq_line(colour = "red") + 
  facet_wrap(~DEVICE + BANDWIDTH) + 
  theme_minimal() 
ggsave("./qq_plot_CLS_per_BANDWIDTH.png", plot = qq_plot, width = 8, height = 6, dpi = 300)
#print(qq_plot)


qq_plot <- ggplot(dati_clean, aes(sample = Energy_trapz_J)) + 
  geom_qq() + 
  geom_qq_line(colour = "red") + 
  facet_wrap(~DEVICE + BANDWIDTH) + 
  theme_minimal() 
ggsave("./qq_plot_Energy_trapz_J_per_BANDWIDTH.png", plot = qq_plot, width = 8, height = 6, dpi = 300)
#print(qq_plot)


#######################################################

###################### KRUSKAL-WALLIS || DUNN'S  ################################


metrics <- c("LCP", "FID", "INP", "CLS", "Energy_trapz_J")

sink("./dunn_test_results_for_metrics_and_device.txt")

desired_order <- c("512kbps", "2mbps", "50mbps") 

for(device in unique(dati_clean$DEVICE)) {
  
  for(metric in metrics) {
    dati_device <- dati_clean[dati_clean$DEVICE == device,]
    
    dati_device$BANDWIDTH <- factor(dati_device$BANDWIDTH, levels = desired_order)
    
    if(any(!is.na(dati_device[[metric]]))) {
      result <- dunn.test(dati_device[[metric]], dati_device$BANDWIDTH, method="bonferroni")
      
      cat(paste("Dunn's Test Results for DEVICE:", device, "and Metric:", metric, "\n"))
      print(result)
      cat("\n")
    } else {
      cat(paste("Invalid data for DEVICE:", device, "and Metric:", metric, "\n\n"))
    }
  }
}


sink() 

cat("Dunn's test results have been saved in 'dunn_test_results_for_metrics_and_device.txt'\n")


##############################################################

######################### Detail analisys ##################################

dati_pixel3 <- filter(dati_clean, DEVICE == "Pixel3")

dati_pixel6 <- filter(dati_clean, DEVICE == "Pixel6")


sink("./details.txt")

MOD1 <- lm(Energy_trapz_J ~ LCP * BANDWIDTH, data=dati_pixel3)
print(summary(MOD1))

MOD2 <- lm(Energy_trapz_J ~ LCP * BANDWIDTH, data=dati_pixel6)
print(summary(MOD2))

MOD3 <- lm(Energy_trapz_J ~ CLS * BANDWIDTH, data=dati_pixel6)
print(summary(MOD3))

sink()


p <- ggplot(dati_pixel6, aes(x = LCP, y = Energy_trapz_J, color = factor(BANDWIDTH))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = factor(BANDWIDTH))) + 
  scale_color_manual(values = c("512kbps" = "blue", "2mbps" = "red", "50mbps" = "green")) +
  labs(x = "LCP (ms)", y = "Energy (J)", color = "Bandwidth", fill = "Bandwidth") +
  theme_minimal()


ggsave("./LCP_per_px6.png", plot = p, width = 8, height = 6, dpi = 300)


p <- ggplot(dati_pixel3, aes(x = LCP, y = Energy_trapz_J, color = factor(BANDWIDTH))) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, aes(fill = factor(BANDWIDTH))) + 
  scale_color_manual(values = c("512kbps" = "blue", "2mbps" = "red", "50mbps" = "green")) + 
  labs(x = "LCP (ms)", y = "Energy (J)", color = "Bandwidth", fill = "Bandwidth") + 
  theme_minimal() 


ggsave("./LCP_per_px3.png", plot = p, width = 8, height = 6, dpi = 300)



p <- ggplot(dati_pixel6, aes(x = CLS, y = Energy_trapz_J, color = factor(BANDWIDTH))) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE, aes(fill = factor(BANDWIDTH))) + 
  scale_color_manual(values = c("512kbps" = "blue", "2mbps" = "red", "50mbps" = "green")) +  
  labs(x = "CLS", y = "Energy (J)", color = "Bandwidth", fill = "Bandwidth") +  
  theme_minimal()  


ggsave("./CLS_per_px6.png", plot = p, width = 8, height = 6, dpi = 300)

########################################################################
