# ============================================================
# COMP1890 Spatial Data Science - Coursework
# Fuel Poverty Spatial Analysis - England
# Author: Zakariya Guechchati
# Date: April 2026
# ============================================================

# 1. LOAD LIBRARIES ==========================================
library(sf)
library(spdep)
library(ggplot2)
library(tmap)
library(dplyr)
library(viridis)

# 2. LOAD DATA ===============================================
lsoa_sf <- st_read("LSOA_2021_EW_BGC_V5.shp")
imd <- read.csv("imd_clean.csv")
fp <- read.csv("fuel_poverty_clean.csv")

# 3. MERGE AND JOIN ==========================================
csv_joined <- imd %>%
  left_join(fp %>% select(LSOA21CD, Region, Households,
                           Households_Fuel_Poor, Fuel_Poverty_Pct),
            by = "LSOA21CD")

lsoa_joined <- lsoa_sf %>%
  inner_join(csv_joined, by = c("LSOA21CD" = "LSOA21CD")) %>%
  filter(substr(LSOA21CD, 1, 1) == "E")

cat("Rows:", nrow(lsoa_joined), "\n")
cat("Columns:", ncol(lsoa_joined), "\n")
cat("CRS:", st_crs(lsoa_joined)$Name, "\n")

# 4. EXPLORATORY SUMMARY =====================================
summary(lsoa_joined$Fuel_Poverty_Pct)
summary(lsoa_joined$IMD_Rank)
table(lsoa_joined$Region)

# 5. SPATIAL WEIGHTS MATRIX ==================================
# Queen contiguity - LSOAs sharing a boundary or corner are neighbours
nb <- poly2nb(lsoa_joined, queen = TRUE)

# Row-standardised weights - ensures equal influence per LSOA
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

# 6. MORAN'S I - GLOBAL SPATIAL AUTOCORRELATION ==============
moran_result <- moran.test(lsoa_joined$Fuel_Poverty_Pct,
                            lw,
                            zero.policy = TRUE)
print(moran_result)

# 7. GETIS-ORD GI* - LOCAL HOTSPOT ANALYSIS =================
gi_star <- localG(lsoa_joined$Fuel_Poverty_Pct, lw, zero.policy = TRUE)
lsoa_joined$gi_star <- as.numeric(gi_star)

# Classify z-scores at 95% and 99% confidence thresholds
lsoa_joined$hotspot <- cut(lsoa_joined$gi_star,
                            breaks = c(-Inf, -2.58, -1.96, 1.96, 2.58, Inf),
                            labels = c("Cold spot 99%", "Cold spot 95%",
                                       "Not significant",
                                       "Hot spot 95%", "Hot spot 99%"))
table(lsoa_joined$hotspot)

# 8. SPEARMAN'S RANK CORRELATION =============================
# Non-parametric test - used because neither variable is normally distributed
cor_result <- cor.test(lsoa_joined$IMD_Rank,
                        lsoa_joined$Fuel_Poverty_Pct,
                        method = "spearman",
                        exact = FALSE)
print(cor_result)

# 9. EXPORT FOR QGIS =========================================
st_write(lsoa_joined, "lsoa_analysis.gpkg", delete_if_exists = TRUE)

# 10. CHARTS =================================================
# Scatter plot - IMD Rank vs Fuel Poverty
png("chart1_scatter.png", width = 2000, height = 1600, res = 300)
ggplot(lsoa_joined, aes(x = IMD_Rank, y = Fuel_Poverty_Pct)) +
  geom_point(alpha = 0.1, size = 0.5, colour = "#d73027") +
  geom_smooth(method = "lm", colour = "black", linewidth = 0.8) +
  labs(title = "Deprivation Rank vs Fuel Poverty Rate",
       x = "IMD Rank (1 = most deprived)",
       y = "Fuel Poverty (%)",
       caption = "Source: DESNZ 2023; DLUHC 2025") +
  theme_minimal()
dev.off()

# Box plot - Fuel Poverty by Region
png("chart2_boxplot.png", width = 2400, height = 1600, res = 300)
ggplot(lsoa_joined, aes(x = reorder(Region, Fuel_Poverty_Pct, median),
                         y = Fuel_Poverty_Pct, fill = Region)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Fuel Poverty Distribution by Region",
       x = "Region",
       y = "Fuel Poverty (%)",
       caption = "Source: DESNZ 2023") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
