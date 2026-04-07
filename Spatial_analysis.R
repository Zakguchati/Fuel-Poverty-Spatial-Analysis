library(sf)
library(spdep)
library(ggplot2)
library(tmap)
library(dplyr)
library(viridis)

lsoa_sf <- st_read("LSOA_2021_EW_BGC_V5.shp")

imd <- read.csv("imd_clean.csv")
fp <- read.csv("fuel_poverty_clean.csv")

# Merge the two CSVs together
csv_joined <- imd %>%
  left_join(fp %>% select(LSOA21CD, Region, Households, 
                          Households_Fuel_Poor, Fuel_Poverty_Pct), 
            by = "LSOA21CD")

# Join to shapefile and filter to England only
lsoa_joined <- lsoa_sf %>%
  inner_join(csv_joined, by = c("LSOA21CD" = "LSOA21CD")) %>%
  filter(substr(LSOA21CD, 1, 1) == "E")

# Check the result
cat("Rows:", nrow(lsoa_joined), "\n")
cat("Columns:", ncol(lsoa_joined), "\n")
cat("CRS:", st_crs(lsoa_joined)$Name, "\n")
st_write(lsoa_joined, "lsoa_joined_clean.gpkg", delete_if_exists = TRUE)

# Check the key variables
summary(lsoa_joined$Fuel_Poverty_Pct)
summary(lsoa_joined$IMD_Rank)
table(lsoa_joined$Region)

summary(lsoa_joined$Fuel_Poverty_Pct)
summary(lsoa_joined$IMD_Rank)

# Build neighbours list (queen contiguity - shared boundary or corner counts)
nb <- poly2nb(lsoa_joined, queen = TRUE)

# Convert to spatial weights
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Check it
summary(nb)

moran_result <- moran.test(lsoa_joined$Fuel_Poverty_Pct, 
                           lw, 
                           zero.policy = TRUE)
print(moran_result)

# Calculate Getis-Ord Gi*
gi_star <- localG(lsoa_joined$Fuel_Poverty_Pct, lw, zero.policy = TRUE)

# Add results to the spatial dataframe
lsoa_joined$gi_star <- as.numeric(gi_star)

# Classify into hotspots and coldspots
lsoa_joined$hotspot <- cut(lsoa_joined$gi_star,
                           breaks = c(-Inf, -2.58, -1.96, 1.96, 2.58, Inf),
                           labels = c("Cold spot 99%", "Cold spot 95%", 
                                      "Not significant",
                                      "Hot spot 95%", "Hot spot 99%"))

# Count each category
table(lsoa_joined$hotspot)

cor_result <- cor.test(lsoa_joined$IMD_Rank, 
                       lsoa_joined$Fuel_Poverty_Pct, 
                       method = "spearman",
                       exact = FALSE)
print(cor_result)

st_write(lsoa_joined, "lsoa_analysis.gpkg", delete_if_exists = TRUE)

names(lsoa_joined)

st_write(lsoa_joined, "lsoa_analysis.gpkg", delete_if_exists = TRUE)

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

