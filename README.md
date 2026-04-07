# Fuel Poverty Spatial Analysis - England
This repository contains the code and map outputs for my COMP1890 Spatial Data Science coursework at the University of Greenwich.

## Research Question
Is fuel poverty in England spatially clustered, and does deprivation predict where fuel poverty is worst?

## Methods
* Moran's I - global spatial autocorrelation (result: I = 0.636, p < 0.001)
*Getis-Ord Gi* - local hotspot identification (3,106 hot spot LSOAs at 99% confidence)
*Spearman's rank correlation - deprivation vs fuel poverty relationship (rho = -0.638, p < 0.001)

## Data Sources
*Fuel poverty: DESNZ Sub-regional fuel poverty statistics 2025 (2023 data)
*Deprivation: MHCLG English Indices of Deprivation 2025
*Boundaries: ONS LSOA 2021 BGC V5 shapefile

## Tools
*R (sf, spdep, ggplot2, dplyr, viridis)
*QGIS for cartographic outputs

## Files
*analysis.R - full R script for data joining, spatial weights, statistical analysis and chart production
*map1_fuel_poverty.png — choropleth map of fuel poverty rates
*map2_hotspot.png — Getis-Ord Gi* hotspot classification map
*chart1_scatter.png — IMD rank vs fuel poverty scatter plot
*chart2_boxplot.png — fuel poverty distribution by region
*workflow_diagram.png — project workflow diagram

## Author
Zakariya Guechchati —#- University of Greenwich, April 2026
