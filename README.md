# Runway Diversity Study — Big Four Fashion Weeks, 2015–2023

A statistical analysis of POC model representation across New York, London, Paris, and Milan fashion weeks using nine seasons of runway data.

## Research Question
Does POC model representation differ significantly across the Big Four fashion capitals, or are the perceived differences just noise?

## Method
One-way ANOVA with Bonferroni-corrected post-hoc testing, built entirely in R.

## Key Finding
Significant city effect confirmed (p = 0.009). The largest gap traced to New York vs. Milan at 13.2 percentage points.

## Files
- `fashion_diversity_analysis.R` — full analysis and visualizations
- `fashion_diversity_ANOVA.pdf` — write-up and figures
- `fashion_diversity.csv` — season-level POC representation data by city

## Data Source
The Fashion Spot Annual Runway Diversity Reports, 2015–2023

## Tools
R 4.5.1 · ggplot2 · One-Way ANOVA · Bonferroni Correction
