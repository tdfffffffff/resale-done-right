# Resale Done Right - Predictive Analytics for Housing Decisions in Singapore  

## Overview  
Resale Done Right is a predictive analytics platform built to address the rising volatility in Singapore's HDB resale market. With prices rising over 10 consecutive quarters, homebuyers face affordability risks, while sellers and agents struggle with pricing uncertainty. Our project uses advanced analytics and machine learning to deliver transparent, data-backed solutions for all stakeholders through interactive dashboards, spatial visualisations, and price prediction models.

## Objectives
- Empower **homebuyers** to find affordable homes aligned with their preferences  
- Assist **home sellers** in setting competitive, data-backed prices  
- Enable **property agents** to offer personalised, predictive advice  

## Datasets  
1. HDB resale transaction data (2017–2023)
   [https://www.kaggle.com/datasets/talietzin/singapore-hdb-resale-prices-1990-to-2023](url)  

2. Consumer Price Index (CPI)
   [https://data.gov.sg/datasets?query=cpi&page=1&resultId=d_dcb352661fb449c4a4c0ab23aa8d6399](url)
    
3. Singapore GeoJSON maps (by town)
[https://www.kaggle.com/datasets/shymammoth/singapore-district-planning-area-geojson](url)

## Key Insights & Features  
- **Price Influencers Identified**: Proximity to the CBD, MRT access, lease duration, and floor area are key drivers of resale price  
- **Prediction Models**: Accurate price forecasts using Linear Regression (R² = 0.867), CART (R² = 0.945), Random Forest (R² = 0.954), and XGBoost (R² = 0.958) 
- **Spatial Demand Clustering**: K-Means, PCA, and Hierarchical Clustering used to understand regional differences in demand 
- **Interactive Dashboards**: Role-based dashboards tailored for each stakeholder group (eg. buyers, sellers, and agents)  
- **Stakeholder Solutions**:
  - Price trend visualisations and comparison tools
  - Role-Based Acess Control (RBAC) for personalised dashboards of stakholders (eg. buyers, sellers and agents)
  - CRM support and predictive modelling education for agents
    
## Visual Highlights  
- Time-series price trends (2017–2023)  
- Heatmaps showing regional price variation  
- Feature importance graphs (eg, CBD proximity, floor area, lease years)  
- Cluster maps highlighting buyer demand zones  

## App Features by Stakeholders 
### For Buyers  
- Property search bar  
- Smart filters by price, location, and amenities  
- Price prediction with historical trend insights  

### For Sellers  
- Comparative pricing tools  
- Demand forecasting by region  
- Listing optimisation recommendations  

### For Agents  
- CRM tools for client tracking  
- Access to predictive model outputs  
- Agent education modules for modelling  

## Technologies  
- **R** (tidyverse, caret, xgboost) for data wrangling and ML  
- **Plotly**, **ggplot2**, **GeoJSON** for spatial and statistical visualisations  
