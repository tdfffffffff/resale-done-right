# Code compilation for BC2406 Project

# Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(gghalves)
library(dplyr)
library(gridExtra)
library(grid)
library(patchwork)
library(car)
library(tidyr)
library(coefplot)
library(magrittr)
library(ggeffects)
library(datawizard)
library(data.table)
library(summarytools)
library(htmltools)
library(tidyverse)
library(scales)
library(ggthemes) 
library(ggsci)
library(ghibli)
library(readtext)
library(ggplot2)
library(dplyr)
library(GGally) 
library(ggfortify) 
library(ggdendro)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(reshape2) 
library(Rtsne)
library(rpart.plot)
library(rpart)
library(car)
library(caret)
library(xgboost)

# Data cleaning

# Table 1 datasets
hdb <- read.csv("/Users/fooyockhaw/Desktop/hdb2017-2023.csv", stringsAsFactors = FALSE)
cpi <- read.csv("/Users/fooyockhaw/Desktop/cpidata.csv", header = FALSE)

# Adjusting price for inflation - creating new variable called inflation_adjusted_resale_price
cpi <- cpi %>%
  mutate(adjustment_factor = V2 / 100)
cpi <- cpi %>%
  rename(year = V1)
cpi$year <- as.factor(cpi$year)
hdb <- hdb %>%
  left_join(cpi %>% select(year, adjustment_factor), by = "year")
hdb <- hdb %>%
  mutate(inflation_adjusted_resale_price = resale_price / adjustment_factor)

# removing 6 rows of NA value for resale price
hdb <- hdb %>%
  filter_all(all_vars(!is.infinite(.) & !is.na(.)))

# converting relevant categorical variables from character to factorial
hdb$town <- as.factor
hdb$closest_mrt <- as.factor(hdb$closest_mrt)
hdb$town <- as.factor(hdb$town)
hdb$flat_type <- as.factor(hdb$flat_type)
hdb$storey_range <- as.factor(hdb$storey_range)
hdb$flat_model <- as.factor(hdb$flat_model)



#3.2.1: Time Series Analysis of Price boxplot (Figure 4)

ggplot(hdb, aes(x = factor(year), y = inflation_adjusted_resale_price)) +
  geom_boxplot(aes(color = factor(year))) +
  custom_colors +
  custom_theme +
  labs(x = "Year", y = "Inflation Adjusted Resale Price", title = "Inflation Adjusted Resale Price by Year")+
  theme_minimal()

#3.2.1: Time Series Analysis of Price lineplot (Figure 5)

monthly_data <- hdb %>%
  mutate(month = as.Date(paste0(month, "-01"))) %>%  # Convert month to date format
  group_by(month) %>%
  summarise(avg_price = mean(inflation_adjusted_resale_price, na.rm = TRUE))
ggplot(monthly_data, aes(x = month, y = avg_price)) +
  geom_line() +
  labs(title = "Time Series of Average Inflation-Adjusted Resale Price",
       x = "Date", y = "Avg Inflation-Adjusted Resale Price") +
  theme_minimal()

#3.2.2: Spatial Price Analysis (Figure 6)

sg <- read_sf("/Users/fooyockhaw/Desktop/sg.geojson")

# data processing for ID of geojson and hdb dataset to match 

hdb <- hdb %>%
  mutate(town = case_when(
    town == "ANG MO KIO" ~ "Ang Mo Kio",
    town == "BEDOK" ~ "Bedok",
    town == "BISHAN" ~ "Bishan",
    town == "BUKIT BATOK" ~ "Bukit Batok",
    town == "BUKIT PANJANG" ~ "Bukit Panjang",
    town == "BUKIT TIMAH" ~ "Bukit Timah",
    town == "CENTRAL AREA" ~ "Central Water Catchment",
    town == "CHOA CHU KANG" ~ "Choa Chu Kang",
    town == "CLEMENTI" ~ "Clementi",
    town == "GEYLANG" ~ "Geylang",
    town == "HOUGANG" ~ "Hougang",
    town == "JURONG EAST" ~ "Jurong East",
    town == "JURONG WEST" ~ "Jurong West",
    town == "KALLANG/WHAMPOA" ~ "Kallang",
    town == "MARINE PARADE" ~ "Marine Parade",
    town == "PASIR RIS" ~ "Pasir Ris",
    town == "PUNGGOL" ~ "Punggol",
    town == "QUEENSTOWN" ~ "Queenstown",
    town == "SEMBAWANG" ~ "Sembawang",
    town == "SENGKANG" ~ "Sengkang",
    town == "SERANGOON" ~ "Serangoon",
    town == "TAMPINES" ~ "Tampines",
    town == "TOA PAYOH" ~ "Toa Payoh",
    town == "WOODLANDS" ~ "Woodlands",
    town == "YISHUN" ~ "Yishun",
    TRUE ~ town  
  ))

# matching ID variable for both datasets
sg$planning_area <- as.factor(sg$planning_area)
hdb_avg_price <- hdb %>%
  group_by(town) %>%
  summarize(avg_resale_price = mean(inflation_adjusted_resale_price, na.rm = TRUE))
sgmerge <- sg %>%
  left_join(hdb_avg_price, by = c("planning_area" = "town")) 

sgmerge_data <- sgmerge %>% filter(!is.na(avg_resale_price))
sgmerge_na <- sgmerge %>% filter(is.na(avg_resale_price))

# Plot in 3.2.2: Spatial Price Analysis

ggplot() +
  geom_sf(data = sgmerge_na, fill = "lightgray", color = "white", lwd = 0.2) +
  geom_sf(data = sgmerge_data, aes(fill = avg_resale_price), color = "white", lwd = 0.2) +
  scale_fill_gradient(
    name = "Average Resale Price", 
    low = "lightblue", high = "darkblue",
    na.value = "gray", 
    limits = c(300000, 800000), 
    breaks = seq(300000, 800000, by = 100000), 
    labels = scales::label_comma(scale = 1) 
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  geom_sf_label(data = sgmerge_data, aes(label = planning_area), 
                size = 2, fill = "white", label.size = 0.1, color = "black")

#3.2.3: Distribution Analysis - Numerical (Figure 7)

custom_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

continuous_vars <- c('inflation_adjusted_resale_price', 'floor_area_sqm', 'cbd_dist', 
                     'closest_mrt_dist', 'years_remaining')

dist_plot_list <- list()

for (col in continuous_vars) {
  p <- ggplot(hdb, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_density(color = "darkblue", size = 1) +
    labs(title = paste("Distribution of", col), x = col, y = "Count") +
    custom_theme
  
  dist_plot_list[[col]] <- p
}

# Arrange and display all distribution plots in a 2x2 + 1 layout
layout_matrix <- rbind(c(1, 2),  # First row for plots 1 and 2
                       c(3, 4),  # Second row for plots 3 and 4
                       c(5, 5))  # Third row for plot 5 spanning two columns

# Create the grid of plots
dist_plots <- grid.arrange(grobs = dist_plot_list, layout_matrix = layout_matrix)

# Save the entire grid of plots
ggsave("hdb_distribution_analysis.png", plot = dist_plots, width = 10, height = 10, dpi = 300)

# 3.2.3: Correlation between numerical variables and resale price (Figure 8)

# convert some factorial variables to numeric for this to work
hdb %>%
  select(closest_mrt, closest_mrt_dist, cbd_dist, floor_area_sqm, years_remaining, flat_type, storey_range, flat_model, inflation_adjusted_resale_price) %>%
  rename(
    MRT = closest_mrt,
    MRT_Dist = closest_mrt_dist,
    CBD_Dist = cbd_dist,
    Area_Sqm = floor_area_sqm,
    Years_Left = years_remaining,
    Resale_Price = inflation_adjusted_resale_price
  ) %>%
  ggcorr(palette = "RdBu", label = TRUE, label_round = 3)

# 3.2.4 Distribution Analysis: Categorical (Figure 9)
categorical_columns <- c('town', 'flat_type', 'flat_model', 'storey_range')

plot_list <- list()

for (col in categorical_columns) {
  sorted_counts <- hdb %>%
    group_by(!!sym(col)) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
p <- ggplot(sorted_counts, aes(x = reorder(!!sym(col), -count), y = count)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = count), hjust = -0.1, size = 3, angle = 0) +
    labs(title = paste("Count of", col), x = col, y = "Count") +
    coord_flip() +  # Flip coordinates for better readability
    custom_theme  # Applying the custom theme
  
  # Add the plot to the list
  plot_list[[col]] <- p
}

# Arrange all plots into one plot object
combined_plot <- arrangeGrob(grobs = plot_list, ncol = 1)

# Save the combined plot to a file with specified width and height
ggsave("categorical_plots.png", plot = combined_plot, width = 10, height = 15, dpi = 300)

# 3.3.1 Examining Factors Affecting Price: Plot on Impact of Floor Area on Resale Price (Figure 10)

#----------------------------------------------------------------------------------------------------
# Analyzing Floor Area Impact on Demand
# we can see that as the floor area increases, the price of the hdb increases 
# and the price range for each type of floor area is rather clear
# this shows that floor area is a significant factor for demand and price
ggplot(hdb, aes(x = floor_area_sqm, y = inflation_adjusted_resale_price, color = flat_type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  custom_colors +
  custom_theme +
  labs(title = "Impact of Floor Area on Resale Price", x = "Floor Area (sqm)", y = "Inflation-Adjusted Resale Price") 

#3.3.2 Town Area and Flat Type, Plots for both 3.3.1 and 3.3.2 (Figure 11, 12)

mean_prices_town <- hdb %>%
  group_by(town) %>%
  summarise(mean_price = mean(inflation_adjusted_resale_price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

sorted_towns <- mean_prices_town$town

mean_prices_flat_type <- hdb %>%
  group_by(flat_type) %>%
  summarise(mean_price = mean(inflation_adjusted_resale_price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

sorted_flat_types <- mean_prices_flat_type$flat_type

mean_prices_flat_model <- hdb %>%
  group_by(flat_model) %>%
  summarise(mean_price = mean(inflation_adjusted_resale_price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

sorted_flat_models <- mean_prices_flat_model$flat_model

mean_prices_storey_range <- hdb %>%
  group_by(storey_range) %>%
  summarise(mean_price = mean(inflation_adjusted_resale_price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

sorted_storey_ranges <- mean_prices_storey_range$storey_range

# Define categorical columns
categorical_columns <- c('town', 'flat_type', 'flat_model', 'storey_range')

# Create a list to store plots
plot_list <- list()

# Generate boxplots for each categorical variable
for (col in categorical_columns) {
  p <- ggplot(hdb, aes_string(x = paste("reorder(", col, ", inflation_adjusted_resale_price, FUN = mean)"), 
                             y = "inflation_adjusted_resale_price")) +
    geom_boxplot(fill = "lightblue") +
    labs(title = paste("Relationship with", col), 
         x = col, 
         y = "Inflation Adjusted Resale Price") +
    custom_theme + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))  # Adjust x-axis text
  
  # Store the plot
  plot_list[[col]] <- p
}

# Arrange and display plots in two separate files
# First file with the first two plots
grid_plot1 <- grid.arrange(plot_list[[1]], plot_list[[2]], ncol = 2)
ggsave("hdb_inflation_adjusted_resale_price_relationships_1.png", 
       plot = grid_plot1, width = 20, height = 10, dpi = 300)  # Increase width

# Second file with the last two plots
grid_plot2 <- grid.arrange(plot_list[[3]], plot_list[[4]], ncol = 2)
ggsave("hdb_inflation_adjusted_resale_price_relationships_2.png", 
       plot = grid_plot2, width = 20, height = 10, dpi = 300)


# 4.1.1 Plots for K-Means Clustering (Figure 13)

k_clusters <- 8
kmeans_result <- kmeans(hdb[, c("latitude", "longitude")], centers = k_clusters)
wcss <- kmeans_result$tot.withinss
print(wcss)

# 4.1.1 Plot for HDB Clusters by Average Inflation-Adjusted Resale Price (Figure 14)

wcss_values <- sapply(1:10, function(k) kmeans(hdb[, c("latitude", "longitude")], centers = k, nstart = 20)$tot.withinss)
plot(1:10, wcss_values, type = "b", xlab = "Number of Clusters", ylab = "WCSS")
k_clusters <- 8
hdb$cluster <- kmeans(hdb[, c("latitude", "longitude")], centers = k_clusters)$cluster

cluster_avg_price <- hdb %>%
  group_by(cluster) %>%
  summarise(avg_price = mean(inflation_adjusted_resale_price, na.rm = TRUE),
            centroid_lat = mean(latitude),
            centroid_lon = mean(longitude))
hdb <- hdb %>%
  left_join(cluster_avg_price, by = "cluster")

hull_points <- hdb %>%
  group_by(cluster) %>%
  slice(chull(longitude, latitude))

# Figure 14

ggplot() +
  geom_sf(data = sg, fill = "gray90", color = "white") +
  geom_point(data = hdb, aes(x = longitude, y = latitude, color = avg_price), size = 0.5, alpha = 0.5) +
  geom_polygon(data = hull_points, aes(x = longitude, y = latitude, group = cluster),
               color = "black", fill = NA, linetype = "dashed") +
  geom_text(data = cluster_avg_price, aes(x = centroid_lon, y = centroid_lat, label = round(avg_price, 0)),
            color = "black", size = 5, fontface = "bold") +
  scale_color_gradient(low = "#2166ac", high = "#F3722C", name = "Avg Price") +
  labs(x = "Longitude", y = "Latitude", title = "HDB Clusters by Average Inflation-Adjusted Resale Price") +
  theme_minimal()

#4.1.2 PCA Plot (Figure 15)

selected_towns <- c("Ang Mo Kio", "Bukit Timah", "Bishan", "Jurong West", "Sengkang", "Sembawang", "Tampines", "Yishun")

filtered_hdb <- hdb %>%
  filter(town %in% selected_towns)

selected_columns <- c("closest_mrt_dist", "cbd_dist", "floor_area_sqm", "years_remaining", "inflation_adjusted_resale_price") 

pca <- filtered_hdb %>%
  select(all_of(selected_columns)) %>% # Use all_of() to select specific columns
  filter_all(all_vars(!is.infinite(.) & !is.na(.)))

res.pca <- PCA(pca, scale.unit = TRUE, ncp = 5, graph = FALSE)

pca_plot <- fviz_pca_biplot(res.pca,
                            col.ind = filtered_hdb$town,  # Use only the selected towns
                            col.var = "black",
                            addEllipses = TRUE,  # Add 95% ellipses
                            repel = TRUE,
                            mean.point = FALSE,
                            geom.ind = "none",  # Remove individual points
                            legend.title = "Town") +
  labs(x = "PC1",
       y = "PC2") +
  scale_shape_manual(name = "Towns",
                     values = seq_along(unique(filtered_hdb$town))) +  # Adjust shape values based on unique towns
  theme(legend.position = "bottom") 

# PCA stuff in Appendix 4.2

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) # scree plot

fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10) # individual variable contribution to PC1 and PC2

corrplot(var$contrib, is.corr=FALSE)  # individual variable contribution to all PCs


# 4.1.2 Hierarchical Clustering: Dendrogram plot (Figure 15)

town_data <- hdb %>%
  group_by(town) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  column_to_rownames("town") 

town_dist <- dist(town_data, method = "euclidean") 

town_hclust <- hclust(town_dist, method = "average") 

ggdendrogram(town_hclust, rotate = TRUE, theme_dendro = FALSE) +
  labs(title = "Dendrogram of HDB Towns", x = "Town", y = "Distance") +
  theme_minimal()


# 4.2.1 Linear regression

hdb <- read.csv("/Users/fooyockhaw/Desktop/hdb2017-2023.csv", stringsAsFactors = FALSE)
hdb <- hdb %>%
  filter_all(all_vars(!is.infinite(.) & !is.na(.)))

# Data cleaning in case something is changed unexpectedly
hdb$closest_mrt <- as.factor(hdb$closest_mrt)
hdb$town <- as.factor(hdb$town)
hdb$flat_type <- as.factor(hdb$flat_type)
hdb$storey_range <- as.factor(hdb$storey_range)
hdb$flat_model <- as.factor(hdb$flat_model)

# Diagnostics do not look good, log transform response variable
hi <- lm(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = hdb)

par(mfrow = c(2,2))
plot(hi)

# Diagnostics looks better now, but still sub-optimal(Appendix 4.1)
hilog <- lm(log(inflation_adjusted_resale_price) ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = hdb)
par(mfrow = c(2,2))
plot(hilog)
par(mfrow = c(1,1))

#AIC for model selection, both null and full models retained all possible variables
basemodel <- lm(log(inflation_adjusted_resale_price) ~ 1, data = hdb)
stepfromnull <- step(basemodel, scope = ~.+closest_mrt+closest_mrt_dist+cbd_dist+town+flat_type+storey_range+floor_area_sqm+ flat_model+years_remaining)
stepfromfull <- step(hilog, scope = ~.)

# Splitting dataset into train and test set
ind <- runif(150437) <= 0.7

traindata1 <- hdb %>% filter(ind)
testdata1 <- hdb %>% filter(!ind)

cat("Dimensions of training set is ", dim(traindata1), "\n")
cat("Dimensions of test set is ", dim(testdata1), "\n")


hilog <- lm(log(inflation_adjusted_resale_price) ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1)
rmse <- function(x,y) {
  sqrt(mean((x-y)^2))
}


predict.hilog.test <- predict(hilog, newdata = testdata1)
testset.error <- log(testdata1$inflation_adjusted_resale_price) - predict.hilog.test
RMSE.hilog.test <- sqrt(mean(testset.error^2))

predict.hilog.train <- predict(hilog, newdata = traindata1)
trainset.error <- log(traindata1$inflation_adjusted_resale_price) - predict.hilog.train
RMSE.hilog.train <- sqrt(mean(trainset.error^2))

# 4.2.1: Plot of RMSE of train and test set errors
rmse_table <- data.frame(
  Dataset = c("Train", "Test"),
  RMSE = c(RMSE.hilog.train, RMSE.hilog.test)
)

print(rmse_table)

# 4.2.1: Evaluation of linear regression model (Figure 18)
stargazer(hilog, type = "text")

# 4.2.1: Summary of linear regression model (Figure 16)
anova(hilog)

# 4.2.1: Significance of predictors of resale price (Figure 17)
library(broom)
coef_data <- tidy(hilog)
coef_data_clean <- na.omit(coef_data)
shorten_labels <- function(label) {
  switch(label,
         "LongVariableName1" = "Var1",
         "LongVariableName2" = "Var2",
         "LongVariableName3" = "Var3",
         label)  
}
coef_data_clean$short_term <- sapply(coef_data_clean$term, shorten_labels)

coef_data_filtered <- coef_data_clean %>%
  filter(short_term != "(Intercept)")  # Remove the intercept

top_15 <- coef_data_filtered %>%
  arrange(desc(estimate)) %>% 
  head(15)  

bottom_15 <- coef_data_filtered %>%
  arrange(estimate) %>% 
  head(15)  

combined_coef_data <- bind_rows(top_15, bottom_15)

# Figure 17

ggplot(combined_coef_data, aes(x = reorder(short_term, estimate), y = estimate)) +
  geom_point(color = "blue") +  # Coefficient estimate points
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                width = 0.2, color = "red") +  # Confidence interval error bars
  coord_flip() +  # Flip the axes for readability
  labs(title = "Coefficient Plot with Confidence Intervals (Logged)", 
       x = "Predictor Variables", 
       y = "Coefficient Estimate (Logged)") +
  theme_minimal()


# 4.2.2: CART and Random Forest code

train_control <- trainControl(method = "cv", number = 5) 
# 5-fold cv = too computationally expensive
fit.tree <- train(
  inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, 
  data = traindata1, 
  method = "rpart", 
  trControl = train_control,
  tuneGrid = expand.grid(cp = seq(0.00001, 0.01, by = 0.00001))  # cp = 0.00002906 is the best
)

# ended up not running cv

fit.tree = rpart(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1, method = "anova", control = rpart.control(minsplit = 2, cp = 0.000001))
printcp(fit.tree)
bestcp <- fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
bestcp

# pruning tree on best cp of non cv model, pruned tree is very complex at 5598 terminal nodes
pruned.tree <- prune(fit.tree, cp = bestcp)
rpart.plot(pruned.tree)
pruned.tree$variable.importance
pred.prune = predict(pruned.tree, testdata1)

# 4.2.2 Printed simpler tree for visualisation purposes (Figure 19)
fit.tree = rpart(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1, method = "anova", cp = .01)
prp(fit.tree,
    main = "Inflation-Adjusted Resale Price Tree",
    type = 2,                     
    extra = 101,                  
    digits = 2,                   
    varlen = 0,                   
    box.palette = "Blues",        # Use a blue gradient
    shadow.col = "gray",        
    tweak = 1,                  
    fallen.leaves = TRUE,        
    split.cex = 1              
)

#no of terminal nodes (Table 3)
num_terminal_nodes <- length(unique(pruned.tree$where))
print(num_terminal_nodes)

# 4.2.2 Results for table 4: Summary of evaluation metrics for accuracy of CART model/Random forest code in next section (Table 3)

# Predictions on the test set (Table 3)
predictions <- predict(pruned.tree, newdata = testdata1)
# Calculate pseudo R²
rss <- sum((testdata1$inflation_adjusted_resale_price - predictions)^2)
tss <- sum((testdata1$inflation_adjusted_resale_price - mean(testdata1$inflation_adjusted_resale_price))^2)
pseudo_r2 <- 1 - rss/tss
pseudo_r2
# Calculate RMSE testset (Table 3)
rmse <- sqrt(mean((testdata1$inflation_adjusted_resale_price - predictions)^2))
rmse
rmse <- sqrt(mean((testdata1$inflation_adjusted_resale_price - predictions)^2))
# calculate RMSE trainset (Table 3)
predictions <- predict(pruned.tree, newdata = traindata1)
rmse <- sqrt(mean((traindata1$inflation_adjusted_resale_price - predictions)^2))
rmse

# 4.2.2 Results of table 4: Random forest code 

mod_rf <- train(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1, method = "ranger",
                num.trees = 50,
                importance = 'impurity',
                trControl = trainControl("oob"))
mod_rf %>%
  predict(testdata1) %>%
  RMSE(testdata1$inflation_adjusted_resale_price)

# code to tune hyperparameters of random forest
rfGrid<-expand.grid(mtry =c(10,20,30,40,50,60),
                    min.node.size =c(5,10,20,40),
                    splitrule ="variance")
mod_rf_tune <- train(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1, method = "ranger",
                     num.trees = 100,
                     importance = 'impurity',
                     tuneGrid = rfGrid,
                     trControl = trainControl("oob"))
mod_rf_tune

mod_rf_tuned <- train(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining, data = traindata1, method = "ranger",
                      num.trees = 100,
                      importance = 'impurity',
                      tuneGrid = expand.grid(mod_rf_tune$bestTune),
                      trControl = trainControl("oob"))

mod_rf_tuned
mod_rf_tuned %>%
  predict(testdata1) %>%
  RMSE(testdata1$inflation_adjusted_resale_price)

# Choosing only the top 50 variables for final Random Forest Tree

top_variables <- mod_rf_tuned$finalModel$variable.importance%>%
  sort(decreasing= TRUE) %>% head(50) %>% names

dummy_data <- model.matrix(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining - 1, data = traindata1)

valid_top_variables <- intersect(top_variables, colnames(dummy_data))

t_data <- as.data.frame(dummy_data[, valid_top_variables])
t_data$inflation_adjusted_resale_price <- traindata1$inflation_adjusted_resale_price

dummy_datatest <- model.matrix(inflation_adjusted_resale_price ~ closest_mrt + closest_mrt_dist + cbd_dist + town + flat_type + storey_range + floor_area_sqm + flat_model + years_remaining - 1, data = testdata1)

v_data <- as.data.frame(dummy_datatest[, valid_top_variables])
v_data$inflation_adjusted_resale_price <- testdata1$inflation_adjusted_resale_price

rfGrid_2 <- expand.grid(mtry = c(3,5,8, 15),
                        min.node.size = c(2,4,8,16,32),
                        splitrule = "variance")

mod_rf_topvar <- train(inflation_adjusted_resale_price ~ ., data = t_data, method = "ranger",
                       num.trees = 50,
                       importance = 'impurity',
                       tuneGrid = expand.grid(rfGrid_2),
                       trControl = trainControl("oob"))

mod_rf_topvar$bestTune

# Final Random forest

mod_rf_final <- train(inflation_adjusted_resale_price~., data = t_data, method ="ranger",
                      num.trees = 500,
                      importance = 'impurity',
                      tuneGrid = expand.grid(mod_rf_topvar$bestTune),
                      trControl = trainControl("oob"))

mod_rf_final %>%
  predict(v_data) %>%
  RMSE(v_data$inflation_adjusted_resale_price)

# 4.2.2: Summary of evaluation metrics for accuracy of Random Forest model (Table 3)
predictions <- predict(mod_rf_final, newdata = v_data)

rss <- sum((testdata1$inflation_adjusted_resale_price - predictions)^2)
tss <- sum((testdata1$inflation_adjusted_resale_price - mean(testdata1$inflation_adjusted_resale_price))^2)
pseudo_r2 <- 1 - rss/tss
pseudo_r2

#4.2.2: Top 20 important variables in predicting resale price (Figure 20)
top_variables <- mod_rf_final$finalModel$variable.importance %>%
  sort(decreasing = TRUE) %>%
  head(20) %>%
  enframe(name = "Variable", value = "Importance")

# Scale importance to 100
top_variables <- top_variables %>%
  mutate(Scaled_Importance = (Importance / max(Importance)) * 100)

ggplot(top_variables, aes(x = reorder(Variable, Scaled_Importance), y = Scaled_Importance, fill = Scaled_Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Gradient from light to dark blue
  labs(title = "Top 20 Variable Importance (Scaled to 100)",
       x = "Variable",
       y = "Importance (Scaled to 100)") +
  theme_minimal() +
  theme(legend.position = "none")

# 4.2.3 XGBoost Code
hdb <- read.csv("/Users/fooyockhaw/Desktop/hdb2017-2023.csv", stringsAsFactors = FALSE)
setwd("/Users/syed/Downloads/School-Related/Y2S2/BC2406/BC2406 Course Materials/1 Group Proj")
hdb <- read.csv("hdb2017-2023.csv")

# Data Preprocessing
# Convert categorical columns to numeric format using factors
hdb$closest_mrt <- as.numeric(as.factor(hdb$closest_mrt))
hdb$town <- as.numeric(as.factor(hdb$town))
hdb$flat_type <- as.numeric(as.factor(hdb$flat_type))
hdb$storey_range <- as.numeric(as.factor(hdb$storey_range))
hdb$flat_model <- as.numeric(as.factor(hdb$flat_model))
hdb$street_name <- as.numeric(as.factor(hdb$street_name))

# Convert remaining_lease to numeric years
hdb$remaining_lease <- as.numeric(gsub(" years .*", "", hdb$remaining_lease))
hdb <- na.omit(hdb)

features <- c("closest_mrt", "closest_mrt_dist", "cbd_dist",  
              "town", "flat_type", "street_name", "storey_range", "floor_area_sqm", "flat_model", 
              "remaining_lease", "year", "years_remaining")

# Set up data for training (target = resale_price)
set.seed(123)
train_index <- createDataPartition(df$resale_price, p = 0.8, list = FALSE)
train_data <- hdb[train_index, ]
test_data <- hdb[-train_index, ]
train_matrix <- as.matrix(train_data[, features])
test_matrix <- as.matrix(test_data[, features])
train_labels <- as.numeric(train_data$resale_price)
test_labels <- as.numeric(test_data$resale_price)
str(train_matrix)
str(test_matrix)

### Train XGBoost Model ###
# Set parameters for XGBoost
xgb_params <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.1, subsample = 0.8, colsample_bytree = 0.8)

# Train the model
xgb_model <- xgboost(data = train_matrix, label = train_labels, params = xgb_params, nrounds = 100, verbose = 0)

### Predict and Evaluate ###
# Predict on test set
predictions <- predict(xgb_model, test_matrix)

# Calculate RMSE (Table 4)
rmse <- sqrt(mean((predictions - test_labels)^2))
cat("RMSE for Price Prediction with All Features:", rmse, "\n")

# Calculate R^2 (Table 4)
r2 <- 1 - (sum((predictions - test_labels)^2) / sum((mean(train_labels) - test_labels)^2))
cat("R^2 for Price Prediction with All Features:", r2, "\n")

### Price Segmentation by Location ###

# Select relevant features for location analysis
location_features <- c("latitude", "longitude", "closest_mrt_dist", "cbd_dist", "floor_area_sqm", "flat_type", "storey_range", "flat_model", "remaining_lease")

# Set up data for training (target = resale_price)
set.seed(123)
location_train_index <- createDataPartition(df$resale_price, p = 0.8, list = FALSE)
location_train_data <- df[location_train_index, ]
location_test_data <- df[-location_train_index, ]

# Convert to matrix format
location_train_matrix <- as.matrix(location_train_data[, location_features])
location_test_matrix <- as.matrix(location_test_data[, location_features])

# Convert labels to numeric
location_train_labels <- as.numeric(location_train_data$resale_price)
location_test_labels <- as.numeric(location_test_data$resale_price)

# Train XGBoost model for Location Segmentation
xgb_location_model <- xgboost(data = location_train_matrix, label = location_train_labels, params = xgb_params, nrounds = 100, verbose = 0)

# Predict and Evaluate
location_predictions <- predict(xgb_location_model, location_test_matrix)

# Calculate RMSE for Location Segmentation (Table 4)
location_rmse <- sqrt(mean((location_predictions - location_test_labels)^2))
cat("RMSE for Price Segmentation by Location:", location_rmse, "\n")

### Feature Importance Plot for Location Segmentation ###
#importance_location <- xgb.importance(model = xgb_location_model, feature_names = location_features)
#xgb.plot.importance(importance_location, main = "Feature Importance for Price Segmentation")
# Calculate R^2 for Location Segmentation
location_r2 <- 1 - (sum((location_predictions - location_test_labels)^2) / sum((mean(location_train_labels) - location_test_labels)^2))
cat("R^2 for Price Segmentation by Location:", location_r2, "\n")

# Appendix code

# Appendix 3: Data exploration findings

# data cleaned with code from 3.2.2
custom_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )
custom_colors <- scale_color_manual(values = c("#b2182b", "#F3722C", "#f4a582", "#92c5de", "#4393c3", "#2166ac", "#48388d"))

# Figure 25 in Data Dictionary
ggplot() +
  geom_sf(data = sg, fill = "gray90", color = "white") +
  geom_point(data = hdb, aes(x = longitude, y = latitude, color = factor(year)), size = 0.2, alpha = 0.1) +
  custom_colors +
  labs(color = "Year", x = "Longitude", y = "Latitude", title = "HDB Locations in Singapore by Year") +
  facet_wrap(~year, nrow = 2, ncol = 4) +
  theme_minimal()+
  theme(axis.text.x = element_blank())

# Appendix 3: tibble information on mean inflation adjusted resale price by flat_model, flat_type and town

summary_stats <- summary(hdb)

# Distribution Analysis
dist_vars <- c('inflation_adjusted_resale_price', 'floor_area_sqm', 'cbd_dist', 'closest_mrt_dist', 'years_remaining', 'flat_type')

plot_list <- lapply(dist_vars, function(col) {
  if (is.numeric(hdb[[col]])) {
    ggplot(hdb, aes_string(x = col)) +
      geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black", bins = 30) +
      geom_density(color = "red") +
      custom_theme +
      ggtitle(paste("Distribution of", col))
  } else {
    ggplot(hdb, aes_string(x = col)) +
      geom_bar(fill = "lightblue", color = "black") +
      custom_theme +
      ggtitle(paste("Distribution of", col))
  }
})

do.call(grid.arrange, c(plot_list, ncol = 1))
