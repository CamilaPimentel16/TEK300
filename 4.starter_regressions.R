#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building final dataset; running analyses

#1. clear all
  rm(list = ls())
#2. set working directory
  #setwd("~/R/coupcats") # Set working file. 
  #setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
  #setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
  #source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 

#------------------------------------------------------------------------------------------------#
#merge DFs together
#------------------------------------------------------------------------------------------------#

#2.a.domestic political
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.a.base_data.csv.gz"
base_data.2a <- fread(url)
rm(url)
#2.b.domestic economic
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.b.base_data.csv.gz"
base_data.2b <- fread(url)
rm(url)
#2.c.political instability
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.c.base_data.csv.gz"
base_data.2c <- fread(url)
rm(url)
#2.d.military variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.d.base_data.csv.gz"
base_data.2d <- fread(url)
rm(url)
#2.e.international variables
url <- "https://raw.githubusercontent.com/thynec/CoupCats/data/2.e.base_data.csv.gz"
base_data.2e <- fread(url)
rm(url)

base_data <- base_data.2a
  rm(base_data.2a)
base_data.2b <- base_data.2b %>%
  select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2b, by=c("ccode", "year", "month"))
  rm(base_data.2b)
base_data.2c <- base_data.2c %>%
  select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2c, by=c("ccode", "year", "month"))
rm(base_data.2c)    
base_data.2d <- base_data.2d %>%
  select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2d, by=c("ccode", "year", "month"))
rm(base_data.2d)    
base_data.2e <- base_data.2e %>%
  select(-country, -coup_attempt, -coup_successful, -coup_failed, -pce, -pce2, -pce3)
base_data <- base_data %>%
  left_join(base_data.2e, by=c("ccode", "year", "month"))
rm(base_data.2e)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#logit with coup attempt as dv and population total, median age, military expenditure (total and percent of GDP)

coup_logit <- glm(coup_attempt ~ 
                    lgdppcl + ch_gdppcl + cw + pres_elec_lag + polyarchy + polyarchy2 + pce + pce2 + pce3, 
                      data = base_data, family = 'binomial')
summary(coup_logit)


coup_logit <- glm(`coup_attempt` ~ `pop` + `median_age` + `milex` + `milper` + euro_cent_asia + LA_carrib + MENA + S_asia + Sub_africa + pce + pce2 + pce3, data = base_data, family = 'binomial')
summary(coup_logit)

#probability change
predicted_logit <- predict(coup_logit, type = 'response') #predicted probability
mfxL <- margins(coup_logit, type = 'response') #marginal effects
summary(mfxL) # R rounds to 0
print(mfxL, digits = 6) # do this to see actual values

# Calculate DFBETAs
dfbetas_values <- dfbetas(coup_logit)

# Find observations with any DFBETA greater than 2/sqrt(n) in absolute value. Can change to 2 for basic threshold
influential_obs <- apply(abs(dfbetas_values), 1, function(x) any(x > 2/sqrt(length(base_data))))

# Display influential observations
which(influential_obs)


# Logit table using GT, error message on color scale, fix rounding and which "statistic"
# Convert model summary into a tidy dataframe
model_summary <- tidy(coup_logit) 
# Create the table
formatted_table <- gt(model_summary)
# Format numerical columns
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4) #may want more or scientific notation
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Title and Subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Logistic Regression Summary**"),
  subtitle = md("*Effect of Social and Military variables on Coup Attempts")) 
# Footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)



# Marginal effects table using GT
# Convert to a tidy dataframe
mfxL_df <- as.data.frame(summary(mfxL))
# Create the table
formatted_table <- gt(mfxL_df)
# Format numerical columns for readability
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red (p < 0.05)
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Add title and subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Logistic Regression*"))
# Add footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")
# Print the formatted table
print(formatted_table)



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#probit with coup attempt as dv and population total, median age, military expenditure (total and percent of GDP)
coup_probit <- glm(`coup_attempt` ~ `pop` + `median_age` + `milex` + `milper` + euro_cent_asia + LA_carrib + MENA + S_asia + Sub_africa + pce + pce2 + pce3, data = base_data, family = binomial(link = 'probit'))
summary(coup_probit)

#predicted probabilities for probit
predicted_probit <- predict(coup_probit, type = 'response') #predicted probabilites
mfxP <- margins(coup_probit) #marginal efffects
summary(mfxP) # R rounds to 0
print(mfxP, digits = 6) # do this to see actual values

# Calculate DFBETAs
dfbetas_values <- dfbetas(coup_probit)

# Find observations with any DFBETA greater than 2/sqrt(n) in absolute value. Can change to 2 for basic threshold
influential_obs <- apply(abs(dfbetas_values), 1, function(x) any(x > 2/sqrt(length(base_data))))

# Display influential observations
which(influential_obs)



# Probit table using GT, error message about color scale, fix rounding and which "statistic"
# Convert model summary into a tidy dataframe
model_summary <- tidy(coup_probit) 
# Create the table
formatted_table <- gt(model_summary)
# Format numerical columns
formatted_table <- fmt_number(
  formatted_table,
  columns = c("estimate", "std.error", "statistic", "p.value"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red
formatted_table <- data_color(
  formatted_table,
  columns = "p.value",
  colors = col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Title and Subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Probit Regression Summary**"),
  subtitle = md("*Effect of Social and Military Variables on Coup Attemps*")) #should probably change this subtitle
# Footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")

print(formatted_table)


# Marginal effects table using GT
# Convert to a tidy dataframe
mfxP_df <- as.data.frame(summary(mfxL))
# Create the table
formatted_table <- gt(mfxL_df)
# Format numerical columns for readability
formatted_table <- fmt_number(
  formatted_table,
  columns = c("AME", "SE", "z", "p", "lower", "upper"),
  decimals = 4)
# Bold column labels
formatted_table <- tab_style(
  formatted_table,
  style = list(cell_text(weight = "bold")),
  locations = cells_column_labels())
# Highlight significant p-values in red (p < 0.05)
formatted_table <- data_color(
  formatted_table,
  columns = "p",
  colors = scales::col_numeric(
    palette = c("red", "black"),
    domain = c(0, 0.05)))
# Add title and subtitle
formatted_table <- tab_header(
  formatted_table,
  title = md("**Marginal Effects Summary**"),
  subtitle = md("*Average Marginal Effects from Probit Regression*"))
# Add footer note
formatted_table <- tab_source_note(
  formatted_table,
  source_note = "Significant p-values are highlighted in red.")
# Print the formatted table
print(formatted_table)


#-------------------------------------------------------------------------------------------------------------------
#DIAGNOSTICS (both models)
#logit pearson residuals
pearson_residuals_logit <- residuals(coup_logit, type = "pearson")
pearson_residuals_logit

#plotted logit pearson residuals
# Create a data frame for plotting
plot_data <- data.frame(
  observation = 1:length(pearson_residuals_logit),  # X-axis: Observation indices
  pearson_residual = pearson_residuals_logit )       # Y-axis: Pearson residuals

ggplot(plot_data, aes(x = observation, y = pearson_residual)) +
  geom_point() +  # Scatter plot
  labs(x = "Observations", y = "Pearson Residuals", title = "Pearson Residuals Plot") +
  theme_minimal()  # Optional: minimal theme for clean look

# Find the index of the largest absolute Pearson residual
max_residual_index <- which.max(abs(pearson_residuals_logit))

# Print the index of the observation with the largest residual
cat("The observation with the largest residual is at index:", max_residual_index, "\n")


#find Cook's Distance (for influential outliers)
logit_CooksD = cooks.distance(coup_logit)

#Logit plotting Cook's Distance (base, need to add clarity and labels)
#Create a data frame for plotting
plot_data <- data.frame(
  observation = 1:length(logit_CooksD),  # X-axis: Observation indices
  cooks_distance = logit_CooksD          # Y-axis: Cook's distances
)
#Make plot
ggplot(plot_data, aes(x = observation, y = cooks_distance)) +
  geom_point() +  # Scatter plot of Cook's distances
  geom_hline(yintercept = 4 / length(logit_CooksD), linetype = "dashed", color = "red") +  # Threshold line
  labs(x = "Observations", y = "Cook's Distance", title = "Cook's Distance Plot for Logit Model") +
  theme_minimal()  # Optional: minimal theme for clean look



#Probit pearson residuals (preferred)
pearson_residuals_probit <- residuals(coup_probit, type = "pearson")
pearson_residuals_probit

#plotting pearson residuals
plot_data <- data.frame(
  observation = 1:length(pearson_residuals_probit),  # X-axis: Observation indices
  pearson_residual = pearson_residuals_probit )       # Y-axis: Pearson residuals

#Make plot
ggplot(plot_data, aes(x = observation, y = pearson_residual)) +
  geom_point() +  # Scatter plot
  labs(x = "Observations", y = "Pearson Residuals", title = "Pearson Residuals Plot") +
  theme_minimal()  # Optional: minimal theme for clean look 
# Find the index of the largest absolute Pearson residual
max_residual_index <- which.max(abs(pearson_residuals_probit))

# Print the index of the observation with the largest residual
cat("The observation with the largest residual is at index:", max_residual_index, "\n")


#find Cook's Distance (for influential outliers)
probit_CooksD = cooks.distance(coup_probit)

#probit plotting Cook's Distance (base, need to add clarity and labels)
plot_data <- data.frame(
  observation = 1:length(probit_CooksD),  # X-axis: Observation indices
  cooks_distance = probit_CooksD          # Y-axis: Cook's distances
)
#Make plot
ggplot(plot_data, aes(x = observation, y = cooks_distance)) +
  geom_point() +  # Scatter plot of Cook's distances
  geom_hline(yintercept = 4 / length(probit_CooksD), linetype = "dashed", color = "red") +  # Threshold line
  labs(x = "Observations", y = "Cook's Distance", title = "Cook's Distance Plot for Probit Model") +
  theme_minimal()  # Optional: minimal theme for clean look






#multicollinearity (probit and logit) >10 have a problem 
vif(coup_logit)
vif(coup_probit)

#heteroscedasticity test (check in OLS)
#run linear model 
OLS_coup <- lm(coup_attempt ~ `pop` + `median_age` + `milex` + `milper` + euro_cent_asia + LA_carrib + MENA + S_asia + Sub_africa + pce + pce2 + pce3, data = base_data)
summary(OLS_coup)

#Breusch-Pagan test, if significant, heteroscedasticity probable in MLE model, further testing (hetprobit)
bptest(OLS_coup)




#Other diagnositc tests need adding
