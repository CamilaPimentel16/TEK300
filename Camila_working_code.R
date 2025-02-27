"Note for readers: Before running the code, please download the working directory 
as established in the 'R' file under the 'General' files tab. This will include a 
three files called 'packages', 'libraries', and 'base_data'." 

setwd("C:/Users/Camila/OneDrive/R related/R/R") # Set working file. #Camila

#CT: I'm going to paste the packages/libraries below. That way you can add in new packages/libraries to a central
#place for others to use. I'll put a '#' in the packages so those don't load and waste time (remember you only need)
#to do those once

#load packages: if needed, go in and remove # but don't forget to put it back#
#####
# Data Management
#install.packages("tidyverse") # Primary package for data management
#install.packages("readr") # Reads flat files (i.e., CSV)
#install.packages("openxlsx") # Reads (and writes) Excel files
#install.packages("haven") # Imports & exports SPSS, SAS, and Stata files
#install.packages("janitor") # Cleans column names and tidies data
#install.packages("Hmisc") # For labeling data. 

# Data Visualization
#install.packages("ggplot2") # Primary package for data visualization
#install.packages("gganimate") # Creates animated plots (with ggplot2)
#install.packages("ggfortify") # Visualizes model outputs (PCA, clustering)
#install.packages("ggmosaic") # Creates mosaic plots
#install.packages("ggpubr") # Enhances ggplot2 with greater details
#install.packages("ggstance") # Horizontal geoms for ggplot2
#install.packages("plotly") # Create interactive plots

# Statistical Analysis and Modeling
#install.packages("broom") # Tidy model outputs into data frames
#install.packages("car") # Regression diagnostics and statistical methods
#install.packages("DescTools") # Descriptive stats and hypothesis testing
#install.packages("emmeans") # Estimated marginal means and model comparisons
#install.packages("olsrr") # Linear regression diagnostics and model selection
#install.packages("lme4") # Linear and generalized linear mixed-effects models
#install.packages("MASS") # Various statistical models and methods

# Spatial Analysis
#install.packages("sf") # Handle spatial data and analysis
#install.packages("spData") # Spatial datasets for use with sf
#install.packages("terra") # Work with raster data and spatial analysis

# Report Generation
#install.packages("rmarkdown") # Dynamic report generation
#install.packages("knitr") # Process and display R code/output in reports

# Model Diagnostics and Evaluation
#install.packages("performance") # Evaluate model performance and diagnostics
#install.packages("DHARMa") # Residual diagnostics for GLMs

# Data Summarization and Reporting
#install.packages("gtsummary") # Summarizes regression models
#install.packages("summarytools") # Quick summaries of your data
#install.packages("stargazer") # Creates regression tables

# Data Export and Import
#install.packages("remotes") # #install R packages from GitHub or other sources

# Miscellaneous Utilities
#install.packages("gapminder") # Global development indicators and example data
#install.packages("peacesciencer")  # Political science/peace studies tools
#install.packages("rcompanion") # Companion functions for statistical tasks
#install.packages("rgl") # 3D visualization

# Data Collection and Statistics
#install.packages("tigerstats") # Common statistical analysis methods
#install.packages("vcd") # Visualize categorical data using association plots
#####
#load libraries
#####
# Data Management
library(tidyverse) # Primary package for data management
library(readr) # Reads flat files (i.e., CSV)
library(openxlsx) # Reads (and writes) Excel files
library(haven) # Imports & exports SPSS, SAS, and Stata files
library(janitor) # Cleans column names and tidies data
library(Hmisc) # For labeling data. 

# Data Visualization
library(ggplot2) # Primary package for data visualization
library(gganimate) # Creates animated plots (with ggplot2)
library(ggfortify) # Visualizes model outputs (PCA, clustering)
library(ggmosaic) # Creates mosaic plots
library(ggpubr) # Enhances ggplot2 with greater details
library(ggstance) # Horizontal geoms for ggplot2
library(plotly) # Create interactive plots

# Statistical Analysis and Modeling
library(broom) # Tidy model outputs into data frames
library(car) # Regression diagnostics and statistical methods
library(DescTools) # Descriptive stats and hypothesis testing
library(emmeans) # Estimated marginal means and model comparisons
library(olsrr) # Linear regression diagnostics and model selection
library(lme4) # Linear and generalized linear mixed-effects models
library(MASS) # Various statistical models and methods

# Spatial Analysis
library(sf) # Handle spatial data and analysis
library(spData) # Spatial datasets for use with sf
library(terra) # Work with raster data and spatial analysis

# Report Generation
library(rmarkdown) # Dynamic report generation
library(knitr) # Process and display R code/output in reports

# Model Diagnostics and Evaluation
library(performance) # Evaluate model performance and diagnostics
library(DHARMa) # Residual diagnostics for GLMs

# Data Summarization and Reporting
library(gtsummary) # Summarizes regression models
library(summarytools) # Quick summaries of your data
library(stargazer) # Creates regression tables

# Data Export and Import
library(remotes) # Install R packages from GitHub or other sources

# Miscellaneous Utilities
library(gapminder) # Global development indicators and example data
library(peacesciencer)  # Political science/peace studies tools
library(rcompanion) # Companion functions for statistical tasks
library(rgl) # 3D visualization

# Data Collection and Statistics
library(tigerstats) # Common statistical analysis methods
library(vcd) # Visualize categorical data using association plots
#####

rm(list=ls())
base_data <- read_csv("https://www.uky.edu/~clthyn2/base_data.csv") # Reading in base data. 

# -------------------------------------------------------- #

#CT: When you're working on separate parts, everyone should start their script with lines 1-134 above; that way
#everyone is working with the same baseline DF so it should be easy to just copy/paste in later.


# ------------------------------------ Political data  ------------------------------------ #

# 1. Coup data (Powell & Thyne 2011) 
# 1.1. Reading in coup data. 
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) # Reading in coup data. 

# 1.2. Cleaning up data. 
coup_data <- coup_data %>%
  subset(select = -c(ccode_gw, ccode_polity, day, version))

# 1.3. Merging into data set. 
camila_data <- base_data %>% 
  left_join(coup_data, by = c("year", "country", "ccode", "month")) %>%
  mutate(coup = ifelse(is.na(coup), 0, as.numeric(coup)))
label(camila_data$coup) <- "2 = successful, 1 = failed"
rm(base_data, coup_data) # Keeping things clean! 

# 2. Regime Type with V-Dem Data

# 2.1. Downloading data 

url <- "https://github.com/vdeminstitute/vdemdata/raw/master/data/vdem.RData"
destfile <- tempfile(fileext = ".RData")  # Save to a temporary file
download.file(url, destfile, mode = "wb")  # Download without manually saving
load(destfile) # Load the dataset into R

unlink(destfile)
rm(url, destfile)


# 2.2. Cleaning up data. 
regime_type <- vdem %>%
  dplyr::select(COWcode, year, v2x_regime) %>% 
  filter(year >= 1950)


# 2.3. Merging
camila_data <- camila_data %>% 
  left_join(regime_type, by = c("year", "ccode" = "COWcode")) 
  label(camila_data$v2x_regime) <- "0 = Closed autocracy, 1 = Electoral autocracy, 2 = Electoral democracy, 3 = Liberal Democracy"

  
# 3. Military Expenditure and Personnel
  
# 3.1. Reading data

# 3.1.1. Military Expenditure
  url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.XPND.CD?downloadformat=csv"
  download.file(url, "data.zip", mode = "wb") #Downloads the dataset and saves it as data.zip in working directory.
  unzip ("data.zip", exdir = "military_data")
  military <-  read.csv("military_data/API_MS.MIL.XPND.CD_DS2_en_csv_v2_76234.csv", skip = 3)
  military <- military %>% mutate(across(where(is.numeric), as.numeric))
  options(scipen = 999)  # Prevent scientific notation
  colnames(military) <- gsub("^X", "", colnames(military))  #remove X in front of years columns
  rm(url)
  unlink("data.zip") #Deletes the downloaded ZIP file.
  unlink("military", recursive = TRUE) # Deletes the extracted folder to clean up space. With recursive = TRUE → It forces deletion of the folder along with everything inside it (files, subfolders, etc.).
  
# 3.1.2. Personnel
  url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.TOTL.P1?downloadformat=csv"
  download.file(url, "data.zip2", mode = "wb") #Downloads the dataset and saves it as data.zip in working directory.
  unzip ("data.zip2", exdir = "personnel_data")
  personnel <-  read.csv("personnel_data/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_6757.csv", skip = 3)
  colnames(personnel) <- gsub("^X", "", colnames(personnel))  #remove X in front of years columns
  
# 3.2 Fixing the problem of not having ccode on military and personnel: here I am adding iso3c code (three letters code) into camila_data to match with ccode
  install.packages("countrycode")
  library(countrycode)
  
  camila_data$iso3 <- countrycode(camila_data$ccode, "cown", "iso3c") # converting Correlates of War (COW) numeric country codes into ISO 3-letter country codes using the countrycode package in R.
  
  camila_data <- camila_data %>%
    mutate(iso3 = case_when(  # I had to match manually the unmached values
      ccode == 260 ~ "DEU",
      ccode == 265 ~ "DDR",
      ccode == 315 ~ "CZE",  
      ccode == 345 ~ "YUG",
      ccode == 347 ~ "XKX",
      ccode == 511 ~ "TZA",
      ccode == 678 ~ "YEM",
      ccode == 680 ~ "YEM",
      ccode == 817 ~ "VNM",
      TRUE ~ iso3  # Keep existing values for other rows
    ))
  
# 3.3 Cleaning and Reshaping Data
  
# 3.3.1. Military
  #do the excel way, it is going to be easier)
  
  military <- military %>%
    subset(select = -c(Indicator.Name, Indicator.Code, Country.Name)) 
  
  military <- military[, colnames(military) != ""]  # Remove empty column
  
  military <- military %>%
    pivot_longer(
      cols = -c(Country.Code),  # Keep country-related columns fixed
      names_to = "Year",  
      values_to = "Military_Expenditure"
    ) %>%
    mutate(Year = as.integer(Year))  # Convert Year to integer
  
# 3.3.2. Personnel
  
  personnel <- personnel %>%
    subset(select = -c(Indicator.Name, Indicator.Code, Country.Name))
  
  personnel <- personnel[, colnames(personnel) != ""]   # Remove empty column
  
  personnel <- personnel %>%
    pivot_longer(
      cols = -c(Country.Code),  
      names_to = "Year",  
      values_to = "Military_Personnel"
    ) %>%
    mutate(Year = as.integer(Year))  # Convert Year to integer
 

# 3.4. Merging both military and personnel into camila_data

  camila_data <- camila_data %>% 
    left_join(military, by = c("iso3" = "Country.Code", "year" = "Year")) %>%
    left_join(personnel, by = c("iso3" = "Country.Code", "year" = "Year"))
  label(camila_data$Military_Expenditure) <- "(in USD)"
 
  
# 4. Time since last coup

# 4.1. Organizing the data
coup_data <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE) # Reading in coup data. 

camila_data <- camila_data %>% 
  select(country, ccode, year, month, coup)

# 4.2. Cleaning up data. 
coup_data <- coup_data %>%
  subset(select = -c(ccode_gw, ccode_polity, country, version))

# 4.3 Making a date
coup_data_date <- coup_data %>%
  mutate(date = make_date(year, month, day)) %>% 
  filter(coup==2) #removing unsuccessful coups


# 4.4 Merging camila_data with coup_data_date

camila_data <- camila_data %>%
  left_join(select(coup_data_date, ccode, year, month, date), by = c("ccode", "year", "month")) %>%  # Merge without 'day'
  mutate(coup_date = if_else(coup == 2, date, as.Date(NA_character_)))  # Assign date only for successful coups

# 4.4 Get number of months since past coup
#This is the part that I am having a hard time






