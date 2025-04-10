#------------------------------------------------------------------------------------------------#
#Front-end stuff
#------------------------------------------------------------------------------------------------#  

#Building military-related variables

#1. clear all
rm(list = ls())
#2. set working directory
#setwd("~/R/coupcats") # Set working file. 
#setwd("C:/Users/clayt/OneDrive - University of Kentucky/elements/current_research/coupcats") #Clay at home
setwd("C:/Users/clthyn2/OneDrive - University of Kentucky/elements/current_research/coupcats") #clay at work
#3. install packages
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/packages.R") 
#4. load libraries
#source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/libraries.R") 
#5. build baseline
source("https://raw.githubusercontent.com/thynec/CoupCats/refs/heads/main/1.building_baseline.R")

#------------------------------------------------------------------------------------------------#
#Milex from SIPRI
#------------------------------------------------------------------------------------------------#  

url <- "https://www.sipri.org/sites/default/files/SIPRI-Milex-data-1948-2023.xlsx"
download.file(url, destfile = "~/SIPRI-Milex-data-1948-2023.xlsx", mode = "wb")
sipri <- read_excel("~/SIPRI-Milex-data-1948-2023.xlsx", sheet = "Constant (2022) US$", skip=5)
rm(url)

#clean; turn
sipri <- sipri %>%
  rename(country=Country) %>%
  select(-...2, -Notes)
sipri <- sipri %>%
  pivot_longer(
    cols=-c(country),
    names_to="year",
    values_to="milex"
  ) %>%
  mutate(year = as.integer(year)) %>% # Convert Year to integer
  mutate(milex=as.integer(milex))

#bring in ccodes; merge to baseline
sipri <- sipri %>%
  rename(sipri_milex=milex) %>%
  mutate(year=year+1) %>%  #just lagged
  left_join(ccodes, by=c("country", "year"))
check <- sipri %>%
  filter(is.na(ccode)) %>%
  select(-year, -sipri_milex) %>%
  distinct()
#View(check) #need Congo, DR=490; Congo, Rep=484; Yemen, North=678
rm(check)
sipri <- sipri %>%
  mutate(ccode=ifelse(country=="Congo, DR", 490, ccode)) %>%
  mutate(ccode=ifelse(country=="Congo, Republic", 484, ccode)) %>%
  mutate(ccode=ifelse(country=="Yemen, North", 678, ccode)) %>%
  rename(sipri_country=country) 
sipri <- sipri %>%
  filter(!is.na(sipri_country)) %>%
  filter(!is.na(ccode)) %>%
  filter(!is.na(sipri_milex)) %>%
  arrange(ccode, year) %>%
  mutate(problem=ifelse(ccode==lag(ccode) & year==lag(year), 1, NA)) %>% #We're good
  select(-problem)
#interpolate to get 2025
sipri <- sipri %>%
  group_by(ccode) %>%
  mutate(expand=ifelse(year==2024, 2, 1)) %>%
  uncount(expand) %>%
  arrange(ccode, year) %>%
  mutate(year=ifelse(year==2024 & lag(year)==2024 & ccode==lag(ccode), 2025, year)) %>%
  mutate(sipri_milex=ifelse(year==2025, NA, sipri_milex)) %>%
  mutate(sipri_milex = if (sum(!is.na(sipri_milex)) >= 2) na.approx(sipri_milex, year, rule = 2) else sipri_milex) %>%
  ungroup()
base_data <- base_data %>%
  left_join(sipri, by=c("ccode", "year"))
check <- base_data %>%
  select(country, sipri_country) %>%
  distinct() %>%
  filter(!is.na(sipri_country)) %>%
  filter(country!=sipri_country)
#View(check) #looks good
rm(check, sipri)
base_data <- base_data %>%
  select(-sipri_country)

#------------------------------------------------------------------------------------------------#
#Milex and milper from COW
#------------------------------------------------------------------------------------------------#  

#Bring in the NMC v6 data; available: https://correlatesofwar.org/data-sets/national-material-capabilities/
url <- "https://correlatesofwar.org/wp-content/uploads/NMC_Documentation-6.0.zip"
download.file(url, "data.zip")
unzip("data.zip", exdir="data")
unlink("data.zip")
unzip("data/NMC-60-wsupplementary.zip", exdir="NMC_data")
nmc <- read_dta("NMC_data/NMC-60-wsupplementary.dta")
unlink("data", recursive=TRUE)
unlink("NMC_data", recursive=TRUE)
rm(url)
nmc <- nmc %>%
  select(statenme, ccode, year, milex, milper) %>%
  rename(cow_milex=milex) %>%
  rename(cow_milper=milper) %>%
  rename(cow_country=statenme) %>%
  mutate(year=year+1) %>% #just lagged
  mutate(cow_milex=ifelse(cow_milex<0, NA, cow_milex)) %>%
  mutate(cow_milper=ifelse(cow_milper<0, NA, cow_milper))
#deal w/ missing
check <- nmc %>%
  filter(year>=1949) %>%
  filter(is.na(cow_milex)) #feels like linear interpolation (but not extrapolation) should be fine for milex
check <- nmc %>%
  filter(year>1949) %>%
  filter(is.na(cow_milper)) #feels like linear interpolation (but not extrapolation) should be fine for milper
rm(check)
nmc <- nmc %>%
  group_by(ccode) %>%
  filter(year>1949) %>%
  mutate(cow_milex = if (sum(!is.na(cow_milex)) >= 2) na.approx(cow_milex, year, rule = 1, na.rm=FALSE) else cow_milex) %>%
  ungroup()
nmc <- nmc %>%
  group_by(ccode) %>%
  filter(year>1949) %>%
  mutate(cow_milper = if (sum(!is.na(cow_milper)) >= 2) na.approx(cow_milper, year, rule = 1, na.rm=FALSE) else cow_milper) %>%
  ungroup()
#merge into base
base_data <- base_data %>%
  left_join(nmc, by=c("ccode", "year"))
check <- base_data %>%
  ungroup() %>%
  select(country, cow_country) %>%
  distinct() %>%
  filter(!is.na(cow_country)) %>%
  filter(country!=cow_country) #looks good
rm(check, nmc)
cor.test(base_data$sipri_milex, base_data$cow_milex) #r=.867 so matching up about what we'd expect

#------------------------------------------------------------------------------------------------#
#Splice SIPRI/COW milex
#------------------------------------------------------------------------------------------------#  

df <- base_data %>%
  select(country, ccode, year, sipri_milex, cow_milex) %>%
  distinct()
df <- df %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(ch=((sipri_milex-lag(sipri_milex))/lag(sipri_milex)))
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milex) %>%
  mutate(ch=ifelse(ccode==101 & year==2018, 1, ch)) %>% #Ven 1018 weird because went from 0 to 1, so ch divided by 0; fixed it with this
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))%>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milex_spliced=splice)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year")) %>%
  select(-sipri_milex, -cow_country, -cow_milex)
rm(df)

#------------------------------------------------------------------------------------------------#
#Milper from WDI
#------------------------------------------------------------------------------------------------#  

wdi <- WDI(country = "all", indicator = "MS.MIL.TOTL.P1", start = 1960, end = 2023)
wdi <- wdi %>%
  select(country, year, MS.MIL.TOTL.P1) %>%
  rename(wdi_milper = MS.MIL.TOTL.P1) %>%
  mutate(year=year+1) %>% #just lagged
  left_join(ccodes, by=c("country", "year"))
check <- wdi %>%
  filter(is.na(ccode)) %>%
  select(country) %>%
  distinct() #need to add Turkey = 640; otherwise looks good
wdi <- wdi %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode)) %>%
  rename(wdi_country=country)
rm(check)
base_data <- base_data %>%
  left_join(wdi, by=c("ccode", "year"))
check <- base_data %>%
  select(country, wdi_country) %>%
  filter(!is.na(wdi_country)) %>%
  distinct() %>%
  filter(country!=wdi_country) #looks good
rm(check, wdi)
base_data <- base_data %>%
  select(-wdi_country)

#bounce it back to country year, deal with missing data in wdi, then splice
df <- base_data %>%
  select(ccode, year, cow_milper, wdi_milper) %>%
  distinct()
df <- df %>%
  group_by(ccode) %>%
  mutate(wdi_milper=if(sum(!is.na(wdi_milper)) >=2) na.approx(wdi_milper, year, rule=1, na.rm=FALSE) else wdi_milper) %>%
  mutate(wdi_milper=if(sum(!is.na(wdi_milper)) >=2) na.approx(wdi_milper, year, rule=2, na.rm=FALSE) else wdi_milper) %>%
  ungroup()
df <- df %>%
  mutate(ch=((wdi_milper-lag(wdi_milper))/lag(wdi_milper))) 
df <- df %>%
  group_by(ccode) %>%
  mutate(splice=cow_milper) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice)) %>%
  mutate(splice=ifelse(is.na(splice), lag(splice)*ch+lag(splice), splice))
df <- df %>%
  select(ccode, year, splice) %>%
  rename(milper_spliced=splice)
base_data <- base_data %>%
  ungroup() %>%
  left_join(df, by=c("ccode", "year")) %>%
  select(-cow_milper, -wdi_milper)
rm(df)
base_data <- base_data %>%
  mutate(milex=log(milex_spliced+1)) %>%
  mutate(milper=log(milper_spliced+1)) %>%
  mutate(solqual=milex/(milper+1)) %>%
  select(-milex_spliced, -milper_spliced)

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.d.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  






#------------------------------------------------------------------------------------------------#
# military integration into politics (Croissan/Eschenauer/Kamerling Euro Pol Science - PRM Dataset)
#------------------------------------------------------------------------------------------------#  

url <- "https://m3-militarization.com/files/M3-Dataset-V1.xlsx"
destfile <- "M3_Dataset_V1.xlsx"
curl::curl_download(url, destfile)
PRM <- read_excel(destfile)
rm(url, destfile)

PRM <- PRM %>% 
  dplyr::select(country_cown, year, mil_origin, mil_leader, mil_mod, mil_veto, mil_impun, mil_repress, mil_repress_count) #keeping only Political Militarization variables
#which of these varaibles do you want us to focus?

PRM <- PRM %>% 
  rename(ccode = country_cown) #making our lives easier

PRM$ccode <- as.numeric(PRM$ccode) #Convert PRM$ccode from Character to Numeric

base_data <- base_data %>% 
  left_join(PRM, by=c("ccode", "year")) #just merge then

###############################################################################################
#Checked through above and ready to produce .csv and upload to github
#clean up if needed and export
write.csv(base_data, gzfile("2.d.base_data.csv.gz"), row.names = FALSE)
#Now push push the file that was just written to the working directory to github
###############################################################################################  


#------------------------------------------------------------------------------------------------#
#below is stuff we probably won't use
#note that we want total milex in constant dollars, which WDI no longer provides; that's why I'm using
#SIPRI plus COW above
#------------------------------------------------------------------------------------------------#    







# Interpolate using dplyr + approx()
df <- df %>%
  mutate(y_interp = approx(x, y, xout = x, method = "linear", rule = 2)$y)












wdi_data <- WDI(indicator = c("MS.MIL.XPND.KD"), country="all", start = 1960, end = 2023)



wdi_data <- WDI(indicator = c("MS.MIL.XPND.KD"), country = "all", start = 1996, end = 2023, extra = TRUE) # Data are reported every two years--need to figure out a way to transform.   









#Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.XPND.CD.?downloadformat=excel"
destfile <- "MS_MIL_XPND_CD.xls"
curl::curl_download(url, destfile)
milex <- read_excel(destfile, skip = 3)
rm(destfile, url)worl

#Reshaping data
milex <- milex %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`))  #removing things we don't want
milex <- milex %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "military expenditure"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer

#merge to base
milex <- milex %>%
  rename(milex='military expenditure') %>%
  set_variable_labels(milex="Milit expenditure, % of GDP") %>%
  mutate(year=year+1) #lagged
milex <- milex %>%
  left_join(ccodes, by=c("country", "year"))
check <- milex %>%
  filter(is.na(ccode)) %>%
  select(-year, -milex) %>%
  distinct()
#View(check) #need to fix turkey=640
milex <- milex %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
rm(check)
milex <- milex %>%
  rename(wdi_country=country)
base_data <- base_data %>%
  left_join(milex, by=c("ccode", "year"))
rm(milex)
check <- base_data %>%
  select(country, wdi_country) %>%
  filter(country!=wdi_country) %>%
  distinct()
View(check) #looks good
rm(check)

#------------------------------------------------------------------------------------------------#
#milper from WB
#------------------------------------------------------------------------------------------------#  

#Loading data
url <- "https://api.worldbank.org/v2/en/indicator/MS.MIL.TOTL.P1?downloadformat=excel"
destfile <- "MS_MIL_TOTL.xls"
curl::curl_download(url, destfile)
milper <- read_excel(destfile, skip = 3)
rm(destfile, url)

#Reshaping data; cleaning a bit
milper <- milper %>%
  rename( "country" = `Country Name`) %>% 
  subset(select = -c(`Indicator Name`, `Indicator Code`, `Country Code`)) #removing things we don't want
milper <- milper %>%
  pivot_longer(
    cols = -c(country),  # Keep country-related columns fixed
    names_to = "year",  
    values_to = "milper"
  ) %>%
  mutate(year = as.integer(year))  # Convert Year to integer

#merge to base
milper <- milper %>%
  rename(milper='military personell') %>%
  set_variable_labels(milper="Milit expenditure, % of GDP") %>%
  mutate(year=year+1) #lagged
milex <- milex %>%
  left_join(ccodes, by=c("country", "year"))
check <- milex %>%
  filter(is.na(ccode)) %>%
  select(-year, -milex) %>%
  distinct()
#View(check) #need to fix turkey=640
milex <- milex %>%
  mutate(ccode=ifelse(country=="Turkiye", 640, ccode))
rm(check)
milex <- milex %>%
  rename(wdi_country=country)
base_data <- base_data %>%
  left_join(milex, by=c("ccode", "year"))
rm(milex)
check <- base_data %>%
  select(country, wdi_country) %>%
  filter(country!=wdi_country) %>%
  distinct()
#View(check) #looks good
rm(check)







#------------------------------------------------------------------------------------------------#  
#milex, milper info from peacesciencer
#------------------------------------------------------------------------------------------------#  


