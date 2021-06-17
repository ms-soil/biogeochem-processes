# In this file, I will statistically analyze paramters in 
# agroforestry and forest in Göttingen, May 2021

# install.packages("readxl")
library(readxl) # reading in Excel files
library(tidyverse) # for slicing, piping

# run one line: ctr-enter
# run whole script: ctrl-shift-enter

# read in data
data <- read_xlsx("data/data-summary-2021.xlsx")

# find out more about data structure
nrow(data)
dim(data)
names(data)

# Let's slice the units away
# pipe or %>% is ctrl - shift - M

data <- data %>% slice(2:21) # taking only lines 2 to 21
head(data)

# Let's remane our variables

names(data)

data <- data %>% rename(
  landuse = `land use`,
  bd = `bulk density`,
  soc = SOC,
  totn = `total N`,
  ecec = `Effective cation exchange capacity`,
  co2flux = `CO2 fluxes`,
  n2oflux = `N2O fluxes`,
  ch4flux = `CH4 fluxes`,
  soiltemp = `soil temp`,
  wfps = WFPS
  )

head(data)

str(data$landuse)

data$landuse <- as.factor(data$landuse)

data$bd <- as.numeric(data$bd)
# or:
data[[3]] <- as.numeric(data[[3]])

# function for making several variables numeric
for (i in list(3,4,5,6,7,8,9,10,11)){
  data[[i]] <- as.numeric(data[[i]])
}
data$n2oflux

str(data)

qplot(landuse, co2flux, data = data)

# View(data)
# find code at: https://s.gwdg.de/N1jdCn










