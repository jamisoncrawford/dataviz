
# DATA VISUALIZATION WORKSHOP: APPLIED PRACTICE DATASET CLEANING 

    #' Date: 2020-01-29
    #' R Version: 3.6.1
    #' RStudio Mode: "Desktop"
    #' RStudio Version: 1.2.1578
    #' OS: Windows 10

    #' DESCRIPTION: The following loads both built-in package datasets and 
    #' external datasets in order to clean them for applied practice in the 
    #' "Picture This: Applied Practice in Data Visualization".

    #' NOTE: Data manipulation is intended to render data compatible with Google 
    #' Data Studio and/or Microsoft Excel visualizations.




# LOAD REQUIRED PACKAGES

rm(list = ls())         # Clear environment objects

setwd("~/GitHub/dataviz/Applied Practice Content")

setwd(path)             # Set working directory

library(lubridate)      # For datetime manipulation
library(forecast)
library(datasets)
library(stringr)        # For text manipulation
library(ggplot2)
library(dplyr)          # For general manipulation
library(ggmap)
library(tidyr)          # For "tidy" data format
library(readr)



# LIST BUILT-IN R DATASETS & CORRESPONDING PACKAGES

forecast::woolyrnq
forecast::wineind
forecast::gold

datasets::chickwts
datasets::mtcars
datasets::iris

ggplot2::txhousing
ggplot2::economics
ggplot2::diamonds
ggplot2::msleep
ggplot2::mpg

ggmap::crime
ggmap::wind



# PROCESS DATASETS: FORECAST (TIMESERIES DATA)

    # forecast::woolyrnq

yarn <- data.frame(Date=time(woolyrnq), 
           Tons = as.matrix(woolyrnq)) %>%
  mutate(Quarter = as.numeric(substr(Date, start = 6, stop = 7)),
         Quarter = ifelse(is.na(Quarter), 1, Quarter),
         Quarter = ifelse(Quarter == 25, 2, Quarter),
         Quarter = ifelse(Quarter == 5, 3, Quarter),
         Quarter = ifelse(Quarter == 75, 4, Quarter),
         Year = as.numeric(substr(Date, start = 1, stop = 4))) %>%
  select(Year, Quarter, Tons)

    # forecast::wineind

wine <- data.frame(Date=time(wineind), 
                   Bottles = as.matrix(wineind)) %>%
  mutate(Month = as.character(substr(Date, start = 6, stop = 8)),
         Month = ifelse(Month == "" | Month == "000", "January", Month),
         Month = ifelse(Month == "083", "February", Month),
         Month = ifelse(Month == "166" | Month == "167", "March", Month),
         Month = ifelse(Month == "25", "April", Month),
         Month = ifelse(Month == "333", "May", Month),
         Month = ifelse(Month == "416" | Month == "417", "June", Month),
         Month = ifelse(Month == "5", "July", Month),
         Month = ifelse(Month == "583", "August", Month),
         Month = ifelse(Month == "666" | Month == "667", "September", Month),
         Month = ifelse(Month == "75", "October", Month),
         Month = ifelse(Month == "833", "November", Month),
         Month = ifelse(Month == "916" | Month == "917", "December", Month),
         Year = as.numeric(substr(Date, start = 1, stop = 4))) %>%
  mutate("Year & Month" = paste(Year, Month, sep = ", "),
         Date = ymd(paste(Year, Month, "1", sep = " ")),
         "Year & Month (ISO)" = as.character(substr(Date, 1, 7))) %>%
  select(Year, Month, Date, `Year & Month`, `Year & Month (ISO)`, Bottles)

    # forecast::gold

data(gold)

gold <- data.frame(Date=time(gold), 
                   USD = as.matrix(gold))

gold$Year <- NA

gold$Year[1:365] <- 1985
gold$Year[366:730] <- 1986
gold$Year[731:1095] <- 1987
gold$Year[1095:1108] <- 1988

gold$Date <- NA

gold$Date[1:365] <- as.character(ymd(as.Date(0:364, origin = "1985-01-01")))
gold$Date[366:730] <- as.character(ymd(as.Date(0:364, origin = "1986-01-01")))
gold$Date[731:1095] <- as.character(ymd(as.Date(0:364, origin = "1987-01-01")))
gold$Date[1095:1108] <- as.character(ymd(as.Date(0:13, origin = "1988-01-01")))


gold <- gold %>%
  mutate(Month = as.character(substr(Date, start = 6, stop = 7)),
         Month = ifelse(Month == "01", "January", Month),
         Month = ifelse(Month == "02", "February", Month),
         Month = ifelse(Month == "03", "March", Month),
         Month = ifelse(Month == "04", "April", Month),
         Month = ifelse(Month == "05", "May", Month),
         Month = ifelse(Month == "06", "June", Month),
         Month = ifelse(Month == "07", "July", Month),
         Month = ifelse(Month == "08", "August", Month),
         Month = ifelse(Month == "09", "September", Month),
         Month = ifelse(Month == "10", "October", Month),
         Month = ifelse(Month == "11", "November", Month),
         Month = ifelse(Month == "12", "December", Month)) %>%
  select(Date, Year, Month, USD)



# PROCESS DATASETS: DATASETS

    # datasets::chickwts

chicks <- chickwts %>%
  rename(Grams = weight,
         `Feed Group` = feed) %>%
  mutate(Chick = 1:nrow(chicks),
         `Feed Group` = as.character(`Feed Group`),
         `Feed Group` = str_to_title(`Feed Group`)) %>%
  select(Chick, `Feed Group`, Grams)
  
    # datasets::mtcars

cars <- mtcars %>%
  mutate(model = rownames(mtcars),
         wt = wt * 1000,
         vs = ifelse(vs == 0, "V-Shaped", vs),
         vs = ifelse(vs == 1, "Straight", vs),
         am = ifelse(am == 0, "Automatic", am),
         am = ifelse(am == 1, "Manual", am)) %>%
  rename(MPG = mpg,
         Cylinders = cyl,
         Displacement = disp,
         Horsepower = hp,
         "Rear Axle Ratio" = drat,
         Pounds = wt,
         "Quarter Mile Seconds" = qsec,
         Engine = vs,
         Transmission = am,
         Gears = gear,
         Carburetors = carb,
         Model = model) %>%
  select(Model, MPG:Carburetors)

    # datasets::iris

flowers <- iris %>%
  mutate(Species = as.character(str_to_title(Species)),
         Iris = rep(1:50, times = 3)) %>%
  rename("Sepal Length" = Sepal.Length,
         "Sepal Width" = Sepal.Width,
         "Petal Length" = Petal.Length,
         "Petal Width" = Petal.Width) %>%
  select(Iris, Species, `Sepal Length`:`Petal Width`)



# PROCESS DATASETS: GGPLOT2

    # ggplot2::txhousing

market <- txhousing %>%
  mutate(State = "Texas") %>%
  select(-date) %>%
  rename(City = city,
         Year = year,
         Month = month,
         `Total Sales` = sales,
         `Total Value` = volume,
         `Median Price` = median,
         `Total Listings` = listings,
         `Months to Sell All` = inventory) %>%
  mutate(Date = ymd(paste(Year, Month, "01", sep = "-")),
         `City & State` = paste(City, "TX", sep = ", ")) %>%
  mutate(Month = ifelse(Month == 1, "January", Month),
         Month = ifelse(Month == 2, "February", Month),
         Month = ifelse(Month == 3, "March", Month),
         Month = ifelse(Month == 4, "April", Month),
         Month = ifelse(Month == 5, "May", Month),
         Month = ifelse(Month == 6, "June", Month),
         Month = ifelse(Month == 7, "July", Month),
         Month = ifelse(Month == 8, "August", Month),
         Month = ifelse(Month == 9, "September", Month),
         Month = ifelse(Month == 10, "October", Month),
         Month = ifelse(Month == 11, "November", Month),
         Month = ifelse(Month == 12, "December", Month)) %>%
  select(Year, City, State, `City & State`, Month, Date, `Total Sales`:`Months to Sell All`) %>%
  arrange(Year, City, Date)

    # ggplot2::economics

spending <- economics %>%
  mutate(Year = as.character(substr(date, 1, 4)),
         Month = as.character(substr(date, 6, 7)),
         Month = ifelse(Month == "01", "January", Month),
         Month = ifelse(Month == "02", "February", Month),
         Month = ifelse(Month == "03", "March", Month),
         Month = ifelse(Month == "04", "April", Month),
         Month = ifelse(Month == "05", "May", Month),
         Month = ifelse(Month == "06", "June", Month),
         Month = ifelse(Month == "07", "July", Month),
         Month = ifelse(Month == "08", "August", Month),
         Month = ifelse(Month == "09", "September", Month),
         Month = ifelse(Month == "10", "October", Month),
         Month = ifelse(Month == "11", "November", Month),
         Month = ifelse(Month == "12", "December", Month),
         pce = pce * 1000000000,
         unemploy = unemploy * 1000,
         pop = pop * 1000) %>%
  rename(Date = date,
         `Personal Consumption Expenditures` = pce,
         Unemployed = unemploy,
         `Median Unemployment Time (Weeks)` = uempmed,
         Population = pop,
         `Personal Savings (%)` = psavert) %>%
  select(Date, Year, Month, Population, `Personal Consumption Expenditures`:Unemployed)

    # ggplot2::diamonds

rocks <- diamonds %>%
  rename(Carats = carat,
         Cut = cut,
         Color = color,
         Clarity = clarity,
         `Depth (%)` = depth,
         `Price (USD)` = price,
         Length = x,
         Width = y,
         Depth = z) %>%
  select(Cut, Carats:Clarity, `Price (USD)`, Length:Depth, `Depth (%)`) %>%
  arrange(desc(Cut), desc(Carats))

    # ggplot2::msleep

sleep <- msleep %>%
  rename(Mammal = name,
         Genus = genus,
         Vore = vore,
         Order = order,
         `Conservation Status` = conservation,
         `Total Sleep (Hrs)` = sleep_total,
         `REM Sleep (Hrs)` = sleep_rem,
         `Sleep Cycle (Hrs)` = sleep_cycle,
         `Awake (Hrs)` = awake,
         `Brain Weight (Kg)` = brainwt,
         `Body Weight (Kg)` = bodywt) %>%
  mutate(Vore = paste0(Vore, "vore"),
         Vore = str_to_title(Vore),
         `Conservation Status` = toupper(`Conservation Status`),
         `Conservation Status` = ifelse(`Conservation Status` == "DOMESTICATED", "Domesticated", `Conservation Status`)) %>%
  arrange(Vore, Mammal)


    # ggplot2::mpg

mpg <- mpg %>%
  mutate(trans = str_replace_all(trans, "\\(.*\\)$", ""),
         trans = ifelse(trans == "Auto", "Automatic", "Manual"),
         drv = ifelse(drv == "f", "Front-Wheel", drv),
         drv = ifelse(drv == "r", "Rear-Wheel", drv),
         drv = ifelse(drv == "4", "Four-Wheel", drv),
         fl = toupper(fl),
         class = str_to_title(class),
         class = ifelse(class == "Suv", "SUV", class),
         class = ifelse(class == "2seater", "Coupe", class),
         manufacturer = str_to_title(manufacturer),
         model = str_to_title(model),
         model = str_replace_all(model, "^Gti$", "GTI"),
         model = str_replace_all(model, "wd$", "WD"),
         model = str_replace_all(model, "4runner", "4Runner")) %>%
  rename(Make = manufacturer,
         Model = model, 
         Displacement = displ,
         Year = year,
         Cylinders = cyl,
         Transmission = trans,
         Drive = drv,
         `City MPG` = cty,
         `Highway MPG` = hwy,
         `Fuel Type` = fl,
         Class = class)



# PROCESS DATA: GGMAP (LOCATION INTELLIGENCE)

    # ggmap::crime
    # ggmap::wind

?crime

str(crime)

houston <- crime %>%
  mutate(date = mdy(date),
         offense = str_to_title(offense),
         street = str_to_title(street),
         type = str_to_title(type),
         month = str_to_title(month),
         day = str_to_title(day),
         location = str_to_title(location),
         location = str_replace_all(location, " \\/ ", "/"),
         address = str_to_title(address),
         address = str_trim(address, side = "both"),
         lonlat = paste(lon, lat, sep = ", "),
         latlon = paste(lat, lon, sep = ", ")) %>%
  rename(Timestamp = time,
         Date = date,
         Hour = hour,
         Premise = premise,
         Offense = offense,
         Beat = beat, 
         Block = block,
         Street = street,
         Type = type,
         Suffix = suffix, 
         Number = number,
         Month = month,
         Day = day,
         Location = location,
         Address = address,
         Longitude = lon,
         Latitude = lat,
         `Longitude-Latitude` = lonlat,
         `Latitude-Longitude` = latlon)



# PROCESSING: EXTERNAL OPEN DATA

    # College Scorecards; 7,112 colleges & 1,978 variables

    # U.S. Department of Education, College Scorecards

    # https://collegescorecard.ed.gov/data/

    # https://collegescorecard.ed.gov/data/documentation/

url <- "https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv"

college <- read_csv(url)

names(college) <- tolower(names(college))

college <- college %>% 
  select(unitid, opeid6, instnm:zip, insturl, latitude:longitude, adm_rate, satvrmid:satwrmid, 
         actcmmid, sat_avg, ugds, tuitfte, avgfacsal, pctpell) %>%
  rename(ID = unitid,
         OPEID = opeid6,
         Institution = instnm,
         City = city,
         State = stabbr,
         ZIP = zip,
         Website = insturl,
         Latitude = latitude,
         Longitude = longitude,
         `Admit Rate` = adm_rate,
         `Median SAT - Crit Thinking` = satvrmid,
         `Median SAT - Math` = satmtmid,
         `Median SAT - Writing` = satwrmid,
         `Median ACT` = actcmmid,
         `Average SAT` = sat_avg,
         `Total Undegrads` = ugds,
         `Tuituion Revenue, FT Students` = tuitfte,
         `Average Monthly Faculty Salary (USD)` = avgfacsal,
         `Pell Eligible (%)` = pctpell) %>%
  mutate(`Latitude-Longitude` = paste(Latitude, Longitude, sep = ", "),
         `Longitude-Latitude` = paste(Longitude, Latitude, sep = ", ")) %>%
  select(ID:ZIP, Longitude, Latitude, `Longitude-Latitude`, `Latitude-Longitude`, Website,
          `Admit Rate`:`Pell Eligible (%)`)

college[college == "NULL"] <- NA

rm(url)



# WRITE TO CSV

write_csv(cars, paste0("data/cars.csv"))
write_csv(college, paste0("data/college.csv"))
write_csv(flowers, paste0("data/flowers.csv"))
write_csv(gold, paste0("data/gold.csv"))
write_csv(houston, paste0("data/houston.csv"))
write_csv(market, paste0("data/market.csv"))
write_csv(mpg, paste0("data/mpg.csv"))
write_csv(rocks, paste0("data/rocks.csv"))
write_csv(sleep, paste0("data/sleep.csv"))
write_csv(spending, paste0("data/spending.csv"))
write_csv(wine, paste0("data/wine.csv"))
write_csv(yarn, paste0("data/yarn.csv"))

rm(list = ls())
