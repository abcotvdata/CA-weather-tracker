# libraries 

library(tidyverse)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(raster)
library(readxl)
library(sf)
library(zoo)
library(ggplot2)

precip_files1 <- c("1970-01","1970-02","1970-03","1970-04","1970-05","1970-06","1970-07","1970-08","1970-09","1970-10","1970-11","1970-12","1971-01","1971-02","1971-03","1971-04","1971-05","1971-06","1971-07","1971-08","1971-09","1971-10","1971-11","1971-12","1972-01","1972-02","1972-03","1972-04","1972-05","1972-06","1972-07","1972-08","1972-09","1972-10","1972-11","1972-12","1973-01","1973-02","1973-03","1973-04","1973-05","1973-06","1973-07","1973-08","1973-09","1973-10","1973-11","1973-12","1974-01","1974-02","1974-03","1974-04","1974-05","1974-06","1974-07","1974-08","1974-09","1974-10","1974-11","1974-12","1975-01","1975-02","1975-03","1975-04","1975-05","1975-06","1975-07","1975-08","1975-09","1975-10","1975-11","1975-12","1976-01","1976-02","1976-03","1976-04","1976-05","1976-06","1976-07","1976-08","1976-09","1976-10","1976-11","1976-12","1977-01","1977-02","1977-03","1977-04","1977-05","1977-06","1977-07","1977-08","1977-09","1977-10","1977-11","1977-12","1978-01","1978-02","1978-03","1978-04","1978-05","1978-06","1978-07","1978-08","1978-09","1978-10","1978-11","1978-12","1979-01","1979-02","1979-03","1979-04","1979-05","1979-06","1979-07","1979-08","1979-09","1979-10","1979-11","1979-12","1980-01","1980-02","1980-03","1980-04","1980-05","1980-06","1980-07","1980-08","1980-09","1980-10","1980-11","1980-12","1981-01","1981-02","1981-03","1981-04","1981-05","1981-06","1981-07","1981-08","1981-09","1981-10","1981-11","1981-12","1982-01","1982-02","1982-03","1982-04","1982-05","1982-06","1982-07","1982-08","1982-09","1982-10","1982-11","1982-12","1983-01","1983-02","1983-03","1983-04","1983-05","1983-06","1983-07","1983-08","1983-09","1983-10","1983-11","1983-12","1984-01","1984-02","1984-03","1984-04","1984-05","1984-06","1984-07","1984-08","1984-09","1984-10","1984-11","1984-12","1985-01","1985-02","1985-03","1985-04","1985-05","1985-06","1985-07","1985-08","1985-09","1985-10","1985-11","1985-12","1986-01","1986-02","1986-03","1986-04","1986-05","1986-06","1986-07","1986-08","1986-09","1986-10","1986-11","1986-12","1987-01","1987-02","1987-03","1987-04","1987-05","1987-06","1987-07","1987-08","1987-09","1987-10","1987-11","1987-12","1988-01","1988-02","1988-03","1988-04","1988-05","1988-06","1988-07","1988-08","1988-09","1988-10","1988-11","1988-12","1989-01","1989-02","1989-03","1989-04","1989-05","1989-06","1989-07","1989-08","1989-09","1989-10","1989-11","1989-12","1990-01","1990-02","1990-03","1990-04","1990-05","1990-06","1990-07","1990-08","1990-09","1990-10","1990-11","1990-12","1991-01","1991-02","1991-03","1991-04","1991-05","1991-06","1991-07","1991-08","1991-09","1991-10","1991-11","1991-12","1992-01","1992-02","1992-03","1992-04","1992-05","1992-06","1992-07","1992-08","1992-09","1992-10","1992-11","1992-12","1993-01","1993-02","1993-03","1993-04","1993-05","1993-06","1993-07","1993-08","1993-09","1993-10","1993-11","1993-12","1994-01","1994-02","1994-03","1994-04","1994-05","1994-06","1994-07","1994-08","1994-09","1994-10","1994-11","1994-12","1995-01","1995-02","1995-03","1995-04","1995-05","1995-06","1995-07","1995-08","1995-09","1995-10","1995-11","1995-12","1996-01","1996-02","1996-03","1996-04","1996-05","1996-06","1996-07","1996-08","1996-09","1996-10","1996-11","1996-12","1997-01","1997-02","1997-03","1997-04","1997-05","1997-06","1997-07","1997-08","1997-09","1997-10","1997-11","1997-12","1998-01","1998-02","1998-03","1998-04","1998-05","1998-06","1998-07","1998-08","1998-09","1998-10","1998-11","1998-12","1999-01","1999-02","1999-03","1999-04","1999-05","1999-06","1999-07","1999-08","1999-09","1999-10","1999-11","1999-12")
precip_all_years1 <- data.frame()
for (file in precip_files1) {
  filename <- paste(sep = "", "CSVs/precip_sum_", file, ".csv")
  print(filename)
  
  precip_current <- read.csv(filename, skip=1) %>% 
    mutate(Month = file)
  
  precip_all_years1 <- rbind(precip_all_years1, precip_current)
}


precip_files2 <- c("2000-01","2000-02","2000-03","2000-04","2000-05","2000-06","2000-07","2000-08","2000-09","2000-10","2000-11","2000-12","2001-01","2001-02","2001-03","2001-04","2001-05","2001-06","2001-07","2001-08","2001-09","2001-10","2001-11","2001-12","2002-01","2002-02","2002-03","2002-04","2002-05","2002-06","2002-07","2002-08","2002-09","2002-10","2002-11","2002-12","2003-01","2003-02","2003-03","2003-04","2003-05","2003-06","2003-07","2003-08","2003-09","2003-10","2003-11","2003-12","2004-01","2004-02","2004-03","2004-04","2004-05","2004-06","2004-07","2004-08","2004-09","2004-10","2004-11","2004-12","2005-01","2005-02","2005-03","2005-04","2005-05","2005-06","2005-07","2005-08","2005-09","2005-10","2005-11","2005-12","2006-01","2006-02","2006-03","2006-04","2006-05","2006-06","2006-07","2006-08","2006-09","2006-10","2006-11","2006-12","2007-01","2007-02","2007-03","2007-04","2007-05","2007-06","2007-07","2007-08","2007-09","2007-10","2007-11","2007-12","2008-01","2008-02","2008-03","2008-04","2008-05","2008-06","2008-07","2008-08","2008-09","2008-10","2008-11","2008-12","2009-01","2009-02","2009-03","2009-04","2009-05","2009-06","2009-07","2009-08","2009-09","2009-10","2009-11","2009-12","2010-01","2010-02","2010-03","2010-04","2010-05","2010-06","2010-07","2010-08","2010-09","2010-10","2010-11","2010-12","2011-01","2011-02","2011-03","2011-04","2011-05","2011-06","2011-07","2011-08","2011-09","2011-10","2011-11","2011-12","2012-01","2012-02","2012-03","2012-04","2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12","2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12","2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09","2014-10","2014-11","2014-12","2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11","2015-12","2016-01","2016-02","2016-03","2016-04","2016-05","2016-06","2016-07","2016-08","2016-09","2016-10","2016-11","2016-12","2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12","2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12","2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12","2020-01","2020-02","2020-03","2020-04","2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08","2021-09","2021-10","2021-11","2021-12","2022-01","2022-02","2022-03","2022-04","2022-05","2022-06","2022-07","2022-08","2022-09","2022-10","2022-11","2022-12","2023-01","2023-02","2023-03", "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09", "2023-10", "2023-11", "2023-12", "2024-01", "2024-02", "2024-03", "2024-04", "2024-05", "2024-06", "2024-07", "2024-08", "2024-09", "2024-10", "2024-11", "2024-12", "2025-01", "2025-02", "2025-03", "2025-04", "2025-05")
precip_all_years2 <- data.frame()
for (file in precip_files2) {
  filename <- paste(sep = "", "CSVs/precip_sum_", file, ".csv")
  print(filename)
  
  precip_current <- read.csv(filename, skip=1) %>% 
    mutate(Month = file)
  
  precip_all_years2 <- rbind(precip_all_years2, precip_current)
}


precip_all_years <- rbind(precip_all_years1, precip_all_years2) %>% 
  mutate(TotalPrecipitation = str_replace(TotalPrecipitation, " ", "")) %>% 
  mutate(TotalPrecipitation_New = case_when(TotalPrecipitation == "T" ~ "0",
                                            TRUE ~ TotalPrecipitation)) %>% 
  mutate(MonthOnly = str_sub(Month, start = -2)) %>% 
  mutate(YearOnly = str_sub(Month, 1,4)) %>% 
  mutate(WaterYear = case_when((MonthOnly == 10 | MonthOnly == 11 | MonthOnly == 12) ~ as.character((as.numeric(YearOnly) + 1)),
                               TRUE ~ YearOnly)) %>% 
  mutate(WaterYear = as.numeric(WaterYear)) %>% 
  mutate(YearOnly = as.numeric(YearOnly)) %>% 
  mutate(MonthOnly = as.numeric(MonthOnly)) %>% 
  mutate(TotalPrecipitation_New = as.numeric(TotalPrecipitation_New))

write.csv(precip_all_years, "CSVs/precip_all_years.csv", row.names = FALSE)

precip_threadex_five_year <- precip_all_years %>%
  filter(YearOnly >= 2018) %>%
  filter(StationType == " ThreadEx") %>%
  dplyr::select(Name, Month, TotalPrecipitation_New)
  
precip_threadex_five_year1 <- precip_threadex_five_year %>%
spread(Name, TotalPrecipitation_New)

write.csv(precip_threadex_five_year1, "CSVs/precip_threadex_stations.csv", row.names = FALSE)


# Alturas Area

precip_threadex_five_year_alturas <- precip_threadex_five_year %>% 
  filter(Name == "Alturas Area")

write.csv(precip_threadex_five_year_alturas, "CSVs/alturas.csv", row.names = FALSE)

# Bakersfield Area

precip_threadex_five_year_bakersfield <- precip_threadex_five_year %>% 
  filter(Name == "Bakersfield Area")

write.csv(precip_threadex_five_year_bakersfield, "CSVs/bakersfield.csv", row.names = FALSE)

# Bishop Area	

precip_threadex_five_year_bishop <- precip_threadex_five_year %>% 
  filter(Name == "Bishop Area")

write.csv(precip_threadex_five_year_bishop, "CSVs/bishop.csv", row.names = FALSE)

# Burbank Glendale Pasadena Area	

precip_threadex_five_year_burbank <- precip_threadex_five_year %>% 
  filter(Name == "Burbank Glendale Pasadena Area")

write.csv(precip_threadex_five_year_burbank, "CSVs/burbank.csv", row.names = FALSE)

# Camarillo Area	

precip_threadex_five_year_camarillo <- precip_threadex_five_year %>% 
  filter(Name == "Camarillo Area")

write.csv(precip_threadex_five_year_camarillo, "CSVs/camarillo.csv", row.names = FALSE)

# Crescent City Area	

precip_threadex_five_year_crescent <- precip_threadex_five_year %>% 
  filter(Name == "Crescent City Area")

write.csv(precip_threadex_five_year_crescent, "CSVs/crescent.csv", row.names = FALSE)

# El Cajon Area	

precip_threadex_five_year_cajon <- precip_threadex_five_year %>% 
  filter(Name == "El Cajon Area")

write.csv(precip_threadex_five_year_cajon, "CSVs/cajon.csv", row.names = FALSE)

# El Centro Area	

precip_threadex_five_year_centro <- precip_threadex_five_year %>% 
  filter(Name == "El Centro Area")

write.csv(precip_threadex_five_year_centro, "CSVs/centro.csv", row.names = FALSE)

# Escondido Area	

precip_threadex_five_year_escondido <- precip_threadex_five_year %>% 
  filter(Name == "Escondido Area")

write.csv(precip_threadex_five_year_escondido, "CSVs/escondido.csv", row.names = FALSE)

# Eureka Area	

precip_threadex_five_year_eureka <- precip_threadex_five_year %>% 
  filter(Name == "Eureka Area")

write.csv(precip_threadex_five_year_eureka, "CSVs/eureka.csv", row.names = FALSE)

# Fresno Area	

precip_threadex_five_year_fresno <- precip_threadex_five_year %>% 
  filter(Name == "Fresno Area")

write.csv(precip_threadex_five_year_fresno, "CSVs/fresno.csv", row.names = FALSE)

# Hanford Area	

precip_threadex_five_year_hanford <- precip_threadex_five_year %>% 
  filter(Name == "Hanford Area")

write.csv(precip_threadex_five_year_hanford, "CSVs/hanford.csv", row.names = FALSE)

# Lancaster Area	

precip_threadex_five_year_lancaster <- precip_threadex_five_year %>% 
  filter(Name == "Lancaster Area")

write.csv(precip_threadex_five_year_lancaster, "CSVs/lancaster.csv", row.names = FALSE)

# Long Beach Area	

precip_threadex_five_year_long_beach <- precip_threadex_five_year %>% 
  filter(Name == "Long Beach Area")

write.csv(precip_threadex_five_year_long_beach, "CSVs/long_beach.csv", row.names = FALSE)

# Los Angeles Airport Area	

precip_threadex_five_year_la_airport <- precip_threadex_five_year %>% 
  filter(Name == "Los Angeles Airport Area")

write.csv(precip_threadex_five_year_la_airport, "CSVs/la_airport.csv", row.names = FALSE)

# Los Angeles Downtown Area	

precip_threadex_five_year_la_downtown <- precip_threadex_five_year %>% 
  filter(Name == "Los Angeles Downtown Area")

write.csv(precip_threadex_five_year_la_downtown, "CSVs/la_downtown.csv", row.names = FALSE)

# Madera Area	

precip_threadex_five_year_madera <- precip_threadex_five_year %>% 
  filter(Name == "Madera Area")

write.csv(precip_threadex_five_year_madera, "CSVs/madera.csv", row.names = FALSE)

# Merced Area	

precip_threadex_five_year_merced <- precip_threadex_five_year %>% 
  filter(Name == "Merced Area")

write.csv(precip_threadex_five_year_merced, "CSVs/merced.csv", row.names = FALSE)

# Mount Shasta Area	

precip_threadex_five_year_shasta <- precip_threadex_five_year %>% 
  filter(Name == "Mount Shasta Area")

write.csv(precip_threadex_five_year_shasta, "CSVs/shasta.csv", row.names = FALSE)

# Needles Area	

precip_threadex_five_year_needles <- precip_threadex_five_year %>% 
  filter(Name == "Needles Area")

write.csv(precip_threadex_five_year_needles, "CSVs/needles.csv", row.names = FALSE)

# Oxnard Area	

precip_threadex_five_year_oxnard <- precip_threadex_five_year %>% 
  filter(Name == "Oxnard Area")

write.csv(precip_threadex_five_year_oxnard, "CSVs/oxnard.csv", row.names = FALSE)

# Palm Springs Area	

precip_threadex_five_year_palm_springs <- precip_threadex_five_year %>% 
  filter(Name == "Palm Springs Area")

write.csv(precip_threadex_five_year_palm_springs, "CSVs/palm_springs.csv", row.names = FALSE)

# Palmdale Area	

precip_threadex_five_year_palmdale <- precip_threadex_five_year %>% 
  filter(Name == "Palmdale Area")

write.csv(precip_threadex_five_year_palmdale, "CSVs/palmdale.csv", row.names = FALSE)

# Ramona Area	

precip_threadex_five_year_ramona <- precip_threadex_five_year %>% 
  filter(Name == "Ramona Area")

write.csv(precip_threadex_five_year_ramona, "CSVs/ramona.csv", row.names = FALSE)

# Redding Area

precip_threadex_five_year_redding <- precip_threadex_five_year %>% 
  filter(Name == "Redding Area")

write.csv(precip_threadex_five_year_redding, "CSVs/redding.csv", row.names = FALSE)

# Riverside Area	

precip_threadex_five_year_riverside <- precip_threadex_five_year %>% 
  filter(Name == "Riverside Area")

write.csv(precip_threadex_five_year_riverside, "CSVs/riverside.csv", row.names = FALSE)

# Sacramento Area	

precip_threadex_five_year_sacramento <- precip_threadex_five_year %>% 
  filter(Name == "Sacramento Area")

write.csv(precip_threadex_five_year_sacramento, "CSVs/sacramento.csv", row.names = FALSE)

# Sacramento Downtown Area	

precip_threadex_five_year_sacramento_downtown <- precip_threadex_five_year %>% 
  filter(Name == "Sacramento Downtown Area")

write.csv(precip_threadex_five_year_sacramento_downtown, "CSVs/sacramento_downtown.csv", row.names = FALSE)

# San Diego Area	

precip_threadex_five_year_san_diego <- precip_threadex_five_year %>% 
  filter(Name == "San Diego Area")

write.csv(precip_threadex_five_year_san_diego, "CSVs/san_diego.csv", row.names = FALSE)

# San Francisco Airport Area	

precip_threadex_five_year_sf_airport <- precip_threadex_five_year %>% 
  filter(Name == "San Francisco Airport Area")

write.csv(precip_threadex_five_year_sf_airport, "CSVs/sf_airport.csv", row.names = FALSE)

# San Jose Area	

precip_threadex_five_year_san_jose <- precip_threadex_five_year %>% 
  filter(Name == "San Jose Area")

write.csv(precip_threadex_five_year_san_jose, "CSVs/san_jose.csv", row.names = FALSE)

# Santa Maria Area

precip_threadex_five_year_santa_maria <- precip_threadex_five_year %>% 
  filter(Name == "Santa Maria Area")

write.csv(precip_threadex_five_year_santa_maria, "CSVs/santa_maria.csv", row.names = FALSE)

# Stockton Area	

precip_threadex_five_year_stockton <- precip_threadex_five_year %>% 
  filter(Name == "Stockton Area")

write.csv(precip_threadex_five_year_stockton, "CSVs/stockton.csv", row.names = FALSE)

# Tahoe City Area	

precip_threadex_five_year_tahoe <- precip_threadex_five_year %>% 
  filter(Name == "Tahoe City Area")

write.csv(precip_threadex_five_year_tahoe, "CSVs/tahoe.csv", row.names = FALSE)

# Ukiah Area

precip_threadex_five_year_ukiah <- precip_threadex_five_year %>% 
  filter(Name == "Ukiah Area")

write.csv(precip_threadex_five_year_ukiah, "CSVs/ukiah.csv", row.names = FALSE)

