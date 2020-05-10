library(dplyr)
library(tidyr)
library(psych)
library(readxl)
library(sjmisc)
library(ggplot2)
library(ggcorrplot)

# it's a kind of magic, skip it
`%--%` <- function(x, y) {
  do.call(sprintf, c(list(x), y))
}


## P R E P R O C E S S I N G   D A T A

# source: https://www.kff.org/other/state-indicator/opioid-overdose-death-rates
# read 20 csv files for overdoses and create panel data from them
naming <- "overdose_rate/overdose_%s.csv" %--% list(1999:2018)
overdose <- data.frame()
for (i in c(1:length(naming))) {
  a <- read.csv(naming[i], skip = 2, header = TRUE, nrows = 52, na.strings = c("NR", "NSD"))
  a$year <- 1998 + i
  overdose <- rbind(overdose, a)
}
# change order of columns and redact their names
overdose <- overdose[c(1,4,2,3)]
names(overdose) <- c("state", "year", "opioid_d_adj", "alldrugs_d_adj")
overdose$year <- as.numeric(as.character(overdose$year))

# read excel file with information about legalization years by state (hand-made)
leg <- read_xlsx("legalization.xlsx", col_names = TRUE, na = c("NA"))
# and create panel data from this table
legal <- data.frame() 
for (i in 1:nrow(leg)) { 
  data <- data.frame(state=leg[i,1], year=c(1990:2019)) 
  data$med <- c(ifelse(data$year < c(leg[i,2]), 0, 1)) 
  data$med_disp <- c(ifelse(data$year < c(leg[i,3]), 0, 1)) 
  data$rec <- c(ifelse(data$year < c(leg[i,4]), 0, 1)) 
  data$rec_disp <- c(ifelse(data$year < c(leg[i,5]), 0, 1)) 
  data$pdmp <- c(ifelse(data$year < c(leg[i,7]), 0, 1)) 
  legal <- rbind(legal, data) 
}

# processing file for unemployment, GDP per capita (real), personal income per capita

# source: https://www.icip.iastate.edu/tables/employment/unemployment-states
# read the file, make long format, delete the first column;
unemployment <- read_xls("unemployment.xls", sheet = "States", skip = 5, col_names = TRUE, n_max = 52) %>%
  gather(year, unemployment, "1980":"2018") %>% select(-1)
# edit col name of state and change year format to numeric
names(unemployment)[1] <- "state"
unemployment$year <- as.numeric(unemployment$year)

# source: https://apps.bea.gov/iTable/iTable.cfm?acrdn=2&isuri=1&reqid=70&step=1#acrdn=2&isuri=1&reqid=70&step=1
# read the file, make long format, delete the first column; edit col name of state and change year format
gdp_per_cap_real <- read_xls("gdp_per_cap_real.xls", skip = 5, col_names = TRUE, n_max = 60) %>% 
  gather(year, gdp_per_cap, "1997":"2018") %>% select(-1)
names(gdp_per_cap_real)[1] <- "state"
gdp_per_cap_real$year <- as.numeric(gdp_per_cap_real$year)

# source: https://apps.bea.gov/iTable/iTable.cfm?acrdn=2&isuri=1&reqid=70&step=1#acrdn=2&isuri=1&reqid=70&step=1
# read the file, make long format, delete the first column; edit col name of state; edit col name of state and change year format
personal_income_per_cap <- read_xls("personal_income_per_cap.xls", skip = 5, col_names = TRUE, n_max = 60) %>%
  gather(year, income_per_cap, "1999":"2018") %>% select(-1)
names(personal_income_per_cap)[1] <- "state"
personal_income_per_cap$year <- as.numeric(personal_income_per_cap$year)


# source: https://www.cdc.gov/nchs/nvss/marriage-divorce.htm
# read the file, make long format, delete the first column; edit col name of state; edit col name of state and change col formats
marriage <- read_xlsx("marriage-rates.xlsx", skip = 5, col_names = TRUE, n_max = 53)[2:52, ] %>%
  gather(year, marriage_rate, "2018":"1999") %>% select(-2, -3)
names(marriage)[1] <- "state"
marriage$year <- as.numeric(marriage$year)
marriage$marriage_rate <- as.numeric(marriage$marriage_rate)

divorce <- read_xlsx("divorce-rates.xlsx", skip = 5, col_names = TRUE, n_max = 53)[2:52, ] %>%
  gather(year, divorce_rate, "2018":"1999") %>% select(-2, -3)
names(divorce)[1] <- "state"
divorce$year <- as.numeric(divorce$year)
divorce$divorce_rate <- as.numeric(divorce$divorce_rate)


# source: https://www.openicpsr.org/openicpsr/project/105583/version/V3/view;jsessionid=5A68D3A66B257C49163707BF7B76B1EE
# mini processing of alcohol consumption data
alco_consumption <- read.csv("alco_consumption.csv") %>% select(state=state, year=year, 
                                                                alco_consumption=ethanol_all_drinks_gallons_per_capita)
alco_consumption$state <- as.character(alco_consumption$state)


# source: https://ucr.fbi.gov/crime-in-the-u.s
# read excel for year 2004 (it has additional hand-made manipulation) and use it as the beggining of panel for crime
crime <- read_xlsx("crime_rate/crime_2004.xlsx", skip = 3, 
                   col_names = c("state", "violent_crime_on100k", "property_crime_on100k"))
crime <- crime %>% mutate(total_crime_on100k = violent_crime_on100k + property_crime_on100k)
crime$year <- 2004
# code for processing other 19 spreadsheets
states <- data.frame(crime[1])
naming <- "crime_rate/crime_%s.xls" %--% list(c(1999:2018))
for (i in c(1:20)[c(-6)]) {
  data <- read_xls(naming[i], col_names = FALSE, skip = 4)
  block <- states
  violent_crime_on100k <- data %>% filter(...1 == "Rate per 100,000 inhabitants") %>% select(violent_crime_on100k=...2)
  property_crime_on100k <- data %>% filter(...1 == "Rate per 100,000 inhabitants") %>% select(property_crime_on100k=...3)
  block <- block %>% cbind(violent_crime_on100k) %>% cbind(property_crime_on100k)
  block <- block %>% mutate(violent_crime_on100k = as.numeric(violent_crime_on100k),
                            property_crime_on100k = as.numeric(property_crime_on100k),
                            total_crime_on100k = violent_crime_on100k + property_crime_on100k)
  block$year <- 1998 + i
  crime <- rbind(crime, block)
}
crime <- crime[c(1,5,2:4)]

#clean environment from already used variables for previous manipulations
rm(a, data, block, states, property_crime_on100k, violent_crime_on100k)


# source: https://www2.census.gov/programs-surveys/popest/datasets
# read two databases, make some manipulations to get total population by state by year and age groups
pop <- read.csv("pop/pop00-10.csv") 

pop_and_age_grp_00_10 <- pop %>% filter(SEX == 0,  ORIGIN == 0, RACE == 0, AGEGRP !=0) %>% 
  gather(year, pop, "POPESTIMATE2000":"POPESTIMATE2009") %>%
  group_by(NAME, year) %>% summarise(population=sum(pop),
                                     age_0_14=sum(subset(pop, AGEGRP %in% c(1:3))) / population,
                                     age_15_24=sum(subset(pop, AGEGRP %in% c(4:5))) / population,
                                     age_25_44=sum(subset(pop, AGEGRP %in% c(6:9))) / population,
                                     age_45_59=sum(subset(pop, AGEGRP %in% c(10:12))) / population,
                                     age_60_more=sum(subset(pop, AGEGRP %in% c(13:18))) / population) %>% data.frame()

pop <- read.csv("pop/pop10-18.csv")

pop_and_age_grp_10_18 <- pop %>% filter(SEX == 0, ORIGIN == 0) %>% 
  gather(year, pop, "POPESTIMATE2010":"POPESTIMATE2018") %>%
  group_by(NAME, year) %>% summarise(population=sum(pop),
                                     age_0_14=sum(subset(pop, AGE %in% c(0:14))) / population,
                                     age_15_24=sum(subset(pop, AGE %in% c(15:24))) / population,
                                     age_25_44=sum(subset(pop, AGE %in% c(25:44))) / population,
                                     age_45_59=sum(subset(pop, AGE %in% c(45:59))) / population,
                                     age_60_more=sum(subset(pop, AGE %in% c(60:85))) / population) %>% data.frame()

# chain two tables
population <- rbind(pop_and_age_grp_00_10, pop_and_age_grp_10_18)
# chain with region and divison codes
population <- pop %>% select(REGION, DIVISION, NAME) %>% distinct() %>% right_join(population, by="NAME")

# make some fixing
population$year <- substr(population$year, 12, 15)
names(population)[1:3] <- c("reg", "div", "state")
population$year <- as.numeric(population$year)

# make dummy variables for region and division
dummy_reg <- population %>% select(reg) %>% to_dummy(var.name = "label")
names(dummy_reg) <- c("northeast", "midwest", "south", "west")

dummy_div <- population %>% select(div) %>% to_dummy(var.name = "label")
names(dummy_div) <- tolower(c("New England", "Middle Atlantic", "East North Central", "West North Central",
                              "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))

# chain dummy varibales with data
population <- cbind(population, dummy_reg) %>% select(-c(1,2))
#population <- cbind(population, dummy_div)

# clean memory
rm(pop, pop_and_age_grp_00_10, pop_and_age_grp_10_18)


# source: http://www.census.gov/data/tables/time-series/demo/income-poverty/cps-pov/pov-46.html
# processing poverty data: read csv and excel files, then combine them
naming <- "poverty/pov_%s.csv" %--% list(2007:2009)
poverty <- data.frame()
for (i in c(1:length(naming))) {
  a <- read.csv(naming[i], skip = 10, header = FALSE, nrows = 52) %>% select(state=V1, poverty=V5)
  a$year <- 2006 + i
  poverty <- rbind(poverty, a)
}
naming <- "poverty/pov_%s.xls" %--% list(2010:2018)
for (i in c(1:length(naming))) {
  a <- read_xls(naming[i], skip = 11, col_names = FALSE, n_max = 52) %>% select(state=...1, poverty=...5)
  a$year <- 2009 + i
  poverty <- rbind(poverty, a)
}
poverty <- poverty[c(1,3,2)]
a <- read_xls("poverty/pov_1999-2006.xls", col_names = TRUE, n_max = 52) %>%
  gather(year, poverty, -state)
poverty <- rbind(poverty, a)
poverty$year <- as.numeric(poverty$year)


# source: https://www2.census.gov/programs-surveys/popest/datasets
# huge processing to get percent of male, black and hispanic origin people
sex_and_race <- data.frame()
# loop for processing 51 state datasets for 2000-2009 years
for (name in unique(tolower(overdose$state))[-1]) {
  a <- data.frame(state = name, year = c(2000:2009))
  file_name <- "race00-10/%s.xls" %--% name
  data <- read_xls(file_name, skip = 4, n_max = 72, col_names = FALSE)
  vital <- data[c(1,4,17,25), c(1,3:12)]
  # make some calculations
  a$percent_male <- t(vital[4, 2:11]) / t(vital[1, 2:11])
  a$percent_black <- t(vital[2, 2:11]) / t(vital[1, 2:11])
  a$percent_hisp_origin <- t(vital[3, 2:11]) / t(vital[1, 2:11])
  # chain tables
  sex_and_race <- rbind(sex_and_race, a)
}
# processing dataset for 2010-2018 years
data <- read.csv("race10-18.csv") %>% 
  filter(Sex.id %in% c("male", "totsex"), Hisp.id != "nhisp",
         substr(Year.display.label, 1, 5) != "April") %>%
  mutate(state = GEO.display.label,
         year = as.numeric(substr(Year.id, 5, 8)),
         sex = Sex.id, hisp_orig = Hisp.id,
         wac = as.numeric(as.character(wac)),
         bac = as.numeric(as.character(bac)),
         iac = as.numeric(as.character(iac)),
         aac = as.numeric(as.character(aac)),
         nac = as.numeric(as.character(nac)),
         all = wac + bac + iac + aac + nac) %>%
  select(state, year, sex, hisp_orig, wac, bac, iac, aac, nac, all)

# and getting useful information about percent of black
a <- data %>% filter(sex == "totsex", hisp_orig == "tothisp") %>% 
  mutate(percent_black = bac / all) %>%
  select(state, year, percent_black) %>% arrange(state, year)

# and percent of male
b1 <- data %>% filter(sex == "totsex", hisp_orig == "tothisp") %>% arrange(state, year) %>% select(all)
b2 <- data %>% filter(sex == "male", hisp_orig == "tothisp") %>% arrange(state, year) %>% select(all)
b <- b2 / b1
names(b) <- "percent_male"

a <- cbind(a, b)

# and percent of hispanic origin
b1 <- data %>% filter(sex == "totsex", hisp_orig == "tothisp") %>% arrange(state, year) %>% select(all)
b2 <- data %>% filter(sex == "totsex", hisp_orig == "hisp") %>% arrange(state, year) %>% select(all)
b <- b2 / b1
names(b) <- "percent_hisp_origin"

a <- cbind(a, b)

# make some corrections
a$state <- tolower(a$state)
a <- a[c(1,2,4,3,5)]

# and chain: it's the end of huge processing
sex_and_race <- rbind(sex_and_race, a)
sex_and_race$state <- as.character(sex_and_race$state)

# some cleaning
rm(vital, a, b, b1, b2, data)


# https://wonder.cdc.gov
# processing databases with deaths grouped by different causes

#deaths_1 <- read.delim("alco_or_drug_or_others.txt") %>%
#  mutate(rate = Deaths / Population * 100000) %>% 
#  select(c(2, 4, 6, 11)) %>% drop_na() %>%
#  spread(MCD...Drug.Alcohol.Induced, rate)
#names(deaths_1) <- c("state", "year", "total_d", "alco_d", "other_d", "drug_d")

deaths_2 <- read.delim("different_deaths.txt") %>%
  mutate(rate = Deaths / Population * 100000) %>% 
  select(c(2, 4, 6, 11)) %>% drop_na() %>%
  spread(UCD...Injury.Intent, rate) %>% select(-c(4, 5, 7, 8))
names(deaths_2) <- c("state", "year", "total_homicide", "total_suicide")

#deaths_3 <- read.delim("more_deaths.txt") %>%
#  mutate(rate = Deaths / Population * 100000) %>%
#  select(c(2, 4, 6, 11)) %>% drop_na() %>%
#  spread(MCD...Drug.Alcohol.Induced.Cause, rate) %>% select(1:2, 5, 7:10)
#names(deaths_3) <- c("state", "year", "drug_other_causes", "drug_homicide",
#                     "drug_suicide", "drug_undetermined", "drug_unintentional")


# Causes of Death:
# All opioid poisoning (illicit: T40.0 T40.1 - and prescription: T40.2 T40.3 T40.4)
naming <- "Multiple_Cause_of_Death_1999-2018_%s.txt" %--% list(1:4)
deaths_0 <- data.frame()
for (i in 1:4) {
  a <- read.delim(naming[i]) %>% select(2, 4, 6:9) %>% drop_na(Year)
  deaths_0 <- rbind(deaths_0, a)
}

# all codes and their discriptions disticntly
du <- deaths_0 %>% mutate(cause = as.character(Multiple.Cause.of.death),
                          code = as.character(Multiple.Cause.of.death.Code),
                          rate = Deaths / Population * 100000,
                          state = as.character(State)) %>%
  select(state, year=Year, cause, code, rate) %>% distinct(code, cause) %>% arrange(code)

# 
deaths <- deaths_0 %>% mutate(cause = as.character(Multiple.Cause.of.death),
                              code = as.character(Multiple.Cause.of.death.Code),
                              rate = Deaths / Population * 100000,
                              state = as.character(State)) %>%
  select(state, year=Year, cause, code, rate) %>%
  filter(code %in% c("T37.4", "T38.3", "T39.0", "T39.1", "T39.3", "T39.8","T40.0", "T40.1", "T40.2", "T40.3", "T40.4", "T40.5", "T40.6", 
                     "T40.7", "T41.0", "T41.1", "T41.2", "T42.1", "T42.3", "T42.4", "T42.6", "T42.7", "T42.8", "T43.0", "T43.2", 
                     "T43.3", "T43.5", "T43.6", "T43.8", "T43.9", "T44.7", "T44.9", "T45.0", "T45.1", "T45.5", "T45.8", "T46.0",
                     "T46.1", "T46.2", "T46.5", "T47.6", "T48.1", "T48.3", "T48.7", "T50.5", "T50.7", "T50.9")) %>%
  group_by(state, year) %>% summarise(opioid_d_all = sum(subset(rate, code %in% c("T40.1", "T40.2", "T40.3", "T40.4"))),
                                      opioid_d_illicit = sum(subset(rate, code %in% c("T40.0","T40.1"))),
                                      opioid_d_prescription = sum(subset(rate, code %in% c("T40.2", "T40.3", "T40.4"))),
                                      drugs_d_all = sum(rate),
                                      drugs_d_illicit = sum(subset(rate, code %in% c("T40.0","T40.1", "T40.5", "T40.7", 
                                                                                     "T40.8", "T40.9", "T43.6")))) %>% 
  data.frame()


# source: https://www.cdc.gov/drugoverdose/maps/rxstate2006.html
# read 13 xlsx hand-made files for prescribing rates and create panel data from them
naming <- "prescr/%s.xlsx" %--% list(2006:2018)
prescribtion <- data.frame()
for (i in c(1:length(naming))) {
  a <- read_xlsx(naming[i], skip = 1, col_names = FALSE, n_max = 52) %>% select(state=...1, prescr_rate=...3)
  a$year <- 2005 + i
  prescribtion <- rbind(prescribtion, a)
}
# change order of columns and fix format of "prescribing rates" column
prescribtion <- prescribtion[c(1,3,2)]
prescribtion$prescr_rate <- as.numeric(prescribtion$prescr_rate)


#добавление пространственной переменной
#строю матрицу смежности 
neighbors <- read_xlsx("neighbors.xlsx")
S <- as.matrix(neighbors[,2:ncol(neighbors)])
a <- neighbors[,1]
rownames(S) <- a[[1]]

for(i in 1:(nrow(S)-1)){
  for(j in (i+1):ncol(S)){
    S[j,i] <- S[i,j]
  }
}

# У нас для каждого года есть вектор легализации (медицинской -- M и рекреационной -- R),
# поэтому количество соседей, легализовавших M составляет SM, а легализовавших R -- SR.
# Важно, что и в векторе и в матрице штаты должны идти в алфавитном порядке.

med_neighbors <- as.data.frame(cbind(tolower(a[[1]]), matrix(0,nrow = 51, ncol = length(c(1998:2019)))))
names(med_neighbors) <- c("state", 1998:2019)


rec_neighbors <- med_neighbors
rec_disp_neighbors <- med_neighbors
med_disp_neighbors <- med_neighbors

for(i in 2:ncol(med_neighbors)){
  base <- legal%>%
    mutate(med = tidyr::replace_na(med, 0),rec = tidyr::replace_na(rec, 0),
           rec_disp = tidyr::replace_na(rec_disp, 0),med_disp = tidyr::replace_na(med_disp, 0)) %>%
    filter(year == as.integer(names(med_neighbors)[i]))%>%arrange(state)
  M <- as.matrix(base$med)
  R <- as.matrix(base$rec)
  MD <- as.matrix(base$med_disp)
  RD <- as.matrix(base$rec_disp)
  med_neighbors[,i] <-  S%*%M
  rec_neighbors[,i] <-  S%*%R
  med_disp_neighbors[,i] <-  S%*%MD
  rec_disp_neighbors[,i] <-  S%*%RD
}


med_1 <- gather(med_neighbors, year, med_neighbors,names(med_neighbors)[2:ncol(med_neighbors)])
rec_1 <- gather(rec_neighbors, year, rec_neighbors,names(rec_neighbors)[2:ncol(rec_neighbors)])
med_disp_1 <- gather(med_disp_neighbors, year, med_disp_neighbors,names(med_disp_neighbors)[2:ncol(med_disp_neighbors)])
rec_disp_1 <- gather(rec_disp_neighbors, year, rec_disp_neighbors,names(rec_disp_neighbors)[2:ncol(rec_disp_neighbors)])


med_1$year <- as.numeric(med_1$year)
rec_1$year <- as.numeric(rec_1$year)
med_disp_1$year <- as.numeric(med_disp_1$year)
rec_disp_1$year <- as.numeric(rec_disp_1$year)

med_1$state <- as.character(med_1$state)
rec_1$state <- as.character(rec_1$state)
med_disp_1$state <- as.character(med_disp_1$state)
rec_disp_1$state <- as.character(rec_disp_1$state)


# Avengers, assemble!
# make all state columns identical
overdose$state <- tolower(overdose$state)
legal$state <- tolower(legal$state)
crime$state <- tolower(crime$state)
gdp_per_cap_real$state <- tolower(gdp_per_cap_real$state)
personal_income_per_cap$state <- tolower(personal_income_per_cap$state)
unemployment$state <- tolower(unemployment$state)
population$state <- tolower(population$state)
poverty$state <- tolower(poverty$state)
marriage$state <- tolower(marriage$state)
divorce$state <- tolower(divorce$state)
deaths_2$state <- tolower(deaths_2$state)
deaths$state <- tolower(deaths$state)
prescribtion$state <- tolower(prescribtion$state)

# join all panel datas
panel_data <- full_join(overdose, legal, by=c("state", "year")) %>%
  full_join(crime, by=c("state", "year")) %>% full_join(gdp_per_cap_real, by=c("state", "year")) %>%
  full_join(personal_income_per_cap, by=c("state", "year")) %>% full_join(unemployment, by=c("state", "year")) %>%
  full_join(population, by=c("state", "year")) %>% full_join(alco_consumption, by=c("state", "year")) %>%
  full_join(poverty, by=c("state", "year")) %>% full_join(sex_and_race, by=c("state", "year")) %>%
  full_join(marriage, by=c("state", "year")) %>% full_join(divorce, by=c("state", "year")) %>%
  full_join(deaths_2, by=c("state", "year")) %>% full_join(deaths, by=c("state", "year")) %>%
  full_join(med_1, by=c("state", "year")) %>% full_join(rec_1, by=c("state", "year")) %>%
  full_join(rec_disp_1, by=c("state", "year")) %>%full_join(med_disp_1, by=c("state", "year")) %>%
  full_join(prescribtion, by=c("state", "year"))

# prepare data for analysis
panel_data_clear <- subset(panel_data, !(state %in% c("puerto rico", "new england", "mideast", 
                                                      "great lakes", "plains", "southeast", 
                                                      "southwest", "rocky mountain", "far west", 
                                                      "northeast region", "midwest region", 
                                                      "south region", "west region", "us total", "united states"))) %>%
  mutate(med = tidyr::replace_na(med, 0), 
         rec = tidyr::replace_na(rec, 0), 
         med_disp = tidyr::replace_na(med_disp, 0), 
         rec_disp = tidyr::replace_na(rec_disp, 0), 
         pdmp = tidyr::replace_na(pdmp, 0)) %>% 
  filter(year > 1998 & year < 2020) %>% 
  arrange(state, year)

# if you need to save data in csv format, use the next command:
# write.csv(panel_data_clear, "panel.csv")


## V I S U A L I Z A T I O N

# prepare mini-data to visualise
legal <- legal %>%
  mutate(med = tidyr::replace_na(med, 0),
         rec = tidyr::replace_na(rec, 0),
         status = med + rec,
         state = tolower(state))

data_for_plot <- inner_join(legal, overdose, by = c('year', 'state'))

# make selection of states
selected_states <- data_for_plot %>%
  group_by(state) %>% summarise(val = sum(status)) %>% 
  filter(val>0) %>% select(1)

# plot1
plot1 <- ggplot(subset(data_for_plot, state %in% c(selected_states[[1]])), 
                aes(x = factor(year), y = opioid_d_adj, col = factor(status))) + 
  geom_line(aes(group = state), size = 2) + facet_wrap(. ~ state) +
  ggtitle("Граф.1 Количество смертей от передозировок опиоидами (на 100 000 населения штата)") + 
  theme(axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(color = "grey20", size = 10),
        legend.text = element_text(color = "grey20", size = 10),
        text = element_text(size = 10), 
        legend.position = "bottom",
        legend.direction = "vertical") +
  scale_color_manual(name = "Cтепень легализации", 
                     labels = c("Полный запрет", "Для медицинских целей", "Для рекреацонных целей"), 
                     values = c("red", "orange", "green")) +
  scale_x_discrete(breaks = c(seq(1999, 2018, 3))) + 
  labs(x = "Годы", y = "Количество передозировок на 100.000 человек")

# plot2
plot2 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = opioid_d_adj, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.2 Динамика количества передозировок опиоидами") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name = "Cтепень легализации", 
                     labels = c("Полный запрет", "Для медицинских целей", "Для рекреацонных целей"), 
                     values = c("red", "orange", "green")) +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(color = "grey20", size = 10),
        legend.text = element_text(color = "grey20", size = 10),
        text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "vertical")

# plot3
plot3 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = alldrugs_d_adj, col = factor(status))) + 
  geom_boxplot() + 
  ggtitle("Граф.3 Динамика количества передозировок всеми типами наркотиков") +
  labs(x = "Годы", y = "Количество передозировок на 100.000 чел.") +
  scale_color_manual(name = "Cтепень легализации", 
                     labels = c("Полный запрет", "Для медицинских целей", "Для рекреацонных целей"), 
                     values = c("red", "orange", "green")) +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(color = "grey20", size = 10),
        legend.text = element_text(color = "grey20", size = 10),
        text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "vertical")

# correlation matrix
corr <- cor(panel_data_clear %>%
              select(-c("state", "year", "opioid_d_adj", "alldrugs_d_adj", "population", "marriage_rate", "divorce_rate",
                        "violent_crime_on100k", "property_crime_on100k", "total_homicide", "total_suicide")) %>%
              drop_na())

correlation <- ggcorrplot(corr, lab = TRUE, lab_size = 2.8, digits = 2) + 
  theme(axis.text.x = element_text(color = "grey20", size = 8), 
        axis.text.y = element_text(color = "grey20", size = 8), 
        text = element_text(size = 8))

# Visualize previous 4 plots
plot1
plot2
plot3
correlation


## M O D E L S

library(plm)
library(modelr)
library(stargazer)
library(matrixStats)

# Функция clse позволяет считать корректные стандартные ошибки 
# в случае использования моделей на панельных данных (FE)
# clustered SEs, clustered on "group"... could also cluster on "time" 
# compute Stata-like degrees of freedom adjustment for number of groups
# See http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg, "id")))
  N = length(index(reg, "id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method = "arellano", type = "HC1", cluster = "group")))
  return(rob)
}


#######################################################################################
# Зависимая - смертность от передозировок всеми наркотиками
#######################################################################################

# Базовая модель
reg1 <- plm(drugs_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, model = "pooling")
summary(reg1)

reg2 <- plm(drugs_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg2)

reg3 <- plm(drugs_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin + factor(year),
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg3)

reg4 <- plm(drugs_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "random", effect = "individual")
summary(reg4)

stargazer(reg1, reg2, reg3, reg4,
          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
          title = "Базовая модель", type="text",
          column.labels = c("pooled", "FE", "FE+time", "RE"),
          df = FALSE, digits = 3, out = "file1.txt")

# тест H0: pooled, H1: значимые фиксированные эффекты
pooltest(reg1, reg2)
pooltest(reg1, reg3)
# тест H0: pooled, H1: значимые рандомные эффекты
plmtest(reg1, effect = "individual", type = "bp")
# тест H0: значимые эффекты лучше, H1: фиксированные эффекты лучше
phtest(reg2, reg4)

# Расширенная модель
reg1 <- plm(drugs_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, model = "pooling")
summary(reg1)


reg2 <- plm(drugs_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg2)

reg3 <- plm(drugs_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin + factor(year),
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg3)

reg4 <- plm(drugs_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "random", effect = "individual")
summary(reg4)

stargazer(reg1, reg2, reg3, reg4,
          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
          title = "Расширенная модель", type="text",
          column.labels = c("pooled", "FE", "FE+time", "RE"),
          df = FALSE, digits = 3, out = "file2.txt")

# тест H0: pooled, H1: значимые фиксированные эффекты
pooltest(reg1, reg2)
pooltest(reg1, reg3)
# тест H0: pooled, H1: значимые рандомные эффекты
plmtest(reg1, effect = "individual", type = "bp")
# тест H0: значимые эффекты лучше, H1: фиксированные эффекты лучше
phtest(reg2, reg4)


#######################################################################################
# Зависимая - смертность от передозировок опиатами
#######################################################################################

# Базовая модель
reg1 <- plm(opioid_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, model = "pooling")
summary(reg1)


reg2 <- plm(opioid_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg2)

reg3 <- plm(opioid_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin + factor(year),
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg3)

reg4 <- plm(opioid_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
              unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "random", effect = "individual")
summary(reg4)

stargazer(reg1, reg2, reg3, reg4,
          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
          title = "Базовая модель", type="text",
          column.labels = c("pooled", "FE", "FE+time", "RE"),
          df = FALSE, digits = 3, out = "file3.txt")

# тест H0: pooled, H1: значимые фиксированные эффекты
pooltest(reg1, reg2)
pooltest(reg1, reg3)
# тест H0: pooled, H1: значимые рандомные эффекты
plmtest(reg1, effect = "individual", type = "bp")
# тест H0: значимые эффекты лучше, H1: фиксированные эффекты лучше
phtest(reg2, reg4)

# Расширенная модель
reg1 <- plm(opioid_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, model = "pooling")
summary(reg1)


reg2 <- plm(opioid_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg2)

reg3 <- plm(opioid_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin + factor(year),
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg3)

reg4 <- plm(opioid_d_all ~ med + rec + pdmp + med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + 
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "random", effect = "individual")
summary(reg4)

stargazer(reg1, reg2, reg3, reg4,
          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
          title = "Расширенная модель", type="text",
          column.labels = c("pooled", "FE", "FE+time", "RE"),
          df = FALSE, digits = 3, out = "file4.txt")

# тест H0: pooled, H1: значимые фиксированные эффекты
pooltest(reg1, reg2)
pooltest(reg1, reg3)
# тест H0: pooled, H1: значимые рандомные эффекты
plmtest(reg1, effect = "individual", type = "bp")
# тест H0: значимые эффекты лучше, H1: фиксированные эффекты лучше
phtest(reg2, reg4)



#######################################################################################
# И Т О Г
#######################################################################################
#Зависимая - смертность от передозировок всеми наркотиками
#######################################################################################

# Пригодистя для bootstrap, в дальнейшем используем этот датасет для моделей (очищен от NA)
panel_data_modeling <- panel_data_clear %>% drop_na(drugs_d_all, med, rec, pdmp, total_crime_on100k, income_per_cap, 
                                                    alco_consumption, unemployment, age_0_14, age_15_24, age_45_59, 
                                                    age_25_44, northeast, midwest, south, percent_male, percent_black, 
                                                    percent_hisp_origin)

reg1 <- plm(drugs_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption + 
              unemployment + age_0_14 + age_15_24 + age_45_59+age_25_44 + northeast + midwest + south +
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_modeling, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg1)

# Гистограмма остатков
hist(reg1$residuals, breaks = 100)
# Находим спрогнозированные игрики
predictions <- panel_data_modeling$drugs_d_all - reg1$residuals
# сэмплируем остатки
boots <- bootstrap(data.frame(reg1$residuals), 100)
coefs = reg1$coefficients
for (i in 1:100) {
  panel_data_modeling$drugs_d_all_m <- predictions + data.frame(boots$strap[[i]])$reg1.residuals
  r <- plm(drugs_d_all_m ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption +
             unemployment + age_0_14 + age_15_24 + age_45_59 + age_25_44 + northeast + midwest + south + 
             percent_male + percent_black + percent_hisp_origin,
           data = panel_data_modeling, index = c("state", "year"),
           model = "within", effect = "individual")
  coefs <- rbind(coefs, r$coefficients)
}

coefs[1, ]
probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
colQuantiles(coefs[2:101, ], probs = probs, cols = names(coefs))


reg2 <- plm(drugs_d_all ~ med + rec + pdmp +  med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59+age_25_44+
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg2)


#######################################################################################
# Зависимая - смертность от передозировок опиатами
#######################################################################################
reg3 <- plm(opioid_d_all ~ med + rec + pdmp + total_crime_on100k + income_per_cap + alco_consumption + 
              unemployment + age_0_14 + age_15_24 + age_45_59+age_25_44 + northeast + midwest + south +
              percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg3)


reg4 <- plm(opioid_d_all ~ med + rec + pdmp +  med_neighbors + rec_neighbors + total_crime_on100k + 
              income_per_cap + alco_consumption + unemployment + age_0_14 + age_15_24 + age_45_59+age_25_44+
              northeast + midwest + south + percent_male + percent_black + percent_hisp_origin,
            data = panel_data_clear, index = c("state", "year"),
            model = "within", effect = "individual")
summary(reg4)


stargazer(reg1, reg2, reg3, reg4,
          se = list(clse(reg1), clse(reg2), clse(reg3), clse(reg4)),
          title = "Итоговый выбор", type="text",
          column.labels = c("FE base", "FE spatial", "FE base", "FE spatial"),
          df = FALSE, digits = 3, out = "final.txt")



##############################################################################
# diff-in-diff
##############################################################################

library(estimatr)
library(lmtest)
library(FNN)

#                                               E X P E R I M E N T S

# пытаемся понять, на сколько кластеров дробить штаты
k_to_try = 2:7
acc_k = rep(x = 0, times = length(k_to_try))

data_for_clustering <- panel_data_clear %>% select(state, year, total_crime_on100k, gdp_per_cap, income_per_cap,
                                                   age_25_44, alco_consumption, percent_black) %>%
  filter(year == 2010 & state != 'district of columbia') %>% drop_na()

for (i in seq_along(k_to_try)) {
  pred = kmeans(data_for_clustering[, 3:8], centers = k_to_try[i], 
                nstart = 30, iter.max = 30, algorithm = "Lloyd")
  acc_k[i] = pred$betweenss/pred$totss
}

# График аккуратности кластеризации
plot(k_to_try, acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20, 
     xlab = "k, number clusters", ylab = "classification accuracy",
     main = "Accuracy Rate vs Clusters")

# понимаем, что 4 кластера более чем достаточно

# оставляем столбцы, по которым хотим получить кластеризацию
data_for_clusters <- panel_data_clear %>% select(state, year, total_crime_on100k, gdp_per_cap, income_per_cap,
                                                 age_25_44, alco_consumption, percent_black) %>% drop_na()

# для каждого кода проводлим кластеризацию штатов и заносим в лист
years <- unique(data_for_clusters$year)

list <- list()
for (i in seq_along(years)) {
  print(years[i])
  data <- data_for_clusters %>% filter(year == years[i])
  kmean <- kmeans(data[, 3:8], centers = 4, nstart = 30, iter.max = 30, algorithm = "Lloyd")
  clusters <- cbind(data[1], kmean$cluster)
  names(clusters) <- c("state", "cluster")
  list[[i]] <- clusters
}

# разделяем датафреймы по кластерам
listik <- list()
for (i in seq_along(years)) {
  print(years[i])
  listik[[i]] = list()
  for (k in 1:4) {
    listik[[i]][[k]] <- list[[i]] %>% filter(cluster == k) %>% select(state)
  }
}

# считаем длч каждого штата, сколько раз он попал с каким-то штатом в один кластер
states <- unique(data_for_clusters$state)
matched_states <- matrix(rep(0, length(states)*length(states)), ncol = length(states), 
                         nrow = length(states), dimnames = list(states, states))

for (s in states) {
  for (i in seq_along(years)) {
    for (k in 1:4) {
      if (s %in% listik[[i]][[k]]$state) {
        matched_states[s, listik[[i]][[k]]$state] <- matched_states[s, listik[[i]][[k]]$state] + 1
      }
    }
  }
}

# изучаем полученную таблицу
matched_states

# colors <- c('red2', 'green3')[as.integer(matched_states[1, ] > 10) + 1]
# pairs(data_for_clustering[, 3:7], col = colors, pch = 1, cex = 1, cex.labels = 1.5)


# модели метода dd для зависимой drugs_d_all, opioid_d_all и регрессора med
################################################################################################################################
##########diff_function берёт год разбиения и считает средние для той переменной, которую. мы хотим усреднить за заданный период
difff <- function(year_split, variable){
  #создаётся пустой  датасет, в который мы кладём все штаты
  diff <- data.frame()
  diff <- data.frame(state = tolower(a[[1]]))
  
  states_after_year_split <- tolower(c((leg%>%subset(medical>= year_split)%>%select(state))[[1]]))   
  m = trunc(mean(c((leg%>%subset(medical >= year_split)%>%select(medical))[[1]])))
  for(i in 1:51){
    diff$status[i] <- if_else(diff$state[i] %in% selected_states[[1]],1,0)
    if(diff$status[i] == 1){
      b <- na.omit(panel_data_clear%>%subset(med == 0 & state == diff$state[i])%>%select(variable))
      c <- na.omit(panel_data_clear%>%subset(med == 1 & state == diff$state[i])%>%select(variable))
    }else{
      b <- na.omit(panel_data_clear%>%subset(year <= m & state == diff$state[i])%>%select(variable))
      c <- na.omit(panel_data_clear%>%subset(year >= m & state == diff$state[i])%>%select(variable))
    }
    colnames(b) <- "new"
    colnames(c) <- "new"
    diff$mean_variable_0[i] <- mean(b$new) #средняя смерность от всех наркотиков до легализации
    diff$mean_variable_1[i] <- mean(c$new) #после
  }
  
  unselected_states <- data_for_plot %>%
    group_by(state) %>% summarise(val = sum(status)) %>%
    filter(val == 0) %>% select(1)
  all <- c(states_after_year_split,c(unselected_states[[1]]))
  
  diff <- diff%>%subset(state %in% all)
  diff <- diff%>%mutate(delta = mean_variable_1 - mean_variable_0)
  return(diff)
}
###########################################################################################################
###############Модели влиянния MML нас смертность##########################################################
drug_set <- difff(2010,"pdmp")%>%select(state,status)
for(x in c("pdmp", "drugs_d_all", "opioid_d_all","opioid_d_illicit","opioid_d_prescription","drugs_d_illicit",
           "med_neighbors","rec_disp_neighbors",
           "total_crime_on100k","income_per_cap","prescr_rate","alco_consumption",
           "unemployment", "age_0_14", "age_15_24", "age_45_59","age_25_44",
           "percent_male","percent_black","percent_hisp_origin")){
  drug_set[x] <- difff(2010,x)%>%select(delta)
}
#pdmp разумно использовать, если брать равное число лет до/после

dd01 <- lm(drugs_d_all~status+med_neighbors+
             total_crime_on100k+income_per_cap+unemployment+
             age_15_24+age_25_44+
             percent_male+percent_black+
             percent_hisp_origin, drug_set)
dd02 <- lm(drugs_d_illicit ~ status+med_neighbors+
             total_crime_on100k+income_per_cap+unemployment+
             age_15_24+age_25_44+
             percent_male+percent_black+
             percent_hisp_origin, drug_set)
dd03 <- lm(opioid_d_all ~ status+med_neighbors+
             total_crime_on100k+income_per_cap+unemployment+
             age_15_24+age_25_44+
             percent_male+percent_black+
             percent_hisp_origin, drug_set)
dd04 <- lm(opioid_d_prescription ~status+med_neighbors+
             total_crime_on100k+income_per_cap+unemployment+
             age_15_24+age_25_44+
             percent_male+percent_black+
             percent_hisp_origin, drug_set)
dd05 <- lm(opioid_d_illicit ~ status+med_neighbors+
             total_crime_on100k+income_per_cap+unemployment+
             age_15_24+age_25_44+
             percent_male+percent_black+
             percent_hisp_origin, drug_set)
stargazer(dd01,dd02,dd03,dd04,dd05, align = TRUE,
          title = "DD оценивание влияния легализации использования марихуаны в медицинских целях",
          header=FALSE,se = starprep(dd01,dd02,dd03,dd04,dd05,se_type = "HC1"),type = "text",out = "dd_med.txt")


###########################################################################################################
###############Модели влиянния RML нас смертность##########################################################
difff_rec <- function(variable){
  
  diff <- data.frame()
  diff <- data.frame(state = tolower(a[[1]]))
  states_rec <- tolower(c((leg%>%subset(!is.na(recreational))%>%select(state))[[1]]))   
  states_control = tolower(c((leg%>%subset((medical<=2010 & is.na(recreational))|state =="Delaware"))[[1]]))
  m1 <- mean(c((leg%>%subset(!is.na(recreational))%>%select(recreational))[[1]]))
  
  for(i in 1:51){
    diff$status[i] <- if_else(diff$state[i] %in% states_control, 0, 1)
    if(diff$status[i] == 1){
      b <- na.omit(panel_data_clear%>%subset(rec == 0 & state == diff$state[i]& year>=2010)%>%select(variable))
      c <- na.omit(panel_data_clear%>%subset(rec == 1 & state == diff$state[i])%>%select(variable))
    }else{
      b <- na.omit(panel_data_clear%>%subset(year <= m1 & year>=2010 & state == diff$state[i])%>%select(variable))
      c <- na.omit(panel_data_clear%>%subset(year >= m1 & state == diff$state[i])%>%select(variable))
    }
    colnames(b) <- "new"
    colnames(c) <- "new"
    diff$mean_variable_0[i] <- mean(b$new) #средняя смертность от всех наркотиков до легализации
    diff$mean_variable_1[i] <- mean(c$new)
  }
  
  all <-c(states_rec,states_control)
  diff <- diff%>%subset(state %in% all)%>%
    mutate(delta = mean_variable_1 - mean_variable_0)
  return(diff)
}
############################################################################################################



drug_set_rec <- difff_rec("pdmp")%>%select(state,status)
for(x in c("pdmp", "drugs_d_all", "opioid_d_all","opioid_d_illicit","opioid_d_prescription","drugs_d_illicit",
           "med_neighbors","rec_disp_neighbors","rec_neighbors",
           "total_crime_on100k","income_per_cap","prescr_rate","alco_consumption",
           "unemployment", "age_0_14", "age_15_24", "age_45_59","age_25_44",
           "percent_male","percent_black","percent_hisp_origin")){
  drug_set_rec[x] <- difff_rec(x)%>%select(delta)
}


dd3 <- lm(drugs_d_all ~ status + 
            income_per_cap+total_crime_on100k+rec_disp_neighbors+
            unemployment+age_25_44+age_15_24, drug_set_rec)
dd4 <- lm( opioid_d_all~ status + 
             income_per_cap+total_crime_on100k+rec_disp_neighbors+
             unemployment+age_25_44+age_15_24, drug_set_rec)

dd5 <- lm(drugs_d_illicit ~ status + 
            income_per_cap+total_crime_on100k+rec_disp_neighbors+
            unemployment+age_25_44+age_15_24, drug_set_rec)
dd6 <- lm(opioid_d_illicit ~ status +
            income_per_cap+total_crime_on100k+rec_disp_neighbors+
            unemployment+age_25_44+age_15_24, drug_set_rec)

dd7 <- lm(opioid_d_prescription ~ status + 
            income_per_cap+total_crime_on100k+rec_disp_neighbors+
            unemployment+age_25_44+age_15_24, drug_set_rec)

stargazer(dd3,dd4,dd5,dd6,dd7, align = TRUE ,
          title = "DD оценивание влияния RML",
          header=FALSE,se = starprep(dd3,dd4,se_type = "HC1"),type = "text" )


######################################################################
######################Муки с мэтчингом################################
library(FNN)
library(Hmisc)

# Пытаемся сделать что-то полезное
data_for_neigh <- panel_data_clear[, c(1:2, 5, 7, 12:13, 17:26, 28:30, 35:39)]
years <- c(2001:2018)
k_neigh <- 1

neighbors_data <- data.frame()
for (y in years) {
  # оставляем всё, кроме интересующих в конце переменных
  data_for_n <- panel_data_clear[, c(1:2, 5, 7, 12:13, 17:26, 28:30)] %>%
    filter(year == y) %>% drop_na()
  # масштабируем признаки, по которым будем кластеризовывать
  data_scaled <- data.frame(scale(data_for_n[, 5:19]))
  states <- data_for_n[data_for_n$med == 1, ]$state
  for (s in states) {
    d <- data.frame(year = y)
    t <- get.knnx(data_scaled[data_for_n$state != s & data_for_n$med != 0, ],
                  query = data_scaled[data_for_n$state == s, ],
                  k = k_neigh, algorithm = "kd_tree")
    c <- data_for_n[data_for_n$state != s & data_for_n$med != 0, ][t$nn.index, 1]
    d$state <- s
    d$clone <- c
    d$diff_opioid <- data_for_neigh[data_for_neigh$state == s & data_for_neigh$year == y, 'opioid_d_all'] -
      data_for_neigh[data_for_neigh$state == c & data_for_neigh$year == y, 'opioid_d_all']
    d$diff_alldrugs <- data_for_neigh[data_for_neigh$state == s & data_for_neigh$year == y, 'drugs_d_all'] -
      data_for_neigh[data_for_neigh$state == c & data_for_neigh$year == y, 'drugs_d_all']
    neighbors_data <- rbind(neighbors_data, d)
  }
}


plot4 <- neighbors_data %>%
  ggplot(aes(x = factor(year), y = diff_opioid)) + 
  geom_boxplot(alpha = 0.5) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "orange") + 
  stat_summary(fun.y = mean, geom = "point", colour = "red") +
  ggtitle("Граф.3 Разница между передозировками в штатах с легализованными наркотиками vs медицинскими") +
  labs(x = "Годы", y = "Дельта передозировок на 100.000 чел.") +
  theme(axis.text.x = element_text(angle = 90, size = 15),
        axis.text.y = element_text(color = "grey20", size = 10),
        legend.text = element_text(color = "grey20", size = 10),
        text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "vertical")

plot4




# That's all :)