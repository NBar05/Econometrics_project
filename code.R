library(dplyr)
library(tidyr)
library(psych)
library(readxl)
library(ggplot2)
library(stargazer)
library(ggcorrplot)

# it's a kind of magic, skip it
`%--%` <- function(x, y) {
  do.call(sprintf, c(list(x), y))
}


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
names(overdose) <- c("state", "year", "opioid_death_rate", "alldrugs_death_rate")
overdose$year <- as.numeric(as.character(overdose$year))

# read excel file with information about legalization years by state (hand-made)
leg <- read_xlsx("legalization.xlsx", col_names = TRUE, na = c("NA"))
# and create panel data from this table
legal <- data.frame()
for (i in 1:nrow(leg)) {
  data <- data.frame(state=leg[i,1], year=c(1990:2019))
  data$med <- c(ifelse(data$year <= c(leg[i,2]), 0, 1))
  data$full <- c(ifelse(data$year <= c(leg[i,4]), 0, 1))
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
                           gather(year, personal_income_per_cap, "1999":"2018") %>% select(-1)
names(personal_income_per_cap)[1] <- "state"
personal_income_per_cap$year <- as.numeric(personal_income_per_cap$year)


# source: https://www.openicpsr.org/openicpsr/project/105583/version/V3/view;jsessionid=5A68D3A66B257C49163707BF7B76B1EE
# mini processing of alcohol consumption data
alco_consumption <- read.csv("alco_consumption.csv") %>% select(state=state, year=year, 
                                                                alco_consumption=ethanol_all_drinks_gallons_per_capita)


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
rm(a, data, block, states, property_crime_on100k, violent_crime_on100k, total_crime_on100k)


# source: https://www2.census.gov/programs-surveys/popest/datasets
# read two databases, make some manipulations to get total population by state by year
pop <- read.csv("pop/pop00-10.csv")
pop00_10 <- pop %>% filter(SEX == 0,  ORIGIN == 0, RACE == 0, AGEGRP ==0) %>% 
  gather(year, pop, "POPESTIMATE2000":"POPESTIMATE2009") %>%
  group_by(NAME, year) %>% summarise(population=sum(pop))

pop <- read.csv("pop/pop10-18.csv")
pop10_18 <- pop %>% filter(SEX == 0, ORIGIN == 0) %>% 
  gather(year, pop, "POPESTIMATE2010":"POPESTIMATE2018") %>%
  group_by(NAME, year) %>% summarise(population=sum(pop))

# chain two tables
population <- rbind(pop00_10, pop10_18)
# make some fixing
population$year <- substr(population$year, 12, 15)
names(population)[1] <- "state"
population$year <- as.numeric(population$year)

# clean memory
rm(pop, pop00_10, pop10_18)


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

# some cleaning
rm(vital, a, b, b1, b2, data)


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
overdose$year = as.integer(overdose$year)

# join all panel datas
panel_data <- full_join(overdose, legal, by=c("state", "year")) %>%
  full_join(crime, by=c("state", "year")) %>% full_join(gdp_per_cap_real, by=c("state", "year")) %>%
  full_join(personal_income_per_cap, by=c("state", "year")) %>% full_join(unemployment, by=c("state", "year")) %>%
  full_join(population, by=c("state", "year")) %>% full_join(alco_consumption, by=c("state", "year")) %>%
  full_join(poverty, by=c("state", "year")) %>% full_join(sex_and_race, by=c("state", "year"))

# prepare data for analysis
panel_data_clear <- subset(panel_data, !(state %in% c("puerto rico", "new england", "mideast", 
                                                      "great lakes", "plains", "southeast", 
                                                      "southwest", "rocky mountain", "far west", 
                                                      "northeast region", "midwest region", 
                                                      "south region", "west region", "us total", "united states"))) %>%
                    mutate(med = replace_na(med, 0),
                           full = replace_na(full, 0)) %>%
                    filter(year >= 1999) %>%
                    arrange(state, year)

# if you need to save data in csv format, use the next command:
# write.csv(panel_data_clear, "panel.csv")

# Graphic analysis
panel_data_clear %>% gather(crime, rate, violent_crime_on100k:property_crime_on100k) %>%
  group_by(year, crime) %>% summarise(avg_rate = mean(rate, na.rm = TRUE)) %>% 
  ggplot(aes(x=year, y=avg_rate, col=crime)) + geom_line() +
  ggtitle("Нисходящая динамика уровня преступности") +
  labs(x = "Годы", y = "Среднее количество преступлений на 100.000 человек") +
  scale_color_manual(name = "Виды преступлений", 
                     labels = c("Мелкий разбой", "С применением насилия"), 
                     values = c("purple", "blue")) + 
  theme(axis.text.x = element_text(color = "grey20", size = 10), 
        axis.text.y = element_text(color = "grey20", size = 10), 
        text = element_text(size = 10))

panel_data_clear %>% 
  ggplot(aes(x=factor(med), y=alldrugs_death_rate)) + geom_boxplot() +
  ggtitle("Различный разброс данных по передозировкам") +
  labs(x = "Статус медицинской легализации", y = "Количество передозировок на 100.000 человек")

panel_data_clear %>% 
  ggplot(aes(x=factor(full), y=alldrugs_death_rate)) + geom_boxplot() +
  ggtitle("Различный разброс данных по передозировкам") +
  labs(x = "Статус рекреационной (полной) легализации", y = "Количество передозировок на 100.000 человек")

ggcorrplot(cor((panel_data_clear %>% 
                  select(-c("state", "population", "year", "total_crime_on100k", "alco_consumption",
                            "percent_male", "percent_black", "percent_hisp_origin")) %>%
                  drop_na())), 
           lab = TRUE, lab_size = 5, digits = 2) + 
  theme(axis.text.x = element_text(color = "grey20", size = 10), 
        axis.text.y = element_text(color = "grey20", size = 10), 
        text = element_text(size = 20))

# prepare mini-data to visualise
legal <- legal %>%
  mutate(med = replace_na(med, 0),
         full = replace_na(full, 0),
         status = med + full,
         state = tolower(state))

data_for_plot <- inner_join(legal, overdose, by = c('year', 'state'))

# make selection of states
selected_states <- data_for_plot %>%
  group_by(state) %>% summarise(val = sum(status)) %>% 
  filter(val>0) %>% select(1)

# plot1
plot1 <- ggplot(subset(data_for_plot, state %in% c(selected_states[[1]])), 
                aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
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

plot1

# plot2
plot2 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = opioid_death_rate, col = factor(status))) + 
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

plot2

# plot3
plot3 <- data_for_plot %>% filter(year > 1998) %>%
  ggplot(aes(x = factor(year), y = alldrugs_death_rate, col = factor(status))) + 
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

plot3


# summary statistics
#install.packages("psych")
# make summary
data <- panel_data_clear %>% select(-c(1, 3:4)) %>% group_by(year)
summary_statistics_by_year <- data.frame(describeBy(data, group="year", mat=TRUE, type=0, digits=2)) %>% select(2, 4:7, 10:11)
# delete unnecessary summary for year
summary_statistics_by_year <- summary_statistics_by_year[-c(1:21),] %>% drop_na()
# edit name of the column
names(summary)[1] <- "year"
# add column of variables and change order of the columns
summary_statistics_by_year$variable <- row.names(summary_statistics_by_year)
summary_statistics_by_year <- summary_statistics_by_year[c(8, 1:7)]

# s <- summary_statistics_by_year %>% filter(year %in% c(2000, 2005, 2010, 2015))

# that's all
