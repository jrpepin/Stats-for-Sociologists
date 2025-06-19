## R Script for Creating dataset for Stats undergrad course

# Install 'gssr' from 'ropensci' universe
install.packages('gssr', repos =
                   c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

# Also recommended: install 'gssrdoc' as well
install.packages('gssrdoc', repos =
                   c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

# install.packages("pacman")       # Install pacman package if not installed
library("pacman")                  # Load pacman package

# Install packages not yet installed & load them
pacman::p_load(
  tidyverse,
  summarytools,
  gssr,
  gssrdoc,
  haven,
  readxl,
  expss,
  gtsummary,
  jtools,
  ggpmisc, # linear regression line on plots
  conflicted,
  sjlabelled,
  gtsummary,
  srvyr,
  openxlsx # import Guttmacher data
)


conflict_scout() # Identify the conflicts
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("zap_labels", "haven")
conflict_prefer("vars", "dplyr")

## Load all gss
data(gss_all)
data(gss_dict)

gss18 <- gss_get_yr(2018)
gss24 <- gss_get_yr(2024)

# Class 1 ----------------------------------------------------------------------
data01 <- gss_all %>%
  select(id, year, sex, premarsx)

data01$premarsx <- labelled::to_character(data01$premarsx)
data01$sex <- labelled::to_character(data01$sex)

data01 %>%
  filter(year == 2024, !is.na(premarsx)) %>%
  count(premarsx, sort = TRUE)

data01 %>%
  filter(!is.na(premarsx), !is.na(sex)) %>%
  group_by(sex) %>%
  count(premarsx, sort = TRUE) %>%
  pivot_wider(
    names_from = sex, # Make 'sex' the column headers
    values_from = n) # Fill with the count values



# Lab 01 -----------------------------------------------------------------------
dataL01 <- gss_all %>%
  select(id, year, age, sex, race, polviews, fefam, happy, premarsx)

dataL01 <- zap_labels(dataL01)

### Polviews
dataL01$polviews <- as.character(dataL01$polviews)

dataL01 <- dataL01 %>%
  mutate(
    polviews = case_when(
      polviews   == "1"   ~ "Extremely liberal",
      polviews   == "2"   ~ "Liberal",
      polviews   == "3"   ~ "Slightly liberal",
      polviews   == "4"   ~ "Moderate, middle of the road",
      polviews   == "5"   ~ "Slightly conservative",
      polviews   == "6"   ~ "Conservative",
      polviews   == "7"   ~ "Extremely conservative",
      TRUE              ~ NA_character_))

dataL01$polviews <- factor(dataL01$polviews, 
                           levels = c("Extremely liberal", "Liberal", "Slightly liberal",
                                      "Moderate, middle of the road", "Slightly conservative",
                                      "Conservative", "Extremely conservative"), ordered = FALSE) 
### Fefam
dataL01$fefam <- as.character(dataL01$fefam)

dataL01 <- dataL01 %>%
  mutate(
    fefam = case_when(
      fefam   == "1"   ~ "Strongly agree",
      fefam   == "2"   ~ "Agree",
      fefam   == "3"   ~ "Disagree",
      fefam   == "4"   ~ "Strongly disagree",
     TRUE              ~ NA_character_))

dataL01$fefam <- factor(dataL01$fefam, 
                           levels = c("Strongly agree", "Agree", 
                                      "Disagree", "Strongly disagree"), ordered = FALSE) 

write.csv(dataL01, "Lab01/data/dataL01.csv")


### Lab 01 figures
dataL01$gender <- factor(dataL01$sex)
levels(dataL01$gender)[1] <- "Men"
levels(dataL01$gender)[2] <- "Women"

percent_dataL01 <- dataL01 %>%
  filter(premarsx <=4) %>%
  count(year, gender, premarsx) %>%
  group_by(year, gender) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()

figL01 <- percent_dataL01 %>%
  filter(premarsx == 4 & year >= 2010) %>%
  drop_na(gender) %>%
  ggplot(aes(x=gender, y=percent, fill=gender)) +
  geom_bar(stat="identity") +
  ylim(0, 80) +
  theme_dark() +
  theme(legend.position="none") +
  labs( x        = "Gender", 
        y        = "%", 
        fill     = " ",
        title    = "Percent of respondents who think premarital sex is 'not wrong at all'",
        caption  = "Source: GSS 2010-2018")

figL01

ggsave("Lab01/images/figL01.png", figL01, height = 6, width = 6.5, dpi = 300)


# Lab 02 -------------------------------------------------------------------------
dataL02 <- gss_all %>%
  select(attend, marital, sibs, degree, sex, year)

dataL02 <- zap_labels(dataL02)

## gender
dataL02$gender <- factor(dataL02$sex)
levels(dataL02$gender)[1] <- "Men"
levels(dataL02$gender)[2] <- "Women"


## attend
dataL02 <- dataL02 %>%
  mutate(
    attend = case_when(
      attend == "0" ~ "Never",
      attend == "1" ~ "Less than once a year",
      attend == "2" ~ "About once or twice a year",
      attend == "3" ~ "Several times a year",
      attend == "4" ~ "About once a month",
      attend == "5" ~ "2-3 times a month",
      attend == "6" ~ "Nearly every week",
      attend == "7" ~ "Every week",
      attend == "8" ~ "Several times a week",
      TRUE              ~ NA_character_))

dataL02$attend <- factor(dataL02$attend, 
                         levels = c("Never", "Less than once a year", 
                                    "About once or twice a year", 
                                    "Several times a year", 
                                    "About once a month", 
                                    "2-3 times a month", 
                                    "Nearly every week", 
                                    "Every week", 
                                    "Several times a week"), 
                         ordered = FALSE)

## Marital
dataL02 <- dataL02 %>%
  mutate(
    marital = case_when(
      marital   == "1"   ~ "Married",
      marital   == "2"   ~ "Widowed",
      marital   == "3"   ~ "Divorced",
      marital   == "4"   ~ "Separated",
      marital   == "5"   ~ "Never married",
      TRUE              ~ NA_character_))

dataL02$marital <- factor(dataL02$marital, 
                         levels = c("Married", "Widowed", "Divorced", 
                                    "Separated", "Never married"), ordered = FALSE) 

## Degree
dataL02$degree <- as.character(dataL02$degree)
 
dataL02 <- dataL02 %>%
   mutate(
     degree = case_when(
       degree   == "0"   ~ "Less than high school",
       degree   == "1"   ~ "High school",
       degree   == "2"   ~ "Associate/Junior college",
       degree   == "3"   ~ "Bachelor's",
       degree   == "4"   ~ "Graduate",
       TRUE              ~ NA_character_))
 
dataL02$degree <- factor(dataL02$degree, 
                          levels = c("Less than high school", "High school", "Associate/Junior college", 
                                     "Bachelor's", "Graduate"), ordered = FALSE) 

write.csv(dataL02, "Lab02/data/dataL02.csv")

# Class 3 -------------------------------------------------------------

## Import the divorce
div.data <-  tibble(read_excel("Class 3/divorce_rates.xlsx"))

div.data$rate <- round(div.data$rate,digits=2)


## By year
div.data %>%
  group_by(year) %>%
  summarise(fivenum = fivenum(rate), 
            IQR     = IQR(rate, type = 5), 
            range   = (max(rate)-min(rate)),
            mean    = mean(rate),
            var     = var(rate),
            sd      = sd(rate))

## All
div.data %>%
  summarise(fivenum = fivenum(rate), 
            IQR     = IQR(rate, type = 5), 
            range   = (max(rate)-min(rate)),
            mean    = mean(rate),
            var     = var(rate),
            sd      = sd(rate))


by(div.data, div.data$year, summary)

div.data$year <- as.factor(div.data$year)

ggplot(div.data, aes(x=rate, fill =year)) + 
  geom_histogram(binwidth = 1.5, alpha = 0.4) +
  theme_minimal()


mar.data <-  tibble(read_excel("Class 3/marriage_rates.xlsx"))

mar.data$rate <- round(mar.data$rate,digits=2)

## All
mar.data %>%
  summarise(fivenum = fivenum(rate), 
            IQR     = IQR(rate, type = 5), 
            range   = (max(rate)-min(rate)),
            mean    = mean(rate),
            var     = var(rate),
            sd      = sd(rate))

# Lab 03 -------------------------------------------------------------------------
dataL03 <- gss_all %>%
  select(sex, hrs1) %>% 
  drop_na()

dataL03 <- zap_labels(dataL03)

write.csv(dataL03, "Lab03/data/dataL03.csv")

dataL03$gender <- factor(dataL03$sex)
levels(dataL03$gender)[1] <- "Men"
levels(dataL03$gender)[2] <- "Women"


dataL03 %>%
  summarise(IQR      = IQR(hrs1),
            VARIANCE = var(hrs1),
            SD       = sd(hrs1),
            RANGE    = (max(hrs1)-min(hrs1)),
            FIVENUM  = fivenum(hrs1),
            MEAN     = mean(hrs1))

ggplot(dataL03, aes(x=hrs1)) + 
  geom_boxplot() +
  facet_wrap(~gender)

ggplot(dataL03, aes(x=hrs1, y= gender, fill =gender)) + 
  geom_boxplot()


# Lab 04 -------------------------------------------------------------------------
dataL04 <- gss_all %>%
  select(year, agekdbrn) %>% 
  drop_na() %>%
  subset(year == 2018) %>%
  select(agekdbrn)

dataL04 <- zap_labels(dataL04)

write.csv(dataL04, "Lab04/data/dataL04.csv")


# Exam 1 Review -------------------------------------------------------------------------
mar.data <-  tibble(read_excel("datatest/marriage-rates.xlsx"))

mar.data$rate <- round(mar.data$rate,digits=2)

mar.data %>%
  group_by(year) %>%
  summarise(IQR      = IQR(rate),
            VARIANCE = var(rate),
            SD       = sd(rate),
            RANGE    = (max(rate)-min(rate)),
            FIVENUM  = fivenum(rate),
            MEAN     = mean(rate))


# Lab 05 -------------------------------------------------------------------------
dataL05 <- gss_all %>%
  select(year, sex, partyid, marital, cappun, owngun) %>%
  subset(year == 2018)

dataL05 <- zap_labels(dataL05)

dataL05$gender <- factor(dataL05$sex)
levels(dataL05$gender)[1] <- "Men"
levels(dataL05$gender)[2] <- "Women"

dataL05$cappun <- factor(dataL05$cappun)
levels(dataL05$cappun)[1] <- "Favor"
levels(dataL05$cappun)[2] <- "Oppose"

dataL05 <- dataL05 %>%
  mutate(partyid = case_when(
    partyid <= 2                 ~ "Democrat",
    partyid == 3                 ~ "Independent",
    partyid >= 4                 ~ "Republican")) %>%
  mutate(marital = case_when(
    marital == 1                 ~ "Married",
    marital == 5                 ~ "Never married",
    marital >= 2 & marital <= 4 ~ "Div./Sep./Widow")) %>%
  mutate(owngun = case_when(
    owngun == 1                ~ "Yes",
    owngun == 2                ~ "No"))

dataL05 <- dataL05 %>%
  select(gender, marital, partyid, cappun, owngun)

dataL05 <- na.omit(dataL05) # Drop missing cases

write.csv(dataL05, "Lab05/data/dataL05.csv")

cols <- c("partyid", "marital", "owngun")
dataL05[cols] <- lapply(dataL05[cols], factor)
dataL05$marital <- factor(dataL05$marital, levels = c("Married", "Never married", "Div./Sep./Widow"))

# Lab 06 -------------------------------------------------------------------------
dataL06 <- gss_all %>%
  select(year, sex, wwwhr, hrs1, hrsrelax)%>%
  subset(year == 2018)

dataL06 <- zap_labels(dataL06)

dataL06$gender <- factor(dataL06$sex)
levels(dataL06$gender)[1] <- "Men"
levels(dataL06$gender)[2] <- "Women"

dataL06 <- dataL06 %>%
  select(year, gender, wwwhr,  hrs1, hrsrelax)

write.csv(dataL06, "Lab06/data/dataL06.csv")

library(Rmisc) # To use CI() function
wwwhr <- dataL06 %>% 
  filter(!is.na(wwwhr)) %>%
  group_by(gender) %>%
  summarise(n     = n(),
            mean  = mean(wwwhr), 
            lowCI = CI(wwwhr)['lower'],
            hiCI  = CI(wwwhr)['upper'])

hrsrelax <- dataL06 %>% 
  filter(!is.na(hrsrelax)) %>%
  group_by(gender) %>%
  summarise(n     = n(),
            mean  = mean(hrsrelax), 
            lowCI = CI(hrsrelax)['lower'],
            hiCI  = CI(hrsrelax)['upper'])

hrs1 <- dataL06 %>% 
  filter(!is.na(hrs1)) %>%
  group_by(gender) %>%
  summarise(n     = n(),
            mean  = mean(hrs1), 
            lowCI = CI(hrs1)['lower'],
            hiCI  = CI(hrs1)['upper'])


wwwhr$variable    <- "wwwhr"
hrsrelax$variable <- "hrsrelax"
hrs1$variable     <- "hrs1"  

cidata <- full_join(wwwhr, hrs1)
cidata <- full_join(cidata, hrsrelax) 

write.csv(cidata, "Lab06/data/confi.csv")


# Lab 07 -------------------------------------------------------------------------
dataL07 <- gss_all %>%
  select(year, childs, gunlaw, educ) %>%
  drop_na() %>%
  subset(year == 2018)

dataL07 <- zap_labels(dataL07)

dataL07 <- dataL07 %>%
  select(childs, gunlaw, educ)

write.csv(dataL07, "Lab07/data/dataL07.csv")

dataL07 <- apply_labels(dataL07,
                        gunlaw = c("Favor"  = 1,
                                   "Oppose" = 2))

dataL07$gunlaw <- sjlabelled::as_factor(dataL07$gunlaw)

# Lab 08 -------------------------------------------------------------------------
dataL08 <- gss_all %>%
  select(year, sexfreq, age, sex, vpsu, vstrat, wtssall) %>%
  subset(year >= 1989) %>%
  drop_na() %>%
  drop_labels()

dataL08$sexfreq  <- as_label(dataL08$sexfreq)
dataL08$sex      <- as_label(dataL08$sex)
dataL08$age      <- as.numeric(dataL08$age)
dataL08$partners <- as_label(dataL08$partners)

dataL08 <- dataL08 %>%
  dplyr::mutate(agecat = case_when(
    age <  30             ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60             ~ "60s +"  ))

dataL08 <- dataL08 %>%
  dplyr::mutate(
    sexfreq2 = case_when(
    sexfreq == "not at all"         ~ "No sex",
    sexfreq == "once or twice"      |
    sexfreq == "once a month"       |
    sexfreq == "2 or 3 times a month"  ~ "Monthly sex",
    sexfreq == "about once a week"  |
    sexfreq == "2 or 3 times a week"|
    sexfreq == "more than 3 times a week" ~ "Weekly or more",
      TRUE                          ~ NA_character_
    ))

write.csv(dataL08, "Lab08/data/dataL08.csv")

dataL08$agecat <- factor(dataL08$agecat, 
                         levels = c("18-29", "30-39", "40-49", "50-59", "60s +"), 
                         ordered = FALSE)

dataL08$sexfreq2 <- factor(dataL08$sexfreq2, 
                           levels = c("Weekly or more", 
                                      "Monthly sex", 
                                      "No sex"), 
                           ordered = FALSE)

library("srvyr") # as_survey_design
options(survey.lonely.psu = "adjust") ## Set survey data
options(na.action="na.pass")
dataL08_svy <- dataL08 %>%
  dplyr::mutate(stratvar = interaction(year, vstrat)) %>%
  as_survey_design(id = vpsu,
                   strata = stratvar,
                   weights = wtssall,
                   nest = TRUE)

dataA <- dataL08_svy %>% # DATA A
  group_by(year, sexfreq) %>%
  dplyr::summarize(prop = survey_mean(vartype = "ci"),
            freq = n())
write.csv(dataA, "Lab08/data/dataA.csv")

dataB <- dataL08_svy %>% # DATA B
  group_by(year, sexfreq2) %>%
  dplyr::summarize(prop = survey_mean(vartype = "ci"),
            freq = n())
write.csv(dataB, "Lab08/data/dataB.csv")

dataC <- dataL08_svy %>%
  filter(age <= 29) %>%
  group_by(year, sexfreq2) %>%
  dplyr::summarize(prop = survey_mean(vartype = "ci"),
            freq = n())
write.csv(dataC, "Lab08/data/dataC.csv")
  
dataD <- dataL08_svy %>%
  filter(age <= 29) %>%
  group_by(year, sex, sexfreq2) %>%
  dplyr::summarize(prop = survey_mean(vartype = "ci"),
            freq = n())
write.csv(dataD, "Lab08/data/dataD.csv")


### Learning Check 08
#### SOURCE: https://www.cdc.gov/nchs/data/nvsr/nvsr70/nvsr70-02-tables-508.pdf#I06
year <- c("2019", "2018", "2017", "2016", "2015", 
          "2014", "2013", "2012", "2011", "2010")
age <- c(27.0, 26.9, 26.8, 26.6, 26.4, 
         26.3, 26.0, 25.8, 25.6, 25.4)
agekdbrn <- data.frame(year, age)

agekid1 <-  agekdbrn %>%
  ggplot(aes(x = year, y = age, fill = year)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        plot.caption       = element_text(color = "grey", face = "italic")) +
  coord_cartesian(ylim=c(0,30)) +
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30))
agekid1
ggsave("Class 8/images/agekid1.png", agekid1, dpi = 300)

agekid2 <-  agekdbrn %>%
  ggplot(aes(x = year, y = age, fill = year)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        plot.caption       = element_text(color = "grey", face = "italic")) +
  labs(title ="U.S. Women's Age \nwhen first child was born",
       caption = "National Vital Statistics Reports") +
  coord_cartesian(ylim=c(0,30)) +
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30))
agekid2
ggsave("Class 8/images/agekid2.png", agekid2, dpi = 300)

agekid3 <-  agekdbrn %>%
  ggplot(aes(x = year, y = age, fill = year)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        plot.caption       = element_text(color = "grey", face = "italic")) +
  labs(title ="U.S. Women's Age \nwhen first child was born",
       caption = "National Vital Statistics Reports") +
  coord_cartesian(ylim=c(20,30)) +
  scale_y_continuous(breaks=c(20, 22.5, 25, 27.5, 30))
agekid3
ggsave("Class 8/images/agekid3.png", agekid3, dpi = 300)

library(cowplot)

agekdbrn$year <- as.numeric(agekdbrn$year)

a1 <- agekdbrn %>%
  ggplot(aes(x = year, y = age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        plot.caption       = element_text(color = "grey", face = "italic")) +
  labs(title ="U.S. Women's Age \nwhen first child was born",
       caption = "National Vital Statistics Reports") +
  coord_cartesian(ylim=c(0,30)) +
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30)) +
  scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018))
a1

a2 <- agekdbrn %>%
  ggplot(aes(x = year, y = age, group = 1)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        plot.caption       = element_text(color = "grey", face = "italic")) +
  labs(title ="U.S. Women's Age \nwhen first child was born",
       caption = "National Vital Statistics Reports") +
  coord_cartesian(ylim=c(20,30))  +
  scale_y_continuous(breaks=c(20, 22.5, 25, 27.5, 30)) +
  scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018))
a2

agekid4 <- plot_grid(a1, a2)

ggsave("Class 8/images/agekid4.png", agekid4, width = 5.5, height = 4, dpi = 300)

# Lab 09 -------------------------------------------------------------------------
dataL09 <- gss_all %>%
  select(year, sex, partyid, age, cappun, fefam) %>%
  subset(year == 2018)

dataL09 <- zap_labels(dataL09)

dataL09$gender <- factor(dataL09$sex)
levels(dataL09$gender)[1] <- "Men"
levels(dataL09$gender)[2] <- "Women"

dataL09$cappun <- factor(dataL09$cappun)
levels(dataL09$cappun)[1] <- "Favor"
levels(dataL09$cappun)[2] <- "Oppose"

dataL09 <- dataL09 %>%
  dplyr::mutate(partyid = case_when(
    partyid <= 2                 ~ "Democrat",
    partyid == 3                 ~ "Independent",
    partyid >= 4                 ~ "Republican")) %>%
  dplyr::mutate(fefam = case_when(
    fefam == 1 | fefam == 2      ~ "Agree",
    fefam == 3 | fefam == 4      ~ "Disagree")) 

dataL09 <- dataL09 %>%
  select(gender, partyid, age, cappun, fefam)

dataL09 <- na.omit(dataL09) # Drop missing cases

write.csv(dataL09, "Lab09/data/dataL09.csv")

cols <- c("gender", "partyid", "cappun", "fefam")
dataL09[cols] <- lapply(dataL09[cols], factor)


# Lab 10 -----------------------------------------------------------------------

dataL10 <- gss_all %>%
  select(year, age, weight, height, lifenow, marital) %>% 
  filter(year == 2014 | year == 2018)

dataL10  <- zap_labels(dataL10 )

dataL10 <- dataL10 %>%
  dplyr::mutate(marital = case_when(
    marital == 1                 ~ "Married",
    marital == 5                 ~ "Never married",
    marital >= 2 & marital <= 4 ~ "Div./Sep./Widow"))
  
dataL10$marital <- factor(dataL10$marital, levels = c("Never married", "Married", "Div./Sep./Widow"))

write.csv(dataL10, "Lab10/data/dataL10.csv")

# Chi-square Review ------------------------------------------------------------
data(gss_all) # reload all years of the GSS

gss_all <- gss_all %>%
  filter(year ==2021)

gss_all$abany   <-  sjlabelled::as_label(gss_all$abany) # apply labels
gss_all$degree <-  sjlabelled::as_label(gss_all$degree) # apply labels
gss_all$sex <-  sjlabelled::as_label(gss_all$sex) # apply labels


gss_all <- gss_all %>%
  dplyr::mutate(abdum = case_when(
    abany == "no"                ~ "no",
    abany == "yes"               ~ "yes")) %>%
  dplyr::mutate(badum = case_when(
    degree == "bachelor's" | degree == "graduate"      ~ "College",
    degree == "less than high school" | degree == "high school" | degree == "associate/junior college"  ~ "Less than BA")) 

## women by education
gss_all %>%
  filter(sex == "female") %>%
  tbl_cross(row = abdum, 
            col = badum,
            percent = "column",
            missing = "no") %>%
  add_p()

# Regression Review ------------------------------------------------------------
data(gss_all) # reload all years of the GSS

gss_all <- gss_all %>%
  filter(year ==2021)

gss_all  <- zap_labels(gss_all )

my.formula <- y ~ x # For equation on ggplot

review01 <- gss_all %>%
  filter(!is.na(agekdbrn)) %>%
  filter(!is.na(educ)) %>%
  ggplot(aes(x = educ, y = agekdbrn)) +
  geom_jitter(alpha = 0.2, colour = "#18BC9C") +
  geom_smooth(method = "lm", size = 1.5, colour = "#3498DB", fill = "#3498DB", se=FALSE) +
  theme_minimal() +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = my.formula, parse = TRUE, label.y = .5)  +
  labs( x        = "Years of education", 
        y        = "Age", 
        title    = "Age when first child was born ",
        subtitle = "By years of education",
        caption  = "General Social Survey (2021)")

review01

ggsave("Exam/images/exam03_review01.png", review01, width = 5.5, height = 4, dpi = 300)

model <- lm(agekdbrn ~ educ, data = gss_all)
summ(model, digits = 3)


## Show with categorical variable

## Degree
gss_all$degree <- as.character(gss_all$degree)

gss_all <- gss_all %>%
  mutate(
    degree = case_when(
      degree   == "0"   ~ "Less than high school",
      degree   == "1"   ~ "High school",
      degree   == "2"   ~ "Associate/Junior college",
      degree   == "3"   ~ "Bachelor's",
      degree   == "4"   ~ "Graduate",
      TRUE              ~ NA_character_))

gss_all$degree <- factor(gss_all$degree, 
                         levels = c("High school", "Less than high school", "Associate/Junior college", 
                                    "Bachelor's", "Graduate"), ordered = FALSE) 

model <- lm(agekdbrn ~ degree, data = gss_all) # reference is high school
summary(model, digits = 3)

## Example 02
### Rates are the number of abortions per 1,000 women aged 15-44; 
### numerator includes all abortions to residents of each state, regardless of age at outcome.
abortion = read.xlsx("https://data.guttmacher.org/download?locationType=state&visualization=trending&measures=81&topic=68",sheet=1)

abortion <- abortion[-c(1, 10, 53:55), ] # remove row that aren't states
rownames(abortion) = seq(length=nrow(abortion)) # renumber rows
names
names(abortion) <- substring(names(abortion), nchar(names(abortion))-3)
names(abortion)[names(abortion) == 'tate'] <- "state"
names(abortion)[names(abortion) == '.[1]'] <- "2017"

abortion <- abortion %>%
  pivot_longer(!state, names_to = "year", values_to = "rate")

abortion$rate  <- as.numeric(abortion$rate)
abortion$year  <- as.numeric(abortion$year)
abortion$state <- as.factor(abortion$state)

my.formula <- y ~ x # For equation on ggplot

review02 <- abortion %>%
  ggplot(aes(x = year, y = rate)) +
  geom_jitter(alpha = 0.2, colour = "#18BC9C") +
  geom_smooth(method = "lm", size = 1.5, colour = "#3498DB", fill = "#3498DB", se=FALSE) +
  theme_minimal() +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = my.formula, parse = TRUE, label.y = .5)  +
  scale_x_continuous(limits = c(1985, 2020)) +
  labs( x        = "Year", 
        y        = "Rate", 
        title    = "U.S. Abortion rate by state of residence (1988 - 2017)",
        subtitle = "number of abortions per 1,000 women aged 15-44",
        caption  = "Guttmacher Institute, 2021
        https://data.guttmacher.org/states/trend?state=NY&topics=68&dataset=data")

review02

ggsave("Exam/images/exam03_review02.png", review02, width = 5.5, height = 4, dpi = 300)


model <- lm(rate ~ year, data = abortion)
summary(model, digits = 3)


# ## Chi-square
# data(gss_all) # reload all years of the GSS
# 
# gss_all$abany <-  sjlabelled::as_label(gss_all$abany) # apply labels
# 
# gss_all$abany <-  car::recode(gss_all$abany, "'no'=0; 'yes'=1;", as.factor=FALSE)
# 
# options(survey.lonely.psu = "adjust")
# options(na.action="na.pass")
# 
# ### combining survey weights...bad idea? probably...
# gss_all  <- gss_all  %>%
#   mutate(
#     svyweight = case_when(
#       year != 2021   ~ wtssall,
#       year == 2021   ~ wtssps
#     ))
# 
# gss_svy <- gss_all %>%
#   filter(year > 1976 & year != 1986) %>%
#   drop_na(abany) %>%
#   mutate(stratvar = interaction(year, vstrat)) %>%
#   as_survey_design(id = vpsu,
#                    strata = stratvar,
#                    weights = svyweight,
#                    nest = TRUE)
# 
# abany_yr <- gss_svy %>%
#   group_by(year, abany) %>%
#   summarize(prop = survey_mean(na.rm = TRUE, vartype = "ci"))
# 
# 
# my.formula <- y ~ x # For equation on ggplot
# 
# 
# review02 <- abany_yr %>%
#   filter(abany == 1) %>%
#   ggplot(aes(x = year, y = prop)) +
#   geom_jitter(alpha = 0.8, colour = "#18BC9C") +
#   geom_smooth(method = "lm", size = 1.5, colour = "#3498DB", fill = "#3498DB", se=FALSE) +
#   theme_minimal() +
#   stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
#                eq.with.lhs = "italic(hat(y))~`=`~",
#                eq.x.rhs = "~italic(x)",
#                formula = my.formula, parse = TRUE, label.y = .5)  +
#   labs( x        = "Years of education", 
#         y        = "Age", 
#         title    = "Age when first child was born ",
#         subtitle = "By years of education",
#         caption  = "General Social Survey (2021)")
# 
# review02


# Exam 03 ----------------------------------------------------------------------
data(gss_all) # reload all years of the GSS

exam03 <- gss_all %>%
  select(year, hompop, age, sex) %>% 
  filter(year == 2018)

exam03  <- zap_labels(exam03 )

write.csv(exam03, "Exam/data/exam03.csv")