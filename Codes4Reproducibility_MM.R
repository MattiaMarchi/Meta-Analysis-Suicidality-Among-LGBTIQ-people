###----------------------------------------------------------------------------------------
###-------------------------Meta Analysis - LGBTIQ Suicidality-----------------------------
###----------------------------------------------------------------------------------------
library(meta)
library(metafor)
library(tidyverse)

###----------------------------------------------------------------------------------------
###-------------------------1. Meta Analysis of Suicide Attempts---------------------------
###----------------------------------------------------------------------------------------
#Import data
SA_data <- structure(list(ID = structure(1:35, .Label = c("Anderssen et al. 2020", 
                                                          "Berona et al. 2020", "Bettis et al. 2020", "Björkenstam et al. 2016", 
                                                          "Bouris et al. 2016", "Bränström et al. 2020", "Clark et al. 2014", 
                                                          "Clements-Nolle et al. 2018", "Coulter et al. 2017", "Cramer et al. 2020", 
                                                          "Dorrell et al. 2021", "Duncan et al. 2014", "Eisenberg et al. 2017", 
                                                          "El-Khoury et al. 2020", "Flynn et al. 2016", "Gambadauro et al. 2020", 
                                                          "Goodin et al. 2019", "Horwitz et al. 2021", "Huang et al. 2018", 
                                                          "Humphries et al. 2021", "Irish et al. 2019", "Johns et al. 2021", 
                                                          "Layland et al. 2020", "Lytle et al. 2018", "Perales et al. 2019", 
                                                          "Peters et al. 2020", "Porta et al. 2018", "Progovac et al. 2020", 
                                                          "Rentería et al. 2021", "Schwartz et al. 2021", "Thoma et al. 2019", 
                                                          "Tsypes et al. 2016", "Wang et al. 2021", "Wichaidit et al. 2021", 
                                                          "Wu et al. 2021"), class = "factor"),
                          Events_LGBTIQ = c(26L, 49L, 122L, 196L, 74L, 54L, 52L, 135L, 318L, 57L, 52L, 17L, 609L, 16L, 91L, 31L, 63L, 379L, 713L,
                                            533L, 41L, 320L, 369L, 247L, 32L, 16L, 3195L, 351L, 17L, 56L, 577L, 44L, 43L, 743L, 12L), 
                          Total_LGBTIQ = c(115L, 119L, 178L, 1715L, 247L, 1281L, 435L, 498L, 1832L, 261L, 533L, 102L, 2168L, 1291L, 577L, 214L,
                                           282L, 963L, 6685L, 2740L, 288L, 1434L, 2591L, 1414L, 231L, 27L, 10863L, 8696L, 595L, 172L, 1148L, 830L, 1032L, 755L, 70L),
                          Events_Controls = c(1919L, 53L, 115L, 2096L, 206L, 232L, 315L, 288L, 714L, 12L, 53L, 26L, 5286L, 85L, 1154L,
                                              104L, 136L, 358L, 3231L, 1229L, 85L, 634L, 1184L, 969L, 97L, 11L, 1264L, 8665L, 33L, 84L, 274L, 130L, 73L, 30243L, 7L), 
                          Total_Controls = c(49836L, 166L, 266L, 67980L, 1659L, 38667L, 7729L, 4230L, 21002L, 73L, 1545L, 1071L, 78761L, 23907L,
                                             33598L, 1744L, 2244L, 5148L, 116774L, 24966L, 1934L, 11745L, 22959L, 19576L, 2950L, 25L, 81197L, 2235809L, 6653L, 920L,
                                             872L, 5221L, 7281L, 31143L, 515L),
                          Adults = structure(c(3L, 3L, 2L, 3L, 2L, 3L, 1L, 2L, 2L, 1L, 3L, 2L, 2L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 2L,
                                               3L, 3L, 2L, 3L), .Label = c("", "No", "Yes"), class = "factor"), 
                          Country = structure(c(6L, 12L, 12L, 8L, 12L, 8L, 7L, 12L, 12L, 12L, 12L, 12L, 12L, 4L, 12L, 1L, 12L, 12L, 3L, 12L, 11L, 12L, 12L, 12L, 2L,
                                                11L, 12L, 12L, 5L, 12L, 12L, 12L, 3L, 10L, 9L),
                                              .Label = c("", "Australia", "China", "France", "Mexico", "Norway", "NZ", "Sweden", "Taiwan", "Thailand", "UK", "US"), class = "factor"),
                          Continent = structure(c(3L, 1L, 1L, 3L, 1L, 3L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L, 3L, 1L, 1L, 2L, 1L, 3L, 1L, 1L, 1L, 4L, 3L, 1L, 1L, 1L, 1L, 1L,
                                                  1L, 2L, 2L, 2L), .Label = c("Americas", "Asia", "EU", "Oceania"), class = "factor"),
                          Weight = structure(c(5L, 5L, 6L, 7L, 6L, 6L, 6L, 7L, 7L, 3L, 6L, 3L, 7L, 4L, 7L, 5L, 6L, 7L, 7L, 7L, 6L, 7L, 7L, 7L, 5L, 1L, 7L, 7L, 4L, 6L, 7L, 6L, 6L, 4L, 2L),
                                             .Label = c("0.02", "0.022", "0.026", "0.027", "0.028", "0.029", "0.03"), class = "factor"),
                          Females._Controls = structure(c(17L, 1L, 2L, 14L, 1L, 10L, 13L, 3L, 6L, 1L, 1L, 9L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 1L, 1L, 1L, 1L, 16L, 4L, 15L, 1L, 1L, 8L, 1L, 1L, 11L, 1L, 12L, 7L),
                                                        .Label = c("", "44.4", "45.1", "47", "47.2", "50.8", "51.3", "52.3", "53.5", "54", "54.4", "54.9", "55", "56", "60", "63.2", "69.1"), class = "factor"),
                          Females. = structure(c(1L, 12L, 1L, 1L, 4L, 1L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 10L, 13L, 11L, 7L, 14L, 1L, 4L, 8L, 7L, 6L, 1L, 1L, 1L, 5L, 1L, 1L, 15L, 9L, 1L, 1L, 1L, 1L),
                                               .Label = c("", "100", "18.6", "49.7", "50.3", "50.8", "51.3", "53.3", "54", "54.1", "56.8", "57.9", "58.9", "59", "79.6"), class = "factor")),
                     row.names = c(NA, -35L), class = "data.frame")
#Calculate Effect Size
SA_dataES <- escalc(measure = "OR",
                    ai = Events_LGBTIQ, ci = Events_Controls,
                    n1i = Total_LGBTIQ, n2i = Total_Controls,
                    data = SA_data)
#Pooling ES
m_re <- rma(yi = SA_dataES$yi, vi = SA_dataES$vi)
m_re
#Forest Plot
forest(m_re, slab = SA_dataES$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re, xlab = "Hedges' g")
#Egger's test
regtest(m_re)
#--------------------------------Meta Regression
#----Univariate
SA_dataES$Females. <- as.character(SA_dataES$Females.)
SA_dataES$Females. <- as.numeric(SA_dataES$Females.)
SA_dataES$Weight <- as.character(SA_dataES$Weight)
SA_dataES$Weight <- as.numeric(SA_dataES$Weight)
str(SA_dataES)
#Age: Adults (YES/NO)
m_multi_age <- rma(yi = yi,
                   vi = vi,
                   mods = ~ Adults,
                   data = SA_dataES)
m_multi_age
#Country
m_multi_country <- rma(yi = yi,
                       vi = vi,
                       mods = ~ Country,
                       data = SA_dataES)
m_multi_country
#Continent
m_multi_continent <- rma(yi = yi,
                         vi = vi,
                         mods = ~ Continent,
                         data = SA_dataES)
m_multi_continent
#Sex
m_multi_sex <- rma(yi = yi,
                   vi = vi,
                   mods = ~ Females.,
                   data = SA_dataES)
m_multi_sex
#Study Weight
m_multi_weight <- rma(yi = yi,
                      vi = vi,
                      mods = ~ Weight,
                      data = SA_dataES)
m_multi_weight
#----Multivariate
SA_dataES$Females.
m_multi <- rma(yi = yi,
               vi = vi,
               mods = ~ Weight + Adults + Continent + Females.,
               data = SA_dataES)
m_multi

###----------------------------------------------------------------------------------------
###-------------------------2. Meta Analysis of Suicide Ideation---------------------------
###----------------------------------------------------------------------------------------
#Import data
SI_data <- structure(list(ID = structure(c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L,
                                           23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                                         .Label = c("", "Anderssen et al. 2020", "Backhaus et al. 2021", "Björkenstam et al. 2016", "Blosnich et al. 2020", "Bouris et al. 2016", "Bränström et al. 2020",
                                                    "Clements-Nolle et al. 2018", "Coulter et al. 2017", "Cramer et al. 2020", "Dorrell et al. 2021", "Duncan et al. 2014", "Eisenberg et al. 2017",
                                                    "El-Khoury et al. 2020", "Gilmour et al. 2019", "Goodin et al. 2019", "Guadamuz et al. 2019", "Higgins Tejera et al. 2019", "Horwitz et al. 2021",
                                                    "Huang et al. 2018", "Lytle et al. 2018", "Mereish et al. 2019", "Nam et al. 2019", "Parr et al. 2020", "Perales et al. 2019",
                                                    "Perez-Brumer et al. 2017", "Porta et al. 2018", "Progovac et al. 2020", "Proulx et al. 2019", "Rentería et al. 2021", "Schwartz et al. 2021",
                                                    "Thoma et al. 2019", "Tsypes et al. 2016", "Wichaidit et al. 2021", "Wu et al. 2021"), class = "factor"),
                          Events_LGBTIQ = c(73L, 4177L, 498L, 602L, 69L, 225L, 214L, 627L, 154L, 216L, 33L, 1202L, 138L, 121L, 109L, 76L, 405L, 781L, 2426L, 148L, 25L, 46L, 594L,
                                            67L, 2467L, 5246L, 1051L, 2319L, 126L, 110L, 974L, 61L, 529L, 26L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Total_LGBTIQ = c(115L, 12060L, 1715L, 2679L, 247L, 1281L, 498L, 1832L, 261L, 533L, 102L, 2168L, 1291L, 1280L, 282L, 489L, 926L, 963L, 6685L, 1414L, 66L, 150L, 1523L, 231L, 7653L,
                                           10863L, 8696L, 6741L, 595L, 172L, 1148L, 830L, 755L, 70L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Events_Controls = c(10067L, 6671L, 7881L, 1164L, 226L, 2409L, 617L, 2289L, 36L, 286L, 101L, 14812L, 1064L, 1030L, 285L, 117L, 1246L, 1086L, 20261L, 721L, 94L, 131L, 5572L, 204L,
                                              94881L, 32602L, 25553L, 5107L, 599L, 214L, 527L, 270L, 20892L, 65L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Total_Controls = c(49836L, 48134L, 67980L, 37471L, 1659L, 48180L, 4230L, 21002L, 73L, 1545L, 1071L, 78761L, 23907L, 44775L, 2244L, 1581L, 9469L, 5148L, 116774L, 19576L, 839L,
                                             1430L, 48915L, 2950L, 567203L, 81197L, 2235809L, 43331L, 6653L, 920L, 872L, 5221L, 31143L, 515L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Weight = structure(c(4L, 7L, 6L, 6L, 5L, 6L, 6L, 6L, 2L, 6L, 3L, 7L, 6L, 6L, 5L, 5L, 6L, 6L, 7L, 6L, 2L, 4L, 6L, 5L, 7L, 7L, 7L, 7L, 6L, 4L, 6L,
                                               5L, 6L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "0.026", "0.027", "0.028", "0.029", "0.03", "0.031"), class = "factor"),
                          Adults = structure(c(3L, 3L, 3L, 3L, 2L, 3L, 2L, 2L, 1L, 3L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 2L, 1L, 1L, 2L, 2L, 2L, 3L, 1L, 3L, 3L, 2L,  3L, 2L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                               1L, 1L), .Label = c("", "No", "Yes"), class = "factor"), 
                          Country = structure(c(7L, 11L, 8L, 11L, 11L, 8L, 11L, 11L, 11L, 11L, 11L, 11L, 5L, 3L, 11L, 10L, 11L, 11L, 4L, 11L,  11L, 11L, 11L, 2L, 11L, 11L, 11L, 11L, 6L, 11L, 11L, 11L,
                                                10L, 9L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "Australia", "Canada", "China", "France", 
                                                                    "Mexico", "Norway", "Sweden", "Taiwan", "Thailand", "US"), class = "factor"), 
                          Continent = structure(c(4L, 2L, 4L, 2L, 2L, 4L, 2L, 2L, 2L, 2L, 2L, 2L, 4L, 2L, 2L, 3L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 5L,  2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 1L, 1L, 1L, 1L, 1L,
                                                  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "Americas", "Asia", "EU", "Oceania"), class = "factor"),
                          Females. = structure(c(1L, 1L, 1L, 1L, 4L, 1L, 1L, 1L, 3L, 2L, 1L, 1L, 8L, 1L, 6L, 1L, 1L, 9L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 1L, 1L, 1L, 10L,
                                                 7L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,  1L, 1L, 1L), .Label = c("", "100", "18.6", "49.7", "50.3", "51.3", "54", "54.1", "59.0", "79.6"), class = "factor")),
                     row.names = c(NA, -48L), class = "data.frame")
#Calculate Effect Size
SI_dataES <- escalc(measure = "OR",
                    ai = Events_LGBTIQ, ci = Events_Controls,
                    n1i = Total_LGBTIQ, n2i = Total_Controls,
                    data = SI_data)
#Pooling ES
m_re.SI <- rma(yi = SI_dataES$yi, vi = SI_dataES$vi)
m_re.SI
#Forest Plot
forest(m_re.SI, slab = SI_dataES$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SI, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SI)
#--------------------------------Meta Regression
#----Univariate
SI_dataES$Females. <- as.character(SI_dataES$Females.)
SI_dataES$Females. <- as.numeric(SI_dataES$Females.)
SI_dataES$Weight <- as.character(SI_dataES$Weight)
SI_dataES$Weight <- as.numeric(SI_dataES$Weight)
str(SI_dataES)
#Age: Adults (YES/NO)
m_multi_age.SI <- rma(yi = yi,
                      vi = vi,
                      mods = ~ Adults,
                      data = SI_dataES)
m_multi_age.SI
#Country
m_multi_country.SI <- rma(yi = yi,
                          vi = vi,
                          mods = ~ Country,
                          data = SI_dataES)
m_multi_country.SI
#Continent
m_multi_continent.SI <- rma(yi = yi,
                            vi = vi,
                            mods = ~ Continent,
                            data = SI_dataES)
m_multi_continent.SI
#Sex
m_multi_sex.SI <- rma(yi = yi,
                      vi = vi,
                      mods = ~ Females.,
                      data = SI_dataES)
m_multi_sex.SI
#Study Weight
m_multi_weight.SI <- rma(yi = yi,
                         vi = vi,
                         mods = ~ Weight,
                         data = SI_dataES)
m_multi_weight.SI
#----Multivariate
SI_dataES$Females.
m_multi.SI <- rma(yi = yi,
                  vi = vi,
                  mods = ~ Weight + Adults + Continent + Females.,
                  data = SI_dataES)
m_multi.SI

###----------------------------------------------------------------------------------------
###-------------------------3. Meta Analysis of Self Harm----------------------------------
###----------------------------------------------------------------------------------------
#Import data
SH_data <- structure(list(ID = structure(c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 1L,
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "Amos et al. 2020", 
                                                                                               "Anderssen et al. 2020", "Berona et al. 2020", "Bettis et al. 2020", 
                                                                                               "Clark et al. 2014", "Davey et al. 2016", "Eisenberg et al. 2017", 
                                                                                               "Fraser et al. 2018", "Goodin et al. 2019", "Irish et al. 2019", 
                                                                                               "Li et al. 2019", "Parr et al. 2020", "Perales et al. 2019", 
                                                                                               "Peters et al. 2020", "Quarshie et al. 2020", "Rentería et al. 2021", 
                                                                                               "Schwartz et al. 2021", "Thoma et al. 2019", "Tsypes et al. 2016", 
                                                                                               "Wang et al. 2021"), class = "factor"),
                          Events_LGBTIQ = c(336L, 64L, 94L, 138L, 155L, 18L, 1076L, 86L, 152L, 75L, 30L, 944L, 62L, 27L, 35L, 83L, 89L, 998L, 244L, 144L, NA, NA, NA, NA, NA,
                                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,  NA, NA, NA, NA, NA, NA),
                          Total_LGBTIQ = c(624L, 115L, 119L, 178L, 435L, 97L, 2168L, 198L, 282L, 288L, 310L, 1523L, 231L, 27L, 74L, 595L, 172L, 1148L, 830L, 1032L, NA, NA, NA, NA, NA, NA, NA, NA,
                                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Events_Controls = c(1307L, 8522L, 99L, 123L, 1809L, 4L, 10650L, 285L, 342L, 235L, 93L, 9676L, 230L, 17L, 85L, 279L, 28L, 515L, 1117L, 299L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Total_Controls = c(9206L, 49836L, 166L, 266L, 7729L, 97L, 78761L, 1601L, 2244L, 1934L, 1500L, 48915L, 2950L, 25L, 370L, 6653L, 920L, 872L, 5221L, 7281L, NA, NA, NA, NA, NA, NA, NA,
                                             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                          Weight = structure(c(14L, 9L, 4L, 8L, 13L, 3L, 14L, 11L, 12L, 11L, 7L, 14L, 10L, 2L, 5L, 12L, 6L, 13L, 14L, 13L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                               1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "0.007", "0.029", "0.047", "0.048", "0.049", "0.05", "0.051", "0.052", "0.053", "0.054", "0.055", "0.056", "0.057"), class = "factor"), 
                          Adults = structure(c(2L, 3L, 3L, 2L, 1L, 3L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 3L, 3L, 2L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                               1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "No", "Yes"), class = "factor"),
                          Country = structure(c(8L, 6L, 9L, 9L, 7L, 8L, 9L, 7L, 9L, 8L, 3L, 9L, 2L, 8L, 4L, 5L, 9L, 9L, 9L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("", "Australia", "China", "Ghana", "Mexico", "Norway", "NZ", "UK", "US"), class = "factor"),
                          Continent = structure(c(5L, 5L, 3L, 3L, 6L, 5L, 3L, 6L, 3L, 5L, 4L, 3L, 6L, 5L, 2L, 3L, 3L, 3L, 3L, 4L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                  1L), .Label = c("", "Africa", "Americas", "Asia", "EU", "Oceania"), class = "factor"),
                          Females. = structure(c(1L, 1L, 7L, 1L, 1L, 1L, 1L, 6L, 3L, 4L, 2L, 1L, 1L, 1L, 1L, 1L, 8L, 5L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
                                               .Label = c("", "44.5", "51.3", "53.3", "54", "56.4", "57.9", "79.6"), class = "factor")), row.names = c(NA, -47L), class = "data.frame")
#Calculating Effect Size (ES)
SH_dataES <- escalc(measure = "OR",
                    ai = Events_LGBTIQ, ci = Events_Controls,
                    n1i = Total_LGBTIQ, n2i = Total_Controls,
                    data = SH_data)
#Pooling ES
m_re.SH <- rma(yi = SH_dataES$yi, vi = SH_dataES$vi)
m_re.SH
#Forest Plot
forest(m_re.SH, slab = SH_dataES$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SH, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SH)
#--------------------------------Meta Regression
#----Univariate
SH_dataES$Females. <- as.character(SH_dataES$Females.)
SH_dataES$Females. <- as.numeric(SH_dataES$Females.)
SH_dataES$Weight <- as.character(SH_dataES$Weight)
SH_dataES$Weight <- as.numeric(SH_dataES$Weight)
str(SH_dataES)
#Age: Adults (YES/NO)
m_multi_age.SH <- rma(yi = yi,
                      vi = vi,
                      mods = ~ Adults,
                      data = SH_dataES)
m_multi_age.SH
#Country
m_multi_country.SH <- rma(yi = yi,
                          vi = vi,
                          mods = ~ Country,
                          data = SH_dataES)
m_multi_country.SH
#Continent
m_multi_continent.SH <- rma(yi = yi,
                            vi = vi,
                            mods = ~ Continent,
                            data = SH_dataES)
m_multi_continent.SH
#Sex
m_multi_sex.SH <- rma(yi = yi,
                      vi = vi,
                      mods = ~ Females.,
                      data = SH_dataES)
m_multi_sex.SH
#Study Weight
m_multi_weight.SH <- rma(yi = yi,
                         vi = vi,
                         mods = ~ Weight,
                         data = SH_dataES)
m_multi_weight.SH
#----Multivariate
SH_dataES$Females.
m_multi.SH <- rma(yi = yi,
                  vi = vi,
                  mods = ~ Weight + Adults + Continent + Females.,
                  data = SH_dataES)
m_multi.SH

###----------------------------------------------------------------------------------------
###-------------------------4. Subgroups Meta Analysis of Suicide Attempts-----------------
###----------------------------------------------------------------------------------------
#----------------------------------4a. LG
#Import data
SA_data.LG <- structure(list(ID = structure(1:8, .Label = c("Björkenstam et al. 2016", "Bränström et al. 2020", "Coulter et al. 2017", "Horwitz et al. 2021",
                                                            "Lytle et al. 2018", "Porta et al. 2018", "Rentería et al. 2021", "Tsypes et al. 2016"), class = "factor"),
                             Events_LG = c(72L, 13L, 66L, 49L, 67L, 3195L, 6L, 12L),
                             Total_LG = c(874L, 443L, 290L, 148L, 485L, 10863L, 144L, 293L),
                             Events_Controls = c(2096L, 232L, 714L, 358L, 969L, 1264L, 33L, 130L),
                             Total_Controls = c(67980L, 38667L, 21002L, 5148L, 19576L, 81197L, 6653L, 9589L),
                             Weight = structure(c(5L, 3L, 5L, 4L, 5L, 6L, 1L, 2L), .Label = c("0.115", "0.122", "0.123", "0.127", "0.128", "0.129"), class = "factor")), class = "data.frame", row.names = c(NA, -8L))
#Calculate Effect Size
SA_dataES.LG <- escalc(measure = "OR",
                       ai = Events_LG, ci = Events_Controls,
                       n1i = Total_LG, n2i = Total_Controls,
                       data = SA_data.LG)
#Pooling ES
m_re.SA.LG <- rma(yi = SA_dataES.LG$yi, vi = SA_dataES.LG$vi)
m_re.SA.LG
#Forest Plot
forest(m_re.SA.LG, slab = SA_dataES.LG$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SA.LG, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SA.LG)

#----------------------------------4b. B
#Import data
SA_data.B <- structure(list(ID = structure(1:7, .Label = c("Björkenstam et al. 2016", "Bränström et al. 2020", "Coulter et al. 2017", "Horwitz et al. 2021",
                                                           "Lytle et al. 2018", "Rentería et al. 2021", "Tsypes et al. 2016"), class = "factor"),
                            Events_B = c(124L, 41L, 188L, 196L, 140L, 11L, 32L),
                            Total_B = c(841L, 838L, 763L, 523L, 696L, 266L, 537L),
                            Events_Controls = c(2096L, 232L, 714L, 358L, 969L, 33L, 130L),
                            Total_Controls = c(67980L, 38667L, 21002L, 5148L, 19576L, 6653L, 9589L),
                            Weight = structure(c(5L, 3L, 6L, 4L, 5L, 1L, 2L), .Label = c("0.073", "0.124", "0.136", "0.164", "0.167", "0.169"),
                                               class = "factor")), class = "data.frame", row.names = c(NA, -7L))
#Calculate Effect Size
SA_dataES.B <- escalc(measure = "OR",
                      ai = Events_B, ci = Events_Controls,
                      n1i = Total_B, n2i = Total_Controls,
                      data = SA_data.B)
#Pooling ES
m_re.SA.B <- rma(yi = SA_dataES.B$yi, vi = SA_dataES.B$vi)
m_re.SA.B
#Forest Plot
forest(m_re.SA.B, slab = SA_dataES.B$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SA.B, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SA.B)

#----------------------------------4c. TIQ
#Import data
SA_data.TIQ <- structure(list(ID = structure(1:8, .Label = c("Anderssen et al. 2020", "Clark et al. 2014", "Clements-Nolle et al. 2018", "Coulter et al. 2017",
                                                             "Cramer et al. 2020", "Eisenberg et al. 2017", "Lytle et al. 2018", "Thoma et al. 2019"), class = "factor"),
                              Events_TIQ = c(26L, 52L, 44L, 64L, 11L, 609L, 40L, 577L),
                              Total_TIQ = c(115L, 435L, 226L, 779L, 45L, 2168L, 233L, 1148L),
                              Events_Controls = c(1919L, 315L, 288L, 714L, 12L, 5286L, 969L, 274L),
                              Total_Controls = c(49836L, 7729L, 4230L, 21002L, 73L, 78761L, 19576L, 872L),
                              Weight = structure(c(2L, 4L, 3L, 5L, 1L, 7L, 3L, 6L), .Label = c("0.074", "0.119", "0.128", "0.131", "0.135", "0.14", "0.144"), class = "factor")),
                         class = "data.frame", row.names = c(NA, -8L))
#Calculating Effect Size
SA_dataES.TIQ <- escalc(measure = "OR",
                        ai = Events_TIQ, ci = Events_Controls,
                        n1i = Total_TIQ, n2i = Total_Controls,
                        data = SA_data.TIQ)
#Pooling ES
m_re.SA.TIQ <- rma(yi = SA_dataES.TIQ$yi, vi = SA_dataES.TIQ$vi)
m_re.SA.TIQ
#Forest Plot
forest(m_re.SA.TIQ, slab = SA_dataES.TIQ$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SA.TIQ, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SA.TIQ)

###----------------------------------------------------------------------------------------
###-------------------------5. Subgroups Meta Analysis of Suicide Ideation-----------------
###----------------------------------------------------------------------------------------
#----------------------------------5a. LG
#Import data
SI_data.LG <- structure(list(ID = structure(1:11, .Label = c("Björkenstam et al. 2016", "Bränström et al. 2020", "Coulter et al. 2017", "Gilmour et al. 2019",
                                                             "Horwitz et al. 2021", "Lytle et al. 2018", "Nam et al. 2019", "Porta et al. 2018", "Proulx et al. 2019",
                                                             "Rentería et al. 2021", "Tsypes et al. 2016"), class = "factor"),
                             Events_LG = c(206L, 55L, 109L, 31L, 95L, 31L, 10L, 5246L, 369L, 29L, 13L),
                             Total_LG = c(874L, 443L, 290L, 579L, 148L, 485L, 63L, 10863L, 1259L, 144L, 293L), 
                             Events_Controls = c(7881L, 2409L, 2289L, 1030L, 1086L, 721L, 131L, 32602L, 5107L, 599L, 270L),
                             Total_Controls = c(67980L, 48180L, 21002L, 44775L, 5148L, 19576L, 1430L, 81197L, 43331L, 6653L, 9589L),
                             Weight = structure(c(8L, 6L, 7L, 4L, 5L, 4L, 1L, 10L, 9L, 3L, 2L), .Label = c("0.071", "0.079", "0.088", "0.09", "0.092", "0.094", "0.096", "0.099", "0.1", "0.101"),
                                                class = "factor")), class = "data.frame", row.names = c(NA, -11L))
#Calculate Effect Size
SI_dataES.LG <- escalc(measure = "OR",
                       ai = Events_LG, ci = Events_Controls,
                       n1i = Total_LG, n2i = Total_Controls,
                       data = SI_data.LG)
#Pooling ES
m_re.SI.LG <- rma(yi = SI_dataES.LG$yi, vi = SI_dataES.LG$vi)
m_re.SI.LG
#Forest Plot
forest(m_re.SI.LG, slab = SI_dataES.LG$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SI.LG, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SI.LG)

#----------------------------------5b. B
#Import data
SI_data.B <- structure(list(ID = structure(1:10, .Label = c("Björkenstam et al. 2016", 
                                                            "Bränström et al. 2020", "Coulter et al. 2017", "Gilmour et al. 2019", 
                                                            "Horwitz et al. 2021", "Lytle et al. 2018", "Nam et al. 2019", 
                                                            "Proulx et al. 2019", "Rentería et al. 2021", "Tsypes et al. 2016"), class = "factor"),
                            Events_B = c(292L, 170L, 351L, 90L, 355L, 79L, 36L, 1400L, 69L, 48L),
                            Total_B = c(841L, 838L, 763L, 701L, 523L, 696L, 86L, 3372L, 266L, 537L),
                            Events_Controls = c(7881L, 2409L, 2289L, 1030L, 1086L, 721L, 131L, 5107L, 599L, 270L),
                            Total_Controls = c(67980L,48180L, 21002L, 44775L, 5148L, 19576L, 1430L, 43331L, 6653L, 9589L),
                            Weight = structure(c(9L, 7L, 8L, 5L, 6L, 4L, 1L, 10L, 3L, 2L),
                                               .Label = c("0.063", "0.085", "0.091", "0.098", "0.1", "0.106", "0.109", "0.113", "0.114", "0.121"), class = "factor")),
                       class = "data.frame", row.names = c(NA, -10L))
#Calculate Effect Size
SI_dataES.B <- escalc(measure = "OR",
                      ai = Events_B, ci = Events_Controls,
                      n1i = Total_B, n2i = Total_Controls,
                      data = SI_data.B)
#Pooling ES
m_re.SI.B <- rma(yi = SI_dataES.B$yi, vi = SI_dataES.B$vi)
m_re.SI.B
#Forest Plot
forest(m_re.SI.B, slab = SI_dataES.B$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SI.B, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SI.B)

#----------------------------------5c. TIQ
#Import data
SI_data.TIQ <- structure(list(ID = structure(1:9, .Label = c("Anderssen et al. 2020", "Clements-Nolle et al. 2018", "Coulter et al. 2017", "Cramer et al. 2020",
                                                             "Eisenberg et al. 2017", "Lytle et al. 2018", "Perez-Brumer et al. 2017", "Proulx et al. 2019", "Thoma et al. 2019"), class = "factor"), 
                              Events_TIQ = c(73L, 87L, 167L, 29L, 1202L, 38L, 2467L, 550L, 974L),
                              Total_TIQ = c(115L, 226L, 779L, 45L, 2168L, 233L, 7653L, 2110L, 1148L),
                              Events_Controls = c(10067L, 617L, 2289L, 36L, 14812L, 721L, 94881L, 5107L, 527L),
                              Total_Controls = c(49836L, 4230L, 21002L, 73L, 78761L, 19576L, 567203L, 43331L, 872L),
                              Weight = structure(c(2L, 4L, 6L, 1L, 7L, 3L, 8L, 7L, 5L), .Label = c("0.067", "0.103", "0.106", "0.112", "0.118", "0.12", "0.124", "0.125"), class = "factor")),
                         class = "data.frame", row.names = c(NA, -9L))
#Calculate Effect Size
SI_dataES.TIQ <- escalc(measure = "OR",
                        ai = Events_TIQ, ci = Events_Controls,
                        n1i = Total_TIQ, n2i = Total_Controls,
                        data = SI_data.TIQ)
#Pooling ES
m_re.SI.TIQ <- rma(yi = SI_dataES.TIQ$yi, vi = SI_dataES.TIQ$vi)
m_re.SI.TIQ
#Forest Plot
forest(m_re.SI.TIQ, slab = SI_dataES.TIQ$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SI.TIQ, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SI.TIQ)

###----------------------------------------------------------------------------------------
###-------------------------6. Subgroups Meta Analysis of Self Harm------------------------
###----------------------------------------------------------------------------------------
#----------------------------------6a. TIQ
#Import data
SH_data.TIQ <- structure(list(ID = structure(1:7, .Label = c("Anderssen et al. 2020", "Clark et al. 2014", "Davey et al. 2016", "Eisenberg et al. 2017",
                                                             "Fraser et al. 2018", "Li et al. 2019", "Thoma et al. 2019"), class = "factor"), 
                              Events_TIQ = c(63L, 155L, 18L, 1076L, 2L, 16L, 998L),
                              Total_TIQ = c(115L, 435L, 97L, 2168L, 5L, 244L, 1148L),
                              Events_Controls = c(8522L, 1809L, 4L, 10650L, 285L, 93L, 515L),
                              Total_Controls = c(49836L, 7729L, 97L, 78761L, 1601L, 1500L, 872L),
                              Weight = structure(c(4L, 6L, 2L, 7L, 1L, 3L, 5L), .Label = c("0.061", "0.101", "0.151", "0.164", "0.173", "0.174", "0.177"), class = "factor")),
                         class = "data.frame", row.names = c(NA, -7L))
#Calculate Effect Size
SH_dataES.TIQ <- escalc(measure = "OR",
                        ai = Events_TIQ, ci = Events_Controls,
                        n1i = Total_TIQ, n2i = Total_Controls,
                        data = SH_data.TIQ)
#Pooling ES
m_re.SH.TIQ <- rma(yi = SH_dataES.TIQ$yi, vi = SH_dataES.TIQ$vi)
m_re.SH.TIQ
#Forest Plot
forest(m_re.SH.TIQ, slab = SH_dataES.TIQ$ID)
#----------------------------------Publication Bias
#Funnel Plot
funnel(m_re.SH.TIQ, xlab = "Hedges' g")
#Egger's test
regtest(m_re.SH.TIQ)

###############################################################################
###############################################################################
###----------------------------------------------------------------------------------------
###-------------------------7. Leave-One-Out Analysis of Suicide Attempt-------------------
###----------------------------------------------------------------------------------------
SA_loo <- leave1out(m_re, transf=exp)

###----------------------------------------------------------------------------------------
###-------------------------8. Leave-One-Out Analysis of Suicide Ideation------------------
###----------------------------------------------------------------------------------------
SI_loo <- leave1out(m_re.SI, transf=exp)

###----------------------------------------------------------------------------------------
###-------------------------9. Leave-One-Out Analysis of Self-Harm-------------------------
###----------------------------------------------------------------------------------------
SH_loo <- leave1out(m_re.SH, transf=exp)