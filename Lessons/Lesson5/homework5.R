library(dplyr)
dt_Policy <- read.csv("C:/Users/Lucia_Sebestova/Documents/Matfyz(4)/VTvAktuarstve/GeneralInsurance_Class/Data/lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("C:/Users/Lucia_Sebestova/Documents/Matfyz(4)/VTvAktuarstve/GeneralInsurance_Class/Data/lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)
#spojime uvedene 2 subory
dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)
library(lubridate)
#pridala som premennu: Time_Exposure, ktora hovori na ako dlho je uzavreta poistka, teda kolko dni trva poistenie
#                      Ultimate Losses - celkove straty jednej poistnej zmluvy 
#                      Burning Cost - kolko vyplati za jeden den na jednu poistnu zmluvu poistovna (kolko poistovnu stoji za den clovek, s ktorym uzavrela poistnu zmluvu)        
dt_pol_w_claims <-dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start),
                                             Ult_Loss = Paid + Reserves,
                                             Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))
library(ggplot2)
#pozrela som sa ake je Burning Case ak vysvetlujuca premenna je vek poistenca
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()
#najvacsie hodnoty Burning Case nadobuda pre ludi vo veku priblizne 40-45. Mohlo to sposobit, ze deti zacali pouzivat vozidlo, ktore je napisane na poistnika. 
#Dalej vidim, ze Burning Case nastalo aj ked vek bol blizko alebo nad 100 rokov.
#Mohlo nastat, ze aj ked poistnik (osoba ktora uzavrela poistnu zmluvu a ktora je vlasnikom vozidla), ktory je v tom veku,
#ale zrejme to vozidlo vyuziva ina osoba, ktora je schopna soferovat. Druha moznost je, ze sa jedna chybny udaj.   

#pozrela som sa ake je Burning Case ak vysvetlujuca premenna je rok vyroby vozidla
#Poznamka data, s ktorymi pracujem su pre poistne zmluvy uzavrete v roku  2012-2015
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()
#z scatterplot vidim, ze Burinig case rastie pre vozidla vyrobene od roku priblizne 2005 a najvacsie to je okolo roku 2011. 
#Co nie je az tak prekvapujuce, kedze vozidla cim su starsie tak stracaju na hodnote, teda ak nastane nejaka poistna udalost, tak odskodne nie je take vysoke v porovnani novsim typom vozidla.
#Dalej som si vsimla ze Burning case bolo vacsinou nulove pre roky od 1977-1995. Mohlo to sposobit, ze ludia ktory vlastia taketo stare vozidlo, tak malokto mal/nahlasil poistnu udalost 
#alebo poistovna uzavrela malo poistnych zmluv, ktore poistovali vozidlo vyrobene v tych rokoch.

