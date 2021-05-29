#Задание 2
#Савкина Анастасия, 124
#создайте модель множественной линейной регрессии ночных потоков углекислого газа за летний период 2013 года по данным измерений методом турбулентной пульсации
# Проверка директории
setwd ("D:/Savkina_R")
getwd ()



library("tidyverse")
library("readr")
library("stringr")
library("dplyr")
library("ggplot2")

eddy = read_csv ( " eddypro.csv " , skip = 1 , na = c ( "  " , " NA " , " -9999 " , " -9999.0 " ), comment = c ( " [ " ));
eddy = eddy [ - 1 ,]
eddy = select ( eddy , - ( ролл ))
eddy = eddy % > % mutate_if ( is.character , factor )

имена ( вихрь ) = имена ( вихрь )% > %
  str_replace_all ( " [!] " , " _exclam_ " )% > %
  str_replace_all ( " [?] " , " _quest_ " )% > %
  str_replace_all ( " [*] " , " _star_ " )% > %
  str_replace_all ( " [+] " , " _plus_ " )% > %
  str_replace_all ( " [-] " , " _minus_ " )% > %
  str_replace_all ( " [@] " , " _at_ " )% > %
  str_replace_all ( " [$] " , " _доллар_ " )% > %
  str_replace_all ( " [#] " , " _hash_ " )% > %
  str_replace_all ( " [/] " , " _slash_ " )% > %
  str_replace_all ( " [%] " , " _pecent_ " )% > %
  str_replace_all ( " [&] " , " _amp_ " )% > %
  str_replace_all ( " [ \\ ^] " , " _power_ " )% > %
  str_replace_all ( " [()] " , " _ " )
проблеск ( вихрь )

eddy  = mutate ( eddy , месяц  = месяц ( число ))

eddy_num = filter ( eddy , month > = 6  &  month < = 8 ) [, sapply ( eddy , is.numeric )]
eddy_non_num = eddy [, ! sapply (eddy , is.numeric)]

cor_td = cor (drop_na ( eddy_num ))% > % as.data.frame % > % select ( co2_flux )
cor_td = cbind ( cor_td , настроено = sqrt ( cor_td $ co2_flux ^ 2 ))
vars = row.names ( cor_td ) [ cor_td $ co2_flux ^ 2 > .1 ]% > % na.exclude ()
correlation_formula = as.formula (paste ( " co2_flux ~ " , paste ( vars , collapse = " + " ), sep = "  " ))


model1 = lm ( correlation_formula , data = eddy_num )

имена ( модель1 )

сводка ( модель1 )

anova ( модель1 )


# удаляем малозначимые показатели ориентируемся по показателю R ^ 2 и Pr (> F)
cor_lrm = cor (drop_na ( eddy_num )% > % as.data.frame % > % select ( vars ))
cor_lrm = sqrt ( cor_lrm ^ 2 )
model3  = lm ( data  =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2 )

сводка ( модель3 )
anova ( модель3 )
# Для модели - 3 показатель R ^ 2 = 0,84, продолжаем исключать малозначимые переменные
model4  = lm ( data  =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                 H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
               -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                 Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                 rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                 rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                 rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                 rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                 H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                 co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                 co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                 co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                 air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                 air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                 эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                 ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                 un_LE : co2  -  un_h2o_flux : co2 )

сводка ( модель4 )
anova ( модель4 )
# Для модели - 4 показатель R ^ 2 = 0,81, продолжаем исключать малозначимые переменные
model5 = lm ( data   =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
              -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                un_LE : co2  -  un_h2o_flux : co2  -  air_molar_volume  -  Tau : air_molar_volume  -  Tau : max_speed  -  rand_err_LE : co2_molar_density  -
                rand_err_LE : air_density  -  rand_err_LE : un_Tau  -  rand_err_LE : co2  -  rand_err_h2o_flux : max_speed  -  rand_err_h2o_flux : ТКЕ  -
                rand_err_h2o_flux : un_LE  -  co2_molar_density : co2  -  co2_mole_fraction : air_density  -  co2_mole_fraction : un_LE  -
                co2_mole_fraction : co2  -  air_density : air_molar_volume  -  air_density : эс  -  эс : co2 )
сводка ( модель5 )
anova ( модель5 )
# Для модели - 5 показатель R ^ 2 = 0,80, продолжаем исключать малозначимые переменные
model6 = lm ( data   =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
              -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                un_LE : co2  -  un_h2o_flux : co2  -  air_molar_volume  -  Tau : air_molar_volume  -  Tau : max_speed  -  rand_err_LE : co2_molar_density  -
                rand_err_LE : air_density  -  rand_err_LE : un_Tau  -  rand_err_LE : co2  -  rand_err_h2o_flux : max_speed  -  rand_err_h2o_flux : ТКЕ  -
                rand_err_h2o_flux : un_LE  -  co2_molar_density : co2  -  co2_mole_fraction : air_density  -  co2_mole_fraction : un_LE  -
                co2_mole_fraction : co2  -  air_density : air_molar_volume  -  air_density : эс  -  эс : co2  -  Tau : rand_err_LE  -  Tau : un_Tau  -
                Тау : un_h2o_flux  -  LE : un_h2o_flux  -  rand_err_h2o_flux : co2_molar_de density  -  rand_err_h2o_flux : co2_molar_de density  -
                rand_err_h2o_flux : co2  -  H_strg : эс  -  co2_molar_density : ТКЕ  -  air_density : un_LE  -  un_LE : un_h2o_flux )
сводка ( модель6 )
anova ( модель6 )
# Для модели - 6 показатель R ^ 2 = 0,79, продолжаем исключать малозначимые переменные

model7 = lm ( data   =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
              -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                un_LE : co2  -  un_h2o_flux : co2  -  air_molar_volume  -  Tau : air_molar_volume  -  Tau : max_speed  -  rand_err_LE : co2_molar_density  -
                rand_err_LE : air_density  -  rand_err_LE : un_Tau  -  rand_err_LE : co2  -  rand_err_h2o_flux : max_speed  -  rand_err_h2o_flux : ТКЕ  -
                rand_err_h2o_flux : un_LE  -  co2_molar_density : co2  -  co2_mole_fraction : air_density  -  co2_mole_fraction : un_LE  -
                co2_mole_fraction : co2  -  air_density : air_molar_volume  -  air_density : эс  -  эс : co2  -  Tau : rand_err_LE  -  Tau : un_Tau  -
                Тау : un_h2o_flux  -  LE : un_h2o_flux  -  rand_err_h2o_flux : co2_molar_de density  -  rand_err_h2o_flux : co2_molar_de density  -
                rand_err_h2o_flux : co2  -  H_strg : эс  -  co2_molar_density : ТКЕ  -  air_density : un_LE  -  un_LE : un_h2o_flux  -  Tau : un_LE  -
                LE : H_strg  -  rand_err_h2o_flux : co2_mole_fraction  -  air_molar_volume : co2 )
сводка ( модель7 )
anova ( модель7 )
# Для модели - 7 показатель R ^ 2 = 0,79, продолжаем исключать малозначимые переменные
model8 = lm ( data   =  eddy_num , co2_flux ~ ( Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
              -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                un_LE : co2  -  un_h2o_flux : co2  -  air_molar_volume  -  Tau : air_molar_volume  -  Tau : max_speed  -  rand_err_LE : co2_molar_density  -
                rand_err_LE : air_density  -  rand_err_LE : un_Tau  -  rand_err_LE : co2  -  rand_err_h2o_flux : max_speed  -  rand_err_h2o_flux : ТКЕ  -
                rand_err_h2o_flux : un_LE  -  co2_molar_density : co2  -  co2_mole_fraction : air_density  -  co2_mole_fraction : un_LE  -
                co2_mole_fraction : co2  -  air_density : air_molar_volume  -  air_density : эс  -  эс : co2  -  Tau : rand_err_LE  -  Tau : un_Tau  -
                Тау : un_h2o_flux  -  LE : un_h2o_flux  -  rand_err_h2o_flux : co2_molar_de density  -  rand_err_h2o_flux : co2_molar_de density  -
                rand_err_h2o_flux : co2  -  H_strg : эс  -  co2_molar_density : ТКЕ  -  air_density : un_LE  -  un_LE : un_h2o_flux  -  Tau : un_LE  -
                LE : H_strg  -  rand_err_h2o_flux : co2_mole_fraction  -  air_molar_volume : co2  -  LE : max_speed  -  LE : un_Tau  -  rand_err_h2o_flux : H_strg  -
                H_strg : un_LE )
сводка ( модель8 )
anova ( модель8 )
сюжет ( модель8 )
# Для модели - 8 показатель R ^ 2 = 0,78, попробуем исключить еще ряд независимых переменных
# Для модели - 8 показатель R ^ 2 = 0,78, остатки рассеяны равномерно, остатки данных близкое к нормальному;
# За пределы диопазона.
# Графическая интерпертация множественной линейной регрессии потоковлекислого газа
# за летний период 2013 года по данным измерений методом турбулентной пульсации (модель8)
qplot ( тау + LE + rand_err_h2o_flux + H_strg + co2_molar_density + air_density +
         эс + max_speed + ТКЕ + un_LE + un_h2o_flux + co2 + Tau : co2 +
         LE : rand_err_LE + LE : rand_err_h2o_flux + LE : co2_molar_de density + LE : воздушная_плотность + LE : air_molar_volume +
         LE : es + LE : TKE + rand_err_LE : es + rand_err_LE : un_LE + rand_err_h2o_flux : air_de density +
         co2_molar_density : max_speed + co2_molar_density : un_LE + co2_molar_density : un_h2o_flux , co2_flux , данные = вихретоковый , альфа = I ( 1 / 4 )) + geom_smooth ( SE = FALSE , метод = лм ) + theme_bw ()

# Для модели - 8 показатель R ^ 2 = 0,78, попробуем исключить еще ряд независимых переменных +
# добавим ошибочно исключенную w_slash_co2_cov, оптимизируем данные
# R ^ 2 = 0.99 охват дисперии протестился - далее упростить модель
model9 = lm ( data   =  eddy_num , co2_flux ~ ( w_slash_co2_cov + Tau + LE + rand_err_LE + rand_err_h2o_flux + H_strg + co2_molar_de density +
                                                  co2_mole_fraction + air_de density + air_molar_volume + es + max_speed + TKE +
                                                  un_Tau + un_LE + un_h2o_flux + co2 ) ^ 2  - LE : co2    -  H_strg : air_density  -
                H_strg : air_molar_volume  -  co2_mole_fraction  -  un_Tau  -  Tau : LE  -  Tau : rand_err_h2o_flux
              -  Тау : H_strg  -  Тау : co2_molar_de density  -  Тау : co2_mole_fraction  -  Тау : воздушная_плотность  -  Тау : co2_mole_fraction  -
                Тау : воздушная_плотность  -  Тау : es  -  Тау : ТКЕ  -  LE : co2_mole_fraction  -  LE : un_LE  -  rand_err_LE : rand_err_h2o_flux  - 
                rand_err_LE : H_strg  -  rand_err_LE : co2_mole_fraction  -  rand_err_LE : air_molar_volume  -  rand_err_LE : max_speed  -
                rand_err_LE : TKE  - rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_LE : max_speed  - 
                rand_err_LE : TKE  -  rand_err_LE : un_h2o_flux  -  rand_err_h2o_flux : air_molar_volume  -  rand_err_h2o_flux : es  -  rand_err_h2o_flux : es -
                rand_err_h2o_flux : un_Tau  -  rand_err_h2o_flux : un_h2o_flux  -  H_strg : co2_mole_fraction  -  H_strg : air_de density  -
                H_strg : max_speed  -  H_strg : TKE  -  H_strg : un_Tau  -  H_strg : un_h2o_flux  -  co2_molar_de density : co2_mole_fraction  - 
                co2_molar_de density : воздушная_плотность  -  co2_molar_de density : air_molar_volume  -  co2_molar_de density : es  -  co2_molar_de density : un_Tau  - 
                co2_mole_fraction : air_molar_volume  - co2_mole_fraction : es  -  co2_mole_fraction : max_speed  -  co2_mole_fraction : TKE  -
                co2_mole_fraction : un_Tau  -  co2_mole_fraction : un_h2o_flux  -  air_de density : max_speed  -  air_de density : TKE  -  air_de density : un_Tau  -
                air_density : un_h2o_flux  -  air_density : co2  -  air_molar_volume : эс  -  air_molar_volume : max_speed  -  air_molar_volume : ТКЕ  -
                air_molar_volume : un_Tau  -  air_molar_volume : un_LE  -  air_molar_volume : un_h2o_flux  -  es : max_speed  -  es : TKE  -  es : un_Tau  -
                эс : un_LE  -  эс : un_h2o_flux  -  max_speed : TKE  -  max_speed : un_Tau  -  max_speed : un_LE  -  max_speed : un_h2o_flux  -  max_speed : co2  -
                ТКЕ : un_Tau  -  ТКЕ : un_LE  -  ТКЕ : un_h2o_flux  -  ТКЕ : co2  -  un_Tau : un_LE  -  un_Tau : un_h2o_flux  -  un_Tau : co2  -
                un_LE : co2  -  un_h2o_flux : co2  -  air_molar_volume  -  Tau : air_molar_volume  -  Tau : max_speed  -  rand_err_LE : co2_molar_density  -
                rand_err_LE : air_density  -  rand_err_LE : un_Tau  -  rand_err_LE : co2  -  rand_err_h2o_flux : max_speed  -  rand_err_h2o_flux : ТКЕ  -
                rand_err_h2o_flux : un_LE  -  co2_molar_density : co2  -  co2_mole_fraction : air_density  -  co2_mole_fraction : un_LE  -
                co2_mole_fraction : co2  -  air_density : air_molar_volume  -  air_density : эс  -  эс : co2  -  Tau : rand_err_LE  -  Tau : un_Tau  -
                Тау : un_h2o_flux  -  LE : un_h2o_flux  -  rand_err_h2o_flux : co2_molar_de density  -  rand_err_h2o_flux : co2_molar_de density  -
                rand_err_h2o_flux : co2  -  H_strg : эс  -  co2_molar_density : ТКЕ  -  air_density : un_LE  -  un_LE : un_h2o_flux  -  Tau : un_LE  -
                LE : H_strg  -  rand_err_h2o_flux : co2_mole_fraction  -  air_molar_volume : co2  -  LE : max_speed  -  LE : un_Tau  -  rand_err_h2o_flux : H_strg  -
                H_strg : un_LE  -  LE : ТКЕ  -  co2_molar_density : max_speed  -  co2_molar_density - LE - max_speed - TKE - co2 - w_slash_co2_cov : эс -
                w_slash_co2_cov : max_speed - w_slash_co2_cov : ТКЕ - w_slash_co2_cov : un_Tau - w_slash_co2_cov : co2 - w_slash_co2_cov : co2 -
                Tau : co2 - LE : rand_err_h2o_flux - LE : air_density - LE : air_molar_volume - rand_err_LE : эс - rand_err_h2o_flux : air_density -
                H_strg : co2_molar_density - H_strg : co2 - эс - LE : co2_molar_density - co2_molar_density : un_LE - co2_molar_density : un_h2o_flux -
                w_slash_co2_cov : rand_err_h2o_flux - air_de density - w_slash_co2_cov : Tau - w_slash_co2_cov : air_molar_volume - w_slash_co2_cov : co2_mole_fraction -
                LE : es - w_slash_co2_cov : H_strg - H_strg - w_slash_co2_cov : air_de density - w_slash_co2_cov : LE - w_slash_co2_cov : rand_err_LE )
сводка ( модель9 )
anova ( модель9 )
# Модель 9 - после упрощения R ^ 2 = 0.98 в состав независимых 13 независимых независимых переменных
# Графическая интерпертация множественной линейной регрессии потоковлекислого газа
# за летний период 2013 года по данным измерений методом турбулентной пульсации (модель9)
qplot ( w_slash_co2_cov + Tau + rand_err_LE + rand_err_h2o_flux +
          un_LE + un_h2o_flux + LE : rand_err_LE + rand_err_LE : un_LE , co2_flux , данные = вихретоковый , альфа = I ( 1 / 4 )) + geom_smooth ( SE = FALSE , метод = лм ) + theme_bw ()
предупреждения ()

eddy_num [is.na ( eddy_num $ co2_flux ), " co2_flux " ] =  0
eddy_num [ eddy_num $ co2_flux > 500 , " co2_flux " ] = 0
model10 = лм ( данные   =  eddy_num , co2_flux ~ ( w_slash_co2_cov + LE + un_H + max_speed + un_LE + ш2 ) ^ 2 - ш2 - w_slash_co2_cov : un_H -
                w_slash_co2_cov : un_H - w_slash_co2_cov : max_speed - w_slash_co2_cov : co2 - LE : H_strg - w_slash_co2_cov : un_H -
                w_slash_co2_cov : max_speed - w_slash_co2_cov : co2 - LE : H_strg - LE : un_H - H_strg : un_H - H_strg : max_speed -
                H_strg : un_LE - H_strg : co2 - max_speed : co2 - LE : max_speed - LE : co2 - un_H : un_LE - un_H : co2 - max_speed : un_LE - un_H : max_speed -
                un_LE : co2 - w_slash_co2_cov : H_strg - un_H  - LE - un_LE - max_speed )

сводка ( модель 10 )
anova ( модель 10 )
участок ( модель10 )
# При дальнейшем отбрасывании независимого числа резкое падение величины R ^ 2;
# Поэтому останавливаемся на вариант регрессионной модели с 6 независимыми переменными; + при R ^ 2 = 0,97

# Изобразим графически;
# Графическая интерпертация множественной линейной регрессии потоковлекислого газа
# за летний период 2013 года по данным измерений методом турбулентной пульсации (модель20)
qplot ( w_slash_co2_cov + LE + H_strg + un_H , co2_flux , данные = вихретоковый , альфа = I ( 1 / 4 )) + geom_smooth ( SE = FALSE , метод = лм ) + theme_bw ()
# Итог- регресионная модель, уравнение построено по 6 независимым переменным R ^ 2 = 0.97; распределени остатков- близкое;
# к нормальному, итогования модель - model20.

# w_slash_co2_cov- наиболее значимая переменная




