###### GRAFIKI szczególne ########################

rm(list=ls())
# logical test | jeżeli biblioteka jest > skip | jeżeli nie ma instaluje !
need_pack <- c("dplyr", "openxlsx", "readxl", "stringr", "chron", "lubridate", "hms", "dwlm", "rstudioapi")
not_installed <- need_pack[!(need_pack %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed) 
# biblioteki
library(dplyr)
library(openxlsx)
library("readxl")
library(stringr)
library(chron)
library("lubridate")
#library("hms")

#### miesce pliku = scieżka robocza
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
######
# ścieżki 
path_2022 <- "dane//grafik_2022.xlsx"
path_2023 <- "dane//grafik_2023.xlsx"
path_output <- "output//do_wgrania.xlsx"
path_manual <- "output//wgrywamy_ręcznie.xlsx"

# wczytujemy grafiki
gf_2022 <- read.xlsx(paste(path_2022), 1)
gf_2023 <- read.xlsx(paste(path_2023), 1)

#### formaty
# test_start
# dodaje nowe kołumny do wzorowania
#gf_2022$WORK_START[2] <- "12:55"
## test_end

gf_2022 <- gf_2022 %>% mutate(start_hour = as.numeric(hour(hm(WORK_START, roll = FALSE, quiet = TRUE))),
                         start_minutes = as.numeric(minute(hm(WORK_START, roll = FALSE, quiet = TRUE))),
                         end_hour = as.numeric(hour(hm(WORK_END, roll = FALSE, quiet = TRUE))),
                         end_minutes = as.numeric(minute(hm(WORK_END, roll = FALSE, quiet = TRUE))))
gf_2023 <- gf_2023 %>% mutate(start_hour = as.numeric(hour(hm(WORK_START, roll = FALSE, quiet = TRUE))),
                              start_minutes = as.numeric(minute(hm(WORK_START, roll = FALSE, quiet = TRUE))),
                              end_hour = as.numeric(hour(hm(WORK_END, roll = FALSE, quiet = TRUE))),
                              end_minutes = as.numeric(minute(hm(WORK_END, roll = FALSE, quiet = TRUE))))                       


###

gf_2023_hpo11 <- gf_2023 %>% filter(CODE == "HP011") #  zostawiam jak jest ="HP011"=, ponieważ biorę jako przykład

lista_2022<- c(unique(gf_2022$CODE))



## gotowy grafik
rd_gr <- data.frame()
manual_work <- data.frame()
avoid_vec <- c()


###                                                 |*== START LOOPS ==*| 
## Logical loop | 
for (i in lista_2022){
  name_dt_2022 <- gf_2022 %>% filter(CODE == paste(i))
  uniq_work_start <- unique(na.omit(name_dt_2022$start_hour))
  if (length(uniq_work_start) > 1){
    avoid_vec <- append(avoid_vec,i)
  }
}
DBCODE_ręcznie <- avoid_vec
manual_work <- data.frame(DBCODE_ręcznie)
## main loop
for (j in lista_2022){
  if (j %in% avoid_vec) next
  name_dt_2022 <- gf_2022 %>% filter(CODE == paste(j))
  q1_copy <- max(name_dt_2022$Q1)
  q2_copy <- max(name_dt_2022$Q2)
  q3_copy <- max(name_dt_2022$Q3)
  q4_copy <- max(name_dt_2022$Q4)
  q5_copy <- max(name_dt_2022$Q5)
  class(name_dt_2022$WORK_START)
  class(name_dt_2022$WORK_END)
  start_hour_copy <- max(na.omit(name_dt_2022$start_hour))
  start_minutes_copy <- max(na.omit(name_dt_2022$start_minutes))
  end_hour_copy <- max(na.omit(name_dt_2022$end_hour))
  end_minutes_copy <- max(na.omit(name_dt_2022$end_minutes))
  
  
  a1 <- paste(start_hour_copy,start_minutes_copy, sep = ":")
  a2 <- paste(end_hour_copy,end_minutes_copy, sep = ":")
  wrk_dt <- gf_2023_hpo11 %>% 
    mutate(CODE = paste(j)) %>% replace(. == 8, q1_copy) %>% mutate(Q2 = ifelse(Q1 != 0, q2_copy, Q2),
                                                                    Q3 = ifelse(Q1 != 0, q3_copy, Q3),
                                                                    Q4 = ifelse(Q1 != 0, q4_copy, Q4),
                                                                    Q5 = ifelse(Q1 != 0, q5_copy, Q5))
  wrk_dt$WORK_START <- str_replace_all(wrk_dt$WORK_START,"08:00", if (nchar(a1) < 5){
    gsub(":0", ":00", a1)
  })
  wrk_dt$WORK_END <- str_replace_all(wrk_dt$WORK_END,"16:00", if (nchar(a1) < 5){
    gsub(":0", ":00", a2)
  })
  wrk_dt <- subset(wrk_dt, select = -c(start_hour, start_minutes, end_hour,end_minutes ))
  rd_gr <- rbind(rd_gr, wrk_dt)
}

##                                                  |*== END LOOPS ==*|
## zapisuje plik gotowy do wgrania
write.xlsx(rd_gr, paste(path_output), overwrite = TRUE)
## zapisuje listę do ręcznego wgrania
write.xlsx(manual_work, paste(path_manual), overwrite = TRUE)

