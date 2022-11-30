###### GRAFIKI OGÓLNE ########################

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

#### miesce pliku = scieżka robocza
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### ścieżka do plików
path_2022 <- "dane//grafik_2022.xlsx"
path_2023 <- "dane//grafik_2023.xlsx"
path_output <- "output//do_wgrania.xlsx"
path_flaged_32 <- "output//flagowane_32.xlsx"
## wczytujemy pliki 

gf_2022 <- read.xlsx(paste(path_2022), 1)
gf_2023 <- read.xlsx(paste(path_2023), 1)

gf_2023_hpo11 <- gf_2023 %>% filter(CODE == "HP011") #  zostawiam jak jest ="HP011"=, ponieważ biorę jako przykład

lista_2022<- c(unique(gf_2022$CODE))

## gotowy grafik
rd_gr <- data.frame()


## main loop
for(i in lista_2022){
  name_dt_2022 <- gf_2022 %>% filter(CODE == paste(i))
  name_value <- name_dt_2022$NAME[1]
  int_value <- max(name_dt_2022$D1)
  str_name_2022 <- str_replace(gf_2022$NAME[1],"\\W","FIR:")
  wrk_dt <- gf_2023_hpo11 %>% mutate(CODE = paste(i), NAME = str_name_2022) %>%
    replace(. == 8, int_value) %>% mutate(QUANTITY_X =rowSums(
      select(., "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9","D10",
             "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20",
             "D21","D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31")))
  rd_gr <- rbind(rd_gr, wrk_dt)
}

write.xlsx(rd_gr, paste(path_output), overwrite = TRUE)


# dodatkowy logical check od Bartka
# Kolumna F sprawdzamy czy ma 32, jeżeli >32 flag ostrzegawczy >>>> 
## test
#rd_gr$NAME[44] <- "asdlaoksdakjsdhjkasjdhakjsdhkjadjahdjkas" 
# end test
flaged_32 <- data.frame("DB_CODE"= as.character(),
                        "NAME" = as.character(),
                        "nr_wierszu" = as.numeric())
### przechodzi przez każdą wartość i zaznacza wartości podejrzane i zwraca ich z powrotem
for(j in 1:length(rd_gr$NAME)){
  if (nchar(rd_gr$NAME[j]) > 32){
    flag_loop <- data.frame("DB_CODE"= as.character(rd_gr$DBCODE[j]),
                            "NAME" = as.character(rd_gr$NAME[j]),
                            "nr_wierszu" = as.numeric(j))
    print(flag_loop)
    flaged_32 <- rbind(flaged_32, flag_loop)
  }
}
write.xlsx(flaged_32, paste(path_flaged_32), overwrite = TRUE)

