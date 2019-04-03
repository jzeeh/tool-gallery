Code Robert: 
library(tidyverse)
library(janitor)
library(DataCombine)


rm(list=ls()) 
################################################
### data
################################################

(df1 <-  data.frame(a=sample(1:3), b = sample(11:13)))
(df2 <-  data.frame(a=sample(1:3), b = sample(11:13)))
(df3 <-  data.frame(a=sample(1:3), c = sample(11:13)))
(df4 <-  data.frame(a=sample(1:3), b = sample(11:13)))


################################################
### functions
################################################

### create list of dfs per pattern from all objects in global environment

udf_create_list_of_dfs_per_pattern_from_globalenv <- function(pattern){
  
  stopifnot(
    length(ls()) > 0,
    is.character(pattern)
  )
  
  
  Filter(function(x) is(x, "data.frame"), mget(ls(pattern = pattern,envir=globalenv()),envir = globalenv()))
  # https://stackoverflow.com/questions/25509879/how-can-i-make-a-list-of-all-dataframes-that-are-in-my-global-environment
}


################################################
### prepare
################################################

### listobject erzeugen

all_df <- udf_create_list_of_dfs_per_pattern_from_globalenv("df")

all_df



################################################
### Exkurs: nuetzliche Addins 
################################################


# zur leichten Installation:
# https://cran.r-project.org/web/packages/addinslist/README.html
install.packages("addinslist")
# dann siehe  Menüleiste "Addin" > ADDINLIST > Browse R Studio Addin List
# unten angefuehrte Addins einfach installieren und R-Studio neu starten


##### STRADDIN #####

# str(uctur) mittels Shortcut anzeigen
# siehe: https://github.com/famuvie/straddin
# objekt markieren und Struktur mittels zuvor zugewiesenem Shortcut anzeigen lassen 
# (oder mittels Addin-Menuefuehrung Addins > STRADDIN > str(1) oder str(2) etc) 
# Erst mit der Shortcutzuweisung entfaltet sich die Power dieses Addins ;-)
# siehe Tools > modify keyboard shortcuts
all_df



##### VIEWXL #####

# dataframe in Excel anzeigen
# siehe Addin  https://github.com/dreamRs/viewxl
# data-frame-objekt markieren und dataframe in Excel mittels zuvor zugewiesenem Shortcut anzeigen lassen 
# (oder mittels Addin-Menuefuehrung Addins > VIEWXL > "View in Excel" oder untenstehendem Code) 

df1 %>% viewxl::view_in_xl()

df1

##### DATAPASTA #####

# https://raw.githubusercontent.com/milesmcbain/datapasta/master/inst/media/tribble_paste.gif
#https://github.com/MilesMcBain/datapasta

# Anfuehrungszeichen in einem Vector rasch entfernen oder hinzfuegen
# Addin Download  https://github.com/MilesMcBain/datapasta
# vector markieren und mittels zuvor gesetztem Shortcut (oder Menuefuehrung Addins >  DATAPASTA > "Toggle Vector Quotes") Anfuehrungszeichen entfernen oder hinzufuegen

c("spalte1","spalte2","spalte2")
c(spalte1,spalte2,spalte2)

# vector/dataframe aus clipboard einfuegen
# mittels zuvor zugewiesenem Shortcut (oder Menuefuehrung Addins > DATAPASTA > Paste as data.frame) 



################################################
# prepare 
################################################

################################################
### zu jedem dataframe im listobjekt soll eine neue spalte hinzugefuegt weden

(all_df_mit_neuer_spalte <- map(.x= all_df,
                        .f= mutate,
                        NEU = "text"))


################################################
### zu allen dataframes des listobjekts sollen daten hinzufuegt werden

# datensatz der gejoint werden soll
(df_to_join <-  data.frame(a=c(2:3), d = c("zwei","drei")))



# "map" funktioniert nur, wenn jeder Datensatz die entsprechenden Spalten enthaelt

all_df
df_to_join

all_df_mit_ergaenzten_infos <- map(.x = all_df, # list object über welches der 'loop' erfolgen soll
                            .f = left_join,
                            y = df_to_join,
                            by = c("a"="a")) # join ueber spalte 'a'

all_df_mit_ergaenzten_infos

################################################
### zu bestimmten dataframes des listobjekts sollen daten hinzufuegt werden
# in diesem Fall wird der df_to_join nur zu df3 hinzugefuegt, weil dieser dataframe die Spalte "c" enthält

# workaround mit "map_if"
(nur_mit_ergaenzten_infos <- map_if(.x = all_df,# list object mit dataframes über welches der 'loop' erfolgen soll 
                               .p =  ~"c" %in% colnames(.), # prüft, ob  der jeweilige dataframe eine bestimmte Spalte enthaelt... 
                               # wenn Bedingung zutrifft ...
                               .f = left_join, #... soll der left_Join durchgfuehrt werden
                               y = df_to_join,
                               by = c("a"="a"))) # über die Spalte "a"


# workaround mit "map_at"
(nur_mit_ergaenzten_infos2 <- map_at(.x = all_df,# list object mit dataframes über welches der 'loop' erfolgen soll 
                                    .at = c("df2","df4"), # nur bestimmte dfs ansteuern
                                    .f = left_join, #... soll der left_Join durchgfuehrt werden
                                    y = df_to_join,
                                    by = c("a"="a"))) # über die Spalte "a"


################################################
### Exkurs: nuezliche packages & funktionen
################################################


(df_problematisch <- data.frame(stringsAsFactors=FALSE,
                           bezeichnung = c("a", NA, NA, "b", NA, NA),
                           datum = c(41275, 41275, 41275, 41276, 41276, 41276),
                           'aA??/#()?.fklf' = c(3L, 3L, 3L, 2L, 2L, 2L),
                           spalte4 = c("a", NA, NA, "f", NA, NA),
                           spalte5 = c("c", NA, "d", "g", NA, NA),
                           spalte6 = c("e", NA, NA, NA, "h", NA),
                           check.names = FALSE))

##### janitor #####

# janitor::clean_names()

colnames(df_problematisch)

colnames_problem_solved <- df_problematisch %>% 
                                  janitor::clean_names()
colnames(colnames_problem_solved)

# janitor::excel_numeric_to_date()
 df_problematisch$datum
excel_numerisch_datum_solved <- df_problematisch %>%
                                      mutate(datum = janitor::excel_numeric_to_date(datum))
excel_numerisch_datum_solved$datum


##### DataCombine #####

# DataCombine::FillDown()
# zB fuer Excel-Dateien, die in Pivot-Format importiert werden, als Beispiel siehe: df_problematisch

df_problematisch
(df_spalte_1_korrekt<- FillDown(df_problematisch, 'bezeichnung'))


# alle Spalten, die den Begriff "Spalte" enthalten auffuellen

df_problematisch %>% 
  mutate_at(vars(starts_with("spalte")), funs(DataCombine::FillDown(Var = .)))

# auch bei Gruppierung moeglich!
df_spalte_1_korrekt %>% 
  group_by(bezeichnung) %>% 
  mutate_at(vars(starts_with("spalte")), funs(DataCombine::FillDown(Var = .)))


# DataCombine::MoveFront()
# Spalte(n) nach vorne verschieben

# Spalte 'datum' wird zur ersten Spalte
MoveFront(df_problematisch,c("datum"))

# DataCombine::rmExcept
# objekte in global environment bis auf die genannten Objekte bereinigen
ls()
DataCombine::rmExcept(c("df_spalte_1_korrekt"))
ls()

