# -------------------------------------------------------------------------
#
#         Analyse Renten
#
# -------------------------------------------------------------------------
#
# Author: Lukas Feuer
# Date: 2024-10-28
#

library(tidyverse)
library(lubridate)

d_raw <- readxl::read_xlsx("data/Renten.xlsx")

d_raw |>
  mutate(
    Nummer = NULL
    , Rente_Beginn_Datum = as_date(Rente_Beginn_Datum, origin = "1899-12-30 UTC")
    , Rente_Ende_Datum = as_date(Rente_Ende_Datum, origin = "1899-12-30 UTC")
    , Geburtsdatum = as_date(Geburtsdatum, origin = "1899-12-30 UTC")
    , Geburtsjahr = year(Geburtsdatum)
    , Geschlecht = factor(Geschlecht
                          , levels = c(2, 1, NA)
                          , labels = c("Weiblich", "Männlich"))
    , Rentendauer = interval(Rente_Beginn_Datum, Rente_Ende_Datum) %/% years(1)
    , Rentenalter = interval(Geburtsdatum, Rente_Beginn_Datum) %/% years(1)
    , Ort = str_extract(Adresse, "\\w+$")
    , PLZ = str_extract(Adresse, "[:space:]\\d{5}[:space:]")
  )

x |>
  ggplot(aes(Segment, Rentenalter, group = Segment)) +
  geom_point(position = "jitter", alpha = 0.1) +
  geom_boxplot(fill = NA)

x |>
  ggplot(aes(Rentenalter)) +
  geom_histogram() +
  facet_wrap(vars(Segment))

## TODO
## - Adressen aufteilen - Darstellung geographische Verteilung
## - Kontrolle Datumsumwandlung
## - Errechnung weiterer Daten/Perioden
## - Geschlecht als Faktor
## - Renten Ende ? nie bei Segment Unfall befüllt -> ggf. vereinbarte Dauer?
## - Modellierung (AV Renten_Auszahlung?)
## - Histogramme, z.B. AV nach Geschlecht, Segment
## - Rentenauszahlung monatlich/Jährlich? in dem Fall zu multiplizieren mit Rentenlänge