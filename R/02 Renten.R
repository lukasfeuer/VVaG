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
library(GGally)

thematic::thematic_off()


d_raw <- readxl::read_xlsx("data/Renten.xlsx")

d <- d_raw |>
  mutate(
    Nummer = NULL
    , Segment = factor(Segment)
    , Rente_Beginn_Datum = lubridate::as_date(Rente_Beginn_Datum, origin = "1899-12-30 UTC")
    , Rente_Ende_Datum = lubridate::as_date(Rente_Ende_Datum, origin = "1899-12-30 UTC")
    , Geburtsdatum = lubridate::as_date(Geburtsdatum, origin = "1899-12-30 UTC")
    , Geburtsjahr = lubridate::year(Geburtsdatum)
    , Geschlecht = factor(Geschlecht
                          , levels = c(2, 1, NA)
                          , labels = c("Weiblich", "Männlich"))
    , Rentendauer = lubridate::interval(Rente_Beginn_Datum, Rente_Ende_Datum) %/% lubridate::years(1)
    , Rentenalter = lubridate::interval(Geburtsdatum, Rente_Beginn_Datum) %/% lubridate::years(1)
    , Ort = factor(str_extract(Adresse, "\\w+$"))
    , PLZ = factor(str_extract(Adresse, "[:space:]\\d{5}[:space:]"))
  )


GGally::ggpairs(d[, c("Renten_Auszahlung", "Geschlecht", "Segment", "Geburtsjahr")])

x <- d |>
  filter(Renten_Auszahlung < 30000) |>
  select("Renten_Auszahlung", "Geschlecht", "Segment", "Geburtsjahr", "Rentenalter") #


GGally::ggpairs(x)

GGally::ggpairs(x, mapping = aes(colour = Segment, alpha = 0.5))

x |> filter(!is.na(Geschlecht), Segment == "Unfall") |>
GGally::ggpairs(mapping = aes(colour = Geschlecht, alpha = 0.5))

x |> filter(!is.na(Geschlecht)) |>
GGally::ggpairs(mapping = aes(colour = Geschlecht, alpha = 0.5))


x |>
  ggplot(aes(Segment, Rentenalter, group = Segment)) +
  geom_point(position = "jitter", alpha = 0.1) +
  geom_boxplot(fill = NA)

x |>
  ggplot(aes(Rentenalter)) +
  geom_histogram() +
  facet_wrap(vars(Segment))


d |>
  mutate(kategorie = case_when(
    str_detect(Segment, "haft") ~ "Haftpflichten"
    , TRUE ~ "Unfall")) |>
  reframe(Rentenauszahlung = sum(Renten_Auszahlung), .by = kategorie)

d |>
  reframe(Rentenauszahlung = sum(Renten_Auszahlung), .by = Segment)


# ggf. sieht das noch eindeutiger aus, wenn "Rentenalter" nicht nur grob auf Jahr gerechnet
d |> filter(!is.na(Rente_Ende_Datum)) |>
  ggplot(aes(Rentenalter, Rentendauer))
  + geom_point()

d |> count(Rentendauer, Segment, sort = T) |> print(n = 50)


# warum sind die Startdaten der Renten so seltsam verteilt?
d |>
  select(Rente_Beginn_Datum, Segment) |>
  transmute(
    ym = year(Rente_Beginn_Datum)
    , Segment
  ) |>
  count(ym, Segment) |>
  tsibble(index = ym, key = Segment) |>
  autoplot() |>
  plotly::ggplotly()

# ggf. erklärt sich die Verteilung oben aus den Geburtsdaten
# Bsp. Unfall: hier scheint es sich um eine sehr weite Koohrte zu handeln
d |>
  select(Geburtsdatum, Segment) |>
  transmute(
    ym = year(Geburtsdatum)
    , Segment
  ) |>
  count(ym, Segment) |>
  tsibble(index = ym, key = Segment) |>
  autoplot() |>
  plotly::ggplotly()

p1 <- d |>
  ggplot(aes(Rente_Beginn_Datum, Geburtsdatum, colour = Segment)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "lightgray")
plotly::ggplotly(p1)
# Berücksichtige die unterschiedlichen Verteilungsmuster pro Segment
# Heilwesen: hier gibt es auch Fälle, bei denen der Rentenbeginn mehrere Jahre vor der Geburt liegt
# so einen FAll gibt es auch bei Unfall

p2 <- d |>
  ggplot(aes(Rente_Beginn_Datum, Rentenalter, colour = Segment)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, colour = "lightgray")
plotly::ggplotly(p2)
# Warum beginnen so viele Unfall-Renten mit dem gesetzlichen
# Rentenalter?
# -----> gesetzliche vs. Private Unfallversicherung
# bzgl. Heilwesen: HDI hat laut Website deutlich länger Erfahrung in diesem Bereich
# , das kann also nicht alles sein --> nur eine Kohorte/ein Produkt?

# keine ersichtliche Saisonalität
d |>
  select(Rente_Beginn_Datum) |>
  transmute(
    ym = yearquarter(Rente_Beginn_Datum)
  ) |>
  count(ym) |>
  tsibble(index = ym) |>
  fill_gaps(n = 0) |>
  gg_subseries(n)

## TODO
## - Adressen aufteilen - Darstellung geographische Verteilung
## - Kontrolle Datumsumwandlung
## - Errechnung weiterer Daten/Perioden
## - Geschlecht als Faktor
## - Renten Ende ? nie bei Segment Unfall befüllt -> ggf. vereinbarte Dauer?
## - Modellierung (AV Renten_Auszahlung?)
## - Histogramme, z.B. AV nach Geschlecht, Segment
## - Rentenauszahlung monatlich/Jährlich? in dem Fall zu multiplizieren mit Rentenlänge
## - Räumliche Darstellung Rentenauszahlung/Häufigkeit nach PLZ/Ort auf Karte
## - Rentendauer in Monate/Wochen/Tage umrechenen -> Muster?
## - Inhaltlich: wann/wem wird die Rente ausgezahlt?
## - Warum habe so viele alte Personen die Unfallrente aber in anderen Bereichen sind es viele junge Leute?
## - Was ist mit den Einträgen ohne Geschlechtsangabe? tendenziell viele hohe Renten --> ignorieren -> sind nur 5
## - Rentenbeginn als Zeitreihe umrechnen
## - ggf. auch Geburtsdaten als Zeitreiehe --> verzerrt da jüngere Geburtstage weniger vorkommen werden