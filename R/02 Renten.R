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
library(fpp3)

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
    , Rente_Beginn_Alter = lubridate::interval(Geburtsdatum, Rente_Beginn_Datum) %/% lubridate::years(1)
    , Rente_Ende_Alter = lubridate::interval(Geburtsdatum, Rente_Ende_Datum) %/% lubridate::years(1)
    , Ort = factor(str_extract(Adresse, "\\w+$"))
    , PLZ = factor(str_squish(str_extract(Adresse, "[:space:]\\d{5}[:space:]")))
    , Strasse = factor(str_squish(str_extract(Adresse, "^\\D+(?=\\d)")))
    , Nr = parse_number(str_extract(Adresse, "[:space:]\\d{1,3}(?=,)"))
    , Rentenmonate = lubridate::interval(Rente_Beginn_Datum, lubridate::as_date("2024-11-05")) %/% months(1)
    #, Rentendauer_Monate = case_when(
    #  !is.na(Rente_Ende_Datum) ~ lubridate::interval(Rente_Beginn_Datum, Rente_Ende_Datum) %/% months(1)
    #  , TRUE ~ lubridate::interval(Rente_Beginn_Datum, lubridate::as_date("2024-11-05")) %/% months(1)
    #)
    , Alter_heute = lubridate::interval(Geburtsdatum, lubridate::as_date("2024-11-05")) %/% lubridate::years(1)
)

# Theoretisches heutiges Alter -> hier fehlen wahrscheinlich Sterbedaten
d |> ggplot(aes(Alter_heute)) +
  geom_histogram() +
  geom_vline(xintercept = 100)

# Korrektur für Inflation?
Rentenzahlungen_ts <- d |>
  filter(year(Rente_Beginn_Datum) > 1991) |>
  transmute(
    Renten_Auszahlung, Rente_Beginn_Datum = yearmonth(Rente_Beginn_Datum)
    , Datum_Zwischenstand = yearmonth("2024-11-05")
  ) |>
  rownames_to_column("id") |>
  pivot_longer(cols = c(Rente_Beginn_Datum, Datum_Zwischenstand)) |>
  as_tsibble(index = value, key = id) |>
  tsibble::fill_gaps() |>
  tidyr::fill(Renten_Auszahlung, .direction = "down") |>
  select(-name)

# Inflationsdaten auf die Schnelle nur ab 1992 - für die Anschauungszwecke okay
# -> ein paar Daten müssen ausgefiltert werden
d |> ggplot(aes(Rente_Beginn_Datum)) + geom_histogram() + geom_vline(xintercept = as_date("1992-01-01"))

CPI <- readxl::read_excel("data/verbraucherpreisindex-lange-reihen-xlsx-5611103.xlsx"
                   , sheet = "CPI")
Inflation <- CPI |>
  mutate(
    Jahr = parse_number(Jahr)
    , Index = Index / 100
  )

Rentenzahlungen_ts |>
  as_tibble() |>
  mutate(Jahr = year(value)) |>
  left_join(Inflation, by = "Jahr") |>
  tidyr::replace_na(
    list(Index = 1) # keine Daten für 2023, 2024
  ) |>
  transmute(
    value #, id
    , Renten_Auszahlung
    , Renten_Auszahlung_ib = Renten_Auszahlung * Index
  ) |>
  group_by(value) |>
  dplyr::summarise(Renten_Auszahlung = sum(Renten_Auszahlung)
            , Renten_Auszahlung_ib = sum(Renten_Auszahlung_ib)
             ) |>
  ggplot(aes(value)) +
  geom_line(aes(y = Renten_Auszahlung), colour = "darkgreen") +
  geom_line(aes(y = Renten_Auszahlung_ib, colour = "darkred")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "€")) +
  labs(x = NULL, y = NULL,
       title = "Monatliche Rentenauszahlungen"
       , subtitle = "Inflationsanpassung in Rot")


Rentenzahlungen_ts |>
  as_tibble() |>
  mutate(Jahr = year(value)) |>
  left_join(Inflation, by = "Jahr") |>
  tidyr::replace_na(
    list(Index = 1) # keine Daten für 2023, 2024
  ) |>
  transmute(
    value #, id
    , Renten_Auszahlung
    , Renten_Auszahlung_ib = Renten_Auszahlung * Index
  ) |>
  dplyr::summarise(Renten_Auszahlung = sum(Renten_Auszahlung)
                   , Renten_Auszahlung_ib = sum(Renten_Auszahlung_ib)
  )




d |>
  count(Ort, sort = T) |>
  ggplot(aes(x = n, y = Ort)) +
  geom_point()

d |>
  count(Strasse, sort = T) |>
  ggplot(aes(x = n, y = Strasse)) +
  geom_point()


library(ggridges)
d |>
  ggplot(aes(x = Renten_Auszahlung, Ort, fill = Ort)) +
  geom_density_ridges(alpha = 0.3) +
  geom_point(position = "jitter", alpha = 0.2, aes(color = Ort)) +
  theme_ridges() +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)


GGally::ggpairs(d[, c("Renten_Auszahlung", "Geschlecht", "Segment", "Geburtsjahr", "Rente_Beginn_Alter", "Rente_Ende_Alter")])

x <- d |>
  filter(Renten_Auszahlung < 30000) |>
  select("Renten_Auszahlung", "Geschlecht", "Segment", "Geburtsjahr", "Rente_Beginn_Alter") #


GGally::ggpairs(x)

GGally::ggpairs(x, mapping = aes(colour = Segment, alpha = 0.5))

x |> filter(!is.na(Geschlecht), Segment == "Unfall") |>
GGally::ggpairs(mapping = aes(colour = Geschlecht, alpha = 0.5))

x |> filter(!is.na(Geschlecht)) |>
GGally::ggpairs(mapping = aes(colour = Geschlecht, alpha = 0.5))


x |>
  ggplot(aes(Segment, Rente_Beginn_Alter, group = Segment)) +
  geom_point(position = "jitter", alpha = 0.1) +
  geom_boxplot(fill = NA)

x |>
  ggplot(aes(Rente_Beginn_Alter)) +
  geom_histogram() +
  facet_wrap(vars(Segment))


d |>
  mutate(kategorie = case_when(
    str_detect(Segment, "haft") ~ "Haftpflichten"
    , TRUE ~ "Unfall")) |>
  reframe(Rentenauszahlung = sum(Renten_Auszahlung), .by = kategorie)

d |>
  reframe(Rentenauszahlung = sum(Renten_Auszahlung), .by = Segment)


# ggf. sieht das noch eindeutiger aus, wenn "Rente_Beginn_Alter" nicht nur grob auf Jahr gerechnet
d |> filter(!is.na(Rente_Ende_Datum)) |>
  ggplot(aes(Rente_Beginn_Alter, Rentendauer))+
  geom_point()

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
  ggplot(aes(Rente_Beginn_Datum, Rente_Beginn_Alter, colour = Segment)) +
  geom_point() +
  geom_abline(slope = 0, intercept = 0, colour = "lightgray")
plotly::ggplotly(p2)
# Warum beginnen so viele Unfall-Renten mit dem gesetzlichen
# Rente_Beginn_Alter?
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


d |>
  filter(Renten_Auszahlung < 10000) |>
  ggplot(aes(Renten_Auszahlung)) +
  geom_histogram(bins = 40)
# log-normal Verteilung?
# was sing ggf. Gemeinsamkeiten bei den Ausreißern
# Statisitiken mean/median

d_mean = d |>
  filter(Renten_Auszahlung < 10000) |>
  summarise(mean(Renten_Auszahlung)) |>
  pull()

d_median = d |>
  filter(Renten_Auszahlung < 10000) |>
  summarise(median(Renten_Auszahlung)) |>
  pull()

d |>
  filter(Renten_Auszahlung < 10000) |>
  ggplot(aes(Renten_Auszahlung)) +
  geom_density() +
  geom_vline(xintercept = d_mean) +
  geom_vline(xintercept = d_median)

library(rethinking)
m1 <- quap(
  flist = alist(
    Renten_Auszahlung ~ dlnorm(mu, sigma)
    , mu ~ dnorm(1, 10)
    , sigma ~ dunif(0, 10)
  )
    , data = d
)
m1 |> precis()
d |>
  transmute(
    Renten_Auszahlung
    , Renten_Auszahlung_log = log(Renten_Auszahlung)
  ) |>
  ggplot(aes(Renten_Auszahlung_log)) +
  geom_density()
d |>
  transmute(
    Renten_Auszahlung
    , Renten_Auszahlung_log = log(Renten_Auszahlung)
  ) |>
  ggplot(aes(Renten_Auszahlung_log)) +
  geom_histogram(bins = 45, fill = "darkgreen", color = "lightgray")
# logarithmierte Rentenauszahlungen sind offenbar normalverteilt als logs


d |>
  ggplot(aes(Rente_Ende_Alter)) +
  geom_histogram(bins = 20, aes(fill = Segment))
# Bei diesen Renten (alle nich-Unfall) - wird die Rente nur bis zum (voraussichtlichen)
# Entrittsdatum der gesetzlichen Altersrente gezahlt, da es sich um eine Rente zum Ausgleich
# des Verdienstausfalls handelt + Ausreißer sind hier Personen mit einem sehr niedrigen Rente_Beginn_Alter
#   UND diese sind alle Heilwesenhaftpflicht
# Frage: was ist bei den Haftpflicht-Renten anders, welche kein Enddatum hinterlegt haben?
#     --> haben sich ggf. schon in Rente befunden?
#
# KF-Haftpflicht: alle in der gleichen PLZ, allerdings untersch. Straßen


d |>
  #filter(Renten_Auszahlung < 10000) |>
  #ggplot(aes(Rente_Beginn_Datum, log(Renten_Auszahlung))) +
  ggplot(aes(Rente_Beginn_Datum, Renten_Auszahlung)) +
  geom_point(aes(colour = Segment)) +
  geom_smooth(method = "lm")



d |> count(Nr, sort = T) |> plot()
GGally::ggpairs(select(d, Ort, PLZ, Nr))


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
## - Renten pro Stadt insgesmat
## - Zeitreihe/Dichte pro Stadt über die Zeit hinweg -> dieses besondere Diagramm mit gestapelten Wellen
## - Irgend ein spaß mit den Hausnummer?
##
## Auffällig: Ot-PLZ ist eine 1:1 Beziehung - synthetische Daten
## Auffällig: es gibt nur ein Enddatum (wahrscheinlich geschätzt wg. Renteneintritt) aber
##            Sterbedaten wären auch zu berücksichtigen