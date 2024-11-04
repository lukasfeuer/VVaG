# -------------------------------------------------------------------------
#
#         Analyse Freie Berufe
#
# -------------------------------------------------------------------------
#
# Author: Lukas Feuer
# Date: 2024-10-28
#

library(tidyverse)

d_raw <- readxl::read_xlsx("data/Aufgabe FB.xlsx", skip = 3)


# Übersicht Inflationsindizes ---------------------------------------------

d <- d_raw |>
  transmute(
    Jahr = factor(Anfalljahr)
    , Schadensbedarf = `Historischer Schadenbedarf`
    , `Index A` = `Jährliche Inflation Index A`
    , `Index B` = `Jährliche Inflation Index B`
    , `Index C` = `Jährliche Inflation Index C`
  ) |>
  pivot_longer(cols = 2:5, names_to = "name", values_to = "Jährliche Inflation")

d |>
  ggplot(aes(Jahr, `Jährliche Inflation`, colour = name, group = name)) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", ncol = 1)



# Absoluter Index ---------------------------------------------------------

d_raw |>
  transmute(
    Jahr = factor(Anfalljahr)
    , Schadensbedarf = `Historischer Schadenbedarf`
    , `Index A` = `Jährliche Inflation Index A` +1
    , `Index B` = `Jährliche Inflation Index B` +1
    , `Index C` = `Jährliche Inflation Index C` +1
  ) -> x

tribble(
  ~Jahr, ~Schadensbedarf, ~`Index A`, ~`Index B`, ~`Index C`
  , 2012, 0, 100, 100, 100
) |> mutate(Jahr = factor(Jahr)) |>
  bind_rows(x) |>
  mutate(
    A = accumulate(`Index A`, `*`)
    , B = accumulate(`Index B`, `*`)
    , C = accumulate(`Index C`, `*`)
  ) |>
  select(Jahr, A, B, C) |>
  pivot_longer(2:4, names_to = "Index", values_to = "Wert") |>
  ggplot(aes(Jahr, Wert, group = Index, colour = Index)) +
  geom_line() +
  facet_wrap(vars(Index), ncol = 1, scales = "free_y")



  tribble(
    ~Jahr, ~CPI, ~Veränderungsraten,
    2012, 91.7, NA,
    2013, 93.1, 0.015267,
    2014, 94, 0.009667,
    2015, 94.5, 0.005319,
    2016, 95, 0.005291,
    2017, 96.4, 0.014737,
    2018, 98.1, 0.017635,
    2019, 99.5, 0.014271,
    2020, 100, 0.005025,
    2021, 103.2, 0.032,
    2022, 110.2, 0.067829,
    2023, 116.7, 0.058984
  ) |>
    filter(Jahr > 2012) |>
    #mutate(Jahr = factor(Jahr)) |>
    ggplot(aes(Jahr, Veränderungsraten)) +
    geom_line()


IndexA1 <- d_raw |>
  transmute(Jahr = factor(Anfalljahr+1)
            , `Index A +1` = `Jährliche Inflation Index A`)

d2 <- d_raw |>
  transmute(
    Jahr = factor(Anfalljahr)
    , Schadensbedarf = `Historischer Schadenbedarf`
    , `Index A` = `Jährliche Inflation Index A`
    , `Index B` = `Jährliche Inflation Index B`
    , `Index C` = `Jährliche Inflation Index C`
  ) |>
  left_join(IndexA1) |>
  filter(Jahr != 2013) |>
  pivot_longer(cols = 2:6, names_to = "name", values_to = "Jährliche Inflation")


d_raw |>
  transmute(
    Jahr = factor(Anfalljahr)
    , Schadensbedarf = `Historischer Schadenbedarf`
    , `Index A` = `Jährliche Inflation Index A`
    , `Index B` = `Jährliche Inflation Index B`
    , `Index C` = `Jährliche Inflation Index C`
  ) |>
  left_join(IndexA1) |>
  filter(Jahr != 2013) |>
  GGally::ggpairs(2:6)
