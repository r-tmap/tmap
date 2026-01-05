# World dataset

World dataset, class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Usage

``` r
World
```

## Source

NED: Natural Earth Data <https://www.naturalearthdata.com/>

HPI: Happy Planet Index <https://happyplanetindex.org/>

UNDP: Human Development Report (2024)
<https://hdr.undp.org/content/human-development-report-2023-24>

WB: World Bank <https://data.worldbank.org>

OWiD: Our World in Data <https://ourworldindata.org>

RSF: Reporters Without Borders <https://rsf.org/en/index>

## Details

|                |            |                                                                                                                                                                                                       |
|----------------|------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Variable**   | **Source** | **Description**                                                                                                                                                                                       |
| `iso_a3`       | NED        | ISO 3166-1 alpha-3 three-letter country code (see below)                                                                                                                                              |
| `name`         | NED        | Country name                                                                                                                                                                                          |
| `sovereignt`   | NED        | Sovereignt country name                                                                                                                                                                               |
| `continent`    | NED        | Continent (primary; some countries are transcontinental)                                                                                                                                              |
| `area`         | NED        | Area in km2                                                                                                                                                                                           |
| `pop_est`      | NED        | Population estimation                                                                                                                                                                                 |
| `pop_est_dens` | NED        | Population estimation per km2                                                                                                                                                                         |
| `economy`      | NED        | Economy class                                                                                                                                                                                         |
| `income_grp`   | NED        | Income group                                                                                                                                                                                          |
| `gdp_cap_est`  | NED        | GDP per capita (estimated)                                                                                                                                                                            |
| `life_exp`     | HPI        | Life expectancy. The average number of years an infant born in that country is expected to live                                                                                                       |
| `well_being`   | HPI        | Well being. Self-reported from 0 (worst) to 10 (best)                                                                                                                                                 |
| `footprint`    | HPI        | Carbon footprint. Per capita greendwelling gas emissions associated with consumption and economic activity                                                                                            |
| `HPI`          | HPI        | Happy Planet Indicator. An index of human well-being and environmental impact that was introduced by the New Economics Foundation in 2006. Approximate formula: `(life_exp * well_being) / footprint` |
| `inequality`   | WB         | Income inequality: Gini coefficient (World Bank variable SI.POV.GINI) A value of 0 represents perfect equality, while 100 implies perfect inequality                                                  |
| `gender`       | UNDP/OWiD  | Gender Inequality Index (GII) Composite metric using reproductive health, empowerment and the labour market. A low value indicates low inequality between women and men, and vice-versa               |
| `press`        | RSF        | World Press Freedom Index. Degree of freedom that journalists, news organizations and netizens have                                                                                                   |

See sources for more detailed information about the variables.

This dataset, created Noveber 2024, is an update from the old version,
which has been created around 2016. All variables from the old version
are included, but updated. Furthermore, gender ineuqlity and press
freedom have been added.

ISO country-code: two countries have user-assigned codes, namely: XKX is
used for Kosovo (conform European Union and World Bank) (was UNK in the
old version); XNC is used for Northern Cyprus (was CYN in the old
version).

For some variables data were available from multiple years, but
availability was different across countries. In those cases, the most
recent values were taken.
