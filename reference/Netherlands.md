# Netherlands datasets

Datasets of the Netherlands for 2022 at three levels: `NLD_prov` (12)
provinces, `NLD_muni` (345) municipalities and `NLD_dist` (3340)
districts , all class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Usage

``` r
NLD_prov

NLD_muni

NLD_dist
```

## Source

https://www.cbs.nl/nl-nl/maatwerk/2024/11/kerncijfers-wijken-en-buurten-2022

## Details

The data variables for `NLD_muni` and `NLD_dist` are identical:

|                      |                                                                                                                                                                                                 |
|----------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Variable**         | **Description**                                                                                                                                                                                 |
| `code`               | Code. Format is "GMaaaa" (municipality/'**g**e**m**eente') and "WKaaaabb" (district/**w**ij**k**). Here, "aaaa" represents the municipality id number, and "bb" the additional district number. |
| `name`               | Name.                                                                                                                                                                                           |
| `province`           | Province name.                                                                                                                                                                                  |
| `area`               | Total area in km2. This area corresponds to the area of the polygons (including inland waters, excluding coastal waters), but is more precise because it is based on non-simplified geometries. |
| `urbanity`           | Level of urbanity. Five classes, determined by the number of addresses per km2 (break values are 2500, 1500, 1000, and 500).                                                                    |
| `population`         | The total population count at 2022-01-01.                                                                                                                                                       |
| `pop_0_14`           | Percentage (rounded) of people between 0 and 15.                                                                                                                                                |
| `pop_15_24`          | Percentage (rounded) of people between 15 and 25.                                                                                                                                               |
| `pop_25_44`          | Percentage (rounded) of people between 25 and 45.                                                                                                                                               |
| `pop_45_64`          | Percentage (rounded) of people between 45 and 65.                                                                                                                                               |
| `pop_65plus`         | Percentage (rounded) of people of 65 and older.                                                                                                                                                 |
| `dwelling_total`     | Number of dwellings.                                                                                                                                                                            |
| `dwelling_value`     | Average dwelling value (Dutch: WOZ-value).                                                                                                                                                      |
| `dwelling_ownership` | Percentage of dwellings owned by the residents.                                                                                                                                                 |
| `employment_rate`    | Share of the employed population within the total population from 15 to 75 years old.                                                                                                           |
| `income_low`         | Percentage of individuals in private households belonging to the lowest 40% of personal income nationwide.                                                                                      |
| `income_high`        | Percentage of individuals in private households belonging to the highest 20% of personal income nationwide.                                                                                     |
| `edu_appl_sci`       | Percentage of people aged 15 to 75 with a university of applied sciences (Dutch: HBO) or university (Dutch: WO) degree.                                                                         |

See source for detailed information about the variables.

This dataset, created Noveber 2024, is an update from the datasets
`NLD_muni` and `NLD_prov` used in tmap \<= 3, which has been created
around 2016. Note that the number of municipalities have been reduced
(due to mergings). All old variables are included, except for variables
related to ethnicity. Many new variable have been added, and moreover,
district (Dutch: wijk) level data have added: `NLD_dist`.

The CRS (coordinate reference system) used is the Rijksdriekhoekstelsel
New, EPSG 28992. Coordinates have been rounded to meters to reduce file
size.

## References

Statistics Netherlands (2024), The Hague/Heerlen, Netherlands,
<https://www.cbs.nl/>.
