Web Scraping mit R
================

Kleines R-Script, um die [Stellenbörse der
Charité](https://www.charite.de/karriere/stellenboerse/) nach Jobs für
Studierende zu durchforsten, die mit dem zugeordneten Institut,
Veröffentlichungsdatum und Bewerbungsfrist sowie dem Link zur jeweiligen
Stellenauschreibung in einem Tibble gesammelt werden.

``` r
url <- "https://www.charite.de/karriere/stellenboerse/"
search_string <- "STUD"
urls <- get_sites_to_crawl(url)
charite_jobs <- get_all_jobs(urls, search_string)
charite_jobs
```

    ## # A tibble: 18 × 4
    ##    institut                                    eingestellt bewerbungsfrist url  
    ##    <chr>                                       <date>      <date>          <chr>
    ##  1 -                                           2022-04-27  2022-05-15      http…
    ##  2 -                                           2022-04-27  2022-05-15      http…
    ##  3 Institut für Pathologie: Diagnostische und… 2022-04-27  2022-05-27      http…
    ##  4 Klinik für Allgemein und Viszeralchirurgie… 2022-04-27  2024-05-31      http…
    ##  5 Klinik für Anästhesiologie m.S. operative … 2022-04-21  2022-05-15      http…
    ##  6 Medizinische Klinik für Kardiologie | CBF:… 2022-04-21  2022-05-15      http…
    ##  7 Sonstige Chirurgische Klinik | CCM | CVK: … 2022-04-14  2022-05-31      http…
    ##  8 Medizinische Klinik mit Schwerpunkt Hämato… 2022-04-13  2022-05-15      http…
    ##  9 -                                           2022-04-13  2022-05-31      http…
    ## 10 Klinik für Neurologie mit Experimenteller … 2022-04-13  2022-05-31      http…
    ## 11 Diagnostische und interventionelle Radiolo… 2022-04-13  2024-02-29      http…
    ## 12 Klinik für Psychiatrie und Psychotherapie … 2022-04-08  2022-05-07      http…
    ## 13 -                                           2022-04-08  2022-05-31      http…
    ## 14 Klinik für Anästhesiologie m.S. operative … 2022-03-31  2022-05-31      http…
    ## 15 Institut für Klinische Pharmakologie und T… 2022-03-31  2022-09-30      http…
    ## 16 Sonstig                                     2022-01-10  2022-06-30      http…
    ## 17 Sonstige Medizinische Klinik für Kardiolog… 2021-07-28  2022-06-30      http…
    ## 18 Sonstige Bibliothek                         2020-11-20  2023-12-31      http…
