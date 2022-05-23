Web Scraping with R
================

Use this R script to search the job board of the
[Charité](https://www.charite.de/karriere/stellenboerse/), e.g. for
students. The search results are collected in a tibble with the assigned
institute, publication date and application deadline as well as the link
to the respective job posting.

``` r
url <- "https://www.charite.de/karriere/stellenboerse/"
search_string <- "STUD"
urls <- get_sites_to_crawl(url)
charite_jobs <- get_all_jobs(urls, search_string)
charite_jobs
```

    ## # A tibble: 18 × 4
    ##    institute                                         published  deadline   url  
    ##    <chr>                                             <date>     <date>     <chr>
    ##  1 -                                                 2022-05-23 2022-06-15 http…
    ##  2 Klinik für Geriatrie und Altersmedizin: Innere M… 2022-05-20 2022-06-15 http…
    ##  3 Klinik für Neurologie mit Experimenteller Neurol… 2022-05-10 2022-05-31 http…
    ##  4 Medizinische Klinik m.S. Hämatologie, Onkologie … 2022-05-10 2022-05-31 http…
    ##  5 Klinik für Dermatologie, Venerologie und Allergo… 2022-05-10 2022-05-31 http…
    ##  6 Institut für Pathologie: Diagnostische und präve… 2022-04-27 2022-05-27 http…
    ##  7 -                                                 2022-04-27 2022-05-31 http…
    ##  8 -                                                 2022-04-27 2022-05-31 http…
    ##  9 Klinik für Allgemein und Viszeralchirurgie | CBF… 2022-04-27 2024-05-31 http…
    ## 10 Medizinische Klinik für Kardiologie | CBF: Herz,… 2022-04-21 2022-06-30 http…
    ## 11 -                                                 2022-04-13 2022-05-31 http…
    ## 12 Klinik für Neurologie mit Experimenteller Neurol… 2022-04-13 2022-05-31 http…
    ## 13 Diagnostische und interventionelle Radiologie un… 2022-04-13 2024-02-29 http…
    ## 14 -                                                 2022-04-08 2022-05-31 http…
    ## 15 Klinik für Anästhesiologie m.S. operative Intens… 2022-03-31 2022-05-31 http…
    ## 16 Institut für Klinische Pharmakologie und Toxikol… 2022-03-31 2022-09-30 http…
    ## 17 Sonstige Medizinische Klinik für Kardiologie | C… 2021-07-28 2022-06-30 http…
    ## 18 Sonstige Bibliothek                               2020-11-20 2023-12-31 http…

If the search string is not provided, all jobs vacant at the Charité are
gathered, which are currently 200.

``` r
get_all_jobs(urls) %>%
  nrow()
```

    ## [1] 200

You can now take a closer look at the jobs found and scrape them
further, for example, to find out the monthly working hours and to
search them again for certain words, for example, “R”. Currently, about
one in five student jobs is somehow related to R!

``` r
charite_jobs_with_details <- charite_jobs$url %>%
  get_all_job_details(search_string = "R") %>%
  cbind(charite_jobs) %>%
  select(institute, published, deadline, everything())
mean(charite_jobs_with_details$contains_search_string)
```

    ## [1] 0.2222222

| institute                                                                                                           | published  | deadline   | starting_date | limited_for | hours_per_month | contains_search_string | url                                                                                                                         |
|:--------------------------------------------------------------------------------------------------------------------|:-----------|:-----------|:--------------|:------------|:----------------|:-----------------------|:----------------------------------------------------------------------------------------------------------------------------|
| \-                                                                                                                  | 2022-05-23 | 2022-06-15 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_052204_psl_qs/>        |
| Klinik für Geriatrie und Altersmedizin: Innere Medizin mit Gastroenterologie und Nephrologie (CC 13)                | 2022-05-20 | 2022-06-15 | ab sofort     | 24 Monate   | 40              | TRUE                   | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_052203_cc13_wgr/>       |
| Klinik für Neurologie mit Experimenteller Neurologie: Neurologie, Neurochirurgie und Psychiatrie (CC 15)            | 2022-05-10 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | TRUE                   | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042213_cc_15_kfn_ncrc/> |
| Medizinische Klinik m.S. Hämatologie, Onkologie und Tumorimmunologie \| CVK: Tumormedizin (CC 14)                   | 2022-05-10 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042204_cc14_mkmshot-1/> |
| Klinik für Dermatologie, Venerologie und Allergologie: Innere Medizin und Dermatologie (CC 12)                      | 2022-05-10 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_052201_cc12_kfdva/>    |
| Institut für Pathologie: Diagnostische und präventive Labormedizin (CC 5)                                           | 2022-04-27 | 2022-05-27 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_032214_cc05_pubfe-1/>  |
| \-                                                                                                                  | 2022-04-27 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | TRUE                   | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042210_bih_agcm_ii/>    |
| \-                                                                                                                  | 2022-04-27 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | TRUE                   | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042210_bih_agcm_ii-1/>  |
| Klinik für Allgemein und Viszeralchirurgie \| CBF: Chirurgische Medizin (CC 8)                                      | 2022-04-27 | 2024-05-31 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_042208_cc08_kfauv/>    |
| Medizinische Klinik für Kardiologie \| CBF: Herz, Kreislauf und Gefäßmedizin (CC 11)                                | 2022-04-21 | 2022-06-30 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042205_cc11_mkfk/>      |
| \-                                                                                                                  | 2022-04-13 | 2022-05-31 | ab sofort     | 24 Monate   | 80              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_032208_gbf_ecs/>       |
| Klinik für Neurologie mit Experimenteller Neurologie: Neurologie, Neurochirurgie und Psychiatrie (CC 15)            | 2022-04-13 | 2022-05-31 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042203_cc15_nnup/>      |
| Diagnostische und interventionelle Radiologie und Nuklearmedizin (CC 6)                                             | 2022-04-13 | 2024-02-29 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_042207_cc06_duirn/>    |
| \-                                                                                                                  | 2022-04-08 | 2022-05-31 | ab sofort     | 31.05.2023  | 80              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_042201_bih_bcrt/>       |
| Klinik für Anästhesiologie m.S. operative Intensivmedizin \| CCM \| CVK: Anästhesiologie und Intensivmedizin (CC 7) | 2022-03-31 | 2022-05-31 | 01.05.2022    | 31.12.2022  | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentische_mitarbeiterin_stud_b_032217_cc07_kfamsoi/>   |
| Institut für Klinische Pharmakologie und Toxikologie: Diagnostische und präventive Labormedizin (CC 5)              | 2022-03-31 | 2022-09-30 | 01.05.2022    | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_b_032214_cc05_pubfe/>    |
| Sonstige Medizinische Klinik für Kardiologie \| CBF: Herz, Kreislauf und Gefäßmedizin (CC 11)                       | 2021-07-28 | 2022-06-30 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/2_studentische_mitarbeiterinnen_stud_hk_072110_cc11_mkk/> |
| Sonstige Bibliothek                                                                                                 | 2020-11-20 | 2023-12-31 | ab sofort     | 24 Monate   | 40              | FALSE                  | <https://www.charite.de/service/stellenangebot/angebot/detailinfo/studentischer_mitarbeiterin_stud_hk_112005_f_mb/>         |
