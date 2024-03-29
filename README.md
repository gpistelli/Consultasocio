# Consultasocio

A package to help researchers study business owners capital composition with [Consultasocio](https://www.consultasocio.com) data

### How to install

Our package can be acessed with devtools:

``` r
devtools::install_github("gpistelli/Consultasocio")
```

## Aims and objectives

This package originated from our [Master Thesis](https://acervodigital.ufpr.br/handle/1884/74947), especially our second chapter, focused in Paraná's industry employers unions (check out its [repo](https://github.com/gpistelli/Federacoes_Ind)), as an attempt to grasp these groups economic diversity.

As a result, we developed a set of functions that offered:

- Webscraping to extract page informations
- Store html pages, saving it in the [web archive](https://web.archive.org/)
- Data mining, fitting these infos into a structured data.frame with categorical data

Our research is deeply influenced by Geometrical Data Analysis methodologies, whose principles were fundamental to our research, so our functions are written to work well with [FactoMineR](https://cran.r-project.org/web/packages/FactoMineR/index.html), [GDAtools](https://cran.r-project.org/web/packages/GDAtools/index.html) and [factoextra](https://cran.r-project.org/web/packages/factoextra/index.html).

You can find a basic introduction to what our package offers right now in our project page: https://gpistelli.github.io/Consultasocio/

For a more methodological debate and introduction, check our research paper in ST07 - 46º ANPOCS: [Mapeando a composição econômica de entidades empresariais: proposição de um método espacial](https://www.encontro2022.anpocs.com/atividade/view?q=YToyOntzOjY6InBhcmFtcyI7czozNjoiYToxOntzOjEyOiJJRF9BVElWSURBREUiO3M6MzoiMTM0Ijt9IjtzOjE6ImgiO3M6MzI6ImE1NDM1YjRjYzcyZmMyMDNkMjk2ODhiNDI2NzRlNTJlIjt9&ID_ATIVIDADE=134).

## Thanks

This package was developed partially, as original source code, during our Master Thesis research, which was funded by CAPES Masters scolarship.

As our acknowledgement of this funding, this code was made public and open-source to all people interested.

## References

CONSULTASOCIO. Disponível em: <https://www.consultasocio.com>

KASSAMBARA, Alboukadel; MUNDT, Fabian Mundt. factoextra: Extract and Visualize the Results of Multivariate Data Analyses. R package version 1.0.7. 2020. Disponível em:
<https://CRAN.Rproject.org/package=factoextra>.

LE, Sebastien; JOSSE, Julie; HUSSON, Francois. FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 2008.

WAYBACK MACHINE. Disponível em: <https://web.archive.org/>

WICKHAM, Hadley. rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.6. 2020. Disponível em: <https://CRAN.R-project.org/package=rvest>.

_____; et al. Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, 2019.
