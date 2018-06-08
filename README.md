
## ANLY_502_aws-honeypot-attack-data

**Miscellaneous:**

* To generate a PDF report, open a the file ending in `.Rmd`, then press (On Mac: Cmd + Shift + K) or (Ctrl + Shift + K)

**Statistics Books:**

* [OpenIntro Statistics](https://www.openintro.org/stat/textbook.php?stat_book=os)
* [Introductory Statistics with Randomization and Simulation](https://www.openintro.org/stat/textbook.php?stat_book=isrs)
* [Advanced High School Statistics](https://www.openintro.org/stat/textbook.php?stat_book=aps)

**R and RStudio Installation:**

* R Installation on Mac OSX: `> brew cask install xquartz r-app rstudio`
* R installation on Linux (Fedora):

```bash
> sudo yum groupinstall "X Software Development
> sudo dnf install gcc-gfortran pcre-devel xz-devel lzma-devel bzip2-devel readline-devel
> wget https://cran.r-project.org/src/base/R-3/R-3.4.4.tar.gz
> tar xzvf R-3.4.4.tar.gz
> mv R-3.4.4 R
> cd R && ./configure
> make && make check
> sudo make install
```

* Run R Studio via Docker:

```bash
docker run --rm -p 8787:8787 -e "USER=user" -e "PASSWORD=password" -e "ROOT=TRUE" rocker/rstudio
```

**Data Manipulation:**

* [Group by one or more variables](https://dplyr.tidyverse.org/reference/group_by.html)
* [Merging in R](https://rpubs.com/NateByers/Merging)
* [How to add a cumulative column to an R dataframe using dplyr?](https://stackoverflow.com/questions/21818427/how-to-add-a-cumulative-column-to-an-r-dataframe-using-dplyr/21818500#21818500)
* [Select distinct/unique rows](https://dplyr.tidyverse.org/reference/distinct.html)

**Mapping with R:**

* [Ploting the world map in R](https://stackoverflow.com/questions/30706124/ploting-the-world-map-in-r)
* [Making Maps with R](http://aejaffe.com/summerR_2015/modules/mapping_module.html)
