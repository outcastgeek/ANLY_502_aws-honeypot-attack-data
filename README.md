
#ANLY 500

**Miscellaneous:**

* R Installation on Mac OSX: `> brew cask install xquartz r-app rstudio`
* R installation on Linux:
```
> sudo yum groupinstall "X Software Development
> sudo dnf install gcc-gfortran pcre-devel xz-devel lzma-devel bzip2-devel readline-devel
> wget https://cran.r-project.org/src/base/R-3/R-3.4.4.tar.gz
> tar xzvf R-3.4.4.tar.gz
> mv R-3.4.4 R
> cd R && ./configure
> make && make check
> sudo make install
```

