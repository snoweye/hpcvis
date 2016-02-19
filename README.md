# scribe

# remoter

* **Version:** 0.1-0
* **URL**: https://github.com/wrathematics/remoter
* **License:** [![License](http://img.shields.io/badge/license-BSD%202--Clause-orange.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)
* **Author:** See section below.




## Installation

The development version is maintained on GitHub, and can easily be installed by any of the packages that offer installations from GitHub:

```r
devtools::install_github("RBigData/scribe")
ghit::install_github("RBigData/scribe")
remotes::install_github("RBigData/scribe")
```



## Usage


```r
x <- system.cache(runif(1e4))
y <- system.cache(runif(2e4))

plot(x,y)
```



## Authors

scribe is authored and maintained by members of the pbdR core team:
* Drew Schmidt
* Wei-Chen Chen

With additional contributions from:
* Gaurav Sehrawat
