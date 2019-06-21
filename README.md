
# mixup

[![Lifecycle
](https://img.shields.io/badge/lifecycle-maturing-blue.svg?style=flat)
![R 
%>%= 3.2.0](https://img.shields.io/badge/R->%3D3.2.0-blue.svg?style=flat)
![Dependencies
](https://img.shields.io/badge/dependencies-none-brightgreen.svg?style=flat)

mixup is an R package for data-agnostic data-augmentation inspired by 
[mixup: Beyond Empirical Risk Minimization](https://arxiv.org/abs/1710.09412)


## Usage 

Create additional training data:
```
library(mixup)

# Use builtin mtcars dataset
data(mtcars)
str(mtcars)
summary(mtcars[, -9])
summary(mtcars$am)

mtcars.mix <- mixup(mtcars[, -9], mtcars$am)
summary(mtcars.mix$x)
summary(mtcars.mix$y)
```


## Installation

Requires R version 3.2.0 and higher.

```
install.packages('devtools') # Install devtools package if necessary
library(devtools)
devtools::install_github('makeyourownmaker/mixup')
```


## Details

...

Limitations 
  Tabular data
  Numeric data
  Binary classification
  'input' mixup
  Duplicate rows are ignored
  ...

Errors
  NAs
  ...
  
Regularisation
  ...

Further info:
mixup: Beyond Empirical Risk Minimization
By Hongyi Zhang, Moustapha Cisse, Yann N. Dauphin, David Lopez-Paz
https://arxiv.org/abs/1710.09412

```
?mixup
```


## Roadmap

* Add docs
  * Expand Details section above
* Add tests


## Contributing
Pull requests are welcome.  For major changes, please open an issue first to discuss what you would like to change.


## License
[GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
