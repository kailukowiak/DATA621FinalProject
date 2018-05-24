DATA 621 Final Presentation 
========================================================
author: Kyle Gilde, Jaan Bernberg, Kai Lukowiak, Michael Muller, Ilya Kats
date: 2018-050-24
autosize: true




Abstract
========================================================

![plot of chunk unnamed-chunk-1](Presentation-figure/unnamed-chunk-1-1.png)

***

* Understanding the factors that go into buying a house is important.
* Investigated prices in Aimes Iowa.
* Most important factors:
  * Location
  * Condition
  

Introduction
========================================================
![House in Aimes](images/AimesHouse.jpeg)
***
The data was originally published in the Journal of Statistics Education (Volume 19, Number 3). It is now parto of a long running Kaaggle competition.

The features describe atributes of the houses such as siding condition and neighborhood. They are both numeric and categorical.


Lierature Review
========================================================

There is extensive literature on house prices:
* Non-physical charactersitics are important.
  * Problematic because we have mostly physical data.
* Neighboring house prices are important but not included. 

Methedology 1
===

The data is split almost equally into training and test data. 

## Data imputation
Some NA values like pool quality were `NA` if there was no pool. values like this were updated
to reflect their actual status.

After these were fixed, there was only 2% of values missing.
***
![Missing Values](https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/master/report_files/fig1_na_dist.png)

Methedology 2
===

Values for both categorical and continuous variables were imputed using `mice` and the random forrest imputation method.

The density plots for the various imputed values can be see here. 

***
![Imputed Values](https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/master/report_files/fig2_imputation.png)


Transformations
===

We created a new variable, age, which was the age at which the house was sold. Any negative values were set to zero.

Ordered categorical variabeles such as quality that clearnly split the response variable were changed to dummy variabels.

Interaction terms were created via a grid search and selected based on their individual $R^2$ values.

Transformations 2
===

Finally a Box-Cox transformation was performed. The optimal $\lambda$ was found to be 0.184. This means that the response variable `SalePrice` was raised to the 0.184 power.

***
![Difference in scatter plots between transformed and raw data](https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/master/images/Scatter_Trans_and_Imp.png){ width: 200px; }
