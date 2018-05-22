# EDA Graphs

These are the notes for the exploratory data analysis.

Please note the code chunks are what should be included to paste the image in an
rmd file.


``` markdown
![]('images/Imputed_Numeric_Histogram.png')
```
![]('images/Imputed_Numeric_Histogram.png')


This is graph shows the distribution of the numeric variables.

Normalcy of the features is not a requirement for OLS so we are not too worried
about skewed features *per se*. Some variables that look problematic are the
ones with very little variance because the offer little to the model.

``` markdown
![]('images/Imputed_Factor_Histogram.png')
```

The main issue here is that many of the variables are lopsided. This means
there is little variance for our model to learn from.

![]('images/Imputed_Factor_Histogram.png')


``` markdown
![]('images/Scatter_Numeric_Imputed.png')
```

The scatter of the numeric variables against `SalePrice` shows relationships
between many of the variables. These are especially strong in `GrLivArea` and
`OverallQual` but are apparent in may others as well.

Notice that many of these relationships look non-linear.

![]('images/Scatter_Numeric_Imputed.png')


``` markdown
![]('images/Scatter_Numeric_Imputed.png')
```
