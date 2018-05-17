Data Exploration DATA621 Final
================
jbrnbrg
May 8, 2018

``` r
library(tidyverse)
library(RCurl)
library(mice)

url_train = "https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/master/house-prices-advanced-regression-techniques/train.csv"
url_test = "https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/master/house-prices-advanced-regression-techniques/test.csv"

stand_read <- function(url){
  return(read.csv(text = getURL(url)))
}

o_train <- stand_read(url_train) %>% mutate(d_name = 'train')
o_test <- stand_read(url_test) %>% mutate(SalePrice = NA, 
                                          d_name = 'test')
full_set <- rbind(o_train, o_test)
# x <- plot_missing(full_set)
```

``` r
na_review <- function(df){
  # returns df of vars w/ NA qty desc.
  na_qty <- colSums(is.na(df)) %>% as.data.frame(stringsAsFactors=F)
  colnames(na_qty) <- c("NA_qty")
  na_qty <- cbind('Variable' = rownames(na_qty), na_qty) %>% 
    select(Variable, NA_qty)
  rownames(na_qty) <- NULL
  
  na_qty <- na_qty %>% 
    arrange(desc(NA_qty)) %>% filter(NA_qty > 0) %>% 
    mutate(Variable = as.character(Variable)) %>% 
    mutate(Pct_of_Tot =  round(NA_qty/nrow(df), 4) * 100)
  
  return(na_qty)
}

first_pass <- full_set %>% 
  # first_pass is train.csv and test.csv combined for NA reviews 
  # and imputation planning and calculated columns
  mutate(House_Age_Yrs = YrSold - YearBuilt, 
         RemodAdd_Age_Yrs = YrSold - YearRemodAdd, 
         Garage_Age_Yrs = YrSold - GarageYrBlt) 
```

``` r
set_asideA <- '2600|2504|2421|2127|2041|2186|2525|1488|949|2349|2218|2219|333' # 13
set_asideB <- '|2550|524|2296|2593' # negative values in '_Age' columns

x <- first_pass %>% 
  # exclude set_aside observations to fill in known NA's
  filter(!grepl(paste0(set_asideA, set_asideB), Id))
  
naVarsx <- na_review(x %>% select(-SalePrice))

# naVarsx # variables with _known_ NA's
#          Variable NA_qty Pct_of_Tot
# 1          PoolQC   2887      99.65
# 2     MiscFeature   2793      96.41
# 3           Alley   2700      93.20
# 4           Fence   2331      80.46
# 5     FireplaceQu   1414      48.81
# 6     LotFrontage    486      16.78
# 7     GarageYrBlt    158       5.45
# 8    GarageFinish    158       5.45
# 9      GarageQual    158       5.45
# 10     GarageCond    158       5.45
# 11 Garage_Age_Yrs    158       5.45
# 12     GarageType    157       5.42
# 13       BsmtQual     78       2.69
# 14       BsmtCond     78       2.69
# 15   BsmtExposure     78       2.69
# 16   BsmtFinType1     78       2.69
# 17   BsmtFinType2     78       2.69

# nrow(x[x$PoolArea==0, ])   # 2,887
# x[is.na(x$MiscFeature),]   # 2,793
# x[is.na(x$Alley),]         # 2,700
# x[is.na(x$Fence),]         # 2,331
# x[is.na(x$FireplaceQu),]   # 1,414 
# nrow(x[x$LotFrontage==0, ])# 486
# x[is.na(x$GarageArea),]    # 158
# x[x$TotalBsmtSF == 0, ]    # 78
```

``` r
obtain_data <- function(df){
  # like first_pass but with imputation that addresses 
  # observations that have known NA's
  df %>%
    mutate(PoolQC = fct_explicit_na(PoolQC, na_level='NoP'),
           MiscFeature = fct_explicit_na(MiscFeature, na_level='NoM'),
           Alley = fct_explicit_na(Alley, na_level='NoA'),
           Fence = fct_explicit_na(Fence, na_level = 'NoF'),
           FireplaceQu = fct_explicit_na(FireplaceQu, na_level = 'NoFp'), 
           LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
           
           # Note GarageYrBlt set to 9999 may be a problem
           GarageYrBlt = ifelse(is.na(GarageYrBlt), 9999, GarageYrBlt), 
           GarageFinish = fct_explicit_na(GarageFinish, na_level = 'NoG'), 
           GarageQual = fct_explicit_na(GarageQual, na_level = 'NoG'), 
           GarageCond = fct_explicit_na(GarageCond, na_level = 'NoG'), 
           # NOTE: Garage_Age_Yrs: 0 doesn't seem appropriate... 
           Garage_Age_Yrs = ifelse(is.na(Garage_Age_Yrs), 0, Garage_Age_Yrs),
           GarageType = fct_explicit_na(GarageType, na_level = 'NoG'), 
          
           BsmtQual = fct_explicit_na(BsmtQual, na_level = 'NoB'),
           BsmtCond = fct_explicit_na(BsmtCond, na_level = 'NoB'),
           BsmtExposure = fct_explicit_na(BsmtExposure, na_level = 'NoB'),
           BsmtFinType1 = fct_explicit_na(BsmtFinType1, na_level = 'NoB'),
           BsmtFinType2 = fct_explicit_na(BsmtFinType2, na_level = 'NoB')
           )
}
```

Number of complete cases original: 0
Number of complete cases after repairing known NA's: 2,861 (≈98%)
Number of true NA's: 58

``` r
# sum(complete.cases(full_set %>% select(-SalePrice)))       # 0
# sum(complete.cases(full_set_clean %>% select(-SalePrice))) # 2,861 ~ 98%
# nrow(full_set_clean) - 2861 # 58 NA
stat_info <- psych::describe(full_set_clean %>% select(num_vars, -Id, -d_name))
stat_info[c(2:nrow(stat_info)),c(2:5,8:9,13:ncol(stat_info)-1)]
```

    ##                     n     mean      sd median  min    max kurtosis
    ## LotFrontage      2919    57.77   33.48   63.0    0    313     2.17
    ## LotArea          2919 10168.11 7887.00 9453.0 1300 215245   264.31
    ## OverallQual      2919     6.09    1.41    6.0    1     10     0.06
    ## OverallCond      2919     5.56    1.11    5.0    1      9     1.47
    ## YearBuilt        2919  1971.31   30.29 1973.0 1872   2010    -0.51
    ## YearRemodAdd     2919  1984.26   20.89 1993.0 1950   2010    -1.35
    ## MasVnrArea       2896   102.20  179.33    0.0    0   1600     9.23
    ## BsmtFinSF1       2918   441.42  455.61  368.5    0   5644     6.88
    ## BsmtFinSF2       2918    49.58  169.21    0.0    0   1526    18.79
    ## BsmtUnfSF        2918   560.77  439.54  467.0    0   2336     0.40
    ## TotalBsmtSF      2918  1051.78  440.77  989.5    0   6110     9.13
    ## X1stFlrSF        2919  1159.58  392.36 1082.0  334   5095     6.94
    ## X2ndFlrSF        2919   336.48  428.70    0.0    0   2065    -0.43
    ## LowQualFinSF     2919     4.69   46.40    0.0    0   1064   174.51
    ## GrLivArea        2919  1500.76  506.05 1444.0  334   5642     4.11
    ## BsmtFullBath     2917     0.43    0.52    0.0    0      3    -0.74
    ## BsmtHalfBath     2917     0.06    0.25    0.0    0      2    14.81
    ## FullBath         2919     1.57    0.55    2.0    0      4    -0.54
    ## HalfBath         2919     0.38    0.50    0.0    0      2    -1.04
    ## BedroomAbvGr     2919     2.86    0.82    3.0    0      8     1.93
    ## KitchenAbvGr     2919     1.04    0.21    1.0    0      3    19.73
    ## TotRmsAbvGrd     2919     6.45    1.57    6.0    2     15     1.16
    ## Fireplaces       2919     0.60    0.65    1.0    0      4     0.07
    ## GarageYrBlt      2918  2412.42 1815.66 1984.0 1895   9999    13.51
    ## GarageCars       2918     1.77    0.76    2.0    0      5     0.23
    ## GarageArea       2918   472.87  215.39  480.0    0   1488     0.93
    ## WoodDeckSF       2919    93.71  126.53    0.0    0   1424     6.72
    ## OpenPorchSF      2919    47.49   67.58   26.0    0    742    10.91
    ## EnclosedPorch    2919    23.10   64.24    0.0    0   1012    28.31
    ## X3SsnPorch       2919     2.60   25.19    0.0    0    508   149.05
    ## ScreenPorch      2919    16.06   56.18    0.0    0    576    17.73
    ## PoolArea         2919     2.25   35.66    0.0    0    800   297.91
    ## MiscVal          2919    50.83  567.40    0.0    0  17000   562.72
    ## MoSold           2919     6.21    2.71    6.0    1     12    -0.46
    ## YrSold           2919  2007.79    1.31 2008.0 2006   2010    -1.16
    ## House_Age_Yrs    2919    36.48   30.34   35.0   -1    136    -0.51
    ## RemodAdd_Age_Yrs 2919    23.53   20.89   15.0   -2     60    -1.34
    ## Garage_Age_Yrs   2918    28.07   25.80   25.0 -200    114     1.61

``` r
summary(full_set_clean %>% select(fac_vars, -Id, -SalePrice, -d_name))
```

    ##     MSZoning     Street      Alley      LotShape   LandContour
    ##  C (all):  25   Grvl:  12   Grvl: 120   IR1: 968   Bnk: 117   
    ##  FV     : 139   Pave:2907   Pave:  78   IR2:  76   HLS: 120   
    ##  RH     :  26               NoA :2700   IR3:  16   Low:  60   
    ##  RL     :2265               NA's:  21   Reg:1859   Lvl:2622   
    ##  RM     : 460                                                 
    ##  NA's   :   4                                                 
    ##                                                               
    ##   Utilities      LotConfig    LandSlope   Neighborhood    Condition1  
    ##  AllPub:2916   Corner : 511   Gtl:2778   NAmes  : 443   Norm   :2511  
    ##  NoSeWa:   1   CulDSac: 176   Mod: 125   CollgCr: 267   Feedr  : 164  
    ##  NA's  :   2   FR2    :  85   Sev:  16   OldTown: 239   Artery :  92  
    ##                FR3    :  14              Edwards: 194   RRAn   :  50  
    ##                Inside :2133              Somerst: 182   PosN   :  39  
    ##                                          NridgHt: 166   RRAe   :  28  
    ##                                          (Other):1428   (Other):  35  
    ##    Condition2     BldgType      HouseStyle     RoofStyle       RoofMatl   
    ##  Norm   :2889   1Fam  :2425   1Story :1471   Flat   :  20   CompShg:2876  
    ##  Feedr  :  13   2fmCon:  62   2Story : 872   Gable  :2310   Tar&Grv:  23  
    ##  Artery :   5   Duplex: 109   1.5Fin : 314   Gambrel:  22   WdShake:   9  
    ##  PosA   :   4   Twnhs :  96   SLvl   : 128   Hip    : 551   WdShngl:   7  
    ##  PosN   :   4   TwnhsE: 227   SFoyer :  83   Mansard:  11   ClyTile:   1  
    ##  RRNn   :   2                 2.5Unf :  24   Shed   :   5   Membran:   1  
    ##  (Other):   2                 (Other):  27                  (Other):   2  
    ##   Exterior1st    Exterior2nd     MasVnrType   ExterQual ExterCond
    ##  VinylSd:1025   VinylSd:1014   BrkCmn :  25   Ex: 107   Ex:  12  
    ##  MetalSd: 450   MetalSd: 447   BrkFace: 879   Fa:  35   Fa:  67  
    ##  HdBoard: 442   HdBoard: 406   None   :1742   Gd: 979   Gd: 299  
    ##  Wd Sdng: 411   Wd Sdng: 391   Stone  : 249   TA:1798   Po:   3  
    ##  Plywood: 221   Plywood: 270   NA's   :  24             TA:2538  
    ##  (Other): 369   (Other): 390                                     
    ##  NA's   :   1   NA's   :   1                                     
    ##   Foundation   BsmtQual    BsmtCond    BsmtExposure  BsmtFinType1
    ##  BrkTil: 311   Ex  : 258   Fa  : 104   Av  : 418    Unf    :851  
    ##  CBlock:1235   Fa  :  88   Gd  : 122   Gd  : 276    GLQ    :849  
    ##  PConc :1308   Gd  :1209   Po  :   5   Mn  : 239    ALQ    :429  
    ##  Slab  :  49   TA  :1283   TA  :2606   No  :1904    Rec    :288  
    ##  Stone :  11   NoB :  78   NoB :  78   NoB :  78    BLQ    :269  
    ##  Wood  :   5   NA's:   3   NA's:   4   NA's:   4    (Other):232  
    ##                                                     NA's   :  1  
    ##   BsmtFinType2   Heating     HeatingQC CentralAir Electrical   KitchenQual
    ##  Unf    :2493   Floor:   1   Ex:1493   N: 196     FuseA: 188   Ex  : 205  
    ##  Rec    : 105   GasA :2874   Fa:  92   Y:2723     FuseF:  50   Fa  :  70  
    ##  LwQ    :  87   GasW :  27   Gd: 474              FuseP:   8   Gd  :1151  
    ##  NoB    :  78   Grav :   9   Po:   3              Mix  :   1   TA  :1492  
    ##  BLQ    :  68   OthW :   2   TA: 857              SBrkr:2671   NA's:   1  
    ##  (Other):  86   Wall :   6                        NA's :   1              
    ##  NA's   :   2                                                             
    ##    Functional   FireplaceQu   GarageType   GarageFinish GarageQual 
    ##  Typ    :2717   Ex  :  43   2Types :  23   Fin : 719    Ex  :   3  
    ##  Min2   :  70   Fa  :  74   Attchd :1723   RFn : 811    Fa  : 124  
    ##  Min1   :  65   Gd  : 744   Basment:  36   Unf :1230    Gd  :  24  
    ##  Mod    :  35   Po  :  46   BuiltIn: 186   NoG : 158    Po  :   5  
    ##  Maj1   :  19   TA  : 592   CarPort:  15   NA's:   1    TA  :2604  
    ##  (Other):  11   NoFp:1414   Detchd : 779                NoG : 158  
    ##  NA's   :   2   NA's:   6   NoG    : 157                NA's:   1  
    ##  GarageCond  PavedDrive  PoolQC       Fence      MiscFeature
    ##  Ex  :   3   N: 216     Ex  :   4   GdPrv: 118   Gar2:   5  
    ##  Fa  :  74   P:  62     Fa  :   2   GdWo : 112   Othr:   4  
    ##  Gd  :  15   Y:2641     Gd  :   4   MnPrv: 329   Shed:  95  
    ##  Po  :  14              NoP :2887   MnWw :  12   TenC:   1  
    ##  TA  :2654              NA's:  22   NoF  :2331   NoM :2793  
    ##  NoG : 158                          NA's :  17   NA's:  21  
    ##  NA's:   1                                                  
    ##     SaleType    SaleCondition 
    ##  WD     :2525   Abnorml: 190  
    ##  New    : 239   AdjLand:  12  
    ##  COD    :  87   Alloca :  24  
    ##  ConLD  :  26   Family :  46  
    ##  CWD    :  12   Normal :2402  
    ##  (Other):  29   Partial: 245  
    ##  NA's   :   1

``` r
train_data <- full_set_clean %>% filter(d_name == 'train') %>% select(-d_name)
test_data <- full_set_clean %>% filter(d_name == 'test') %>% select(-d_name)
#View(train_data)
dim(train_data)
```

    ## [1] 1460   84

``` r
dim(test_data)
```

    ## [1] 1459   84

``` r
# Data Exploration Plots
#plot_boxplot()
```

``` r
full_set_clean %>% 
  filter(Garage_Age_Yrs < 0 | RemodAdd_Age_Yrs < 0 | Garage_Age_Yrs < 0) # Ids c(524, 2296, 2550, 2593)
```

    ##     Id MSSubClass MSZoning LotFrontage LotArea Street Alley LotShape
    ## 1  524         60       RL         130   40094   Pave  <NA>      IR1
    ## 2 2296         60       RL         134   16659   Pave  <NA>      IR1
    ## 3 2550         20       RL         128   39290   Pave  <NA>      IR1
    ## 4 2593         20       RL          68    8298   Pave  <NA>      IR1
    ##   LandContour Utilities LotConfig LandSlope Neighborhood Condition1
    ## 1         Bnk    AllPub    Inside       Gtl      Edwards       PosN
    ## 2         Lvl    AllPub    Corner       Gtl      NridgHt       Norm
    ## 3         Bnk    AllPub    Inside       Gtl      Edwards       Norm
    ## 4         HLS    AllPub    Inside       Gtl       Timber       Norm
    ##   Condition2 BldgType HouseStyle OverallQual OverallCond YearBuilt
    ## 1       PosN     1Fam     2Story          10           5      2007
    ## 2       Norm     1Fam     2Story           8           5      2007
    ## 3       Norm     1Fam     1Story          10           5      2008
    ## 4       Norm     1Fam     1Story           8           5      2006
    ##   YearRemodAdd RoofStyle RoofMatl Exterior1st Exterior2nd MasVnrType
    ## 1         2008       Hip  CompShg     CemntBd     CmentBd      Stone
    ## 2         2008     Gable  CompShg     VinylSd     VinylSd       None
    ## 3         2009       Hip  CompShg     CemntBd     CmentBd      Stone
    ## 4         2007       Hip  CompShg     VinylSd     VinylSd       <NA>
    ##   MasVnrArea ExterQual ExterCond Foundation BsmtQual BsmtCond BsmtExposure
    ## 1        762        Ex        TA      PConc       Ex       TA           Gd
    ## 2          0        Gd        TA      PConc       Gd       TA           No
    ## 3       1224        Ex        TA      PConc       Ex       TA           Gd
    ## 4         NA        Gd        TA      PConc       Gd       TA           Av
    ##   BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF
    ## 1          GLQ       2260          Unf          0       878        3138
    ## 2          Unf          0          Unf          0      1582        1582
    ## 3          GLQ       4010          Unf          0      1085        5095
    ## 4          GLQ        583          Unf          0       963        1546
    ##   Heating HeatingQC CentralAir Electrical X1stFlrSF X2ndFlrSF LowQualFinSF
    ## 1    GasA        Ex          Y      SBrkr      3138      1538            0
    ## 2    GasA        Ex          Y      SBrkr      1582       570            0
    ## 3    GasA        Ex          Y      SBrkr      5095         0            0
    ## 4    GasA        Ex          Y      SBrkr      1564         0            0
    ##   GrLivArea BsmtFullBath BsmtHalfBath FullBath HalfBath BedroomAbvGr
    ## 1      4676            1            0        3        1            3
    ## 2      2152            0            0        2        1            3
    ## 3      5095            1            1        2        1            2
    ## 4      1564            0            0        2        0            2
    ##   KitchenAbvGr KitchenQual TotRmsAbvGrd Functional Fireplaces FireplaceQu
    ## 1            1          Ex           11        Typ          1          Gd
    ## 2            1          Gd            7        Typ          1          Gd
    ## 3            1          Ex           15        Typ          2          Gd
    ## 4            1          Ex            6        Typ          1          Gd
    ##   GarageType GarageYrBlt GarageFinish GarageCars GarageArea GarageQual
    ## 1    BuiltIn        2007          Fin          3        884         TA
    ## 2     Detchd        2007          Unf          2        728         TA
    ## 3     Attchd        2008          Fin          3       1154         TA
    ## 4     Attchd        2207          RFn          2        502         TA
    ##   GarageCond PavedDrive WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch
    ## 1         TA          Y        208         406             0          0
    ## 2         TA          Y          0         368             0          0
    ## 3         TA          Y        546         484             0          0
    ## 4         TA          Y        132           0             0          0
    ##   ScreenPorch PoolArea PoolQC Fence MiscFeature MiscVal MoSold YrSold
    ## 1           0        0   <NA>  <NA>        <NA>       0     10   2007
    ## 2           0        0   <NA>  <NA>        <NA>       0      6   2007
    ## 3           0        0   <NA>  <NA>        <NA>   17000     10   2007
    ## 4           0        0   <NA>  <NA>        <NA>       0      9   2007
    ##   SaleType SaleCondition SalePrice d_name House_Age_Yrs RemodAdd_Age_Yrs
    ## 1      New       Partial    184750  train             0               -1
    ## 2      New       Partial        NA   test             0               -1
    ## 3      New       Partial        NA   test            -1               -2
    ## 4      New       Partial        NA   test             1                0
    ##   Garage_Age_Yrs
    ## 1              0
    ## 2              0
    ## 3             -1
    ## 4           -200
