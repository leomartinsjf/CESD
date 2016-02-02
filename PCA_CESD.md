# CES-D
Leonardo Martins  
17 de janeiro de 2016  

#Preparing new analysis to CES-D manuscript review

Loading required packages

```r
require(foreign) # Read data stored SPSS
```

```
## Loading required package: foreign
```

```r
require(car) #Recode Variables
```

```
## Loading required package: car
```

```r
require(psych) #Psychometrics
```

```
## Loading required package: psych
## 
## Attaching package: 'psych'
## 
## The following object is masked from 'package:car':
## 
##     logit
```

```r
require(lavaan) #Confirmatory and SEM
```

```
## Loading required package: lavaan
## This is lavaan 0.5-18
## lavaan is BETA software! Please report any bugs.
```

```r
require(semPlot) # Plots for SEM
```

```
## Loading required package: semPlot
```


```r
#Setting Directory
setwd("~/CESD")

#Importing SPSS file .sav
base.dat <- read.spss("PD10.sav", to.data.frame = T)
```

```
## Warning in read.spss("PD10.sav", to.data.frame = T): PD10.sav: Unrecognized
## record type 7, subtype 18 encountered in system file
```

```
## re-encoding from latin1
```

```r
#Sum CESD itens in order to find NA
base.dat$scaleSum <- rowSums(base.dat[,267:286])

#Creating a subset for analysis without NA
base.CESD <- subset(base.dat, subset=!is.na(base.dat$scaleSum))

#Creating a subset only with CESD
fullScale  <- base.CESD[ , 267:286]

#Recoding reversed itens
fullScale$F4r<- recode(fullScale$F4r, "0=3; 1=2; 2=1; 3=0")
fullScale$F8r<- recode(fullScale$F8r, "0=3; 1=2; 2=1; 3=0")
fullScale$F12r<- recode(fullScale$F12r, "0=3; 1=2; 2=1; 3=0")
fullScale$F16r<- recode(fullScale$F16r, "0=3; 1=2; 2=1; 3=0")

#Aninha needs to check if are these ones the reversed itens

#Creating a Correlation Matrix
correl <- cor(fullScale)

#Creating a polychoric correlation
fullScaleT<-polychoric(fullScale)

#Cloning fullScale
fullScale2 <- fullScale

#Creating a categorical ordered subset for CFA analisys
orderedScale <-fullScale2[,c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r")] <-
lapply(fullScale2[,c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r")], ordered)
orderedScale<-as.data.frame(orderedScale)
```


```r
# Bartlett Test
cortest.bartlett(fullScaleT$rho, n=nrow(fullScale))
```

```
## $chisq
## [1] 4462.717
## 
## $p.value
## [1] 0
## 
## $df
## [1] 190
```


```r
# KMO
KMO(fullScaleT$rho)
```

```
## Kaiser-Meyer-Olkin factor adequacy
## Call: KMO(r = fullScaleT$rho)
## Overall MSA =  0.9
## MSA for each item = 
##  F1r  F2r  F3r  F4r  F5r  F6r  F7r  F8r  F9r F10r F11r F12r F13r F14r F15r 
## 0.92 0.89 0.93 0.75 0.92 0.92 0.95 0.79 0.95 0.91 0.91 0.83 0.88 0.94 0.84 
## F16r F17r F18r F19r F20r 
## 0.84 0.85 0.89 0.87 0.93
```


```r
# Parallel Analysis
fa.parallel(fullScaleT$rho, fm="minres", fa="both", n.obs=513)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-5-1.png) 

```
## Parallel analysis suggests that the number of factors =  6  and the number of components =  2
```


```r
#Very Simple Structure
VSS(fullScaleT$rho, n.obs=513)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-6-1.png) 

```
## 
## Very Simple Structure
## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
##     n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
## VSS complexity 1 achieves a maximimum of 0.83  with  1  factors
## VSS complexity 2 achieves a maximimum of 0.88  with  2  factors
## 
## The Velicer MAP achieves a minimum of 0.02  with  2  factors 
## BIC achieves a minimum of  -324.51  with  4  factors
## Sample Size adjusted BIC achieves a minimum of  5.4  with  8  factors
## 
## Statistics by number of factors 
##   vss1 vss2   map dof chisq     prob sqresid  fit RMSEA  BIC SABIC complex
## 1 0.83 0.00 0.016 170  1105 7.9e-137    11.2 0.83 0.105   44 584.1     1.0
## 2 0.77 0.88 0.015 151   761  3.5e-82     8.4 0.88 0.090 -181 298.5     1.2
## 3 0.59 0.82 0.018 133   556  3.6e-53     7.6 0.89 0.080 -273 148.7     1.6
## 4 0.47 0.76 0.021 116   399  8.6e-33     6.8 0.90 0.070 -325  43.7     1.9
## 5 0.43 0.70 0.027 100   339  9.8e-28     6.1 0.91 0.069 -285  32.3     2.2
## 6 0.29 0.54 0.032  85   276  4.8e-22     5.6 0.92 0.067 -254  15.4     2.5
## 7 0.29 0.53 0.041  71   231  7.9e-19     5.3 0.92 0.068 -212  13.4     2.7
## 8 0.26 0.50 0.048  58   183  6.6e-15     4.9 0.93 0.066 -179   5.4     2.8
##   eChisq  SRMR eCRMS   eBIC
## 1   1064 0.074 0.078    3.3
## 2    472 0.049 0.055 -469.8
## 3    350 0.042 0.051 -480.2
## 4    235 0.035 0.044 -488.8
## 5    172 0.030 0.041 -452.1
## 6    130 0.026 0.039 -400.9
## 7    109 0.024 0.039 -333.9
## 8     81 0.020 0.037 -280.8
```

#Principal Components Analysis

```r
#PCA - 2 components unrotated
PCA2u <- principal(fullScaleT$rho, nfactors = 2)
print.psych(PCA2u, digits=2, cut= .4)
```

```
## Principal Components Analysis
## Call: principal(r = fullScaleT$rho, nfactors = 2)
## Standardized loadings (pattern matrix) based upon correlation matrix
##        PC1   PC2   h2   u2 com
## F1r   0.61       0.41 0.59 1.2
## F2r   0.54       0.30 0.70 1.0
## F3r   0.59       0.39 0.61 1.2
## F4r         0.71 0.51 0.49 1.0
## F5r   0.63       0.40 0.60 1.0
## F6r   0.74       0.61 0.39 1.2
## F7r   0.63       0.41 0.59 1.0
## F8r         0.62 0.38 0.62 1.0
## F9r   0.69       0.50 0.50 1.1
## F10r  0.69       0.51 0.49 1.1
## F11r  0.62       0.42 0.58 1.2
## F12r        0.75 0.67 0.33 1.4
## F13r  0.41       0.21 0.79 1.4
## F14r  0.69       0.59 0.41 1.4
## F15r  0.63       0.42 0.58 1.1
## F16r        0.66 0.52 0.48 1.4
## F17r  0.62       0.39 0.61 1.0
## F18r  0.77       0.67 0.33 1.3
## F19r  0.71       0.54 0.46 1.1
## F20r  0.66       0.50 0.50 1.3
## 
##                        PC1  PC2
## SS loadings           6.89 2.46
## Proportion Var        0.34 0.12
## Cumulative Var        0.34 0.47
## Proportion Explained  0.74 0.26
## Cumulative Proportion 0.74 1.00
## 
## Mean item complexity =  1.2
## Test of the hypothesis that 2 components are sufficient.
## 
## The root mean square of the residuals (RMSR) is  0.06 
## 
## Fit based upon off diagonal values = 0.97
```

```r
plot.psych(PCA2u)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-7-1.png) 

```r
#PCA - 2 components oblique rotated (assuming the components are correlated)
PCA2 <- principal(fullScaleT$rho, nfactors = 2, rotate="oblimin")
```

```
## Loading required namespace: GPArotation
```

```r
print.psych(PCA2, digits=2, cut= .4)
```

```
## Principal Components Analysis
## Call: principal(r = fullScaleT$rho, nfactors = 2, rotate = "oblimin")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        PC1   PC2   h2   u2 com
## F1r   0.66       0.41 0.59 1.5
## F2r   0.56       0.30 0.70 1.0
## F3r   0.60       0.39 0.61 1.0
## F4r         0.74 0.51 0.49 1.1
## F5r   0.66       0.40 0.60 1.0
## F6r   0.75       0.61 0.39 1.0
## F7r   0.66       0.41 0.59 1.0
## F8r         0.63 0.38 0.62 1.0
## F9r   0.70       0.50 0.50 1.0
## F10r  0.70       0.51 0.49 1.0
## F11r  0.63       0.42 0.58 1.0
## F12r        0.70 0.67 0.33 1.3
## F13r  0.45       0.21 0.79 1.7
## F14r  0.69       0.59 0.41 1.1
## F15r  0.65       0.42 0.58 1.0
## F16r        0.62 0.52 0.48 1.3
## F17r  0.65       0.39 0.61 1.1
## F18r  0.78       0.67 0.33 1.0
## F19r  0.72       0.54 0.46 1.0
## F20r  0.66       0.50 0.50 1.1
## 
##                        PC1  PC2
## SS loadings           7.20 2.15
## Proportion Var        0.36 0.11
## Cumulative Var        0.36 0.47
## Proportion Explained  0.77 0.23
## Cumulative Proportion 0.77 1.00
## 
##  With component correlations of 
##      PC1  PC2
## PC1 1.00 0.31
## PC2 0.31 1.00
## 
## Mean item complexity =  1.1
## Test of the hypothesis that 2 components are sufficient.
## 
## The root mean square of the residuals (RMSR) is  0.06 
## 
## Fit based upon off diagonal values = 0.97
```

```r
plot.psych(PCA2)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-7-2.png) 

```r
### Alfa de Cronbach (FA 2 Components)
alpha(fullScale, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = fullScale, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.87      0.87    0.89      0.26 6.9 0.011  1.6 0.65
## 
##  lower alpha upper     95% confidence boundaries
## 0.85 0.87 0.9 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F1r       0.87      0.87    0.88      0.26 6.7    0.012
## F2r       0.87      0.87    0.88      0.26 6.6    0.012
## F3r       0.87      0.87    0.88      0.25 6.5    0.012
## F4r       0.88      0.88    0.89      0.28 7.2    0.011
## F5r       0.87      0.87    0.88      0.25 6.5    0.012
## F6r       0.86      0.86    0.88      0.25 6.2    0.012
## F7r       0.87      0.87    0.88      0.25 6.5    0.012
## F8r       0.88      0.88    0.89      0.27 7.2    0.011
## F9r       0.86      0.86    0.88      0.25 6.4    0.012
## F10r      0.86      0.86    0.88      0.25 6.3    0.012
## F11r      0.87      0.87    0.88      0.25 6.5    0.012
## F12r      0.87      0.87    0.88      0.26 6.5    0.012
## F13r      0.88      0.88    0.89      0.27 7.0    0.011
## F14r      0.86      0.86    0.88      0.25 6.3    0.012
## F15r      0.87      0.87    0.88      0.25 6.5    0.012
## F16r      0.87      0.87    0.88      0.26 6.7    0.012
## F17r      0.87      0.87    0.88      0.26 6.6    0.012
## F18r      0.86      0.86    0.87      0.24 6.2    0.012
## F19r      0.86      0.86    0.88      0.25 6.3    0.012
## F20r      0.86      0.86    0.88      0.25 6.4    0.012
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F1r  513  0.47  0.47  0.43   0.40  1.5 1.1
## F2r  513  0.50  0.50  0.46   0.43  1.4 1.2
## F3r  513  0.57  0.57  0.54   0.51  1.7 1.2
## F4r  513  0.24  0.24  0.17   0.15  1.4 1.2
## F5r  513  0.56  0.56  0.53   0.49  1.5 1.2
## F6r  513  0.69  0.69  0.68   0.64  2.0 1.1
## F7r  513  0.56  0.56  0.53   0.49  1.7 1.2
## F8r  513  0.26  0.27  0.20   0.18  1.1 1.2
## F9r  513  0.63  0.62  0.60   0.56  1.8 1.2
## F10r 513  0.64  0.64  0.62   0.58  1.5 1.2
## F11r 513  0.58  0.58  0.55   0.51  1.9 1.2
## F12r 513  0.56  0.55  0.53   0.49  1.6 1.2
## F13r 513  0.32  0.32  0.25   0.23  1.4 1.2
## F14r 513  0.68  0.68  0.66   0.62  1.8 1.2
## F15r 513  0.58  0.58  0.55   0.51  1.3 1.2
## F16r 513  0.48  0.48  0.44   0.40  1.5 1.3
## F17r 513  0.52  0.52  0.49   0.45  1.2 1.2
## F18r 513  0.72  0.72  0.73   0.68  2.0 1.1
## F19r 513  0.65  0.65  0.64   0.59  1.4 1.3
## F20r 513  0.63  0.62  0.60   0.56  1.7 1.2
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F1r  0.27 0.26 0.22 0.26    0
## F2r  0.34 0.21 0.21 0.24    0
## F3r  0.21 0.22 0.19 0.38    0
## F4r  0.33 0.21 0.21 0.24    0
## F5r  0.27 0.23 0.21 0.29    0
## F6r  0.15 0.16 0.20 0.50    0
## F7r  0.25 0.17 0.21 0.37    0
## F8r  0.44 0.18 0.19 0.18    0
## F9r  0.24 0.18 0.15 0.43    0
## F10r 0.30 0.19 0.18 0.32    0
## F11r 0.19 0.15 0.18 0.47    0
## F12r 0.27 0.17 0.21 0.34    0
## F13r 0.30 0.25 0.17 0.28    0
## F14r 0.23 0.16 0.16 0.45    0
## F15r 0.36 0.22 0.16 0.26    0
## F16r 0.36 0.12 0.18 0.34    0
## F17r 0.41 0.19 0.14 0.26    0
## F18r 0.15 0.17 0.17 0.51    0
## F19r 0.36 0.18 0.15 0.31    0
## F20r 0.24 0.18 0.16 0.41    0
```

```r
#Pay attention to negative correlated itens in component one

#Component 1
C1_PCA2 <- fullScale[, c("F1r","F2r","F3r","F5r","F6r","F7r","F9r","F10r","F11r","F13r","F14r","F15r","F17r","F18r","F19r","F20r")]
alpha(C1_PCA2, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = C1_PCA2, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.88      0.88    0.88      0.31 7.3 0.012  1.6 0.71
## 
##  lower alpha upper     95% confidence boundaries
## 0.86 0.88 0.9 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F1r       0.88      0.88    0.88      0.32 7.1    0.012
## F2r       0.88      0.88    0.88      0.32 7.1    0.012
## F3r       0.87      0.87    0.88      0.32 6.9    0.012
## F5r       0.87      0.87    0.88      0.32 6.9    0.012
## F6r       0.87      0.87    0.87      0.31 6.6    0.013
## F7r       0.87      0.87    0.88      0.32 6.9    0.012
## F9r       0.87      0.87    0.88      0.31 6.8    0.013
## F10r      0.87      0.87    0.87      0.31 6.7    0.013
## F11r      0.87      0.87    0.88      0.32 6.9    0.012
## F13r      0.88      0.88    0.89      0.34 7.6    0.012
## F14r      0.87      0.87    0.87      0.31 6.7    0.013
## F15r      0.87      0.87    0.88      0.31 6.9    0.012
## F17r      0.88      0.88    0.88      0.32 7.0    0.012
## F18r      0.87      0.87    0.87      0.30 6.5    0.013
## F19r      0.87      0.87    0.87      0.31 6.7    0.013
## F20r      0.87      0.87    0.88      0.31 6.8    0.013
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F1r  513  0.52  0.53  0.47   0.44  1.5 1.1
## F2r  513  0.52  0.52  0.47   0.44  1.4 1.2
## F3r  513  0.58  0.58  0.54   0.50  1.7 1.2
## F5r  513  0.58  0.58  0.54   0.51  1.5 1.2
## F6r  513  0.69  0.69  0.68   0.63  2.0 1.1
## F7r  513  0.58  0.58  0.54   0.51  1.7 1.2
## F9r  513  0.64  0.64  0.61   0.57  1.8 1.2
## F10r 513  0.65  0.65  0.62   0.58  1.5 1.2
## F11r 513  0.59  0.59  0.55   0.51  1.9 1.2
## F13r 513  0.36  0.36  0.29   0.27  1.4 1.2
## F14r 513  0.67  0.67  0.65   0.60  1.8 1.2
## F15r 513  0.60  0.60  0.57   0.52  1.3 1.2
## F17r 513  0.56  0.55  0.51   0.48  1.2 1.2
## F18r 513  0.72  0.73  0.72   0.67  2.0 1.1
## F19r 513  0.67  0.66  0.64   0.60  1.4 1.3
## F20r 513  0.63  0.63  0.59   0.56  1.7 1.2
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F1r  0.27 0.26 0.22 0.26    0
## F2r  0.34 0.21 0.21 0.24    0
## F3r  0.21 0.22 0.19 0.38    0
## F5r  0.27 0.23 0.21 0.29    0
## F6r  0.15 0.16 0.20 0.50    0
## F7r  0.25 0.17 0.21 0.37    0
## F9r  0.24 0.18 0.15 0.43    0
## F10r 0.30 0.19 0.18 0.32    0
## F11r 0.19 0.15 0.18 0.47    0
## F13r 0.30 0.25 0.17 0.28    0
## F14r 0.23 0.16 0.16 0.45    0
## F15r 0.36 0.22 0.16 0.26    0
## F17r 0.41 0.19 0.14 0.26    0
## F18r 0.15 0.17 0.17 0.51    0
## F19r 0.36 0.18 0.15 0.31    0
## F20r 0.24 0.18 0.16 0.41    0
```

```r
#Component 2
C2_PCA2 <- fullScale[, c("F4r","F8r","F12r","F16r")]
alpha(C2_PCA2, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = C2_PCA2, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.62      0.61    0.57      0.28 1.6 0.043  1.4 0.82
## 
##  lower alpha upper     95% confidence boundaries
## 0.53 0.62 0.7 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r  S/N alpha se
## F4r       0.59      0.58    0.51      0.32 1.39    0.052
## F8r       0.61      0.60    0.53      0.34 1.53    0.051
## F12r      0.45      0.45    0.35      0.21 0.82    0.060
## F16r      0.52      0.52    0.42      0.27 1.09    0.056
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F4r  513  0.63  0.64  0.43   0.34  1.4 1.2
## F8r  513  0.61  0.62  0.39   0.31  1.1 1.2
## F12r 513  0.76  0.76  0.67   0.52  1.6 1.2
## F16r 513  0.72  0.70  0.57   0.42  1.5 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F4r  0.33 0.21 0.21 0.24    0
## F8r  0.44 0.18 0.19 0.18    0
## F12r 0.27 0.17 0.21 0.34    0
## F16r 0.36 0.12 0.18 0.34    0
```

#Fatorial Analysis

```r
## FA - 4 factors unrotated
fa4u <- fa(fullScaleT$rho, nfactors = 4, fm="minres")
print.psych(fa4u, digits=2, cut= .4)
```

```
## Factor Analysis using method =  minres
## Call: fa(r = fullScaleT$rho, nfactors = 4, fm = "minres")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR2   MR3   MR1   MR4   h2     u2 com
## F1r   0.65                   0.35 0.6456 1.1
## F2r   0.59                   0.31 0.6898 1.0
## F3r   0.63                   0.42 0.5815 1.2
## F4r         0.51             0.24 0.7553 1.7
## F5r   0.57                   0.38 0.6163 1.0
## F6r   0.54                   0.62 0.3786 1.6
## F7r   0.54                   0.39 0.6143 1.1
## F8r         0.41             0.16 0.8404 1.1
## F9r   0.47                   0.47 0.5331 1.4
## F10r  0.56                   0.49 0.5138 1.1
## F11r  0.46                   0.39 0.6076 1.3
## F12r        0.84             0.78 0.2194 1.0
## F13r                         0.14 0.8631 3.0
## F14r                         0.56 0.4401 3.5
## F15r                    0.83 0.66 0.3411 1.0
## F16r        0.63             0.47 0.5282 1.1
## F17r              0.66       0.49 0.5137 1.2
## F18r              0.94       0.99 0.0066 1.0
## F19r                    0.71 0.68 0.3151 1.1
## F20r                         0.48 0.5220 2.7
## 
##                        MR2  MR3  MR1  MR4
## SS loadings           3.63 1.90 2.06 1.89
## Proportion Var        0.18 0.09 0.10 0.09
## Cumulative Var        0.18 0.28 0.38 0.47
## Proportion Explained  0.38 0.20 0.22 0.20
## Cumulative Proportion 0.38 0.58 0.80 1.00
## 
##  With factor correlations of 
##      MR2  MR3  MR1  MR4
## MR2 1.00 0.38 0.65 0.63
## MR3 0.38 1.00 0.42 0.34
## MR1 0.65 0.42 1.00 0.58
## MR4 0.63 0.34 0.58 1.00
## 
## Mean item complexity =  1.5
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  190  and the objective function was  8.85
## The degrees of freedom for the model are 116  and the objective function was  0.8 
## 
## The root mean square of the residuals (RMSR) is  0.03 
## The df corrected root mean square of the residuals is  0.04 
## 
## Fit based upon off diagonal values = 0.99
## Measures of factor score adequacy             
##                                                 MR2  MR3  MR1  MR4
## Correlation of scores with factors             0.93 0.92 1.00 0.91
## Multiple R square of scores with factors       0.87 0.84 0.99 0.83
## Minimum correlation of possible factor scores  0.73 0.67 0.98 0.66
```

```r
fa.diagram(fa4u)

## FA - 4 factors oblique rotated (assuming the components are correlated)
fa4 <- fa(fullScaleT$rho, nfactors = 4, rotate = "oblimin", fm="minres")
print.psych(fa4, digits=2, cut= .4)
```

```
## Factor Analysis using method =  minres
## Call: fa(r = fullScaleT$rho, nfactors = 4, rotate = "oblimin", fm = "minres")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR2   MR3   MR1   MR4   h2     u2 com
## F1r   0.65                   0.35 0.6456 1.1
## F2r   0.59                   0.31 0.6898 1.0
## F3r   0.63                   0.42 0.5815 1.2
## F4r         0.51             0.24 0.7553 1.7
## F5r   0.57                   0.38 0.6163 1.0
## F6r   0.54                   0.62 0.3786 1.6
## F7r   0.54                   0.39 0.6143 1.1
## F8r         0.41             0.16 0.8404 1.1
## F9r   0.47                   0.47 0.5331 1.4
## F10r  0.56                   0.49 0.5138 1.1
## F11r  0.46                   0.39 0.6076 1.3
## F12r        0.84             0.78 0.2194 1.0
## F13r                         0.14 0.8631 3.0
## F14r                         0.56 0.4401 3.5
## F15r                    0.83 0.66 0.3411 1.0
## F16r        0.63             0.47 0.5282 1.1
## F17r              0.66       0.49 0.5137 1.2
## F18r              0.94       0.99 0.0066 1.0
## F19r                    0.71 0.68 0.3151 1.1
## F20r                         0.48 0.5220 2.7
## 
##                        MR2  MR3  MR1  MR4
## SS loadings           3.63 1.90 2.06 1.89
## Proportion Var        0.18 0.09 0.10 0.09
## Cumulative Var        0.18 0.28 0.38 0.47
## Proportion Explained  0.38 0.20 0.22 0.20
## Cumulative Proportion 0.38 0.58 0.80 1.00
## 
##  With factor correlations of 
##      MR2  MR3  MR1  MR4
## MR2 1.00 0.38 0.65 0.63
## MR3 0.38 1.00 0.42 0.34
## MR1 0.65 0.42 1.00 0.58
## MR4 0.63 0.34 0.58 1.00
## 
## Mean item complexity =  1.5
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  190  and the objective function was  8.85
## The degrees of freedom for the model are 116  and the objective function was  0.8 
## 
## The root mean square of the residuals (RMSR) is  0.03 
## The df corrected root mean square of the residuals is  0.04 
## 
## Fit based upon off diagonal values = 0.99
## Measures of factor score adequacy             
##                                                 MR2  MR3  MR1  MR4
## Correlation of scores with factors             0.93 0.92 1.00 0.91
## Multiple R square of scores with factors       0.87 0.84 0.99 0.83
## Minimum correlation of possible factor scores  0.73 0.67 0.98 0.66
```

```r
fa.diagram(fa4)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-8-1.png) 

```r
## FA - 4 factors oblique rotated (assuming the components are correlated) - without three unloaded itens (weight < 0.4)
fa4r <- fa(fullScaleT$rho[-c(13,14,20),-c(13,14,20) ], nfactors = 4, rotate = "oblimin", fm="minres")
print.psych(fa4r, digits=2, cut= 0.4)
```

```
## Factor Analysis using method =  minres
## Call: fa(r = fullScaleT$rho[-c(13, 14, 20), -c(13, 14, 20)], nfactors = 4, 
##     rotate = "oblimin", fm = "minres")
## Standardized loadings (pattern matrix) based upon correlation matrix
##        MR4   MR3   MR1   MR2   h2    u2 com
## F1r   0.64                   0.35 0.654 1.2
## F2r   0.59                   0.31 0.685 1.0
## F3r   0.64                   0.41 0.588 1.2
## F4r         0.52             0.24 0.761 1.3
## F5r   0.57                   0.38 0.623 1.0
## F6r   0.56                   0.63 0.372 1.6
## F7r   0.56                   0.39 0.613 1.1
## F8r         0.42             0.17 0.833 1.2
## F9r   0.52                   0.46 0.538 1.2
## F10r  0.64                   0.50 0.500 1.0
## F11r  0.45                   0.40 0.604 1.3
## F12r        0.81             0.74 0.264 1.0
## F15r                    0.99 1.00 0.005 1.0
## F16r        0.65             0.50 0.504 1.0
## F17r              0.66       0.49 0.506 1.2
## F18r              0.94       1.00 0.005 1.0
## F19r                    0.44 0.57 0.433 2.0
## 
##                        MR4  MR3  MR1  MR2
## SS loadings           3.55 1.74 1.77 1.45
## Proportion Var        0.21 0.10 0.10 0.09
## Cumulative Var        0.21 0.31 0.42 0.50
## Proportion Explained  0.42 0.20 0.21 0.17
## Cumulative Proportion 0.42 0.62 0.83 1.00
## 
##  With factor correlations of 
##      MR4  MR3  MR1  MR2
## MR4 1.00 0.39 0.65 0.50
## MR3 0.39 1.00 0.41 0.26
## MR1 0.65 0.41 1.00 0.44
## MR2 0.50 0.26 0.44 1.00
## 
## Mean item complexity =  1.2
## Test of the hypothesis that 4 factors are sufficient.
## 
## The degrees of freedom for the null model are  136  and the objective function was  7.1
## The degrees of freedom for the model are 74  and the objective function was  0.51 
## 
## The root mean square of the residuals (RMSR) is  0.03 
## The df corrected root mean square of the residuals is  0.05 
## 
## Fit based upon off diagonal values = 0.99
## Measures of factor score adequacy             
##                                                 MR4  MR3  MR1  MR2
## Correlation of scores with factors             0.93 0.90 1.00 1.00
## Multiple R square of scores with factors       0.87 0.81 0.99 0.99
## Minimum correlation of possible factor scores  0.74 0.63 0.99 0.99
```

```r
fa.diagram(fa4r)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-8-2.png) 

```r
#Pay attention to negative correlated itens in component one

#Factor 1
F1_FA4 <- fullScale[, c("F1r","F2r","F3r","F5r","F6r","F7r","F9r","F10r","F11r")]
alpha(F1_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F1_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.82      0.82     0.8      0.33 4.4 0.019  1.7 0.75
## 
##  lower alpha upper     95% confidence boundaries
## 0.78 0.82 0.85 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F1r       0.81      0.81    0.79      0.34 4.1    0.021
## F2r       0.80      0.80    0.79      0.34 4.1    0.021
## F3r       0.80      0.80    0.78      0.33 3.9    0.022
## F5r       0.80      0.80    0.78      0.33 3.9    0.021
## F6r       0.79      0.79    0.77      0.32 3.7    0.022
## F7r       0.80      0.80    0.78      0.33 4.0    0.021
## F9r       0.79      0.79    0.78      0.32 3.8    0.022
## F10r      0.79      0.79    0.77      0.32 3.8    0.022
## F11r      0.80      0.80    0.78      0.33 4.0    0.021
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F1r  513  0.57  0.58  0.49   0.44  1.5 1.1
## F2r  513  0.59  0.59  0.51   0.46  1.4 1.2
## F3r  513  0.64  0.64  0.57   0.52  1.7 1.2
## F5r  513  0.63  0.63  0.56   0.51  1.5 1.2
## F6r  513  0.70  0.70  0.66   0.59  2.0 1.1
## F7r  513  0.63  0.63  0.56   0.50  1.7 1.2
## F9r  513  0.66  0.66  0.60   0.54  1.8 1.2
## F10r 513  0.68  0.68  0.63   0.57  1.5 1.2
## F11r 513  0.62  0.62  0.55   0.49  1.9 1.2
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F1r  0.27 0.26 0.22 0.26    0
## F2r  0.34 0.21 0.21 0.24    0
## F3r  0.21 0.22 0.19 0.38    0
## F5r  0.27 0.23 0.21 0.29    0
## F6r  0.15 0.16 0.20 0.50    0
## F7r  0.25 0.17 0.21 0.37    0
## F9r  0.24 0.18 0.15 0.43    0
## F10r 0.30 0.19 0.18 0.32    0
## F11r 0.19 0.15 0.18 0.47    0
```

```r
#Factor 2
F2_FA4 <-  fullScale[, c("F4r","F8r","F12r","F16r")]
alpha(F2_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F2_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.62      0.61    0.57      0.28 1.6 0.043  1.4 0.82
## 
##  lower alpha upper     95% confidence boundaries
## 0.53 0.62 0.7 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r  S/N alpha se
## F4r       0.59      0.58    0.51      0.32 1.39    0.052
## F8r       0.61      0.60    0.53      0.34 1.53    0.051
## F12r      0.45      0.45    0.35      0.21 0.82    0.060
## F16r      0.52      0.52    0.42      0.27 1.09    0.056
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F4r  513  0.63  0.64  0.43   0.34  1.4 1.2
## F8r  513  0.61  0.62  0.39   0.31  1.1 1.2
## F12r 513  0.76  0.76  0.67   0.52  1.6 1.2
## F16r 513  0.72  0.70  0.57   0.42  1.5 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F4r  0.33 0.21 0.21 0.24    0
## F8r  0.44 0.18 0.19 0.18    0
## F12r 0.27 0.17 0.21 0.34    0
## F16r 0.36 0.12 0.18 0.34    0
```

```r
#Factor 3
F3_FA4 <- fullScale[, c("F17r","F18r")]
alpha(F3_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F3_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean sd
##       0.68      0.68    0.52      0.52 2.1 0.067  1.6  1
## 
##  lower alpha upper     95% confidence boundaries
## 0.55 0.68 0.81 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F17r      0.52      0.52    0.27      0.52  NA       NA
## F18r      0.52      0.52    0.27      0.52  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F17r 513  0.88  0.87  0.63   0.52  1.2 1.2
## F18r 513  0.86  0.87  0.63   0.52  2.0 1.1
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F17r 0.41 0.19 0.14 0.26    0
## F18r 0.15 0.17 0.17 0.51    0
```

```r
#Factor 4
F4_FA4 <- fullScale[, c("F15r","F19r")]
alpha(F4_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F4_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
##       0.71      0.71    0.55      0.55 2.5 0.065  1.4 1.1
## 
##  lower alpha upper     95% confidence boundaries
## 0.58 0.71 0.84 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F15r      0.55      0.55     0.3      0.55  NA       NA
## F19r      0.55      0.55     0.3      0.55  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F15r 513  0.87  0.88  0.65   0.55  1.3 1.2
## F19r 513  0.89  0.88  0.65   0.55  1.4 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F15r 0.36 0.22 0.16 0.26    0
## F19r 0.36 0.18 0.15 0.31    0
```

#Confirmatory Models


```r
#Batistoni CFA Model

#Model Identification
Batistoni <- '
              # latent variable definitions 
               f1 =~ F18r + F14r + F6r + F13r + F19r + F17r + F9r + F10r + F20r
               f2 =~ F3r + F1r + F7r + F5r + F2r
               f3 =~F12r + F8r + F16r + F4r

              # variances and covariances 
               f1 ~~ f2 
               f2 ~~ f3 
               f1 ~~ f3
                        '
#CFA Fit
fitBatistoni <- cfa(Batistoni, data = orderedScale,
           ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))
```

```
## Found more than one class "Model" in cache; using the first, from namespace 'MatrixModels'
```

```r
#Model Summary 
summary(fitBatistoni, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  32 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              224.932     319.826
##   Degrees of freedom                               132         132
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.773
##   Shift parameter                                           28.752
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            10633.966    5116.052
##   Degrees of freedom                               153         153
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.991       0.962
##   Tucker-Lewis Index (TLI)                       0.990       0.956
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.037       0.053
##   90 Percent Confidence Interval          0.029  0.045       0.045  0.060
##   P-value RMSEA <= 0.05                          0.996       0.263
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           1.042       1.042
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F18r              1.000                               0.840    0.840
##     F14r              0.902    0.037   24.670    0.000    0.757    0.757
##     F6r               0.935    0.036   25.951    0.000    0.785    0.785
##     F13r              0.356    0.053    6.699    0.000    0.299    0.299
##     F19r              0.839    0.038   22.110    0.000    0.704    0.704
##     F17r              0.704    0.041   17.136    0.000    0.591    0.591
##     F9r               0.818    0.040   20.635    0.000    0.687    0.687
##     F10r              0.817    0.040   20.352    0.000    0.686    0.686
##     F20r              0.823    0.041   19.972    0.000    0.691    0.691
##   f2 =~
##     F3r               1.000                               0.654    0.654
##     F1r               0.839    0.075   11.225    0.000    0.549    0.549
##     F7r               0.995    0.077   12.893    0.000    0.650    0.650
##     F5r               0.996    0.076   13.073    0.000    0.651    0.651
##     F2r               0.823    0.068   12.068    0.000    0.538    0.538
##   f3 =~
##     F12r              1.000                               0.897    0.897
##     F8r               0.406    0.067    6.086    0.000    0.364    0.364
##     F16r              0.810    0.068   11.844    0.000    0.726    0.726
##     F4r               0.368    0.067    5.503    0.000    0.330    0.330
## 
## Covariances:
##   f1 ~~
##     f2                0.474    0.033   14.239    0.000    0.864    0.864
##   f2 ~~
##     f3                0.248    0.035    7.049    0.000    0.424    0.424
##   f1 ~~
##     f3                0.435    0.036   12.081    0.000    0.578    0.578
## 
## Intercepts:
##     F18r              0.000                               0.000    0.000
##     F14r              0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F13r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F20r              0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F1r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F12r              0.000                               0.000    0.000
##     F8r               0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F4r               0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
##     f3                0.000                               0.000    0.000
## 
## Thresholds:
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
##     F14r|t1          -0.752    0.061  -12.229    0.000   -0.752   -0.752
##     F14r|t2          -0.290    0.056   -5.154    0.000   -0.290   -0.290
##     F14r|t3           0.115    0.056    2.073    0.038    0.115    0.115
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F13r|t1          -0.529    0.058   -9.081    0.000   -0.529   -0.529
##     F13r|t2           0.115    0.056    2.073    0.038    0.115    0.115
##     F13r|t3           0.575    0.059    9.771    0.000    0.575    0.575
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F20r|t1          -0.695    0.061  -11.475    0.000   -0.695   -0.695
##     F20r|t2          -0.189    0.056   -3.394    0.001   -0.189   -0.189
##     F20r|t3           0.224    0.056    4.011    0.000    0.224    0.224
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F8r|t1           -0.140    0.056   -2.513    0.012   -0.140   -0.140
##     F8r|t2            0.321    0.056    5.680    0.000    0.321    0.321
##     F8r|t3            0.918    0.065   14.173    0.000    0.918    0.918
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F4r|t1           -0.431    0.057   -7.517    0.000   -0.431   -0.431
##     F4r|t2            0.110    0.056    1.984    0.047    0.110    0.110
##     F4r|t3            0.701    0.061   11.559    0.000    0.701    0.701
## 
## Variances:
##     F18r              0.295                               0.295    0.295
##     F14r              0.426                               0.426    0.426
##     F6r               0.384                               0.384    0.384
##     F13r              0.911                               0.911    0.911
##     F19r              0.504                               0.504    0.504
##     F17r              0.651                               0.651    0.651
##     F9r               0.528                               0.528    0.528
##     F10r              0.530                               0.530    0.530
##     F20r              0.523                               0.523    0.523
##     F3r               0.573                               0.573    0.573
##     F1r               0.699                               0.699    0.699
##     F7r               0.577                               0.577    0.577
##     F5r               0.576                               0.576    0.576
##     F2r               0.710                               0.710    0.710
##     F12r              0.196                               0.196    0.196
##     F8r               0.867                               0.867    0.867
##     F16r              0.473                               0.473    0.473
##     F4r               0.891                               0.891    0.891
##     f1                0.705    0.039                      1.000    1.000
##     f2                0.427    0.048                      1.000    1.000
##     f3                0.804    0.072                      1.000    1.000
## 
## R-Square:
## 
##     F18r              0.705
##     F14r              0.574
##     F6r               0.616
##     F13r              0.089
##     F19r              0.496
##     F17r              0.349
##     F9r               0.472
##     F10r              0.470
##     F20r              0.477
##     F3r               0.427
##     F1r               0.301
##     F7r               0.423
##     F5r               0.424
##     F2r               0.290
##     F12r              0.804
##     F8r               0.133
##     F16r              0.527
##     F4r               0.109
```

```r
#Model Fit Measures
fitMeasures(fitBatistoni)
```

```
##                          npar                          fmin 
##                        75.000                         0.219 
##                         chisq                            df 
##                       224.932                       132.000 
##                        pvalue                  chisq.scaled 
##                         0.000                       319.826 
##                     df.scaled                 pvalue.scaled 
##                       132.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.773                     10633.966 
##                   baseline.df               baseline.pvalue 
##                       153.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      5116.052                       153.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.112 
##                           cfi                           tli 
##                         0.991                         0.990 
##                          nnfi                           rfi 
##                         0.990                         0.975 
##                           nfi                          pnfi 
##                         0.979                         0.844 
##                           ifi                           rni 
##                         0.991                         0.991 
##                    cfi.scaled                    tli.scaled 
##                         0.962                         0.956 
##                   nnfi.scaled                    rfi.scaled 
##                         0.956                         0.928 
##                    nfi.scaled                    ifi.scaled 
##                         0.937                         0.937 
##                    rni.scaled                         rmsea 
##                         0.982                         0.037 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.029                         0.045 
##                  rmsea.pvalue                  rmsea.scaled 
##                         0.996                         0.053 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.045                         0.060 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.263                         1.042 
##                         cn_05                         cn_01 
##                       364.775                       394.132 
##                           gfi                          agfi 
##                         0.984                         0.975 
##                          pgfi                           mfi 
##                         0.627                         0.913
```

```r
#Parameters Estimates
EstBatistoni <- parameterEstimates(fitBatistoni, standardized=T, ci=F)
subset(EstBatistoni, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~ F18r 1.000 0.000     NA     NA  0.840   0.840   0.840
## 2   f1 =~ F14r 0.902 0.037 24.670      0  0.757   0.757   0.757
## 3   f1 =~  F6r 0.935 0.036 25.951      0  0.785   0.785   0.785
## 4   f1 =~ F13r 0.356 0.053  6.699      0  0.299   0.299   0.299
## 5   f1 =~ F19r 0.839 0.038 22.110      0  0.704   0.704   0.704
## 6   f1 =~ F17r 0.704 0.041 17.136      0  0.591   0.591   0.591
## 7   f1 =~  F9r 0.818 0.040 20.635      0  0.687   0.687   0.687
## 8   f1 =~ F10r 0.817 0.040 20.352      0  0.686   0.686   0.686
## 9   f1 =~ F20r 0.823 0.041 19.972      0  0.691   0.691   0.691
## 10  f2 =~  F3r 1.000 0.000     NA     NA  0.654   0.654   0.654
## 11  f2 =~  F1r 0.839 0.075 11.225      0  0.549   0.549   0.549
## 12  f2 =~  F7r 0.995 0.077 12.893      0  0.650   0.650   0.650
## 13  f2 =~  F5r 0.996 0.076 13.073      0  0.651   0.651   0.651
## 14  f2 =~  F2r 0.823 0.068 12.068      0  0.538   0.538   0.538
## 15  f3 =~ F12r 1.000 0.000     NA     NA  0.897   0.897   0.897
## 16  f3 =~  F8r 0.406 0.067  6.086      0  0.364   0.364   0.364
## 17  f3 =~ F16r 0.810 0.068 11.844      0  0.726   0.726   0.726
## 18  f3 =~  F4r 0.368 0.067  5.503      0  0.330   0.330   0.330
```

```r
#Parameters Table
parTable(fitBatistoni)
```

```
##      id  lhs op  rhs user group free ustart exo label eq.id unco plabel
## 1     1   f1 =~ F18r    1     1    0      1   0           0    0   .p1.
## 2     2   f1 =~ F14r    1     1    1     NA   0           0    1   .p2.
## 3     3   f1 =~  F6r    1     1    2     NA   0           0    2   .p3.
## 4     4   f1 =~ F13r    1     1    3     NA   0           0    3   .p4.
## 5     5   f1 =~ F19r    1     1    4     NA   0           0    4   .p5.
## 6     6   f1 =~ F17r    1     1    5     NA   0           0    5   .p6.
## 7     7   f1 =~  F9r    1     1    6     NA   0           0    6   .p7.
## 8     8   f1 =~ F10r    1     1    7     NA   0           0    7   .p8.
## 9     9   f1 =~ F20r    1     1    8     NA   0           0    8   .p9.
## 10   10   f2 =~  F3r    1     1    0      1   0           0    0  .p10.
## 11   11   f2 =~  F1r    1     1    9     NA   0           0    9  .p11.
## 12   12   f2 =~  F7r    1     1   10     NA   0           0   10  .p12.
## 13   13   f2 =~  F5r    1     1   11     NA   0           0   11  .p13.
## 14   14   f2 =~  F2r    1     1   12     NA   0           0   12  .p14.
## 15   15   f3 =~ F12r    1     1    0      1   0           0    0  .p15.
## 16   16   f3 =~  F8r    1     1   13     NA   0           0   13  .p16.
## 17   17   f3 =~ F16r    1     1   14     NA   0           0   14  .p17.
## 18   18   f3 =~  F4r    1     1   15     NA   0           0   15  .p18.
## 19   19   f1 ~~   f2    1     1   16     NA   0           0   16  .p19.
## 20   20   f2 ~~   f3    1     1   17     NA   0           0   17  .p20.
## 21   21   f1 ~~   f3    1     1   18     NA   0           0   18  .p21.
## 22   22 F18r  |   t1    0     1   19     NA   0           0   19  .p22.
## 23   23 F18r  |   t2    0     1   20     NA   0           0   20  .p23.
## 24   24 F18r  |   t3    0     1   21     NA   0           0   21  .p24.
## 25   25 F14r  |   t1    0     1   22     NA   0           0   22  .p25.
## 26   26 F14r  |   t2    0     1   23     NA   0           0   23  .p26.
## 27   27 F14r  |   t3    0     1   24     NA   0           0   24  .p27.
## 28   28  F6r  |   t1    0     1   25     NA   0           0   25  .p28.
## 29   29  F6r  |   t2    0     1   26     NA   0           0   26  .p29.
## 30   30  F6r  |   t3    0     1   27     NA   0           0   27  .p30.
## 31   31 F13r  |   t1    0     1   28     NA   0           0   28  .p31.
## 32   32 F13r  |   t2    0     1   29     NA   0           0   29  .p32.
## 33   33 F13r  |   t3    0     1   30     NA   0           0   30  .p33.
## 34   34 F19r  |   t1    0     1   31     NA   0           0   31  .p34.
## 35   35 F19r  |   t2    0     1   32     NA   0           0   32  .p35.
## 36   36 F19r  |   t3    0     1   33     NA   0           0   33  .p36.
## 37   37 F17r  |   t1    0     1   34     NA   0           0   34  .p37.
## 38   38 F17r  |   t2    0     1   35     NA   0           0   35  .p38.
## 39   39 F17r  |   t3    0     1   36     NA   0           0   36  .p39.
## 40   40  F9r  |   t1    0     1   37     NA   0           0   37  .p40.
## 41   41  F9r  |   t2    0     1   38     NA   0           0   38  .p41.
## 42   42  F9r  |   t3    0     1   39     NA   0           0   39  .p42.
## 43   43 F10r  |   t1    0     1   40     NA   0           0   40  .p43.
## 44   44 F10r  |   t2    0     1   41     NA   0           0   41  .p44.
## 45   45 F10r  |   t3    0     1   42     NA   0           0   42  .p45.
## 46   46 F20r  |   t1    0     1   43     NA   0           0   43  .p46.
## 47   47 F20r  |   t2    0     1   44     NA   0           0   44  .p47.
## 48   48 F20r  |   t3    0     1   45     NA   0           0   45  .p48.
## 49   49  F3r  |   t1    0     1   46     NA   0           0   46  .p49.
## 50   50  F3r  |   t2    0     1   47     NA   0           0   47  .p50.
## 51   51  F3r  |   t3    0     1   48     NA   0           0   48  .p51.
## 52   52  F1r  |   t1    0     1   49     NA   0           0   49  .p52.
## 53   53  F1r  |   t2    0     1   50     NA   0           0   50  .p53.
## 54   54  F1r  |   t3    0     1   51     NA   0           0   51  .p54.
## 55   55  F7r  |   t1    0     1   52     NA   0           0   52  .p55.
## 56   56  F7r  |   t2    0     1   53     NA   0           0   53  .p56.
## 57   57  F7r  |   t3    0     1   54     NA   0           0   54  .p57.
## 58   58  F5r  |   t1    0     1   55     NA   0           0   55  .p58.
## 59   59  F5r  |   t2    0     1   56     NA   0           0   56  .p59.
## 60   60  F5r  |   t3    0     1   57     NA   0           0   57  .p60.
## 61   61  F2r  |   t1    0     1   58     NA   0           0   58  .p61.
## 62   62  F2r  |   t2    0     1   59     NA   0           0   59  .p62.
## 63   63  F2r  |   t3    0     1   60     NA   0           0   60  .p63.
## 64   64 F12r  |   t1    0     1   61     NA   0           0   61  .p64.
## 65   65 F12r  |   t2    0     1   62     NA   0           0   62  .p65.
## 66   66 F12r  |   t3    0     1   63     NA   0           0   63  .p66.
## 67   67  F8r  |   t1    0     1   64     NA   0           0   64  .p67.
## 68   68  F8r  |   t2    0     1   65     NA   0           0   65  .p68.
## 69   69  F8r  |   t3    0     1   66     NA   0           0   66  .p69.
## 70   70 F16r  |   t1    0     1   67     NA   0           0   67  .p70.
## 71   71 F16r  |   t2    0     1   68     NA   0           0   68  .p71.
## 72   72 F16r  |   t3    0     1   69     NA   0           0   69  .p72.
## 73   73  F4r  |   t1    0     1   70     NA   0           0   70  .p73.
## 74   74  F4r  |   t2    0     1   71     NA   0           0   71  .p74.
## 75   75  F4r  |   t3    0     1   72     NA   0           0   72  .p75.
## 76   76 F18r ~~ F18r    0     1    0      1   0           0    0  .p76.
## 77   77 F14r ~~ F14r    0     1    0      1   0           0    0  .p77.
## 78   78  F6r ~~  F6r    0     1    0      1   0           0    0  .p78.
## 79   79 F13r ~~ F13r    0     1    0      1   0           0    0  .p79.
## 80   80 F19r ~~ F19r    0     1    0      1   0           0    0  .p80.
## 81   81 F17r ~~ F17r    0     1    0      1   0           0    0  .p81.
## 82   82  F9r ~~  F9r    0     1    0      1   0           0    0  .p82.
## 83   83 F10r ~~ F10r    0     1    0      1   0           0    0  .p83.
## 84   84 F20r ~~ F20r    0     1    0      1   0           0    0  .p84.
## 85   85  F3r ~~  F3r    0     1    0      1   0           0    0  .p85.
## 86   86  F1r ~~  F1r    0     1    0      1   0           0    0  .p86.
## 87   87  F7r ~~  F7r    0     1    0      1   0           0    0  .p87.
## 88   88  F5r ~~  F5r    0     1    0      1   0           0    0  .p88.
## 89   89  F2r ~~  F2r    0     1    0      1   0           0    0  .p89.
## 90   90 F12r ~~ F12r    0     1    0      1   0           0    0  .p90.
## 91   91  F8r ~~  F8r    0     1    0      1   0           0    0  .p91.
## 92   92 F16r ~~ F16r    0     1    0      1   0           0    0  .p92.
## 93   93  F4r ~~  F4r    0     1    0      1   0           0    0  .p93.
## 94   94   f1 ~~   f1    0     1   73     NA   0           0   73  .p94.
## 95   95   f2 ~~   f2    0     1   74     NA   0           0   74  .p95.
## 96   96   f3 ~~   f3    0     1   75     NA   0           0   75  .p96.
## 97   97 F18r ~1         0     1    0      0   0           0    0  .p97.
## 98   98 F14r ~1         0     1    0      0   0           0    0  .p98.
## 99   99  F6r ~1         0     1    0      0   0           0    0  .p99.
## 100 100 F13r ~1         0     1    0      0   0           0    0 .p100.
## 101 101 F19r ~1         0     1    0      0   0           0    0 .p101.
## 102 102 F17r ~1         0     1    0      0   0           0    0 .p102.
## 103 103  F9r ~1         0     1    0      0   0           0    0 .p103.
## 104 104 F10r ~1         0     1    0      0   0           0    0 .p104.
## 105 105 F20r ~1         0     1    0      0   0           0    0 .p105.
## 106 106  F3r ~1         0     1    0      0   0           0    0 .p106.
## 107 107  F1r ~1         0     1    0      0   0           0    0 .p107.
## 108 108  F7r ~1         0     1    0      0   0           0    0 .p108.
## 109 109  F5r ~1         0     1    0      0   0           0    0 .p109.
## 110 110  F2r ~1         0     1    0      0   0           0    0 .p110.
## 111 111 F12r ~1         0     1    0      0   0           0    0 .p111.
## 112 112  F8r ~1         0     1    0      0   0           0    0 .p112.
## 113 113 F16r ~1         0     1    0      0   0           0    0 .p113.
## 114 114  F4r ~1         0     1    0      0   0           0    0 .p114.
## 115 115   f1 ~1         0     1    0      0   0           0    0 .p115.
## 116 116   f2 ~1         0     1    0      0   0           0    0 .p116.
## 117 117   f3 ~1         0     1    0      0   0           0    0 .p117.
##      start
## 1    1.000
## 2    0.819
## 3    0.802
## 4    0.345
## 5    0.771
## 6    0.661
## 7    0.677
## 8    0.742
## 9    0.693
## 10   1.000
## 11   0.803
## 12   0.884
## 13   0.898
## 14   0.832
## 15   1.000
## 16   0.478
## 17   0.746
## 18   0.522
## 19   0.000
## 20   0.000
## 21   0.000
## 22  -1.019
## 23  -0.452
## 24  -0.017
## 25  -0.752
## 26  -0.290
## 27   0.115
## 28  -1.028
## 29  -0.502
## 30   0.012
## 31  -0.529
## 32   0.115
## 33   0.575
## 34  -0.346
## 35   0.100
## 36   0.490
## 37  -0.224
## 38   0.264
## 39   0.658
## 40  -0.707
## 41  -0.204
## 42   0.179
## 43  -0.518
## 44  -0.007
## 45   0.469
## 46  -0.695
## 47  -0.189
## 48   0.224
## 49  -0.818
## 50  -0.184
## 51   0.310
## 52  -0.616
## 53   0.071
## 54   0.658
## 55  -0.676
## 56  -0.199
## 57   0.326
## 58  -0.622
## 59   0.002
## 60   0.546
## 61  -0.409
## 62   0.120
## 63   0.707
## 64  -0.604
## 65  -0.145
## 66   0.404
## 67  -0.140
## 68   0.321
## 69   0.918
## 70  -0.362
## 71  -0.046
## 72   0.415
## 73  -0.431
## 74   0.110
## 75   0.701
## 76   1.000
## 77   1.000
## 78   1.000
## 79   1.000
## 80   1.000
## 81   1.000
## 82   1.000
## 83   1.000
## 84   1.000
## 85   1.000
## 86   1.000
## 87   1.000
## 88   1.000
## 89   1.000
## 90   1.000
## 91   1.000
## 92   1.000
## 93   1.000
## 94   0.050
## 95   0.050
## 96   0.050
## 97   0.000
## 98   0.000
## 99   0.000
## 100  0.000
## 101  0.000
## 102  0.000
## 103  0.000
## 104  0.000
## 105  0.000
## 106  0.000
## 107  0.000
## 108  0.000
## 109  0.000
## 110  0.000
## 111  0.000
## 112  0.000
## 113  0.000
## 114  0.000
## 115  0.000
## 116  0.000
## 117  0.000
```

```r
#Model Coefficients
coef(fitBatistoni)
```

```
## f1=~F14r  f1=~F6r f1=~F13r f1=~F19r f1=~F17r  f1=~F9r f1=~F10r f1=~F20r 
##    0.902    0.935    0.356    0.839    0.704    0.818    0.817    0.823 
##  f2=~F1r  f2=~F7r  f2=~F5r  f2=~F2r  f3=~F8r f3=~F16r  f3=~F4r   f1~~f2 
##    0.839    0.995    0.996    0.823    0.406    0.810    0.368    0.474 
##   f2~~f3   f1~~f3  F18r|t1  F18r|t2  F18r|t3  F14r|t1  F14r|t2  F14r|t3 
##    0.248    0.435   -1.019   -0.452   -0.017   -0.752   -0.290    0.115 
##   F6r|t1   F6r|t2   F6r|t3  F13r|t1  F13r|t2  F13r|t3  F19r|t1  F19r|t2 
##   -1.028   -0.502    0.012   -0.529    0.115    0.575   -0.346    0.100 
##  F19r|t3  F17r|t1  F17r|t2  F17r|t3   F9r|t1   F9r|t2   F9r|t3  F10r|t1 
##    0.490   -0.224    0.264    0.658   -0.707   -0.204    0.179   -0.518 
##  F10r|t2  F10r|t3  F20r|t1  F20r|t2  F20r|t3   F3r|t1   F3r|t2   F3r|t3 
##   -0.007    0.469   -0.695   -0.189    0.224   -0.818   -0.184    0.310 
##   F1r|t1   F1r|t2   F1r|t3   F7r|t1   F7r|t2   F7r|t3   F5r|t1   F5r|t2 
##   -0.616    0.071    0.658   -0.676   -0.199    0.326   -0.622    0.002 
##   F5r|t3   F2r|t1   F2r|t2   F2r|t3  F12r|t1  F12r|t2  F12r|t3   F8r|t1 
##    0.546   -0.409    0.120    0.707   -0.604   -0.145    0.404   -0.140 
##   F8r|t2   F8r|t3  F16r|t1  F16r|t2  F16r|t3   F4r|t1   F4r|t2   F4r|t3 
##    0.321    0.918   -0.362   -0.046    0.415   -0.431    0.110    0.701 
##   f1~~f1   f2~~f2   f3~~f3 
##    0.705    0.427    0.804
```

```r
#Modification Index
MIBatistoni<-modindices(fitBatistoni)
MIIBatistoni<- MIBatistoni[which(MIBatistoni$mi>30),]
print(MIIBatistoni)
```

```
##    lhs op  rhs     mi mi.scaled   epc sepc.lv sepc.all sepc.nox
## 1 F18r ~~ F17r 31.979    41.382 0.232   0.232    0.232    0.232
```

```r
#Model Plot
semPaths(fitBatistoni,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-9-1.png) 



```r
#Silveira CFA Model

#Model Identification
Silveira <- '
              # latent variable definitions 
               f1 =~ F18r + F14r + F6r + F3r + F13r
               f2 =~ F19r + F15r + F17r + F1r + F9r + F10r
               f3 =~ F20r + F7r + F5r + F11r
               f4 =~ F12r + F8r + F16r

             # variances and covariances 
               f1 ~~ f2 
               f2 ~~ f3 
               f3 ~~ f4
               f1 ~~ f4
               f1 ~~ f3 
                       '

#Model Fit
fitSilveira <- cfa(Silveira, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitSilveira, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  35 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              218.990     325.302
##   Degrees of freedom                               129         129
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.734
##   Shift parameter                                           27.010
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            12355.368    5666.919
##   Degrees of freedom                               153         153
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.993       0.964
##   Tucker-Lewis Index (TLI)                       0.991       0.958
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.037       0.055
##   90 Percent Confidence Interval          0.028  0.045       0.047  0.062
##   P-value RMSEA <= 0.05                          0.996       0.152
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           1.029       1.029
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F18r              1.000                               0.847    0.847
##     F14r              0.905    0.036   25.393    0.000    0.767    0.767
##     F6r               0.922    0.035   26.048    0.000    0.781    0.781
##     F3r               0.689    0.045   15.330    0.000    0.584    0.584
##     F13r              0.358    0.053    6.785    0.000    0.303    0.303
##   f2 =~
##     F19r              1.000                               0.759    0.759
##     F15r              0.870    0.043   20.162    0.000    0.661    0.661
##     F17r              0.796    0.051   15.743    0.000    0.605    0.605
##     F1r               0.658    0.052   12.693    0.000    0.500    0.500
##     F9r               0.913    0.049   18.565    0.000    0.693    0.693
##     F10r              0.927    0.047   19.826    0.000    0.704    0.704
##   f3 =~
##     F20r              1.000                               0.711    0.711
##     F7r               0.864    0.059   14.601    0.000    0.614    0.614
##     F5r               0.858    0.057   15.082    0.000    0.609    0.609
##     F11r              0.885    0.062   14.167    0.000    0.629    0.629
##   f4 =~
##     F12r              1.000                               0.866    0.866
##     F8r               0.377    0.070    5.384    0.000    0.327    0.327
##     F16r              0.830    0.072   11.456    0.000    0.719    0.719
## 
## Covariances:
##   f1 ~~
##     f2                0.605    0.031   19.248    0.000    0.941    0.941
##   f2 ~~
##     f3                0.511    0.032   15.818    0.000    0.948    0.948
##   f3 ~~
##     f4                0.357    0.037    9.559    0.000    0.581    0.581
##   f1 ~~
##     f4                0.474    0.037   12.716    0.000    0.646    0.646
##     f3                0.550    0.033   16.612    0.000    0.914    0.914
##   f2 ~~
##     f4                0.308    0.036    8.499    0.000    0.469    0.469
## 
## Intercepts:
##     F18r              0.000                               0.000    0.000
##     F14r              0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F13r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F1r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F20r              0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F12r              0.000                               0.000    0.000
##     F8r               0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
##     f3                0.000                               0.000    0.000
##     f4                0.000                               0.000    0.000
## 
## Thresholds:
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
##     F14r|t1          -0.752    0.061  -12.229    0.000   -0.752   -0.752
##     F14r|t2          -0.290    0.056   -5.154    0.000   -0.290   -0.290
##     F14r|t3           0.115    0.056    2.073    0.038    0.115    0.115
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F13r|t1          -0.529    0.058   -9.081    0.000   -0.529   -0.529
##     F13r|t2           0.115    0.056    2.073    0.038    0.115    0.115
##     F13r|t3           0.575    0.059    9.771    0.000    0.575    0.575
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F20r|t1          -0.695    0.061  -11.475    0.000   -0.695   -0.695
##     F20r|t2          -0.189    0.056   -3.394    0.001   -0.189   -0.189
##     F20r|t3           0.224    0.056    4.011    0.000    0.224    0.224
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F8r|t1           -0.140    0.056   -2.513    0.012   -0.140   -0.140
##     F8r|t2            0.321    0.056    5.680    0.000    0.321    0.321
##     F8r|t3            0.918    0.065   14.173    0.000    0.918    0.918
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
## 
## Variances:
##     F18r              0.282                               0.282    0.282
##     F14r              0.412                               0.412    0.412
##     F6r               0.390                               0.390    0.390
##     F3r               0.659                               0.659    0.659
##     F13r              0.908                               0.908    0.908
##     F19r              0.424                               0.424    0.424
##     F15r              0.564                               0.564    0.564
##     F17r              0.635                               0.635    0.635
##     F1r               0.750                               0.750    0.750
##     F9r               0.520                               0.520    0.520
##     F10r              0.505                               0.505    0.505
##     F20r              0.495                               0.495    0.495
##     F7r               0.623                               0.623    0.623
##     F5r               0.629                               0.629    0.629
##     F11r              0.604                               0.604    0.604
##     F12r              0.250                               0.250    0.250
##     F8r               0.893                               0.893    0.893
##     F16r              0.483                               0.483    0.483
##     f1                0.718    0.038                      1.000    1.000
##     f2                0.576    0.039                      1.000    1.000
##     f3                0.505    0.046                      1.000    1.000
##     f4                0.750    0.071                      1.000    1.000
## 
## R-Square:
## 
##     F18r              0.718
##     F14r              0.588
##     F6r               0.610
##     F3r               0.341
##     F13r              0.092
##     F19r              0.576
##     F15r              0.436
##     F17r              0.365
##     F1r               0.250
##     F9r               0.480
##     F10r              0.495
##     F20r              0.505
##     F7r               0.377
##     F5r               0.371
##     F11r              0.396
##     F12r              0.750
##     F8r               0.107
##     F16r              0.517
```

```r
#Model Fit Measures
fitMeasures(fitSilveira)
```

```
##                          npar                          fmin 
##                        78.000                         0.213 
##                         chisq                            df 
##                       218.990                       129.000 
##                        pvalue                  chisq.scaled 
##                         0.000                       325.302 
##                     df.scaled                 pvalue.scaled 
##                       129.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.734                     12355.368 
##                   baseline.df               baseline.pvalue 
##                       153.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      5666.919                       153.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.213 
##                           cfi                           tli 
##                         0.993                         0.991 
##                          nnfi                           rfi 
##                         0.991                         0.979 
##                           nfi                          pnfi 
##                         0.982                         0.828 
##                           ifi                           rni 
##                         0.993                         0.993 
##                    cfi.scaled                    tli.scaled 
##                         0.964                         0.958 
##                   nnfi.scaled                    rfi.scaled 
##                         0.958                         0.932 
##                    nfi.scaled                    ifi.scaled 
##                         0.943                         0.943 
##                    rni.scaled                         rmsea 
##                         0.984                         0.037 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.028                         0.045 
##                  rmsea.pvalue                  rmsea.scaled 
##                         0.996                         0.055 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.047                         0.062 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.152                         1.029 
##                         cn_05                         cn_01 
##                       366.916                       396.775 
##                           gfi                          agfi 
##                         0.986                         0.978 
##                          pgfi                           mfi 
##                         0.615                         0.916
```

```r
#Parameters Estimates
EstSilveira <- parameterEstimates(fitSilveira, standardized=T, ci=F)
subset(EstSilveira, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~ F18r 1.000 0.000     NA     NA  0.847   0.847   0.847
## 2   f1 =~ F14r 0.905 0.036 25.393      0  0.767   0.767   0.767
## 3   f1 =~  F6r 0.922 0.035 26.048      0  0.781   0.781   0.781
## 4   f1 =~  F3r 0.689 0.045 15.330      0  0.584   0.584   0.584
## 5   f1 =~ F13r 0.358 0.053  6.785      0  0.303   0.303   0.303
## 6   f2 =~ F19r 1.000 0.000     NA     NA  0.759   0.759   0.759
## 7   f2 =~ F15r 0.870 0.043 20.162      0  0.661   0.661   0.661
## 8   f2 =~ F17r 0.796 0.051 15.743      0  0.605   0.605   0.605
## 9   f2 =~  F1r 0.658 0.052 12.693      0  0.500   0.500   0.500
## 10  f2 =~  F9r 0.913 0.049 18.565      0  0.693   0.693   0.693
## 11  f2 =~ F10r 0.927 0.047 19.826      0  0.704   0.704   0.704
## 12  f3 =~ F20r 1.000 0.000     NA     NA  0.711   0.711   0.711
## 13  f3 =~  F7r 0.864 0.059 14.601      0  0.614   0.614   0.614
## 14  f3 =~  F5r 0.858 0.057 15.082      0  0.609   0.609   0.609
## 15  f3 =~ F11r 0.885 0.062 14.167      0  0.629   0.629   0.629
## 16  f4 =~ F12r 1.000 0.000     NA     NA  0.866   0.866   0.866
## 17  f4 =~  F8r 0.377 0.070  5.384      0  0.327   0.327   0.327
## 18  f4 =~ F16r 0.830 0.072 11.456      0  0.719   0.719   0.719
```

```r
#Parameters Table
parTable(fitSilveira)
```

```
##      id  lhs op  rhs user group free ustart exo label eq.id unco plabel
## 1     1   f1 =~ F18r    1     1    0      1   0           0    0   .p1.
## 2     2   f1 =~ F14r    1     1    1     NA   0           0    1   .p2.
## 3     3   f1 =~  F6r    1     1    2     NA   0           0    2   .p3.
## 4     4   f1 =~  F3r    1     1    3     NA   0           0    3   .p4.
## 5     5   f1 =~ F13r    1     1    4     NA   0           0    4   .p5.
## 6     6   f2 =~ F19r    1     1    0      1   0           0    0   .p6.
## 7     7   f2 =~ F15r    1     1    5     NA   0           0    5   .p7.
## 8     8   f2 =~ F17r    1     1    6     NA   0           0    6   .p8.
## 9     9   f2 =~  F1r    1     1    7     NA   0           0    7   .p9.
## 10   10   f2 =~  F9r    1     1    8     NA   0           0    8  .p10.
## 11   11   f2 =~ F10r    1     1    9     NA   0           0    9  .p11.
## 12   12   f3 =~ F20r    1     1    0      1   0           0    0  .p12.
## 13   13   f3 =~  F7r    1     1   10     NA   0           0   10  .p13.
## 14   14   f3 =~  F5r    1     1   11     NA   0           0   11  .p14.
## 15   15   f3 =~ F11r    1     1   12     NA   0           0   12  .p15.
## 16   16   f4 =~ F12r    1     1    0      1   0           0    0  .p16.
## 17   17   f4 =~  F8r    1     1   13     NA   0           0   13  .p17.
## 18   18   f4 =~ F16r    1     1   14     NA   0           0   14  .p18.
## 19   19   f1 ~~   f2    1     1   15     NA   0           0   15  .p19.
## 20   20   f2 ~~   f3    1     1   16     NA   0           0   16  .p20.
## 21   21   f3 ~~   f4    1     1   17     NA   0           0   17  .p21.
## 22   22   f1 ~~   f4    1     1   18     NA   0           0   18  .p22.
## 23   23   f1 ~~   f3    1     1   19     NA   0           0   19  .p23.
## 24   24 F18r  |   t1    0     1   20     NA   0           0   20  .p24.
## 25   25 F18r  |   t2    0     1   21     NA   0           0   21  .p25.
## 26   26 F18r  |   t3    0     1   22     NA   0           0   22  .p26.
## 27   27 F14r  |   t1    0     1   23     NA   0           0   23  .p27.
## 28   28 F14r  |   t2    0     1   24     NA   0           0   24  .p28.
## 29   29 F14r  |   t3    0     1   25     NA   0           0   25  .p29.
## 30   30  F6r  |   t1    0     1   26     NA   0           0   26  .p30.
## 31   31  F6r  |   t2    0     1   27     NA   0           0   27  .p31.
## 32   32  F6r  |   t3    0     1   28     NA   0           0   28  .p32.
## 33   33  F3r  |   t1    0     1   29     NA   0           0   29  .p33.
## 34   34  F3r  |   t2    0     1   30     NA   0           0   30  .p34.
## 35   35  F3r  |   t3    0     1   31     NA   0           0   31  .p35.
## 36   36 F13r  |   t1    0     1   32     NA   0           0   32  .p36.
## 37   37 F13r  |   t2    0     1   33     NA   0           0   33  .p37.
## 38   38 F13r  |   t3    0     1   34     NA   0           0   34  .p38.
## 39   39 F19r  |   t1    0     1   35     NA   0           0   35  .p39.
## 40   40 F19r  |   t2    0     1   36     NA   0           0   36  .p40.
## 41   41 F19r  |   t3    0     1   37     NA   0           0   37  .p41.
## 42   42 F15r  |   t1    0     1   38     NA   0           0   38  .p42.
## 43   43 F15r  |   t2    0     1   39     NA   0           0   39  .p43.
## 44   44 F15r  |   t3    0     1   40     NA   0           0   40  .p44.
## 45   45 F17r  |   t1    0     1   41     NA   0           0   41  .p45.
## 46   46 F17r  |   t2    0     1   42     NA   0           0   42  .p46.
## 47   47 F17r  |   t3    0     1   43     NA   0           0   43  .p47.
## 48   48  F1r  |   t1    0     1   44     NA   0           0   44  .p48.
## 49   49  F1r  |   t2    0     1   45     NA   0           0   45  .p49.
## 50   50  F1r  |   t3    0     1   46     NA   0           0   46  .p50.
## 51   51  F9r  |   t1    0     1   47     NA   0           0   47  .p51.
## 52   52  F9r  |   t2    0     1   48     NA   0           0   48  .p52.
## 53   53  F9r  |   t3    0     1   49     NA   0           0   49  .p53.
## 54   54 F10r  |   t1    0     1   50     NA   0           0   50  .p54.
## 55   55 F10r  |   t2    0     1   51     NA   0           0   51  .p55.
## 56   56 F10r  |   t3    0     1   52     NA   0           0   52  .p56.
## 57   57 F20r  |   t1    0     1   53     NA   0           0   53  .p57.
## 58   58 F20r  |   t2    0     1   54     NA   0           0   54  .p58.
## 59   59 F20r  |   t3    0     1   55     NA   0           0   55  .p59.
## 60   60  F7r  |   t1    0     1   56     NA   0           0   56  .p60.
## 61   61  F7r  |   t2    0     1   57     NA   0           0   57  .p61.
## 62   62  F7r  |   t3    0     1   58     NA   0           0   58  .p62.
## 63   63  F5r  |   t1    0     1   59     NA   0           0   59  .p63.
## 64   64  F5r  |   t2    0     1   60     NA   0           0   60  .p64.
## 65   65  F5r  |   t3    0     1   61     NA   0           0   61  .p65.
## 66   66 F11r  |   t1    0     1   62     NA   0           0   62  .p66.
## 67   67 F11r  |   t2    0     1   63     NA   0           0   63  .p67.
## 68   68 F11r  |   t3    0     1   64     NA   0           0   64  .p68.
## 69   69 F12r  |   t1    0     1   65     NA   0           0   65  .p69.
## 70   70 F12r  |   t2    0     1   66     NA   0           0   66  .p70.
## 71   71 F12r  |   t3    0     1   67     NA   0           0   67  .p71.
## 72   72  F8r  |   t1    0     1   68     NA   0           0   68  .p72.
## 73   73  F8r  |   t2    0     1   69     NA   0           0   69  .p73.
## 74   74  F8r  |   t3    0     1   70     NA   0           0   70  .p74.
## 75   75 F16r  |   t1    0     1   71     NA   0           0   71  .p75.
## 76   76 F16r  |   t2    0     1   72     NA   0           0   72  .p76.
## 77   77 F16r  |   t3    0     1   73     NA   0           0   73  .p77.
## 78   78 F18r ~~ F18r    0     1    0      1   0           0    0  .p78.
## 79   79 F14r ~~ F14r    0     1    0      1   0           0    0  .p79.
## 80   80  F6r ~~  F6r    0     1    0      1   0           0    0  .p80.
## 81   81  F3r ~~  F3r    0     1    0      1   0           0    0  .p81.
## 82   82 F13r ~~ F13r    0     1    0      1   0           0    0  .p82.
## 83   83 F19r ~~ F19r    0     1    0      1   0           0    0  .p83.
## 84   84 F15r ~~ F15r    0     1    0      1   0           0    0  .p84.
## 85   85 F17r ~~ F17r    0     1    0      1   0           0    0  .p85.
## 86   86  F1r ~~  F1r    0     1    0      1   0           0    0  .p86.
## 87   87  F9r ~~  F9r    0     1    0      1   0           0    0  .p87.
## 88   88 F10r ~~ F10r    0     1    0      1   0           0    0  .p88.
## 89   89 F20r ~~ F20r    0     1    0      1   0           0    0  .p89.
## 90   90  F7r ~~  F7r    0     1    0      1   0           0    0  .p90.
## 91   91  F5r ~~  F5r    0     1    0      1   0           0    0  .p91.
## 92   92 F11r ~~ F11r    0     1    0      1   0           0    0  .p92.
## 93   93 F12r ~~ F12r    0     1    0      1   0           0    0  .p93.
## 94   94  F8r ~~  F8r    0     1    0      1   0           0    0  .p94.
## 95   95 F16r ~~ F16r    0     1    0      1   0           0    0  .p95.
## 96   96   f1 ~~   f1    0     1   74     NA   0           0   74  .p96.
## 97   97   f2 ~~   f2    0     1   75     NA   0           0   75  .p97.
## 98   98   f3 ~~   f3    0     1   76     NA   0           0   76  .p98.
## 99   99   f4 ~~   f4    0     1   77     NA   0           0   77  .p99.
## 100 100   f2 ~~   f4    0     1   78     NA   0           0   78 .p100.
## 101 101 F18r ~1         0     1    0      0   0           0    0 .p101.
## 102 102 F14r ~1         0     1    0      0   0           0    0 .p102.
## 103 103  F6r ~1         0     1    0      0   0           0    0 .p103.
## 104 104  F3r ~1         0     1    0      0   0           0    0 .p104.
## 105 105 F13r ~1         0     1    0      0   0           0    0 .p105.
## 106 106 F19r ~1         0     1    0      0   0           0    0 .p106.
## 107 107 F15r ~1         0     1    0      0   0           0    0 .p107.
## 108 108 F17r ~1         0     1    0      0   0           0    0 .p108.
## 109 109  F1r ~1         0     1    0      0   0           0    0 .p109.
## 110 110  F9r ~1         0     1    0      0   0           0    0 .p110.
## 111 111 F10r ~1         0     1    0      0   0           0    0 .p111.
## 112 112 F20r ~1         0     1    0      0   0           0    0 .p112.
## 113 113  F7r ~1         0     1    0      0   0           0    0 .p113.
## 114 114  F5r ~1         0     1    0      0   0           0    0 .p114.
## 115 115 F11r ~1         0     1    0      0   0           0    0 .p115.
## 116 116 F12r ~1         0     1    0      0   0           0    0 .p116.
## 117 117  F8r ~1         0     1    0      0   0           0    0 .p117.
## 118 118 F16r ~1         0     1    0      0   0           0    0 .p118.
## 119 119   f1 ~1         0     1    0      0   0           0    0 .p119.
## 120 120   f2 ~1         0     1    0      0   0           0    0 .p120.
## 121 121   f3 ~1         0     1    0      0   0           0    0 .p121.
## 122 122   f4 ~1         0     1    0      0   0           0    0 .p122.
##      start
## 1    1.000
## 2    0.952
## 3    0.940
## 4    0.651
## 5    0.419
## 6    1.000
## 7    0.775
## 8    0.603
## 9    0.511
## 10   0.744
## 11   0.739
## 12   1.000
## 13   1.004
## 14   1.060
## 15   0.779
## 16   1.000
## 17   0.396
## 18   0.731
## 19   0.000
## 20   0.000
## 21   0.000
## 22   0.000
## 23   0.000
## 24  -1.019
## 25  -0.452
## 26  -0.017
## 27  -0.752
## 28  -0.290
## 29   0.115
## 30  -1.028
## 31  -0.502
## 32   0.012
## 33  -0.818
## 34  -0.184
## 35   0.310
## 36  -0.529
## 37   0.115
## 38   0.575
## 39  -0.346
## 40   0.100
## 41   0.490
## 42  -0.357
## 43   0.209
## 44   0.658
## 45  -0.224
## 46   0.264
## 47   0.658
## 48  -0.616
## 49   0.071
## 50   0.658
## 51  -0.707
## 52  -0.204
## 53   0.179
## 54  -0.518
## 55  -0.007
## 56   0.469
## 57  -0.695
## 58  -0.189
## 59   0.224
## 60  -0.676
## 61  -0.199
## 62   0.326
## 63  -0.622
## 64   0.002
## 65   0.546
## 66  -0.867
## 67  -0.393
## 68   0.071
## 69  -0.604
## 70  -0.145
## 71   0.404
## 72  -0.140
## 73   0.321
## 74   0.918
## 75  -0.362
## 76  -0.046
## 77   0.415
## 78   1.000
## 79   1.000
## 80   1.000
## 81   1.000
## 82   1.000
## 83   1.000
## 84   1.000
## 85   1.000
## 86   1.000
## 87   1.000
## 88   1.000
## 89   1.000
## 90   1.000
## 91   1.000
## 92   1.000
## 93   1.000
## 94   1.000
## 95   1.000
## 96   0.050
## 97   0.050
## 98   0.050
## 99   0.050
## 100  0.000
## 101  0.000
## 102  0.000
## 103  0.000
## 104  0.000
## 105  0.000
## 106  0.000
## 107  0.000
## 108  0.000
## 109  0.000
## 110  0.000
## 111  0.000
## 112  0.000
## 113  0.000
## 114  0.000
## 115  0.000
## 116  0.000
## 117  0.000
## 118  0.000
## 119  0.000
## 120  0.000
## 121  0.000
## 122  0.000
```

```r
#Model Coefficients
coef(fitSilveira)
```

```
## f1=~F14r  f1=~F6r  f1=~F3r f1=~F13r f2=~F15r f2=~F17r  f2=~F1r  f2=~F9r 
##    0.905    0.922    0.689    0.358    0.870    0.796    0.658    0.913 
## f2=~F10r  f3=~F7r  f3=~F5r f3=~F11r  f4=~F8r f4=~F16r   f1~~f2   f2~~f3 
##    0.927    0.864    0.858    0.885    0.377    0.830    0.605    0.511 
##   f3~~f4   f1~~f4   f1~~f3  F18r|t1  F18r|t2  F18r|t3  F14r|t1  F14r|t2 
##    0.357    0.474    0.550   -1.019   -0.452   -0.017   -0.752   -0.290 
##  F14r|t3   F6r|t1   F6r|t2   F6r|t3   F3r|t1   F3r|t2   F3r|t3  F13r|t1 
##    0.115   -1.028   -0.502    0.012   -0.818   -0.184    0.310   -0.529 
##  F13r|t2  F13r|t3  F19r|t1  F19r|t2  F19r|t3  F15r|t1  F15r|t2  F15r|t3 
##    0.115    0.575   -0.346    0.100    0.490   -0.357    0.209    0.658 
##  F17r|t1  F17r|t2  F17r|t3   F1r|t1   F1r|t2   F1r|t3   F9r|t1   F9r|t2 
##   -0.224    0.264    0.658   -0.616    0.071    0.658   -0.707   -0.204 
##   F9r|t3  F10r|t1  F10r|t2  F10r|t3  F20r|t1  F20r|t2  F20r|t3   F7r|t1 
##    0.179   -0.518   -0.007    0.469   -0.695   -0.189    0.224   -0.676 
##   F7r|t2   F7r|t3   F5r|t1   F5r|t2   F5r|t3  F11r|t1  F11r|t2  F11r|t3 
##   -0.199    0.326   -0.622    0.002    0.546   -0.867   -0.393    0.071 
##  F12r|t1  F12r|t2  F12r|t3   F8r|t1   F8r|t2   F8r|t3  F16r|t1  F16r|t2 
##   -0.604   -0.145    0.404   -0.140    0.321    0.918   -0.362   -0.046 
##  F16r|t3   f1~~f1   f2~~f2   f3~~f3   f4~~f4   f2~~f4 
##    0.415    0.718    0.576    0.505    0.750    0.308
```

```r
#Modification Index
MISilveira<-modindices(fitSilveira)
MIISilveira<- MISilveira[which(MISilveira$mi>30),]
print(MIISilveira)
```

```
##    lhs op  rhs     mi mi.scaled   epc sepc.lv sepc.all sepc.nox
## 1 F18r ~~ F17r 37.259    50.752 0.250   0.250    0.250    0.250
## 2 F19r ~~ F15r 45.897    62.518 0.254   0.254    0.254    0.254
```

```r
#Model Plot
semPaths(fitSilveira,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-10-1.png) 



```r
#Marliere - Principal Components Analysis - Two Components Solution - CFA Model 

PCA2_CFA <- '
              # latent variable definitions 
               f1 =~ F4r + F8r + F12r + F16r
               f2 =~ F1r + F2r + F3r + F5r + F6r + F7r + F9r + F10r + F11r + F13r + F14r + F15r + F17r + F18r + F19r + F20r

                #factor covariances 
                f1~~f2
                       '
fitPCA2 <- cfa(PCA2_CFA, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitPCA2, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  33 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              354.536     479.936
##   Degrees of freedom                               169         169
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.809
##   Shift parameter                                           41.803
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            13400.870    6035.299
##   Degrees of freedom                               190         190
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.986       0.947
##   Tucker-Lewis Index (TLI)                       0.984       0.940
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.046       0.060
##   90 Percent Confidence Interval          0.040  0.053       0.054  0.066
##   P-value RMSEA <= 0.05                          0.811       0.005
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           1.191       1.191
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F4r               1.000                               0.327    0.327
##     F8r               1.086    0.237    4.591    0.000    0.355    0.355
##     F12r              2.745    0.512    5.362    0.000    0.898    0.898
##     F16r              2.225    0.410    5.422    0.000    0.728    0.728
##   f2 =~
##     F1r               1.000                               0.490    0.490
##     F2r               1.028    0.099   10.420    0.000    0.504    0.504
##     F3r               1.210    0.110   11.018    0.000    0.593    0.593
##     F5r               1.202    0.110   10.967    0.000    0.589    0.589
##     F6r               1.575    0.123   12.800    0.000    0.772    0.772
##     F7r               1.210    0.110   10.966    0.000    0.593    0.593
##     F9r               1.377    0.114   12.070    0.000    0.675    0.675
##     F10r              1.403    0.120   11.726    0.000    0.688    0.688
##     F11r              1.266    0.115   10.988    0.000    0.621    0.621
##     F13r              0.610    0.098    6.211    0.000    0.299    0.299
##     F14r              1.534    0.124   12.358    0.000    0.752    0.752
##     F15r              1.315    0.117   11.195    0.000    0.645    0.645
##     F17r              1.195    0.113   10.588    0.000    0.586    0.586
##     F18r              1.693    0.138   12.295    0.000    0.830    0.830
##     F19r              1.507    0.119   12.694    0.000    0.739    0.739
##     F20r              1.391    0.122   11.443    0.000    0.682    0.682
## 
## Covariances:
##   f1 ~~
##     f2                0.088    0.019    4.704    0.000    0.548    0.548
## 
## Intercepts:
##     F4r               0.000                               0.000    0.000
##     F8r               0.000                               0.000    0.000
##     F12r              0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F1r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F13r              0.000                               0.000    0.000
##     F14r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F18r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F20r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
## 
## Thresholds:
##     F4r|t1           -0.431    0.057   -7.517    0.000   -0.431   -0.431
##     F4r|t2            0.110    0.056    1.984    0.047    0.110    0.110
##     F4r|t3            0.701    0.061   11.559    0.000    0.701    0.701
##     F8r|t1           -0.140    0.056   -2.513    0.012   -0.140   -0.140
##     F8r|t2            0.321    0.056    5.680    0.000    0.321    0.321
##     F8r|t3            0.918    0.065   14.173    0.000    0.918    0.918
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F13r|t1          -0.529    0.058   -9.081    0.000   -0.529   -0.529
##     F13r|t2           0.115    0.056    2.073    0.038    0.115    0.115
##     F13r|t3           0.575    0.059    9.771    0.000    0.575    0.575
##     F14r|t1          -0.752    0.061  -12.229    0.000   -0.752   -0.752
##     F14r|t2          -0.290    0.056   -5.154    0.000   -0.290   -0.290
##     F14r|t3           0.115    0.056    2.073    0.038    0.115    0.115
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F20r|t1          -0.695    0.061  -11.475    0.000   -0.695   -0.695
##     F20r|t2          -0.189    0.056   -3.394    0.001   -0.189   -0.189
##     F20r|t3           0.224    0.056    4.011    0.000    0.224    0.224
## 
## Variances:
##     F4r               0.893                               0.893    0.893
##     F8r               0.874                               0.874    0.874
##     F12r              0.193                               0.193    0.193
##     F16r              0.469                               0.469    0.469
##     F1r               0.760                               0.760    0.760
##     F2r               0.746                               0.746    0.746
##     F3r               0.648                               0.648    0.648
##     F5r               0.653                               0.653    0.653
##     F6r               0.404                               0.404    0.404
##     F7r               0.648                               0.648    0.648
##     F9r               0.544                               0.544    0.544
##     F10r              0.527                               0.527    0.527
##     F11r              0.615                               0.615    0.615
##     F13r              0.910                               0.910    0.910
##     F14r              0.434                               0.434    0.434
##     F15r              0.584                               0.584    0.584
##     F17r              0.657                               0.657    0.657
##     F18r              0.311                               0.311    0.311
##     F19r              0.454                               0.454    0.454
##     F20r              0.535                               0.535    0.535
##     f1                0.107    0.038                      1.000    1.000
##     f2                0.240    0.037                      1.000    1.000
## 
## R-Square:
## 
##     F4r               0.107
##     F8r               0.126
##     F12r              0.807
##     F16r              0.531
##     F1r               0.240
##     F2r               0.254
##     F3r               0.352
##     F5r               0.347
##     F6r               0.596
##     F7r               0.352
##     F9r               0.456
##     F10r              0.473
##     F11r              0.385
##     F13r              0.090
##     F14r              0.566
##     F15r              0.416
##     F17r              0.343
##     F18r              0.689
##     F19r              0.546
##     F20r              0.465
```

```r
#Model Fit Measures
fitMeasures(fitPCA2)
```

```
##                          npar                          fmin 
##                        81.000                         0.346 
##                         chisq                            df 
##                       354.536                       169.000 
##                        pvalue                  chisq.scaled 
##                         0.000                       479.936 
##                     df.scaled                 pvalue.scaled 
##                       169.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.809                     13400.870 
##                   baseline.df               baseline.pvalue 
##                       190.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      6035.299                       190.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.260 
##                           cfi                           tli 
##                         0.986                         0.984 
##                          nnfi                           rfi 
##                         0.984                         0.970 
##                           nfi                          pnfi 
##                         0.974                         0.866 
##                           ifi                           rni 
##                         0.986                         0.986 
##                    cfi.scaled                    tli.scaled 
##                         0.947                         0.940 
##                   nnfi.scaled                    rfi.scaled 
##                         0.940                         0.911 
##                    nfi.scaled                    ifi.scaled 
##                         0.920                         0.920 
##                    rni.scaled                         rmsea 
##                         0.976                         0.046 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.040                         0.053 
##                  rmsea.pvalue                  rmsea.scaled 
##                         0.811                         0.060 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.054                         0.066 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.005                         1.191 
##                         cn_05                         cn_01 
##                       290.310                       311.036 
##                           gfi                          agfi 
##                         0.979                         0.970 
##                          pgfi                           mfi 
##                         0.662                         0.834
```

```r
#Parameters Estimates
EstPCA2 <- parameterEstimates(fitPCA2, standardized=T, ci=F)
subset(EstPCA2, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~  F4r 1.000 0.000     NA     NA  0.327   0.327   0.327
## 2   f1 =~  F8r 1.086 0.237  4.591      0  0.355   0.355   0.355
## 3   f1 =~ F12r 2.745 0.512  5.362      0  0.898   0.898   0.898
## 4   f1 =~ F16r 2.225 0.410  5.422      0  0.728   0.728   0.728
## 5   f2 =~  F1r 1.000 0.000     NA     NA  0.490   0.490   0.490
## 6   f2 =~  F2r 1.028 0.099 10.420      0  0.504   0.504   0.504
## 7   f2 =~  F3r 1.210 0.110 11.018      0  0.593   0.593   0.593
## 8   f2 =~  F5r 1.202 0.110 10.967      0  0.589   0.589   0.589
## 9   f2 =~  F6r 1.575 0.123 12.800      0  0.772   0.772   0.772
## 10  f2 =~  F7r 1.210 0.110 10.966      0  0.593   0.593   0.593
## 11  f2 =~  F9r 1.377 0.114 12.070      0  0.675   0.675   0.675
## 12  f2 =~ F10r 1.403 0.120 11.726      0  0.688   0.688   0.688
## 13  f2 =~ F11r 1.266 0.115 10.988      0  0.621   0.621   0.621
## 14  f2 =~ F13r 0.610 0.098  6.211      0  0.299   0.299   0.299
## 15  f2 =~ F14r 1.534 0.124 12.358      0  0.752   0.752   0.752
## 16  f2 =~ F15r 1.315 0.117 11.195      0  0.645   0.645   0.645
## 17  f2 =~ F17r 1.195 0.113 10.588      0  0.586   0.586   0.586
## 18  f2 =~ F18r 1.693 0.138 12.295      0  0.830   0.830   0.830
## 19  f2 =~ F19r 1.507 0.119 12.694      0  0.739   0.739   0.739
## 20  f2 =~ F20r 1.391 0.122 11.443      0  0.682   0.682   0.682
```

```r
#Model Coefficients
coef(fitPCA2)
```

```
##  f1=~F8r f1=~F12r f1=~F16r  f2=~F2r  f2=~F3r  f2=~F5r  f2=~F6r  f2=~F7r 
##    1.086    2.745    2.225    1.028    1.210    1.202    1.575    1.210 
##  f2=~F9r f2=~F10r f2=~F11r f2=~F13r f2=~F14r f2=~F15r f2=~F17r f2=~F18r 
##    1.377    1.403    1.266    0.610    1.534    1.315    1.195    1.693 
## f2=~F19r f2=~F20r   f1~~f2   F4r|t1   F4r|t2   F4r|t3   F8r|t1   F8r|t2 
##    1.507    1.391    0.088   -0.431    0.110    0.701   -0.140    0.321 
##   F8r|t3  F12r|t1  F12r|t2  F12r|t3  F16r|t1  F16r|t2  F16r|t3   F1r|t1 
##    0.918   -0.604   -0.145    0.404   -0.362   -0.046    0.415   -0.616 
##   F1r|t2   F1r|t3   F2r|t1   F2r|t2   F2r|t3   F3r|t1   F3r|t2   F3r|t3 
##    0.071    0.658   -0.409    0.120    0.707   -0.818   -0.184    0.310 
##   F5r|t1   F5r|t2   F5r|t3   F6r|t1   F6r|t2   F6r|t3   F7r|t1   F7r|t2 
##   -0.622    0.002    0.546   -1.028   -0.502    0.012   -0.676   -0.199 
##   F7r|t3   F9r|t1   F9r|t2   F9r|t3  F10r|t1  F10r|t2  F10r|t3  F11r|t1 
##    0.326   -0.707   -0.204    0.179   -0.518   -0.007    0.469   -0.867 
##  F11r|t2  F11r|t3  F13r|t1  F13r|t2  F13r|t3  F14r|t1  F14r|t2  F14r|t3 
##   -0.393    0.071   -0.529    0.115    0.575   -0.752   -0.290    0.115 
##  F15r|t1  F15r|t2  F15r|t3  F17r|t1  F17r|t2  F17r|t3  F18r|t1  F18r|t2 
##   -0.357    0.209    0.658   -0.224    0.264    0.658   -1.019   -0.452 
##  F18r|t3  F19r|t1  F19r|t2  F19r|t3  F20r|t1  F20r|t2  F20r|t3   f1~~f1 
##   -0.017   -0.346    0.100    0.490   -0.695   -0.189    0.224    0.107 
##   f2~~f2 
##    0.240
```

```r
#Modification Index
MIPCA2<-modindices(fitPCA2)
MIIPCA2<- MIPCA2[which(MIPCA2$mi>30),]
print(MIIPCA2)
```

```
##    lhs op  rhs     mi mi.scaled   epc sepc.lv sepc.all sepc.nox
## 1 F15r ~~ F19r 53.772    66.451 0.258   0.258    0.258    0.258
## 2 F17r ~~ F18r 33.973    41.984 0.233   0.233    0.233    0.233
```

```r
#Model Plot
semPaths(fitPCA2,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-11-1.png) 


```r
#Marliere - Principal Components Analysis - Two Components Solution Reviewed - CFA Model (removed itens - 4, 8, 13)

PCA2_CFAr <- '
              # latent variable definitions 
               f1 =~ F12r + F16r
               f2 =~ F1r + F2r + F3r + F5r + F6r + F7r + F9r + F10r + F11r + F14r + F15r + F17r + F18r + F19r + F20r

                #factor covariances 
                f1~~f2
                       '
fitPCA2r <- cfa(PCA2_CFAr, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitPCA2r, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  26 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              263.246     393.304
##   Degrees of freedom                               118         118
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.712
##   Shift parameter                                           23.644
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            12752.383    5676.220
##   Degrees of freedom                               136         136
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.988       0.950
##   Tucker-Lewis Index (TLI)                       0.987       0.943
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.049       0.068
##   90 Percent Confidence Interval          0.041  0.057       0.060  0.075
##   P-value RMSEA <= 0.05                          0.567       0.000
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           1.186       1.186
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F12r              1.000                               0.856    0.856
##     F16r              0.832    0.077   10.795    0.000    0.712    0.712
##   f2 =~
##     F1r               1.000                               0.494    0.494
##     F2r               1.020    0.097   10.491    0.000    0.504    0.504
##     F3r               1.197    0.108   11.056    0.000    0.591    0.591
##     F5r               1.195    0.108   11.072    0.000    0.590    0.590
##     F6r               1.561    0.121   12.938    0.000    0.771    0.771
##     F7r               1.201    0.109   11.022    0.000    0.593    0.593
##     F9r               1.368    0.112   12.173    0.000    0.675    0.675
##     F10r              1.394    0.118   11.808    0.000    0.688    0.688
##     F11r              1.258    0.114   11.049    0.000    0.621    0.621
##     F14r              1.513    0.122   12.411    0.000    0.747    0.747
##     F15r              1.304    0.116   11.240    0.000    0.644    0.644
##     F17r              1.191    0.112   10.643    0.000    0.588    0.588
##     F18r              1.679    0.136   12.357    0.000    0.829    0.829
##     F19r              1.497    0.117   12.796    0.000    0.739    0.739
##     F20r              1.380    0.120   11.525    0.000    0.681    0.681
## 
## Covariances:
##   f1 ~~
##     f2                0.250    0.028    8.885    0.000    0.591    0.591
## 
## Intercepts:
##     F12r              0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F1r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F14r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F18r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F20r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
## 
## Thresholds:
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F14r|t1          -0.752    0.061  -12.229    0.000   -0.752   -0.752
##     F14r|t2          -0.290    0.056   -5.154    0.000   -0.290   -0.290
##     F14r|t3           0.115    0.056    2.073    0.038    0.115    0.115
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F20r|t1          -0.695    0.061  -11.475    0.000   -0.695   -0.695
##     F20r|t2          -0.189    0.056   -3.394    0.001   -0.189   -0.189
##     F20r|t3           0.224    0.056    4.011    0.000    0.224    0.224
## 
## Variances:
##     F12r              0.267                               0.267    0.267
##     F16r              0.493                               0.493    0.493
##     F1r               0.756                               0.756    0.756
##     F2r               0.746                               0.746    0.746
##     F3r               0.650                               0.650    0.650
##     F5r               0.652                               0.652    0.652
##     F6r               0.406                               0.406    0.406
##     F7r               0.648                               0.648    0.648
##     F9r               0.544                               0.544    0.544
##     F10r              0.526                               0.526    0.526
##     F11r              0.614                               0.614    0.614
##     F14r              0.442                               0.442    0.442
##     F15r              0.585                               0.585    0.585
##     F17r              0.654                               0.654    0.654
##     F18r              0.313                               0.313    0.313
##     F19r              0.454                               0.454    0.454
##     F20r              0.536                               0.536    0.536
##     f1                0.733    0.075                      1.000    1.000
##     f2                0.244    0.038                      1.000    1.000
## 
## R-Square:
## 
##     F12r              0.733
##     F16r              0.507
##     F1r               0.244
##     F2r               0.254
##     F3r               0.350
##     F5r               0.348
##     F6r               0.594
##     F7r               0.352
##     F9r               0.456
##     F10r              0.474
##     F11r              0.386
##     F14r              0.558
##     F15r              0.415
##     F17r              0.346
##     F18r              0.687
##     F19r              0.546
##     F20r              0.464
```

```r
#Model Fit Measures
fitMeasures(fitPCA2r)
```

```
##                          npar                          fmin 
##                        69.000                         0.257 
##                         chisq                            df 
##                       263.246                       118.000 
##                        pvalue                  chisq.scaled 
##                         0.000                       393.304 
##                     df.scaled                 pvalue.scaled 
##                       118.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.712                     12752.383 
##                   baseline.df               baseline.pvalue 
##                       136.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      5676.220                       136.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.277 
##                           cfi                           tli 
##                         0.988                         0.987 
##                          nnfi                           rfi 
##                         0.987                         0.976 
##                           nfi                          pnfi 
##                         0.979                         0.850 
##                           ifi                           rni 
##                         0.989                         0.988 
##                    cfi.scaled                    tli.scaled 
##                         0.950                         0.943 
##                   nnfi.scaled                    rfi.scaled 
##                         0.943                         0.920 
##                    nfi.scaled                    ifi.scaled 
##                         0.931                         0.931 
##                    rni.scaled                         rmsea 
##                         0.978                         0.049 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.041                         0.057 
##                  rmsea.pvalue                  rmsea.scaled 
##                         0.567                         0.068 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.060                         0.075 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.000                         1.186 
##                         cn_05                         cn_01 
##                       281.760                       305.673 
##                           gfi                          agfi 
##                         0.983                         0.974 
##                          pgfi                           mfi 
##                         0.621                         0.868
```

```r
#Parameters Estimates
EstPCA2r <- parameterEstimates(fitPCA2r, standardized=T, ci=F)
subset(EstPCA2r, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~ F12r 1.000 0.000     NA     NA  0.856   0.856   0.856
## 2   f1 =~ F16r 0.832 0.077 10.795      0  0.712   0.712   0.712
## 3   f2 =~  F1r 1.000 0.000     NA     NA  0.494   0.494   0.494
## 4   f2 =~  F2r 1.020 0.097 10.491      0  0.504   0.504   0.504
## 5   f2 =~  F3r 1.197 0.108 11.056      0  0.591   0.591   0.591
## 6   f2 =~  F5r 1.195 0.108 11.072      0  0.590   0.590   0.590
## 7   f2 =~  F6r 1.561 0.121 12.938      0  0.771   0.771   0.771
## 8   f2 =~  F7r 1.201 0.109 11.022      0  0.593   0.593   0.593
## 9   f2 =~  F9r 1.368 0.112 12.173      0  0.675   0.675   0.675
## 10  f2 =~ F10r 1.394 0.118 11.808      0  0.688   0.688   0.688
## 11  f2 =~ F11r 1.258 0.114 11.049      0  0.621   0.621   0.621
## 12  f2 =~ F14r 1.513 0.122 12.411      0  0.747   0.747   0.747
## 13  f2 =~ F15r 1.304 0.116 11.240      0  0.644   0.644   0.644
## 14  f2 =~ F17r 1.191 0.112 10.643      0  0.588   0.588   0.588
## 15  f2 =~ F18r 1.679 0.136 12.357      0  0.829   0.829   0.829
## 16  f2 =~ F19r 1.497 0.117 12.796      0  0.739   0.739   0.739
## 17  f2 =~ F20r 1.380 0.120 11.525      0  0.681   0.681   0.681
```

```r
#Model Coefficients
coef(fitPCA2r)
```

```
## f1=~F16r  f2=~F2r  f2=~F3r  f2=~F5r  f2=~F6r  f2=~F7r  f2=~F9r f2=~F10r 
##    0.832    1.020    1.197    1.195    1.561    1.201    1.368    1.394 
## f2=~F11r f2=~F14r f2=~F15r f2=~F17r f2=~F18r f2=~F19r f2=~F20r   f1~~f2 
##    1.258    1.513    1.304    1.191    1.679    1.497    1.380    0.250 
##  F12r|t1  F12r|t2  F12r|t3  F16r|t1  F16r|t2  F16r|t3   F1r|t1   F1r|t2 
##   -0.604   -0.145    0.404   -0.362   -0.046    0.415   -0.616    0.071 
##   F1r|t3   F2r|t1   F2r|t2   F2r|t3   F3r|t1   F3r|t2   F3r|t3   F5r|t1 
##    0.658   -0.409    0.120    0.707   -0.818   -0.184    0.310   -0.622 
##   F5r|t2   F5r|t3   F6r|t1   F6r|t2   F6r|t3   F7r|t1   F7r|t2   F7r|t3 
##    0.002    0.546   -1.028   -0.502    0.012   -0.676   -0.199    0.326 
##   F9r|t1   F9r|t2   F9r|t3  F10r|t1  F10r|t2  F10r|t3  F11r|t1  F11r|t2 
##   -0.707   -0.204    0.179   -0.518   -0.007    0.469   -0.867   -0.393 
##  F11r|t3  F14r|t1  F14r|t2  F14r|t3  F15r|t1  F15r|t2  F15r|t3  F17r|t1 
##    0.071   -0.752   -0.290    0.115   -0.357    0.209    0.658   -0.224 
##  F17r|t2  F17r|t3  F18r|t1  F18r|t2  F18r|t3  F19r|t1  F19r|t2  F19r|t3 
##    0.264    0.658   -1.019   -0.452   -0.017   -0.346    0.100    0.490 
##  F20r|t1  F20r|t2  F20r|t3   f1~~f1   f2~~f2 
##   -0.695   -0.189    0.224    0.733    0.244
```

```r
#Modification Index
MIPCA2r<-modindices(fitPCA2r)
MIIPCA2r<- MIPCA2r[which(MIPCA2r$mi>30),]
print(MIIPCA2r)
```

```
##    lhs op  rhs     mi mi.scaled   epc sepc.lv sepc.all sepc.nox
## 1 F15r ~~ F19r 54.117    75.993 0.259   0.259    0.259    0.259
## 2 F17r ~~ F18r 33.720    47.351 0.233   0.233    0.233    0.233
```

```r
#Model Plot
semPaths(fitPCA2r,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-12-1.png) 


```r
#Marliere - Final Solution - Principal Components Analysis - Two Components Solution Reviewed - CFA Model (removed itens - F4r, F8r, F13r) - and included error covariance

PCA2_CFAr <- '
              # latent variable definitions 
               f1 =~ F12r + F16r
               f2 =~ F1r + F2r + F3r + F5r + F6r + F7r + F9r + F10r + F11r + F14r + F15r + F17r + F18r + F19r + F20r

                #factor covariances 
                f1~~f2

                #error covariance
                F15r ~~ F19r
                F17r ~~ F18r
                       '
fitPCA2r <- cfa(PCA2_CFAr, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitPCA2r, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  28 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              176.935     274.014
##   Degrees of freedom                               116         116
##   P-value (Chi-square)                           0.000       0.000
##   Scaling correction factor                                  0.705
##   Shift parameter                                           23.039
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic            12752.383    5676.220
##   Degrees of freedom                               136         136
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.995       0.971
##   Tucker-Lewis Index (TLI)                       0.994       0.967
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.032       0.052
##   90 Percent Confidence Interval          0.022  0.041       0.044  0.060
##   P-value RMSEA <= 0.05                          1.000       0.360
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           0.973       0.973
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F12r              1.000                               0.857    0.857
##     F16r              0.831    0.077   10.830    0.000    0.712    0.712
##   f2 =~
##     F1r               1.000                               0.499    0.499
##     F2r               1.020    0.097   10.528    0.000    0.509    0.509
##     F3r               1.198    0.108   11.086    0.000    0.597    0.597
##     F5r               1.195    0.108   11.105    0.000    0.596    0.596
##     F6r               1.563    0.120   12.973    0.000    0.779    0.779
##     F7r               1.201    0.109   11.045    0.000    0.599    0.599
##     F9r               1.368    0.112   12.193    0.000    0.682    0.682
##     F10r              1.394    0.118   11.832    0.000    0.695    0.695
##     F11r              1.258    0.114   11.072    0.000    0.627    0.627
##     F14r              1.514    0.122   12.425    0.000    0.755    0.755
##     F15r              1.184    0.110   10.733    0.000    0.591    0.591
##     F17r              1.089    0.110    9.897    0.000    0.543    0.543
##     F18r              1.619    0.132   12.298    0.000    0.807    0.807
##     F19r              1.396    0.111   12.576    0.000    0.696    0.696
##     F20r              1.380    0.120   11.527    0.000    0.688    0.688
## 
## Covariances:
##   f1 ~~
##     f2                0.255    0.028    8.948    0.000    0.597    0.597
##   F15r ~~
##     F19r              0.253    0.033    7.793    0.000    0.253    0.437
##   F17r ~~
##     F18r              0.227    0.036    6.297    0.000    0.227    0.458
## 
## Intercepts:
##     F12r              0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F1r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F14r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F18r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F20r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
## 
## Thresholds:
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F14r|t1          -0.752    0.061  -12.229    0.000   -0.752   -0.752
##     F14r|t2          -0.290    0.056   -5.154    0.000   -0.290   -0.290
##     F14r|t3           0.115    0.056    2.073    0.038    0.115    0.115
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F20r|t1          -0.695    0.061  -11.475    0.000   -0.695   -0.695
##     F20r|t2          -0.189    0.056   -3.394    0.001   -0.189   -0.189
##     F20r|t3           0.224    0.056    4.011    0.000    0.224    0.224
## 
## Variances:
##     F12r              0.266                               0.266    0.266
##     F16r              0.493                               0.493    0.493
##     F1r               0.751                               0.751    0.751
##     F2r               0.741                               0.741    0.741
##     F3r               0.643                               0.643    0.643
##     F5r               0.645                               0.645    0.645
##     F6r               0.393                               0.393    0.393
##     F7r               0.642                               0.642    0.642
##     F9r               0.535                               0.535    0.535
##     F10r              0.517                               0.517    0.517
##     F11r              0.607                               0.607    0.607
##     F14r              0.430                               0.430    0.430
##     F15r              0.651                               0.651    0.651
##     F17r              0.705                               0.705    0.705
##     F18r              0.348                               0.348    0.348
##     F19r              0.515                               0.515    0.515
##     F20r              0.527                               0.527    0.527
##     f1                0.734    0.075                      1.000    1.000
##     f2                0.249    0.038                      1.000    1.000
## 
## R-Square:
## 
##     F12r              0.734
##     F16r              0.507
##     F1r               0.249
##     F2r               0.259
##     F3r               0.357
##     F5r               0.355
##     F6r               0.607
##     F7r               0.358
##     F9r               0.465
##     F10r              0.483
##     F11r              0.393
##     F14r              0.570
##     F15r              0.349
##     F17r              0.295
##     F18r              0.652
##     F19r              0.485
##     F20r              0.473
```

```r
#Model Fit Measures
fitMeasures(fitPCA2r)
```

```
##                          npar                          fmin 
##                        71.000                         0.172 
##                         chisq                            df 
##                       176.935                       116.000 
##                        pvalue                  chisq.scaled 
##                         0.000                       274.014 
##                     df.scaled                 pvalue.scaled 
##                       116.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.705                     12752.383 
##                   baseline.df               baseline.pvalue 
##                       136.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      5676.220                       136.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.277 
##                           cfi                           tli 
##                         0.995                         0.994 
##                          nnfi                           rfi 
##                         0.994                         0.984 
##                           nfi                          pnfi 
##                         0.986                         0.841 
##                           ifi                           rni 
##                         0.995                         0.995 
##                    cfi.scaled                    tli.scaled 
##                         0.971                         0.967 
##                   nnfi.scaled                    rfi.scaled 
##                         0.967                         0.943 
##                    nfi.scaled                    ifi.scaled 
##                         0.952                         0.952 
##                    rni.scaled                         rmsea 
##                         0.987                         0.032 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.022                         0.041 
##                  rmsea.pvalue                  rmsea.scaled 
##                         1.000                         0.052 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.044                         0.060 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.360                         0.973 
##                         cn_05                         cn_01 
##                       412.307                       447.627 
##                           gfi                          agfi 
##                         0.989                         0.982 
##                          pgfi                           mfi 
##                         0.613                         0.942
```

```r
#Parameters Estimates
EstPCA2r <- parameterEstimates(fitPCA2r, standardized=T, ci=F)
subset(EstPCA2r, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~ F12r 1.000 0.000     NA     NA  0.857   0.857   0.857
## 2   f1 =~ F16r 0.831 0.077 10.830      0  0.712   0.712   0.712
## 3   f2 =~  F1r 1.000 0.000     NA     NA  0.499   0.499   0.499
## 4   f2 =~  F2r 1.020 0.097 10.528      0  0.509   0.509   0.509
## 5   f2 =~  F3r 1.198 0.108 11.086      0  0.597   0.597   0.597
## 6   f2 =~  F5r 1.195 0.108 11.105      0  0.596   0.596   0.596
## 7   f2 =~  F6r 1.563 0.120 12.973      0  0.779   0.779   0.779
## 8   f2 =~  F7r 1.201 0.109 11.045      0  0.599   0.599   0.599
## 9   f2 =~  F9r 1.368 0.112 12.193      0  0.682   0.682   0.682
## 10  f2 =~ F10r 1.394 0.118 11.832      0  0.695   0.695   0.695
## 11  f2 =~ F11r 1.258 0.114 11.072      0  0.627   0.627   0.627
## 12  f2 =~ F14r 1.514 0.122 12.425      0  0.755   0.755   0.755
## 13  f2 =~ F15r 1.184 0.110 10.733      0  0.591   0.591   0.591
## 14  f2 =~ F17r 1.089 0.110  9.897      0  0.543   0.543   0.543
## 15  f2 =~ F18r 1.619 0.132 12.298      0  0.807   0.807   0.807
## 16  f2 =~ F19r 1.396 0.111 12.576      0  0.696   0.696   0.696
## 17  f2 =~ F20r 1.380 0.120 11.527      0  0.688   0.688   0.688
```

```r
#Model Coefficients
coef(fitPCA2r)
```

```
##   f1=~F16r    f2=~F2r    f2=~F3r    f2=~F5r    f2=~F6r    f2=~F7r 
##      0.831      1.020      1.198      1.195      1.563      1.201 
##    f2=~F9r   f2=~F10r   f2=~F11r   f2=~F14r   f2=~F15r   f2=~F17r 
##      1.368      1.394      1.258      1.514      1.184      1.089 
##   f2=~F18r   f2=~F19r   f2=~F20r     f1~~f2 F15r~~F19r F17r~~F18r 
##      1.619      1.396      1.380      0.255      0.253      0.227 
##    F12r|t1    F12r|t2    F12r|t3    F16r|t1    F16r|t2    F16r|t3 
##     -0.604     -0.145      0.404     -0.362     -0.046      0.415 
##     F1r|t1     F1r|t2     F1r|t3     F2r|t1     F2r|t2     F2r|t3 
##     -0.616      0.071      0.658     -0.409      0.120      0.707 
##     F3r|t1     F3r|t2     F3r|t3     F5r|t1     F5r|t2     F5r|t3 
##     -0.818     -0.184      0.310     -0.622      0.002      0.546 
##     F6r|t1     F6r|t2     F6r|t3     F7r|t1     F7r|t2     F7r|t3 
##     -1.028     -0.502      0.012     -0.676     -0.199      0.326 
##     F9r|t1     F9r|t2     F9r|t3    F10r|t1    F10r|t2    F10r|t3 
##     -0.707     -0.204      0.179     -0.518     -0.007      0.469 
##    F11r|t1    F11r|t2    F11r|t3    F14r|t1    F14r|t2    F14r|t3 
##     -0.867     -0.393      0.071     -0.752     -0.290      0.115 
##    F15r|t1    F15r|t2    F15r|t3    F17r|t1    F17r|t2    F17r|t3 
##     -0.357      0.209      0.658     -0.224      0.264      0.658 
##    F18r|t1    F18r|t2    F18r|t3    F19r|t1    F19r|t2    F19r|t3 
##     -1.019     -0.452     -0.017     -0.346      0.100      0.490 
##    F20r|t1    F20r|t2    F20r|t3     f1~~f1     f2~~f2 
##     -0.695     -0.189      0.224      0.734      0.249
```

```r
#Modification Index
MIPCA2r<-modindices(fitPCA2r)
MIIPCA2r<- MIPCA2r[which(MIPCA2r$mi>30),]
print(MIIPCA2r)
```

```
## [1] lhs       op        rhs       mi        mi.scaled epc       sepc.lv  
## [8] sepc.all  sepc.nox 
## <0 rows> (or 0-length row.names)
```

```r
#Reability Alpha

#Component 1
C1_PCA2final <- fullScale[, c("F1r","F2r","F3r","F5r","F6r","F7r","F9r","F10r","F11r","F14r","F15r","F17r","F18r","F19r","F20r")]
alpha(C1_PCA2final, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = C1_PCA2final, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.88      0.88    0.89      0.34 7.6 0.012  1.6 0.74
## 
##  lower alpha upper     95% confidence boundaries
## 0.86 0.88 0.91 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F1r       0.88      0.88    0.88      0.34 7.4    0.012
## F2r       0.88      0.88    0.88      0.34 7.4    0.012
## F3r       0.88      0.88    0.88      0.34 7.2    0.012
## F5r       0.88      0.88    0.88      0.34 7.2    0.012
## F6r       0.87      0.87    0.87      0.33 6.8    0.013
## F7r       0.88      0.88    0.88      0.34 7.2    0.012
## F9r       0.87      0.87    0.88      0.33 7.0    0.013
## F10r      0.87      0.87    0.88      0.33 6.9    0.013
## F11r      0.88      0.88    0.88      0.34 7.1    0.013
## F14r      0.87      0.87    0.88      0.33 6.9    0.013
## F15r      0.88      0.88    0.88      0.34 7.1    0.013
## F17r      0.88      0.88    0.88      0.34 7.3    0.012
## F18r      0.87      0.87    0.87      0.32 6.7    0.013
## F19r      0.87      0.87    0.87      0.33 6.9    0.013
## F20r      0.88      0.88    0.88      0.33 7.0    0.013
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F1r  513  0.52  0.52  0.47   0.44  1.5 1.1
## F2r  513  0.52  0.53  0.47   0.44  1.4 1.2
## F3r  513  0.58  0.59  0.54   0.51  1.7 1.2
## F5r  513  0.58  0.59  0.54   0.51  1.5 1.2
## F6r  513  0.69  0.69  0.68   0.63  2.0 1.1
## F7r  513  0.59  0.59  0.54   0.51  1.7 1.2
## F9r  513  0.65  0.64  0.61   0.57  1.8 1.2
## F10r 513  0.66  0.66  0.63   0.59  1.5 1.2
## F11r 513  0.59  0.59  0.55   0.52  1.9 1.2
## F14r 513  0.67  0.66  0.64   0.60  1.8 1.2
## F15r 513  0.60  0.60  0.56   0.52  1.3 1.2
## F17r 513  0.56  0.56  0.52   0.48  1.2 1.2
## F18r 513  0.72  0.73  0.72   0.67  2.0 1.1
## F19r 513  0.67  0.67  0.65   0.60  1.4 1.3
## F20r 513  0.63  0.63  0.59   0.56  1.7 1.2
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F1r  0.27 0.26 0.22 0.26    0
## F2r  0.34 0.21 0.21 0.24    0
## F3r  0.21 0.22 0.19 0.38    0
## F5r  0.27 0.23 0.21 0.29    0
## F6r  0.15 0.16 0.20 0.50    0
## F7r  0.25 0.17 0.21 0.37    0
## F9r  0.24 0.18 0.15 0.43    0
## F10r 0.30 0.19 0.18 0.32    0
## F11r 0.19 0.15 0.18 0.47    0
## F14r 0.23 0.16 0.16 0.45    0
## F15r 0.36 0.22 0.16 0.26    0
## F17r 0.41 0.19 0.14 0.26    0
## F18r 0.15 0.17 0.17 0.51    0
## F19r 0.36 0.18 0.15 0.31    0
## F20r 0.24 0.18 0.16 0.41    0
```

```r
#Component 2
C2_PCA2final <- fullScale[, c("F12r","F16r")]
alpha(C2_PCA2final, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = C2_PCA2final, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
##       0.67      0.67     0.5       0.5   2 0.068  1.6 1.1
## 
##  lower alpha upper     95% confidence boundaries
## 0.54 0.67 0.8 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F12r       0.5       0.5    0.25       0.5  NA       NA
## F16r       0.5       0.5    0.25       0.5  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F12r 513  0.86  0.87  0.62    0.5  1.6 1.2
## F16r 513  0.88  0.87  0.62    0.5  1.5 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F12r 0.27 0.17 0.21 0.34    0
## F16r 0.36 0.12 0.18 0.34    0
```

```r
#Model Plot
semPaths(fitPCA2r,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-13-1.png) 



```r
#Marliere - Factorial Analysis -4 Factos Solution - CFA Model 

FA4_CFA <- '
              # latent variable definitions 
              f1 =~ F1r  + F2r  + F3r  + F5r  + F6r  + F7r  + F9r  + F10r  + F11r 
              f2 =~ F4r  + F8r  + F12r  + F16r 
              f3 =~ F15r  + F19r
              f4 =~ F17r + F18r

             # variances and covariances 
              f1 ~~ f2
              f1 ~~ f3
              f1 ~~ f4
              f2 ~~ f3
              f2 ~~ f4
              f3 ~~ f4
                       '
fitFA4 <- cfa(FA4_CFA, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitFA4, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  46 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              168.095     246.715
##   Degrees of freedom                               113         113
##   P-value (Chi-square)                           0.001       0.000
##   Scaling correction factor                                  0.751
##   Shift parameter                                           22.948
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic             9293.924    4655.600
##   Degrees of freedom                               136         136
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.994       0.970
##   Tucker-Lewis Index (TLI)                       0.993       0.964
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.031       0.048
##   90 Percent Confidence Interval          0.020  0.040       0.040  0.056
##   P-value RMSEA <= 0.05                          1.000       0.640
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           0.948       0.948
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F1r               1.000                               0.502    0.502
##     F2r               1.070    0.102   10.489    0.000    0.537    0.537
##     F3r               1.221    0.113   10.759    0.000    0.612    0.612
##     F5r               1.205    0.111   10.829    0.000    0.604    0.604
##     F6r               1.587    0.126   12.631    0.000    0.796    0.796
##     F7r               1.226    0.114   10.783    0.000    0.615    0.615
##     F9r               1.376    0.117   11.793    0.000    0.690    0.690
##     F10r              1.433    0.122   11.716    0.000    0.719    0.719
##     F11r              1.288    0.116   11.078    0.000    0.646    0.646
##   f2 =~
##     F4r               1.000                               0.337    0.337
##     F8r               1.090    0.226    4.834    0.000    0.367    0.367
##     F12r              2.677    0.482    5.550    0.000    0.901    0.901
##     F16r              2.136    0.375    5.695    0.000    0.719    0.719
##   f3 =~
##     F15r              1.000                               0.750    0.750
##     F19r              1.182    0.069   17.122    0.000    0.886    0.886
##   f4 =~
##     F17r              1.000                               0.672    0.672
##     F18r              1.475    0.103   14.339    0.000    0.990    0.990
## 
## Covariances:
##   f1 ~~
##     f2                0.087    0.018    4.734    0.000    0.513    0.513
##     f3                0.277    0.029    9.611    0.000    0.737    0.737
##     f4                0.257    0.029    8.856    0.000    0.763    0.763
##   f2 ~~
##     f3                0.101    0.024    4.255    0.000    0.401    0.401
##     f4                0.107    0.022    4.785    0.000    0.473    0.473
##   f3 ~~
##     f4                0.336    0.034    9.805    0.000    0.668    0.668
## 
## Intercepts:
##     F1r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F4r               0.000                               0.000    0.000
##     F8r               0.000                               0.000    0.000
##     F12r              0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F18r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
##     f3                0.000                               0.000    0.000
##     f4                0.000                               0.000    0.000
## 
## Thresholds:
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F4r|t1           -0.431    0.057   -7.517    0.000   -0.431   -0.431
##     F4r|t2            0.110    0.056    1.984    0.047    0.110    0.110
##     F4r|t3            0.701    0.061   11.559    0.000    0.701    0.701
##     F8r|t1           -0.140    0.056   -2.513    0.012   -0.140   -0.140
##     F8r|t2            0.321    0.056    5.680    0.000    0.321    0.321
##     F8r|t3            0.918    0.065   14.173    0.000    0.918    0.918
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
## 
## Variances:
##     F1r               0.748                               0.748    0.748
##     F2r               0.712                               0.712    0.712
##     F3r               0.625                               0.625    0.625
##     F5r               0.635                               0.635    0.635
##     F6r               0.366                               0.366    0.366
##     F7r               0.622                               0.622    0.622
##     F9r               0.524                               0.524    0.524
##     F10r              0.483                               0.483    0.483
##     F11r              0.582                               0.582    0.582
##     F4r               0.887                               0.887    0.887
##     F8r               0.865                               0.865    0.865
##     F12r              0.188                               0.188    0.188
##     F16r              0.483                               0.483    0.483
##     F15r              0.438                               0.438    0.438
##     F19r              0.215                               0.215    0.215
##     F17r              0.549                               0.549    0.549
##     F18r              0.019                               0.019    0.019
##     f1                0.252    0.039                      1.000    1.000
##     f2                0.113    0.038                      1.000    1.000
##     f3                0.562    0.046                      1.000    1.000
##     f4                0.451    0.045                      1.000    1.000
## 
## R-Square:
## 
##     F1r               0.252
##     F2r               0.288
##     F3r               0.375
##     F5r               0.365
##     F6r               0.634
##     F7r               0.378
##     F9r               0.476
##     F10r              0.517
##     F11r              0.418
##     F4r               0.113
##     F8r               0.135
##     F12r              0.812
##     F16r              0.517
##     F15r              0.562
##     F19r              0.785
##     F17r              0.451
##     F18r              0.981
```

```r
#Model Fit Measures
fitMeasures(fitFA4)
```

```
##                          npar                          fmin 
##                        74.000                         0.164 
##                         chisq                            df 
##                       168.095                       113.000 
##                        pvalue                  chisq.scaled 
##                         0.001                       246.715 
##                     df.scaled                 pvalue.scaled 
##                       113.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.751                      9293.924 
##                   baseline.df               baseline.pvalue 
##                       136.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      4655.600                       136.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.026 
##                           cfi                           tli 
##                         0.994                         0.993 
##                          nnfi                           rfi 
##                         0.993                         0.978 
##                           nfi                          pnfi 
##                         0.982                         0.816 
##                           ifi                           rni 
##                         0.994                         0.994 
##                    cfi.scaled                    tli.scaled 
##                         0.970                         0.964 
##                   nnfi.scaled                    rfi.scaled 
##                         0.964                         0.936 
##                    nfi.scaled                    ifi.scaled 
##                         0.947                         0.947 
##                    rni.scaled                         rmsea 
##                         0.985                         0.031 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.020                         0.040 
##                  rmsea.pvalue                  rmsea.scaled 
##                         1.000                         0.048 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.040                         0.056 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.640                         0.948 
##                         cn_05                         cn_01 
##                       423.804                       460.570 
##                           gfi                          agfi 
##                         0.987                         0.978 
##                          pgfi                           mfi 
##                         0.596                         0.948
```

```r
#Parameters Estimates
EstFA4 <- parameterEstimates(fitFA4, standardized=T, ci=F)
subset(EstFA4, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~  F1r 1.000 0.000     NA     NA  0.502   0.502   0.502
## 2   f1 =~  F2r 1.070 0.102 10.489      0  0.537   0.537   0.537
## 3   f1 =~  F3r 1.221 0.113 10.759      0  0.612   0.612   0.612
## 4   f1 =~  F5r 1.205 0.111 10.829      0  0.604   0.604   0.604
## 5   f1 =~  F6r 1.587 0.126 12.631      0  0.796   0.796   0.796
## 6   f1 =~  F7r 1.226 0.114 10.783      0  0.615   0.615   0.615
## 7   f1 =~  F9r 1.376 0.117 11.793      0  0.690   0.690   0.690
## 8   f1 =~ F10r 1.433 0.122 11.716      0  0.719   0.719   0.719
## 9   f1 =~ F11r 1.288 0.116 11.078      0  0.646   0.646   0.646
## 10  f2 =~  F4r 1.000 0.000     NA     NA  0.337   0.337   0.337
## 11  f2 =~  F8r 1.090 0.226  4.834      0  0.367   0.367   0.367
## 12  f2 =~ F12r 2.677 0.482  5.550      0  0.901   0.901   0.901
## 13  f2 =~ F16r 2.136 0.375  5.695      0  0.719   0.719   0.719
## 14  f3 =~ F15r 1.000 0.000     NA     NA  0.750   0.750   0.750
## 15  f3 =~ F19r 1.182 0.069 17.122      0  0.886   0.886   0.886
## 16  f4 =~ F17r 1.000 0.000     NA     NA  0.672   0.672   0.672
## 17  f4 =~ F18r 1.475 0.103 14.339      0  0.990   0.990   0.990
```

```r
#Parameters Table
parTable(fitFA4)
```

```
##      id  lhs op  rhs user group free ustart exo label eq.id unco plabel
## 1     1   f1 =~  F1r    1     1    0      1   0           0    0   .p1.
## 2     2   f1 =~  F2r    1     1    1     NA   0           0    1   .p2.
## 3     3   f1 =~  F3r    1     1    2     NA   0           0    2   .p3.
## 4     4   f1 =~  F5r    1     1    3     NA   0           0    3   .p4.
## 5     5   f1 =~  F6r    1     1    4     NA   0           0    4   .p5.
## 6     6   f1 =~  F7r    1     1    5     NA   0           0    5   .p6.
## 7     7   f1 =~  F9r    1     1    6     NA   0           0    6   .p7.
## 8     8   f1 =~ F10r    1     1    7     NA   0           0    7   .p8.
## 9     9   f1 =~ F11r    1     1    8     NA   0           0    8   .p9.
## 10   10   f2 =~  F4r    1     1    0      1   0           0    0  .p10.
## 11   11   f2 =~  F8r    1     1    9     NA   0           0    9  .p11.
## 12   12   f2 =~ F12r    1     1   10     NA   0           0   10  .p12.
## 13   13   f2 =~ F16r    1     1   11     NA   0           0   11  .p13.
## 14   14   f3 =~ F15r    1     1    0      1   0           0    0  .p14.
## 15   15   f3 =~ F19r    1     1   12     NA   0           0   12  .p15.
## 16   16   f4 =~ F17r    1     1    0      1   0           0    0  .p16.
## 17   17   f4 =~ F18r    1     1   13     NA   0           0   13  .p17.
## 18   18   f1 ~~   f2    1     1   14     NA   0           0   14  .p18.
## 19   19   f1 ~~   f3    1     1   15     NA   0           0   15  .p19.
## 20   20   f1 ~~   f4    1     1   16     NA   0           0   16  .p20.
## 21   21   f2 ~~   f3    1     1   17     NA   0           0   17  .p21.
## 22   22   f2 ~~   f4    1     1   18     NA   0           0   18  .p22.
## 23   23   f3 ~~   f4    1     1   19     NA   0           0   19  .p23.
## 24   24  F1r  |   t1    0     1   20     NA   0           0   20  .p24.
## 25   25  F1r  |   t2    0     1   21     NA   0           0   21  .p25.
## 26   26  F1r  |   t3    0     1   22     NA   0           0   22  .p26.
## 27   27  F2r  |   t1    0     1   23     NA   0           0   23  .p27.
## 28   28  F2r  |   t2    0     1   24     NA   0           0   24  .p28.
## 29   29  F2r  |   t3    0     1   25     NA   0           0   25  .p29.
## 30   30  F3r  |   t1    0     1   26     NA   0           0   26  .p30.
## 31   31  F3r  |   t2    0     1   27     NA   0           0   27  .p31.
## 32   32  F3r  |   t3    0     1   28     NA   0           0   28  .p32.
## 33   33  F5r  |   t1    0     1   29     NA   0           0   29  .p33.
## 34   34  F5r  |   t2    0     1   30     NA   0           0   30  .p34.
## 35   35  F5r  |   t3    0     1   31     NA   0           0   31  .p35.
## 36   36  F6r  |   t1    0     1   32     NA   0           0   32  .p36.
## 37   37  F6r  |   t2    0     1   33     NA   0           0   33  .p37.
## 38   38  F6r  |   t3    0     1   34     NA   0           0   34  .p38.
## 39   39  F7r  |   t1    0     1   35     NA   0           0   35  .p39.
## 40   40  F7r  |   t2    0     1   36     NA   0           0   36  .p40.
## 41   41  F7r  |   t3    0     1   37     NA   0           0   37  .p41.
## 42   42  F9r  |   t1    0     1   38     NA   0           0   38  .p42.
## 43   43  F9r  |   t2    0     1   39     NA   0           0   39  .p43.
## 44   44  F9r  |   t3    0     1   40     NA   0           0   40  .p44.
## 45   45 F10r  |   t1    0     1   41     NA   0           0   41  .p45.
## 46   46 F10r  |   t2    0     1   42     NA   0           0   42  .p46.
## 47   47 F10r  |   t3    0     1   43     NA   0           0   43  .p47.
## 48   48 F11r  |   t1    0     1   44     NA   0           0   44  .p48.
## 49   49 F11r  |   t2    0     1   45     NA   0           0   45  .p49.
## 50   50 F11r  |   t3    0     1   46     NA   0           0   46  .p50.
## 51   51  F4r  |   t1    0     1   47     NA   0           0   47  .p51.
## 52   52  F4r  |   t2    0     1   48     NA   0           0   48  .p52.
## 53   53  F4r  |   t3    0     1   49     NA   0           0   49  .p53.
## 54   54  F8r  |   t1    0     1   50     NA   0           0   50  .p54.
## 55   55  F8r  |   t2    0     1   51     NA   0           0   51  .p55.
## 56   56  F8r  |   t3    0     1   52     NA   0           0   52  .p56.
## 57   57 F12r  |   t1    0     1   53     NA   0           0   53  .p57.
## 58   58 F12r  |   t2    0     1   54     NA   0           0   54  .p58.
## 59   59 F12r  |   t3    0     1   55     NA   0           0   55  .p59.
## 60   60 F16r  |   t1    0     1   56     NA   0           0   56  .p60.
## 61   61 F16r  |   t2    0     1   57     NA   0           0   57  .p61.
## 62   62 F16r  |   t3    0     1   58     NA   0           0   58  .p62.
## 63   63 F15r  |   t1    0     1   59     NA   0           0   59  .p63.
## 64   64 F15r  |   t2    0     1   60     NA   0           0   60  .p64.
## 65   65 F15r  |   t3    0     1   61     NA   0           0   61  .p65.
## 66   66 F19r  |   t1    0     1   62     NA   0           0   62  .p66.
## 67   67 F19r  |   t2    0     1   63     NA   0           0   63  .p67.
## 68   68 F19r  |   t3    0     1   64     NA   0           0   64  .p68.
## 69   69 F17r  |   t1    0     1   65     NA   0           0   65  .p69.
## 70   70 F17r  |   t2    0     1   66     NA   0           0   66  .p70.
## 71   71 F17r  |   t3    0     1   67     NA   0           0   67  .p71.
## 72   72 F18r  |   t1    0     1   68     NA   0           0   68  .p72.
## 73   73 F18r  |   t2    0     1   69     NA   0           0   69  .p73.
## 74   74 F18r  |   t3    0     1   70     NA   0           0   70  .p74.
## 75   75  F1r ~~  F1r    0     1    0      1   0           0    0  .p75.
## 76   76  F2r ~~  F2r    0     1    0      1   0           0    0  .p76.
## 77   77  F3r ~~  F3r    0     1    0      1   0           0    0  .p77.
## 78   78  F5r ~~  F5r    0     1    0      1   0           0    0  .p78.
## 79   79  F6r ~~  F6r    0     1    0      1   0           0    0  .p79.
## 80   80  F7r ~~  F7r    0     1    0      1   0           0    0  .p80.
## 81   81  F9r ~~  F9r    0     1    0      1   0           0    0  .p81.
## 82   82 F10r ~~ F10r    0     1    0      1   0           0    0  .p82.
## 83   83 F11r ~~ F11r    0     1    0      1   0           0    0  .p83.
## 84   84  F4r ~~  F4r    0     1    0      1   0           0    0  .p84.
## 85   85  F8r ~~  F8r    0     1    0      1   0           0    0  .p85.
## 86   86 F12r ~~ F12r    0     1    0      1   0           0    0  .p86.
## 87   87 F16r ~~ F16r    0     1    0      1   0           0    0  .p87.
## 88   88 F15r ~~ F15r    0     1    0      1   0           0    0  .p88.
## 89   89 F19r ~~ F19r    0     1    0      1   0           0    0  .p89.
## 90   90 F17r ~~ F17r    0     1    0      1   0           0    0  .p90.
## 91   91 F18r ~~ F18r    0     1    0      1   0           0    0  .p91.
## 92   92   f1 ~~   f1    0     1   71     NA   0           0   71  .p92.
## 93   93   f2 ~~   f2    0     1   72     NA   0           0   72  .p93.
## 94   94   f3 ~~   f3    0     1   73     NA   0           0   73  .p94.
## 95   95   f4 ~~   f4    0     1   74     NA   0           0   74  .p95.
## 96   96  F1r ~1         0     1    0      0   0           0    0  .p96.
## 97   97  F2r ~1         0     1    0      0   0           0    0  .p97.
## 98   98  F3r ~1         0     1    0      0   0           0    0  .p98.
## 99   99  F5r ~1         0     1    0      0   0           0    0  .p99.
## 100 100  F6r ~1         0     1    0      0   0           0    0 .p100.
## 101 101  F7r ~1         0     1    0      0   0           0    0 .p101.
## 102 102  F9r ~1         0     1    0      0   0           0    0 .p102.
## 103 103 F10r ~1         0     1    0      0   0           0    0 .p103.
## 104 104 F11r ~1         0     1    0      0   0           0    0 .p104.
## 105 105  F4r ~1         0     1    0      0   0           0    0 .p105.
## 106 106  F8r ~1         0     1    0      0   0           0    0 .p106.
## 107 107 F12r ~1         0     1    0      0   0           0    0 .p107.
## 108 108 F16r ~1         0     1    0      0   0           0    0 .p108.
## 109 109 F15r ~1         0     1    0      0   0           0    0 .p109.
## 110 110 F19r ~1         0     1    0      0   0           0    0 .p110.
## 111 111 F17r ~1         0     1    0      0   0           0    0 .p111.
## 112 112 F18r ~1         0     1    0      0   0           0    0 .p112.
## 113 113   f1 ~1         0     1    0      0   0           0    0 .p113.
## 114 114   f2 ~1         0     1    0      0   0           0    0 .p114.
## 115 115   f3 ~1         0     1    0      0   0           0    0 .p115.
## 116 116   f4 ~1         0     1    0      0   0           0    0 .p116.
##      start
## 1    1.000
## 2    0.917
## 3    1.118
## 4    1.060
## 5    1.266
## 6    1.069
## 7    1.156
## 8    1.191
## 9    1.072
## 10   1.000
## 11   0.912
## 12   1.574
## 13   1.352
## 14   1.000
## 15   0.664
## 16   1.000
## 17   0.665
## 18   0.000
## 19   0.000
## 20   0.000
## 21   0.000
## 22   0.000
## 23   0.000
## 24  -0.616
## 25   0.071
## 26   0.658
## 27  -0.409
## 28   0.120
## 29   0.707
## 30  -0.818
## 31  -0.184
## 32   0.310
## 33  -0.622
## 34   0.002
## 35   0.546
## 36  -1.028
## 37  -0.502
## 38   0.012
## 39  -0.676
## 40  -0.199
## 41   0.326
## 42  -0.707
## 43  -0.204
## 44   0.179
## 45  -0.518
## 46  -0.007
## 47   0.469
## 48  -0.867
## 49  -0.393
## 50   0.071
## 51  -0.431
## 52   0.110
## 53   0.701
## 54  -0.140
## 55   0.321
## 56   0.918
## 57  -0.604
## 58  -0.145
## 59   0.404
## 60  -0.362
## 61  -0.046
## 62   0.415
## 63  -0.357
## 64   0.209
## 65   0.658
## 66  -0.346
## 67   0.100
## 68   0.490
## 69  -0.224
## 70   0.264
## 71   0.658
## 72  -1.019
## 73  -0.452
## 74  -0.017
## 75   1.000
## 76   1.000
## 77   1.000
## 78   1.000
## 79   1.000
## 80   1.000
## 81   1.000
## 82   1.000
## 83   1.000
## 84   1.000
## 85   1.000
## 86   1.000
## 87   1.000
## 88   1.000
## 89   1.000
## 90   1.000
## 91   1.000
## 92   0.050
## 93   0.050
## 94   0.050
## 95   0.050
## 96   0.000
## 97   0.000
## 98   0.000
## 99   0.000
## 100  0.000
## 101  0.000
## 102  0.000
## 103  0.000
## 104  0.000
## 105  0.000
## 106  0.000
## 107  0.000
## 108  0.000
## 109  0.000
## 110  0.000
## 111  0.000
## 112  0.000
## 113  0.000
## 114  0.000
## 115  0.000
## 116  0.000
```

```r
#Model Coefficients
coef(fitFA4)
```

```
##  f1=~F2r  f1=~F3r  f1=~F5r  f1=~F6r  f1=~F7r  f1=~F9r f1=~F10r f1=~F11r 
##    1.070    1.221    1.205    1.587    1.226    1.376    1.433    1.288 
##  f2=~F8r f2=~F12r f2=~F16r f3=~F19r f4=~F18r   f1~~f2   f1~~f3   f1~~f4 
##    1.090    2.677    2.136    1.182    1.475    0.087    0.277    0.257 
##   f2~~f3   f2~~f4   f3~~f4   F1r|t1   F1r|t2   F1r|t3   F2r|t1   F2r|t2 
##    0.101    0.107    0.336   -0.616    0.071    0.658   -0.409    0.120 
##   F2r|t3   F3r|t1   F3r|t2   F3r|t3   F5r|t1   F5r|t2   F5r|t3   F6r|t1 
##    0.707   -0.818   -0.184    0.310   -0.622    0.002    0.546   -1.028 
##   F6r|t2   F6r|t3   F7r|t1   F7r|t2   F7r|t3   F9r|t1   F9r|t2   F9r|t3 
##   -0.502    0.012   -0.676   -0.199    0.326   -0.707   -0.204    0.179 
##  F10r|t1  F10r|t2  F10r|t3  F11r|t1  F11r|t2  F11r|t3   F4r|t1   F4r|t2 
##   -0.518   -0.007    0.469   -0.867   -0.393    0.071   -0.431    0.110 
##   F4r|t3   F8r|t1   F8r|t2   F8r|t3  F12r|t1  F12r|t2  F12r|t3  F16r|t1 
##    0.701   -0.140    0.321    0.918   -0.604   -0.145    0.404   -0.362 
##  F16r|t2  F16r|t3  F15r|t1  F15r|t2  F15r|t3  F19r|t1  F19r|t2  F19r|t3 
##   -0.046    0.415   -0.357    0.209    0.658   -0.346    0.100    0.490 
##  F17r|t1  F17r|t2  F17r|t3  F18r|t1  F18r|t2  F18r|t3   f1~~f1   f2~~f2 
##   -0.224    0.264    0.658   -1.019   -0.452   -0.017    0.252    0.113 
##   f3~~f3   f4~~f4 
##    0.562    0.451
```

```r
#Modification Index
MIFA4<-modindices(fitFA4)
MIIFA4<- MIFA4[which(MIFA4$mi>30),]
print(MIIFA4)
```

```
## [1] lhs       op        rhs       mi        mi.scaled epc       sepc.lv  
## [8] sepc.all  sepc.nox 
## <0 rows> (or 0-length row.names)
```

```r
#Model Plot
semPaths(fitFA4,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-14-1.png) 


```r
#Marliere - Final Factorial Analysis -4 Factos Solution - CFA Model - Item Removed (F4r e F8r)

FA4_CFA <- '
              # latent variable definitions 
              f1 =~ F1r  + F2r  + F3r  + F5r  + F6r  + F7r  + F9r  + F10r  + F11r 
              f2 =~ F12r + F16r 
              f3 =~ F15r + F19r
              f4 =~ F17r + F18r

             # variances and covariances 
              f1 ~~ f2
              f1 ~~ f3
              f1 ~~ f4
              f2 ~~ f3
              f2 ~~ f4
              f3 ~~ f4
                       '
fitFA4 <- cfa(FA4_CFA, data = orderedScale,
        ordered=c("F1r",
        "F2r",
        "F3r",
        "F4r",
        "F5r",
        "F6r",
        "F7r",
        "F8r",
        "F9r",
        "F10r",
        "F11r",
        "F12r",
        "F13r",
        "F14r",
        "F15r",
        "F16r",
        "F17r",
        "F18r",
        "F19r",
        "F20r"))

#Model Summary 
summary(fitFA4, standardized=T, fit.measures=T, rsquare=T)
```

```
## lavaan (0.5-18) converged normally after  32 iterations
## 
##   Number of observations                           513
## 
##   Estimator                                       DWLS      Robust
##   Minimum Function Test Statistic              107.252     175.351
##   Degrees of freedom                                84          84
##   P-value (Chi-square)                           0.044       0.000
##   Scaling correction factor                                  0.661
##   Shift parameter                                           13.151
##     for simple second-order correction (Mplus variant)
## 
## Model test baseline model:
## 
##   Minimum Function Test Statistic             8961.132    4472.271
##   Degrees of freedom                               105         105
##   P-value                                        0.000       0.000
## 
## User model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.997       0.979
##   Tucker-Lewis Index (TLI)                       0.997       0.974
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.023       0.046
##   90 Percent Confidence Interval          0.004  0.035       0.036  0.056
##   P-value RMSEA <= 0.05                          1.000       0.739
## 
## Weighted Root Mean Square Residual:
## 
##   WRMR                                           0.846       0.846
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                           Robust.sem
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   f1 =~
##     F1r               1.000                               0.509    0.509
##     F2r               1.055    0.099   10.670    0.000    0.537    0.537
##     F3r               1.195    0.110   10.885    0.000    0.608    0.608
##     F5r               1.190    0.108   11.033    0.000    0.606    0.606
##     F6r               1.562    0.121   12.929    0.000    0.795    0.795
##     F7r               1.209    0.110   11.001    0.000    0.616    0.616
##     F9r               1.354    0.113   12.013    0.000    0.689    0.689
##     F10r              1.410    0.118   11.923    0.000    0.718    0.718
##     F11r              1.269    0.112   11.288    0.000    0.646    0.646
##   f2 =~
##     F12r              1.000                               0.857    0.857
##     F16r              0.831    0.084    9.914    0.000    0.712    0.712
##   f3 =~
##     F15r              1.000                               0.751    0.751
##     F19r              1.179    0.069   17.173    0.000    0.885    0.885
##   f4 =~
##     F17r              1.000                               0.673    0.673
##     F18r              1.470    0.102   14.420    0.000    0.989    0.989
## 
## Covariances:
##   f1 ~~
##     f2                0.240    0.029    8.285    0.000    0.550    0.550
##     f3                0.282    0.029    9.774    0.000    0.737    0.737
##     f4                0.261    0.029    8.994    0.000    0.763    0.763
##   f2 ~~
##     f3                0.270    0.039    6.927    0.000    0.419    0.419
##     f4                0.296    0.035    8.525    0.000    0.514    0.514
##   f3 ~~
##     f4                0.337    0.034    9.832    0.000    0.668    0.668
## 
## Intercepts:
##     F1r               0.000                               0.000    0.000
##     F2r               0.000                               0.000    0.000
##     F3r               0.000                               0.000    0.000
##     F5r               0.000                               0.000    0.000
##     F6r               0.000                               0.000    0.000
##     F7r               0.000                               0.000    0.000
##     F9r               0.000                               0.000    0.000
##     F10r              0.000                               0.000    0.000
##     F11r              0.000                               0.000    0.000
##     F12r              0.000                               0.000    0.000
##     F16r              0.000                               0.000    0.000
##     F15r              0.000                               0.000    0.000
##     F19r              0.000                               0.000    0.000
##     F17r              0.000                               0.000    0.000
##     F18r              0.000                               0.000    0.000
##     f1                0.000                               0.000    0.000
##     f2                0.000                               0.000    0.000
##     f3                0.000                               0.000    0.000
##     f4                0.000                               0.000    0.000
## 
## Thresholds:
##     F1r|t1           -0.616    0.059  -10.371    0.000   -0.616   -0.616
##     F1r|t2            0.071    0.055    1.279    0.201    0.071    0.071
##     F1r|t3            0.658    0.060   10.967    0.000    0.658    0.658
##     F2r|t1           -0.409    0.057   -7.168    0.000   -0.409   -0.409
##     F2r|t2            0.120    0.056    2.161    0.031    0.120    0.120
##     F2r|t3            0.707    0.061   11.643    0.000    0.707    0.707
##     F3r|t1           -0.818    0.063  -13.053    0.000   -0.818   -0.818
##     F3r|t2           -0.184    0.056   -3.306    0.001   -0.184   -0.184
##     F3r|t3            0.310    0.056    5.505    0.000    0.310    0.310
##     F5r|t1           -0.622    0.059  -10.456    0.000   -0.622   -0.622
##     F5r|t2            0.002    0.055    0.044    0.965    0.002    0.002
##     F5r|t3            0.546    0.059    9.340    0.000    0.546    0.546
##     F6r|t1           -1.028    0.067  -15.237    0.000   -1.028   -1.028
##     F6r|t2           -0.502    0.058   -8.648    0.000   -0.502   -0.502
##     F6r|t3            0.012    0.055    0.221    0.825    0.012    0.012
##     F7r|t1           -0.676    0.060  -11.222    0.000   -0.676   -0.676
##     F7r|t2           -0.199    0.056   -3.571    0.000   -0.199   -0.199
##     F7r|t3            0.326    0.056    5.768    0.000    0.326    0.326
##     F9r|t1           -0.707    0.061  -11.643    0.000   -0.707   -0.707
##     F9r|t2           -0.204    0.056   -3.659    0.000   -0.204   -0.204
##     F9r|t3            0.179    0.056    3.218    0.001    0.179    0.179
##     F10r|t1          -0.518    0.058   -8.908    0.000   -0.518   -0.518
##     F10r|t2          -0.007    0.055   -0.132    0.895   -0.007   -0.007
##     F10r|t3           0.469    0.058    8.127    0.000    0.469    0.469
##     F11r|t1          -0.867    0.064  -13.619    0.000   -0.867   -0.867
##     F11r|t2          -0.393    0.057   -6.906    0.000   -0.393   -0.393
##     F11r|t3           0.071    0.055    1.279    0.201    0.071    0.071
##     F12r|t1          -0.604    0.059  -10.200    0.000   -0.604   -0.604
##     F12r|t2          -0.145    0.056   -2.602    0.009   -0.145   -0.145
##     F12r|t3           0.404    0.057    7.081    0.000    0.404    0.404
##     F16r|t1          -0.362    0.057   -6.381    0.000   -0.362   -0.362
##     F16r|t2          -0.046    0.055   -0.838    0.402   -0.046   -0.046
##     F16r|t3           0.415    0.057    7.256    0.000    0.415    0.415
##     F15r|t1          -0.357    0.057   -6.294    0.000   -0.357   -0.357
##     F15r|t2           0.209    0.056    3.747    0.000    0.209    0.209
##     F15r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F19r|t1          -0.346    0.057   -6.119    0.000   -0.346   -0.346
##     F19r|t2           0.100    0.055    1.808    0.071    0.100    0.100
##     F19r|t3           0.490    0.058    8.474    0.000    0.490    0.490
##     F17r|t1          -0.224    0.056   -4.011    0.000   -0.224   -0.224
##     F17r|t2           0.264    0.056    4.714    0.000    0.264    0.264
##     F17r|t3           0.658    0.060   10.967    0.000    0.658    0.658
##     F18r|t1          -1.019    0.067  -15.163    0.000   -1.019   -1.019
##     F18r|t2          -0.452    0.057   -7.866    0.000   -0.452   -0.452
##     F18r|t3          -0.017    0.055   -0.309    0.758   -0.017   -0.017
## 
## Variances:
##     F1r               0.741                               0.741    0.741
##     F2r               0.712                               0.712    0.712
##     F3r               0.630                               0.630    0.630
##     F5r               0.633                               0.633    0.633
##     F6r               0.368                               0.368    0.368
##     F7r               0.621                               0.621    0.621
##     F9r               0.525                               0.525    0.525
##     F10r              0.485                               0.485    0.485
##     F11r              0.583                               0.583    0.583
##     F12r              0.266                               0.266    0.266
##     F16r              0.493                               0.493    0.493
##     F15r              0.437                               0.437    0.437
##     F19r              0.216                               0.216    0.216
##     F17r              0.548                               0.548    0.548
##     F18r              0.022                               0.022    0.022
##     f1                0.259    0.040                      1.000    1.000
##     f2                0.734    0.081                      1.000    1.000
##     f3                0.563    0.046                      1.000    1.000
##     f4                0.452    0.044                      1.000    1.000
## 
## R-Square:
## 
##     F1r               0.259
##     F2r               0.288
##     F3r               0.370
##     F5r               0.367
##     F6r               0.632
##     F7r               0.379
##     F9r               0.475
##     F10r              0.515
##     F11r              0.417
##     F12r              0.734
##     F16r              0.507
##     F15r              0.563
##     F19r              0.784
##     F17r              0.452
##     F18r              0.978
```

```r
#Model Fit Measures
fitMeasures(fitFA4)
```

```
##                          npar                          fmin 
##                        66.000                         0.105 
##                         chisq                            df 
##                       107.252                        84.000 
##                        pvalue                  chisq.scaled 
##                         0.044                       175.351 
##                     df.scaled                 pvalue.scaled 
##                        84.000                         0.000 
##          chisq.scaling.factor                baseline.chisq 
##                         0.661                      8961.132 
##                   baseline.df               baseline.pvalue 
##                       105.000                         0.000 
##         baseline.chisq.scaled            baseline.df.scaled 
##                      4472.271                       105.000 
##        baseline.pvalue.scaled baseline.chisq.scaling.factor 
##                         0.000                         2.028 
##                           cfi                           tli 
##                         0.997                         0.997 
##                          nnfi                           rfi 
##                         0.997                         0.985 
##                           nfi                          pnfi 
##                         0.988                         0.790 
##                           ifi                           rni 
##                         0.997                         0.997 
##                    cfi.scaled                    tli.scaled 
##                         0.979                         0.974 
##                   nnfi.scaled                    rfi.scaled 
##                         0.974                         0.951 
##                    nfi.scaled                    ifi.scaled 
##                         0.961                         0.961 
##                    rni.scaled                         rmsea 
##                         0.990                         0.023 
##                rmsea.ci.lower                rmsea.ci.upper 
##                         0.004                         0.035 
##                  rmsea.pvalue                  rmsea.scaled 
##                         1.000                         0.046 
##         rmsea.ci.lower.scaled         rmsea.ci.upper.scaled 
##                         0.036                         0.056 
##           rmsea.pvalue.scaled                          wrmr 
##                         0.739                         0.846 
##                         cn_05                         cn_01 
##                       508.906                       559.803 
##                           gfi                          agfi 
##                         0.991                         0.984 
##                          pgfi                           mfi 
##                         0.555                         0.978
```

```r
#Parameters Estimates
EstFA4 <- parameterEstimates(fitFA4, standardized=T, ci=F)
subset(EstFA4, op == "=~")
```

```
##    lhs op  rhs   est    se      z pvalue std.lv std.all std.nox
## 1   f1 =~  F1r 1.000 0.000     NA     NA  0.509   0.509   0.509
## 2   f1 =~  F2r 1.055 0.099 10.670      0  0.537   0.537   0.537
## 3   f1 =~  F3r 1.195 0.110 10.885      0  0.608   0.608   0.608
## 4   f1 =~  F5r 1.190 0.108 11.033      0  0.606   0.606   0.606
## 5   f1 =~  F6r 1.562 0.121 12.929      0  0.795   0.795   0.795
## 6   f1 =~  F7r 1.209 0.110 11.001      0  0.616   0.616   0.616
## 7   f1 =~  F9r 1.354 0.113 12.013      0  0.689   0.689   0.689
## 8   f1 =~ F10r 1.410 0.118 11.923      0  0.718   0.718   0.718
## 9   f1 =~ F11r 1.269 0.112 11.288      0  0.646   0.646   0.646
## 10  f2 =~ F12r 1.000 0.000     NA     NA  0.857   0.857   0.857
## 11  f2 =~ F16r 0.831 0.084  9.914      0  0.712   0.712   0.712
## 12  f3 =~ F15r 1.000 0.000     NA     NA  0.751   0.751   0.751
## 13  f3 =~ F19r 1.179 0.069 17.173      0  0.885   0.885   0.885
## 14  f4 =~ F17r 1.000 0.000     NA     NA  0.673   0.673   0.673
## 15  f4 =~ F18r 1.470 0.102 14.420      0  0.989   0.989   0.989
```

```r
#Parameters Table
parTable(fitFA4)
```

```
##      id  lhs op  rhs user group free ustart exo label eq.id unco plabel
## 1     1   f1 =~  F1r    1     1    0      1   0           0    0   .p1.
## 2     2   f1 =~  F2r    1     1    1     NA   0           0    1   .p2.
## 3     3   f1 =~  F3r    1     1    2     NA   0           0    2   .p3.
## 4     4   f1 =~  F5r    1     1    3     NA   0           0    3   .p4.
## 5     5   f1 =~  F6r    1     1    4     NA   0           0    4   .p5.
## 6     6   f1 =~  F7r    1     1    5     NA   0           0    5   .p6.
## 7     7   f1 =~  F9r    1     1    6     NA   0           0    6   .p7.
## 8     8   f1 =~ F10r    1     1    7     NA   0           0    7   .p8.
## 9     9   f1 =~ F11r    1     1    8     NA   0           0    8   .p9.
## 10   10   f2 =~ F12r    1     1    0      1   0           0    0  .p10.
## 11   11   f2 =~ F16r    1     1    9     NA   0           0    9  .p11.
## 12   12   f3 =~ F15r    1     1    0      1   0           0    0  .p12.
## 13   13   f3 =~ F19r    1     1   10     NA   0           0   10  .p13.
## 14   14   f4 =~ F17r    1     1    0      1   0           0    0  .p14.
## 15   15   f4 =~ F18r    1     1   11     NA   0           0   11  .p15.
## 16   16   f1 ~~   f2    1     1   12     NA   0           0   12  .p16.
## 17   17   f1 ~~   f3    1     1   13     NA   0           0   13  .p17.
## 18   18   f1 ~~   f4    1     1   14     NA   0           0   14  .p18.
## 19   19   f2 ~~   f3    1     1   15     NA   0           0   15  .p19.
## 20   20   f2 ~~   f4    1     1   16     NA   0           0   16  .p20.
## 21   21   f3 ~~   f4    1     1   17     NA   0           0   17  .p21.
## 22   22  F1r  |   t1    0     1   18     NA   0           0   18  .p22.
## 23   23  F1r  |   t2    0     1   19     NA   0           0   19  .p23.
## 24   24  F1r  |   t3    0     1   20     NA   0           0   20  .p24.
## 25   25  F2r  |   t1    0     1   21     NA   0           0   21  .p25.
## 26   26  F2r  |   t2    0     1   22     NA   0           0   22  .p26.
## 27   27  F2r  |   t3    0     1   23     NA   0           0   23  .p27.
## 28   28  F3r  |   t1    0     1   24     NA   0           0   24  .p28.
## 29   29  F3r  |   t2    0     1   25     NA   0           0   25  .p29.
## 30   30  F3r  |   t3    0     1   26     NA   0           0   26  .p30.
## 31   31  F5r  |   t1    0     1   27     NA   0           0   27  .p31.
## 32   32  F5r  |   t2    0     1   28     NA   0           0   28  .p32.
## 33   33  F5r  |   t3    0     1   29     NA   0           0   29  .p33.
## 34   34  F6r  |   t1    0     1   30     NA   0           0   30  .p34.
## 35   35  F6r  |   t2    0     1   31     NA   0           0   31  .p35.
## 36   36  F6r  |   t3    0     1   32     NA   0           0   32  .p36.
## 37   37  F7r  |   t1    0     1   33     NA   0           0   33  .p37.
## 38   38  F7r  |   t2    0     1   34     NA   0           0   34  .p38.
## 39   39  F7r  |   t3    0     1   35     NA   0           0   35  .p39.
## 40   40  F9r  |   t1    0     1   36     NA   0           0   36  .p40.
## 41   41  F9r  |   t2    0     1   37     NA   0           0   37  .p41.
## 42   42  F9r  |   t3    0     1   38     NA   0           0   38  .p42.
## 43   43 F10r  |   t1    0     1   39     NA   0           0   39  .p43.
## 44   44 F10r  |   t2    0     1   40     NA   0           0   40  .p44.
## 45   45 F10r  |   t3    0     1   41     NA   0           0   41  .p45.
## 46   46 F11r  |   t1    0     1   42     NA   0           0   42  .p46.
## 47   47 F11r  |   t2    0     1   43     NA   0           0   43  .p47.
## 48   48 F11r  |   t3    0     1   44     NA   0           0   44  .p48.
## 49   49 F12r  |   t1    0     1   45     NA   0           0   45  .p49.
## 50   50 F12r  |   t2    0     1   46     NA   0           0   46  .p50.
## 51   51 F12r  |   t3    0     1   47     NA   0           0   47  .p51.
## 52   52 F16r  |   t1    0     1   48     NA   0           0   48  .p52.
## 53   53 F16r  |   t2    0     1   49     NA   0           0   49  .p53.
## 54   54 F16r  |   t3    0     1   50     NA   0           0   50  .p54.
## 55   55 F15r  |   t1    0     1   51     NA   0           0   51  .p55.
## 56   56 F15r  |   t2    0     1   52     NA   0           0   52  .p56.
## 57   57 F15r  |   t3    0     1   53     NA   0           0   53  .p57.
## 58   58 F19r  |   t1    0     1   54     NA   0           0   54  .p58.
## 59   59 F19r  |   t2    0     1   55     NA   0           0   55  .p59.
## 60   60 F19r  |   t3    0     1   56     NA   0           0   56  .p60.
## 61   61 F17r  |   t1    0     1   57     NA   0           0   57  .p61.
## 62   62 F17r  |   t2    0     1   58     NA   0           0   58  .p62.
## 63   63 F17r  |   t3    0     1   59     NA   0           0   59  .p63.
## 64   64 F18r  |   t1    0     1   60     NA   0           0   60  .p64.
## 65   65 F18r  |   t2    0     1   61     NA   0           0   61  .p65.
## 66   66 F18r  |   t3    0     1   62     NA   0           0   62  .p66.
## 67   67  F1r ~~  F1r    0     1    0      1   0           0    0  .p67.
## 68   68  F2r ~~  F2r    0     1    0      1   0           0    0  .p68.
## 69   69  F3r ~~  F3r    0     1    0      1   0           0    0  .p69.
## 70   70  F5r ~~  F5r    0     1    0      1   0           0    0  .p70.
## 71   71  F6r ~~  F6r    0     1    0      1   0           0    0  .p71.
## 72   72  F7r ~~  F7r    0     1    0      1   0           0    0  .p72.
## 73   73  F9r ~~  F9r    0     1    0      1   0           0    0  .p73.
## 74   74 F10r ~~ F10r    0     1    0      1   0           0    0  .p74.
## 75   75 F11r ~~ F11r    0     1    0      1   0           0    0  .p75.
## 76   76 F12r ~~ F12r    0     1    0      1   0           0    0  .p76.
## 77   77 F16r ~~ F16r    0     1    0      1   0           0    0  .p77.
## 78   78 F15r ~~ F15r    0     1    0      1   0           0    0  .p78.
## 79   79 F19r ~~ F19r    0     1    0      1   0           0    0  .p79.
## 80   80 F17r ~~ F17r    0     1    0      1   0           0    0  .p80.
## 81   81 F18r ~~ F18r    0     1    0      1   0           0    0  .p81.
## 82   82   f1 ~~   f1    0     1   63     NA   0           0   63  .p82.
## 83   83   f2 ~~   f2    0     1   64     NA   0           0   64  .p83.
## 84   84   f3 ~~   f3    0     1   65     NA   0           0   65  .p84.
## 85   85   f4 ~~   f4    0     1   66     NA   0           0   66  .p85.
## 86   86  F1r ~1         0     1    0      0   0           0    0  .p86.
## 87   87  F2r ~1         0     1    0      0   0           0    0  .p87.
## 88   88  F3r ~1         0     1    0      0   0           0    0  .p88.
## 89   89  F5r ~1         0     1    0      0   0           0    0  .p89.
## 90   90  F6r ~1         0     1    0      0   0           0    0  .p90.
## 91   91  F7r ~1         0     1    0      0   0           0    0  .p91.
## 92   92  F9r ~1         0     1    0      0   0           0    0  .p92.
## 93   93 F10r ~1         0     1    0      0   0           0    0  .p93.
## 94   94 F11r ~1         0     1    0      0   0           0    0  .p94.
## 95   95 F12r ~1         0     1    0      0   0           0    0  .p95.
## 96   96 F16r ~1         0     1    0      0   0           0    0  .p96.
## 97   97 F15r ~1         0     1    0      0   0           0    0  .p97.
## 98   98 F19r ~1         0     1    0      0   0           0    0  .p98.
## 99   99 F17r ~1         0     1    0      0   0           0    0  .p99.
## 100 100 F18r ~1         0     1    0      0   0           0    0 .p100.
## 101 101   f1 ~1         0     1    0      0   0           0    0 .p101.
## 102 102   f2 ~1         0     1    0      0   0           0    0 .p102.
## 103 103   f3 ~1         0     1    0      0   0           0    0 .p103.
## 104 104   f4 ~1         0     1    0      0   0           0    0 .p104.
##      start
## 1    1.000
## 2    0.917
## 3    1.118
## 4    1.060
## 5    1.266
## 6    1.069
## 7    1.156
## 8    1.191
## 9    1.072
## 10   1.000
## 11   0.610
## 12   1.000
## 13   0.664
## 14   1.000
## 15   0.665
## 16   0.000
## 17   0.000
## 18   0.000
## 19   0.000
## 20   0.000
## 21   0.000
## 22  -0.616
## 23   0.071
## 24   0.658
## 25  -0.409
## 26   0.120
## 27   0.707
## 28  -0.818
## 29  -0.184
## 30   0.310
## 31  -0.622
## 32   0.002
## 33   0.546
## 34  -1.028
## 35  -0.502
## 36   0.012
## 37  -0.676
## 38  -0.199
## 39   0.326
## 40  -0.707
## 41  -0.204
## 42   0.179
## 43  -0.518
## 44  -0.007
## 45   0.469
## 46  -0.867
## 47  -0.393
## 48   0.071
## 49  -0.604
## 50  -0.145
## 51   0.404
## 52  -0.362
## 53  -0.046
## 54   0.415
## 55  -0.357
## 56   0.209
## 57   0.658
## 58  -0.346
## 59   0.100
## 60   0.490
## 61  -0.224
## 62   0.264
## 63   0.658
## 64  -1.019
## 65  -0.452
## 66  -0.017
## 67   1.000
## 68   1.000
## 69   1.000
## 70   1.000
## 71   1.000
## 72   1.000
## 73   1.000
## 74   1.000
## 75   1.000
## 76   1.000
## 77   1.000
## 78   1.000
## 79   1.000
## 80   1.000
## 81   1.000
## 82   0.050
## 83   0.050
## 84   0.050
## 85   0.050
## 86   0.000
## 87   0.000
## 88   0.000
## 89   0.000
## 90   0.000
## 91   0.000
## 92   0.000
## 93   0.000
## 94   0.000
## 95   0.000
## 96   0.000
## 97   0.000
## 98   0.000
## 99   0.000
## 100  0.000
## 101  0.000
## 102  0.000
## 103  0.000
## 104  0.000
```

```r
#Model Coefficients
coef(fitFA4)
```

```
##  f1=~F2r  f1=~F3r  f1=~F5r  f1=~F6r  f1=~F7r  f1=~F9r f1=~F10r f1=~F11r 
##    1.055    1.195    1.190    1.562    1.209    1.354    1.410    1.269 
## f2=~F16r f3=~F19r f4=~F18r   f1~~f2   f1~~f3   f1~~f4   f2~~f3   f2~~f4 
##    0.831    1.179    1.470    0.240    0.282    0.261    0.270    0.296 
##   f3~~f4   F1r|t1   F1r|t2   F1r|t3   F2r|t1   F2r|t2   F2r|t3   F3r|t1 
##    0.337   -0.616    0.071    0.658   -0.409    0.120    0.707   -0.818 
##   F3r|t2   F3r|t3   F5r|t1   F5r|t2   F5r|t3   F6r|t1   F6r|t2   F6r|t3 
##   -0.184    0.310   -0.622    0.002    0.546   -1.028   -0.502    0.012 
##   F7r|t1   F7r|t2   F7r|t3   F9r|t1   F9r|t2   F9r|t3  F10r|t1  F10r|t2 
##   -0.676   -0.199    0.326   -0.707   -0.204    0.179   -0.518   -0.007 
##  F10r|t3  F11r|t1  F11r|t2  F11r|t3  F12r|t1  F12r|t2  F12r|t3  F16r|t1 
##    0.469   -0.867   -0.393    0.071   -0.604   -0.145    0.404   -0.362 
##  F16r|t2  F16r|t3  F15r|t1  F15r|t2  F15r|t3  F19r|t1  F19r|t2  F19r|t3 
##   -0.046    0.415   -0.357    0.209    0.658   -0.346    0.100    0.490 
##  F17r|t1  F17r|t2  F17r|t3  F18r|t1  F18r|t2  F18r|t3   f1~~f1   f2~~f2 
##   -0.224    0.264    0.658   -1.019   -0.452   -0.017    0.259    0.734 
##   f3~~f3   f4~~f4 
##    0.563    0.452
```

```r
#Modification Index
MIFA4<-modindices(fitFA4)
MIIFA4<- MIFA4[which(MIFA4$mi>30),]
print(MIIFA4)
```

```
## [1] lhs       op        rhs       mi        mi.scaled epc       sepc.lv  
## [8] sepc.all  sepc.nox 
## <0 rows> (or 0-length row.names)
```

```r
#Model Plot
semPaths(fitFA4,"std", edge.label.cex = 0.5, exoVar = T, exoCov = T, layout = "tree2", optimizeLatRes=F, style = "lisrel", curve= 0.9, sizeLat = 5, sizeLat2 = 5, sizeMan = 2, sizeMan2 = 2, title = F, ThreshAtSide=F)
```

![](PCA_CESD_files/figure-html/unnamed-chunk-15-1.png) 

```r
#Factor 1
F1_FA4 <- fullScale[, c("F1r","F2r","F3r","F5r","F6r","F7r","F9r","F10r","F11r")]
alpha(F1_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F1_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.82      0.82     0.8      0.33 4.4 0.019  1.7 0.75
## 
##  lower alpha upper     95% confidence boundaries
## 0.78 0.82 0.85 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F1r       0.81      0.81    0.79      0.34 4.1    0.021
## F2r       0.80      0.80    0.79      0.34 4.1    0.021
## F3r       0.80      0.80    0.78      0.33 3.9    0.022
## F5r       0.80      0.80    0.78      0.33 3.9    0.021
## F6r       0.79      0.79    0.77      0.32 3.7    0.022
## F7r       0.80      0.80    0.78      0.33 4.0    0.021
## F9r       0.79      0.79    0.78      0.32 3.8    0.022
## F10r      0.79      0.79    0.77      0.32 3.8    0.022
## F11r      0.80      0.80    0.78      0.33 4.0    0.021
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F1r  513  0.57  0.58  0.49   0.44  1.5 1.1
## F2r  513  0.59  0.59  0.51   0.46  1.4 1.2
## F3r  513  0.64  0.64  0.57   0.52  1.7 1.2
## F5r  513  0.63  0.63  0.56   0.51  1.5 1.2
## F6r  513  0.70  0.70  0.66   0.59  2.0 1.1
## F7r  513  0.63  0.63  0.56   0.50  1.7 1.2
## F9r  513  0.66  0.66  0.60   0.54  1.8 1.2
## F10r 513  0.68  0.68  0.63   0.57  1.5 1.2
## F11r 513  0.62  0.62  0.55   0.49  1.9 1.2
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F1r  0.27 0.26 0.22 0.26    0
## F2r  0.34 0.21 0.21 0.24    0
## F3r  0.21 0.22 0.19 0.38    0
## F5r  0.27 0.23 0.21 0.29    0
## F6r  0.15 0.16 0.20 0.50    0
## F7r  0.25 0.17 0.21 0.37    0
## F9r  0.24 0.18 0.15 0.43    0
## F10r 0.30 0.19 0.18 0.32    0
## F11r 0.19 0.15 0.18 0.47    0
```

```r
#Factor 2
F2_FA4 <-  fullScale[, c("F12r","F16r")]
alpha(F2_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F2_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
##       0.67      0.67     0.5       0.5   2 0.068  1.6 1.1
## 
##  lower alpha upper     95% confidence boundaries
## 0.54 0.67 0.8 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F12r       0.5       0.5    0.25       0.5  NA       NA
## F16r       0.5       0.5    0.25       0.5  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F12r 513  0.86  0.87  0.62    0.5  1.6 1.2
## F16r 513  0.88  0.87  0.62    0.5  1.5 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F12r 0.27 0.17 0.21 0.34    0
## F16r 0.36 0.12 0.18 0.34    0
```

```r
#Factor 3
F4_FA3 <- fullScale[, c("F15r","F19r")]
alpha(F4_FA3, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F4_FA3, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd
##       0.71      0.71    0.55      0.55 2.5 0.065  1.4 1.1
## 
##  lower alpha upper     95% confidence boundaries
## 0.58 0.71 0.84 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F15r      0.55      0.55     0.3      0.55  NA       NA
## F19r      0.55      0.55     0.3      0.55  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F15r 513  0.87  0.88  0.65   0.55  1.3 1.2
## F19r 513  0.89  0.88  0.65   0.55  1.4 1.3
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F15r 0.36 0.22 0.16 0.26    0
## F19r 0.36 0.18 0.15 0.31    0
```

```r
#Factor 4
F3_FA4 <- fullScale[, c("F17r","F18r")]
alpha(F3_FA4, check.keys = TRUE)
```

```
## 
## Reliability analysis   
## Call: alpha(x = F3_FA4, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean sd
##       0.68      0.68    0.52      0.52 2.1 0.067  1.6  1
## 
##  lower alpha upper     95% confidence boundaries
## 0.55 0.68 0.81 
## 
##  Reliability if an item is dropped:
##      raw_alpha std.alpha G6(smc) average_r S/N alpha se
## F17r      0.52      0.52    0.27      0.52  NA       NA
## F18r      0.52      0.52    0.27      0.52  NA       NA
## 
##  Item statistics 
##        n raw.r std.r r.cor r.drop mean  sd
## F17r 513  0.88  0.87  0.63   0.52  1.2 1.2
## F18r 513  0.86  0.87  0.63   0.52  2.0 1.1
## 
## Non missing response frequency for each item
##         0    1    2    3 miss
## F17r 0.41 0.19 0.14 0.26    0
## F18r 0.15 0.17 0.17 0.51    0
```


#Final Solutions

```r
#PCA2
#Sum CESD itens PCA2

#Component 1
base.dat$PCA2C1 <- base.dat$F1r+ base.dat$F2r+ base.dat$F3r+ base.dat$F5r+ base.dat$F6r+ base.dat$F7r+ base.dat$F9r+ base.dat$F10r+ base.dat$F11r+ base.dat$F14r+ base.dat$F15r+ base.dat$F17r+ base.dat$F18r+ base.dat$F19r+ base.dat$F20r

#Component 1
base.dat$PCA2C2 <- base.dat$F12r+ base.dat$F16r


#FA4
#Sum CESD itens FA4

#Factor 1
base.dat$FA4F1 <- base.dat$F1r  + base.dat$F2r  + base.dat$F3r  + base.dat$F5r  + base.dat$F6r  + base.dat$F7r  + base.dat$F9r  + base.dat$F10r  + base.dat$F11r

#Factor 2
base.dat$FA4F2 <- base.dat$F12r  + base.dat$F16r 

#Factor 3
base.dat$FA4F3 <- base.dat$F15r  + base.dat$F19r

#Factor 4
base.dat$FA4F4 <- base.dat$F17r + base.dat$F18r
```


```r
#Correlation - ISMI and CES-D

MatrixcorrPCA2<- base.dat[,c(230:236,288:289)]

corr.test(MatrixcorrPCA2)
```

```
## Call:corr.test(x = MatrixcorrPCA2)
## Correlation matrix 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 PCA2C1 PCA2C2
## ISMIG1   1.00   0.97   0.84   0.77   0.77   0.83   0.12   0.45  -0.23
## ISMIG2   0.97   1.00   0.83   0.74   0.74   0.81   0.35   0.44  -0.24
## ISMIF1   0.84   0.83   1.00   0.47   0.56   0.67   0.15   0.39  -0.21
## ISMIF2   0.77   0.74   0.47   1.00   0.51   0.49   0.05   0.30  -0.13
## ISMIF3   0.77   0.74   0.56   0.51   1.00   0.49   0.06   0.35  -0.16
## ISMIF4   0.83   0.81   0.67   0.49   0.49   1.00   0.11   0.39  -0.23
## ISMIF5   0.12   0.35   0.15   0.05   0.06   0.11   1.00   0.09  -0.11
## PCA2C1   0.45   0.44   0.39   0.30   0.35   0.39   0.09   1.00  -0.44
## PCA2C2  -0.23  -0.24  -0.21  -0.13  -0.16  -0.23  -0.11  -0.44   1.00
## Sample Size 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 PCA2C1 PCA2C2
## ISMIG1    525    525    525    525    525    525    525    516    520
## ISMIG2    525    525    525    525    525    525    525    516    520
## ISMIF1    525    525    525    525    525    525    525    516    520
## ISMIF2    525    525    525    525    525    525    525    516    520
## ISMIF3    525    525    525    525    525    525    525    516    520
## ISMIF4    525    525    525    525    525    525    525    516    520
## ISMIF5    525    525    525    525    525    525    525    516    520
## PCA2C1    516    516    516    516    516    516    516    516    515
## PCA2C2    520    520    520    520    520    520    520    515    520
## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 PCA2C1 PCA2C2
## ISMIG1   0.00      0      0   0.00   0.00   0.00   0.04   0.00   0.00
## ISMIG2   0.00      0      0   0.00   0.00   0.00   0.00   0.00   0.00
## ISMIF1   0.00      0      0   0.00   0.00   0.00   0.01   0.00   0.00
## ISMIF2   0.00      0      0   0.00   0.00   0.00   0.30   0.00   0.03
## ISMIF3   0.00      0      0   0.00   0.00   0.00   0.30   0.00   0.00
## ISMIF4   0.00      0      0   0.00   0.00   0.00   0.05   0.00   0.00
## ISMIF5   0.01      0      0   0.23   0.15   0.01   0.00   0.15   0.06
## PCA2C1   0.00      0      0   0.00   0.00   0.00   0.05   0.00   0.00
## PCA2C2   0.00      0      0   0.00   0.00   0.00   0.01   0.00   0.00
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
```

```r
MatrixcorrFA4<- base.dat[,c(230:236,290:293)]

corr.test(MatrixcorrFA4)
```

```
## Call:corr.test(x = MatrixcorrFA4)
## Correlation matrix 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 FA4F1 FA4F2 FA4F3
## ISMIG1   1.00   0.97   0.84   0.77   0.77   0.83   0.12  0.41 -0.23  0.36
## ISMIG2   0.97   1.00   0.83   0.74   0.74   0.81   0.35  0.40 -0.24  0.36
## ISMIF1   0.84   0.83   1.00   0.47   0.56   0.67   0.15  0.36 -0.21  0.34
## ISMIF2   0.77   0.74   0.47   1.00   0.51   0.49   0.05  0.29 -0.13  0.21
## ISMIF3   0.77   0.74   0.56   0.51   1.00   0.49   0.06  0.32 -0.16  0.31
## ISMIF4   0.83   0.81   0.67   0.49   0.49   1.00   0.11  0.36 -0.23  0.33
## ISMIF5   0.12   0.35   0.15   0.05   0.06   0.11   1.00  0.06 -0.11  0.06
## FA4F1    0.41   0.40   0.36   0.29   0.32   0.36   0.06  1.00 -0.40  0.55
## FA4F2   -0.23  -0.24  -0.21  -0.13  -0.16  -0.23  -0.11 -0.40  1.00 -0.30
## FA4F3    0.36   0.36   0.34   0.21   0.31   0.33   0.06  0.55 -0.30  1.00
## FA4F4    0.33   0.33   0.30   0.23   0.27   0.25   0.10  0.58 -0.33  0.47
##        FA4F4
## ISMIG1  0.33
## ISMIG2  0.33
## ISMIF1  0.30
## ISMIF2  0.23
## ISMIF3  0.27
## ISMIF4  0.25
## ISMIF5  0.10
## FA4F1   0.58
## FA4F2  -0.33
## FA4F3   0.47
## FA4F4   1.00
## Sample Size 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 FA4F1 FA4F2 FA4F3
## ISMIG1    525    525    525    525    525    525    525   520   520   521
## ISMIG2    525    525    525    525    525    525    525   520   520   521
## ISMIF1    525    525    525    525    525    525    525   520   520   521
## ISMIF2    525    525    525    525    525    525    525   520   520   521
## ISMIF3    525    525    525    525    525    525    525   520   520   521
## ISMIF4    525    525    525    525    525    525    525   520   520   521
## ISMIF5    525    525    525    525    525    525    525   520   520   521
## FA4F1     520    520    520    520    520    520    520   520   518   519
## FA4F2     520    520    520    520    520    520    520   518   520   519
## FA4F3     521    521    521    521    521    521    521   519   519   521
## FA4F4     521    521    521    521    521    521    521   519   519   520
##        FA4F4
## ISMIG1   521
## ISMIG2   521
## ISMIF1   521
## ISMIF2   521
## ISMIF3   521
## ISMIF4   521
## ISMIF5   521
## FA4F1    519
## FA4F2    519
## FA4F3    520
## FA4F4    521
## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
##        ISMIG1 ISMIG2 ISMIF1 ISMIF2 ISMIF3 ISMIF4 ISMIF5 FA4F1 FA4F2 FA4F3
## ISMIG1   0.00      0      0   0.00   0.00   0.00   0.05  0.00  0.00  0.00
## ISMIG2   0.00      0      0   0.00   0.00   0.00   0.00  0.00  0.00  0.00
## ISMIF1   0.00      0      0   0.00   0.00   0.00   0.01  0.00  0.00  0.00
## ISMIF2   0.00      0      0   0.00   0.00   0.00   0.59  0.00  0.04  0.00
## ISMIF3   0.00      0      0   0.00   0.00   0.00   0.59  0.00  0.00  0.00
## ISMIF4   0.00      0      0   0.00   0.00   0.00   0.07  0.00  0.00  0.00
## ISMIF5   0.01      0      0   0.23   0.15   0.01   0.00  0.59  0.09  0.59
## FA4F1    0.00      0      0   0.00   0.00   0.00   0.15  0.00  0.00  0.00
## FA4F2    0.00      0      0   0.00   0.00   0.00   0.01  0.00  0.00  0.00
## FA4F3    0.00      0      0   0.00   0.00   0.00   0.16  0.00  0.00  0.00
## FA4F4    0.00      0      0   0.00   0.00   0.00   0.03  0.00  0.00  0.00
##        FA4F4
## ISMIG1  0.00
## ISMIG2  0.00
## ISMIF1  0.00
## ISMIF2  0.00
## ISMIF3  0.00
## ISMIF4  0.00
## ISMIF5  0.14
## FA4F1   0.00
## FA4F2   0.00
## FA4F3   0.00
## FA4F4   0.00
## 
##  To see confidence intervals of the correlations, print with the short=FALSE option
```


```r
#Summaries for PCA2
summary(base.dat$PCA2C1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   16.00   25.00   24.57   33.00   45.00       9
```

```r
summary(base.dat$PCA2C2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   3.000   2.896   5.000   6.000       5
```

```r
#Summaries for FA4
summary(base.dat$FA4F1)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0      10      16      15      20      27       5
```

```r
summary(base.dat$FA4F2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   3.000   2.896   5.000   6.000       5
```

```r
summary(base.dat$FA4F3)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   3.000   2.693   4.000   6.000       4
```

```r
summary(base.dat$FA4F4)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   3.251   5.000   6.000       4
```



