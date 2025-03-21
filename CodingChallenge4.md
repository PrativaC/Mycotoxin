# Link for the Paper

- [Link for paper](https://doi.org/10.1094/PDIS-06-21-1253-RE)

# Q1

*In the context of R Markdown , the YAML header appears at the top of
the document, enclosed by — lines, and it contains metadata about the
document.* *Literate Programming(For eg. R Markdown) is documentation
and code live together in the same file, making it easy for others (and
your future self!) to understand not just what the code does, but why it
was written that way.*

# Q2

## 2 (B)

``` r
mycotoxin=read.csv("MycotoxinData.csv", na.strings = "na")
mycotoxin$Treatment <- as.factor(mycotoxin$Treatment)
mycotoxin$Cultivar <- as.factor(mycotoxin$Cultivar)
```

## 2(C)

### CbbPalette

``` r
cbbPalette <- c("#56B4E9", "#009E73")
```

### DON_PLOT1

``` r
Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("Treatment") + 
  ylab("DON (ppm)") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 
Plot1
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### 15XADON_PLOT2

``` r
Plot2 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("Treatment") +  
  ylab("15XADON") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 

Plot2
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### SeedMass_PLOT3

``` r
Plot3 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("Treatment") +  
  ylab("Seed Mass (mg)") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 
Plot3
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### 3Combined_PLOT

``` r
figure=ggarrange(
  Plot1,
  Plot2,
  Plot3,
  labels = c("A", "B", "C"),
  nrow = 1,
  ncol = 3,
  common.legend=TRUE
)
```

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 8 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
figure
```

![](CodingChallenge4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Q(5)

*ALso available in GitHub Mycotoxin Repository*

- [README](README.md)

# Q(6)

*Link TO GitHub respository along with README*

- [GitHub Link](https://github.com/PrativaC/Mycotoxin)
