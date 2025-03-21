``` r
install.packages('tinytex')
tinytex::install_tinytex()
```

``` r
data("mtcars")
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(color = wt)) +
  xlab("Weight") + 
  ylab("Miles per gallon") +
  scale_colour_gradient(low = "forestgreen", high = "black")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Coding-Practice-4_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# First-level header

## Second-level header

### Third-level header

*italic* *italic* **bold** **bold** \> “I thoroughly disapprove of
duels. If a man should challenge me, I would take him kindly and
forgivingly by the hand and lead him to a quiet place and kill him.” \>
\> — Mark Twain

- one item
- one item
- one item
  - one more item
  - one more item
  - one more item

1.  the first item
2.  the second item
3.  the third item
    - one unordered item
    - one unordered item

<https://agriculture.auburn.edu/about/directory/faculty/zachary-noel/>
<https://agriculture.auburn.edu/about/directory/faculty/zachary-noel/>
[Noel
Lab](https://agriculture.auburn.edu/about/directory/faculty/zachary-noel/)

<figure>
<img src="Picture.jpg" alt="Smut" />
<figcaption aria-hidden="true">Smut</figcaption>
</figure>

| First Header | Second Header |
|--------------|---------------|
| Content Cell | Content Cell  |
| Content Cell | Content Cell  |

``` r
kable(head(mtcars, n = 5), digits = 3, format = "markdown")
```

|                   |  mpg | cyl | disp |  hp | drat |    wt |  qsec |  vs |  am | gear | carb |
|:------------------|-----:|----:|-----:|----:|-----:|------:|------:|----:|----:|-----:|-----:|
| Mazda RX4         | 21.0 |   6 |  160 | 110 | 3.90 | 2.620 | 16.46 |   0 |   1 |    4 |    4 |
| Mazda RX4 Wag     | 21.0 |   6 |  160 | 110 | 3.90 | 2.875 | 17.02 |   0 |   1 |    4 |    4 |
| Datsun 710        | 22.8 |   4 |  108 |  93 | 3.85 | 2.320 | 18.61 |   1 |   1 |    4 |    1 |
| Hornet 4 Drive    | 21.4 |   6 |  258 | 110 | 3.08 | 3.215 | 19.44 |   1 |   0 |    3 |    1 |
| Hornet Sportabout | 18.7 |   8 |  360 | 175 | 3.15 | 3.440 | 17.02 |   0 |   0 |    3 |    2 |

- [Coding Pratice 4](Coding%20Practice%204.Rmd)

## File Tree

    .
    └── TinyTeX-1.zip
