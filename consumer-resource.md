---
title: "Social-ecological network dynamics"
output:
  html_document:
    keep_md: yes
  pdf_document: 
    keep_tex: yes
---

An analysis of a simple dynamical model of a consumer-resource system with network effects.

# Setup
First load the necessary packages. *deSolve* for solving the diffeqs and *phaseR* for the phase plane analyses.




```r
library(deSolve)
library(phaseR)
```

Second let's write a convenience function that calls on phaseR under the hood to generate the flow field, nullclines, and sample trajectories for a given system and parameterization.


```r
phasePlot <- function(mod, params, xmax = 1, ymax = 1){
  xlim <- c(0, xmax)
  ylim <- c(0, ymax)
  
  y0 <- matrix(c(.5,.5, 1,1, .1,.1),
             ncol = 2, nrow = 3,
             byrow = TRUE)  
  
  flw <- flowField(mod, xlim = xlim, ylim = ylim, parameters = params, 
                   xlab = 'Population', ylab = 'Resource Biomass', add = F)
  nc <- nullclines(mod, xlim = xlim, ylim = ylim, parameters = params)
  trj <- trajectory(mod, y0 = y0, tlim = c(0,100), col = rep('black', nrow(y0)), parameters = params)
}
```

# Model 1: Simple consumer-resource system with network effects
First, we'll replicate the model of Muneepeerakul and Qubbaj (2012). It's a simple consumer resource system, with parameterized flows of population and resources (i.e. immigration and trade).

Setup the model.


```r
netMod <- function(t, y, parameters){
    H <- parameters[1]
    M <- parameters[2]
    alpha <- parameters[3]
    beta <- parameters [4]
    mu <- parameters[5]
    xi <- parameters[6]
    
    dy <- numeric(2)
    dy[1] <- H * y[2] * y[1]^beta - M * y[1]^alpha + xi
    dy[2] <- y[2] * (1 - y[2]) - H * y[2] * y[1]^beta + mu
    list(dy)
} 
```

No scaling

```r
phasePlot(netMod, c(.5, .32, 1, 1, 0, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Superlinear scaling of harvest ability

```r
phasePlot(netMod, c(.5, .32, 1, 1.2, 0, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Sublinear scaling of resource conversion efficiency

```r
phasePlot(netMod, c(.5, .32, .8, 1, 0, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Both scaling processes.
lesser scaling

```r
phasePlot(netMod, c(.5, .32, .9, 1.1, 0, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
greater scaling

```r
phasePlot(netMod, c(.5, .32, .8, 1.2, 0, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Trade

```r
phasePlot(netMod, c(.5, .32, .8, 1.2, .08, 0), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Immigration

```r
phasePlot(netMod, c(.5, .32, .8, 1.2, 0, .008), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
phasePlot(netMod, c(.5, .32, .8, 1.2, 0, .02), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
phasePlot(netMod, c(.5, .32, .8, 1.2, 0, .03), xmax = 1.3, ymax = 1.3)
```

![](consumer-resource_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


