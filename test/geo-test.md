

# Extensive geo test


## Load data


```r
data(Europe)
data(rivers)
```

## geo_fill tests


### constant

```r
geo_shape(Europe) +
	geo_fill()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### color variable

```r
Europe$color <- ifelse(Europe$iso_a3=="NLD", "orange", "steelblue")
geo_shape(Europe) +
	geo_fill("color")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### numeric variables

```r
geo_shape(Europe) +
	geo_fill("gdp_cap_est")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens"))
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


```r
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens")) +
	geo_facets(free.scales.fill=FALSE)
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

### categorical variables

```r
geo_shape(Europe) +
	geo_fill("continent")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


```r
geo_shape(Europe) +
	geo_fill(c("continent", "economy"))
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


```r
geo_shape(Europe) +
	geo_fill(c("continent", "economy")) +
	geo_facets(free.scales=FALSE)
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

### mixed variables

```r
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "economy"))
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


```r
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "economy")) +
	geo_facets(free.scales.fill=FALSE)
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

### group by

```r
geo_shape(Europe) +
	geo_fill("red") +
	geo_facets(by="part")
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


```r
geo_shape(Europe) +
	geo_fill("color") +
	geo_facets(by="part")
```

```
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
## Warning: the condition has length > 1 and only the first element will be used
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 


```r
geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_facets(by="part")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


```r
geo_shape(Europe) +
	geo_fill(c("pop_est_dens", "gdp_cap_est")) +
	geo_facets(by="part")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 


```r
geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_facets(by="part", free.scales=TRUE)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 


```r
geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 


```r
geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part", free.scales=FALSE)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 


```r
geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part", free.scales=TRUE)
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 


## bubble tests

### constant

```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles()
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(size=1, col="red")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(size=c(.5, 1), col=c("red", "purple"), scale=1)
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(size=c(.5, 1, 2), col=c("red", "purple"), scale=1)
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 

### size

```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", scale=2)
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("red", "purple"), scale=2)
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), scale=2)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), scale=2) +
	geo_facets(free.scales=FALSE)
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27.png) 

### color

```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(col="income_grp", scale=1)
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(col=c("income_grp", "economy"), scale=1)
```

![plot of chunk unnamed-chunk-29](figure/unnamed-chunk-29.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(col=c("income_grp", "economy"), scale=1) +
	geo_facets(free.scales=FALSE)
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 

### size and color

```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), col="income_grp", scale=2)
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("income_grp", "economy"), scale=2)
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("income_grp", "economy"), scale=2) +
	geo_facets(free.scales=FALSE)
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33.png) 

### by

```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles() +
	geo_facets("part")
```

![plot of chunk unnamed-chunk-34](figure/unnamed-chunk-34.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", scale=2) +
	geo_facets("part")
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part")
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part", free.scales.bubble.size=TRUE)
```

![plot of chunk unnamed-chunk-37](figure/unnamed-chunk-37.png) 


```r
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part", free.scales.bubble.col=TRUE)
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38.png) 

## line tests


### default

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines()
```

![plot of chunk unnamed-chunk-39](figure/unnamed-chunk-39.png) 

### constant color

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("blue")
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40.png) 

### constant color and lwd

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("blue", lwd=2)
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41.png) 

### constant color and lwd multiples

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("blue", lwd=c(2,1))
```

![plot of chunk unnamed-chunk-42](figure/unnamed-chunk-42.png) 


```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines(c("blue", "gold", "red"), lwd=c(2,1))
```

![plot of chunk unnamed-chunk-43](figure/unnamed-chunk-43.png) 

### variable line color (categorical)

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("type")
```

![plot of chunk unnamed-chunk-44](figure/unnamed-chunk-44.png) 

### variable line color (numeric)

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("scalerank", lwd=3)
```

![plot of chunk unnamed-chunk-45](figure/unnamed-chunk-45.png) 


```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines(lwd="scalerank", scale=4)
```

![plot of chunk unnamed-chunk-46](figure/unnamed-chunk-46.png) 

### variable line color (numeric) and lwd

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("type", "scalerank", scale=4)
```

![plot of chunk unnamed-chunk-47](figure/unnamed-chunk-47.png) 


```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("type", c("scalerank", "strokelwd"), scale=4)
```

![plot of chunk unnamed-chunk-48](figure/unnamed-chunk-48.png) 

### by

```r
geo_shape(Europe) + geo_borders() + geo_shape(rivers) +
	geo_lines("type", c("strokelwd"), scale=4) +
	geo_facets("type")
```

![plot of chunk unnamed-chunk-49](figure/unnamed-chunk-49.png) 
