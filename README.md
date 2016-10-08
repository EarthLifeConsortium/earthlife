# earthlife

![](ELC-logo-M.png)

The `earthlife` package provides a programmatic interface to the [EarthLife Consorium](http://earthlifeconsortium.org)'s combined API for the [Neotoma Paleoecological Database](http://neotomadb.org) and the [Paleobiology Database](https://paleobiodb.org/). The combined API provides access to biological records obtained from fossil samples spanning from the present to the origin of life on earth.

### Development

*We welcome contributions from any individual, whether code, documentation, or issue tracking.  All participants are expected to follow the [code of conduct](https://github.com/EarthLifeConsortium/earthlife/blob/master/CONDUCT.md) for this project.*

+ [Simon Goring](http://downwithtime.wordpress.com) - University of Wisconsin-Madison, Department of Geography

## Example

Find all fossil records for the genus *Canis* and include all lower taxa (all species).

```R

# Find all Canis related fossils:
all_canis <- get_by_taxon("Canis", lower = TRUE)

```
