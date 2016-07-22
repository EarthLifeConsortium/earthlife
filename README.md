# earthlife

<img src="ELC-logo-M.png" width="100">

The goal of the `earthlife` package is to provide a programmatic interface to the EarthLife Consorium's combined API for the Neotoma Paleoecological Database and the Paleobiology Database.  This combined API provides access to biological records obtained from fossil samples spanning from the present to the origin of life on earth.

### Development

*We welcome contributions from any individual, whether code, documentation, or issue tracking.  All participants are expected to follow the [code of conduct](https://github.com/EarthLifeConsortium/earthlife/blob/master/code_of_conduct.md) for this project.*

+ [Simon Goring](http://downwithtime.wordpress.com) - University of Wisconsin-Madison, Department of Geography

## Example

Find all fossil records for the genus *Canis* and include all lower taxa (all species).

```R

# Find all Canis related fossils:
all_canis <- get_by_taxon("Canis", lower = TRUE)

```
