# synaptome.db: programmatic access to the Synaptic proteome database
## Description
The package contains local copy of the Synaptic proteome database together with a set of utility R functions to query and analyse its content.

The process of data acqusition, organisation and database structure are described in the paper `Sorokina et al. 2021`.

## Installation

At the moment package could be installed from GitHub:
```
devtools::install_github('lptolik/synaptome.db')
```
### `GitHub` Versions

#### Stable version

Install the latest stable version (see below for latest [development](https://github.com/lptolik/synaptome.db/tree/develop#development-version) version) of `synaptome.db` from `GitHub` (as bug-free as it can be):

```{r}
devtools::install_github('lptolik/synaptome.db')
```

Check after installation that the following code does not throw any error and that the welcome message confirms you have installed [the latest version](https://github.com/lptolik/synaptome.db/blob/master/DESCRIPTION#L4):

```{r}
library(synaptome.db) 
#> Loaded synaptome.db ?.?.?
```

### Development version

You can also install the [development version](https://github.com/lptolik/synaptome.db/blob/develop/DESCRIPTION#L4) for new features yet to be widely tested:
```{r}
devtools::install_github('lptolik/synaptome.db@devel")
```

# TODO: add cache clearance description

# References
1. Sorokina O, Mclean C, Croning MDR, Heil KF, Wysocka E, He X, Sterratt D, Grant SGN, Simpson TI, Armstrong JD (2021). “A unified resource and configurable model of the synapse proteome and its role in disease.” Scientific Reports, 11(1), 9967–9. doi: 10.1038/s41598-021-88945-7, https://www.nature.com/articles/s41598-021-88945-7.

