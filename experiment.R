# install.packages("plyr") 
require(plyr)

## DATA SET ---------------------------------------------------------------------

planets.all       <- read.csv("data/exoplanets.1392162267.csv")
planets.by.method <- split(planets.all, planets.all$PLANETDISCMETH)

# Summary of the different discovert methods:
#ddply(planets.all, 'PLANETDISCMETH', function(m) c(PLANETCOUNT=nrow(m)))

# "We chose to use RV data only for our analysis since the planets in that data set 
# typically have known and relatively reliably measured eccentricities."

# "If the eccentricity of the planet was not listed or if it was given as zero, the
# exoplanet was excluded from our sample."

planets.selected  <- subset(planets.by.method$RV, ECC != 0.0)

cat("Cataloged RV planets in dataset:", nrow(planets.method$RV)) # Expecting 441
cat("Planets in selected dataset:    ", nrow(planets.selected))  # Expecting 403

# Data for the Solar System
# Source:   http://en.wikipedia.org/wiki/List_of_gravitationally_rounded_objects_of_the_Solar_System#Planets
# Accessed: 2014-08-31
mercury <- data.frame(STAR="Sun", ECC=0.20563069, NCOMP=8) 
venus   <- data.frame(STAR="Sun", ECC=0.00677323, NCOMP=8)
earth   <- data.frame(STAR="Sun", ECC=0.01671022, NCOMP=8)
mars    <- data.frame(STAR="Sun", ECC=0.09341233, NCOMP=8)
jupiter <- data.frame(STAR="Sun", ECC=0.04839266, NCOMP=8)
saturn  <- data.frame(STAR="Sun", ECC=0.05415060, NCOMP=8)
uranus  <- data.frame(STAR="Sun", ECC=0.04716771, NCOMP=8)
neptune <- data.frame(STAR="Sun", ECC=0.00858587, NCOMP=8)

# Append Solar System to the selected data set:
planets.selected <- rbind.fill(planets.selected, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune)

## BASIC COUNTS  ----------------------------------------------------------------

# For each star, a count of the number of planets (a) reported in the system and (b) in the dataset.
# "the multiplicity of a system may be three planets based on observations, however the 
# motion of those three planets may imply that there are additional companions and therefore the
# multiplicity of the system may be listed as four planets rather than three on exoplanet.org. In 
# all such cases, we simply adopt the data listed on the website for consistency."
star.planets      <- ddply(planets.selected, 'STAR', function(star) {
  c( multiplicity.sys = star$NCOMP[1:1], 
     multiplicity.obs = nrow(star) )
})


## TABLE 1 ---------------------------------------------------------------------
# For each system multiplicity, the total number of observed planets:
planets.by.multiplicity <- count(star.planets, 'multiplicity.sys', 'multiplicity.obs')

# "The 5- and 6-planet systems, one of each, were combined into one bin so
# that there was sufficient data for a statical analysis."
multiplicities.by.count <- c("1 Planet", "2 Planets", "3 Planets", "4 Planets", "5 or 6 Planets", "5 or 6 Planets", "7 Planets", "8 Planets (SS)")
binned <- within(planets.by.multiplicity, multiplicity <- multiplicities.by.count[multiplicity.sys])

table1 <- count(binned, 'multiplicity', 'freq')
print(table1)

