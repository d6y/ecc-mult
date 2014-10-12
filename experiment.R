#' ---
#' title: "Replication: Orbital Eccentricity-Multiplicity Relation"
#' author: "Richard Dallaway"
#' 
#' ---
#' 


#' Summary
#' -------
#' 
#' (text to go here)


# install.packages("plyr") 
require(plyr)

#' The Dataset
#' -----------
#' 
#' (text to go here)

planets.all <- read.csv("data/exoplanets.1392162267.csv")

#' A summary of the discovery methods in this dataset:
table(planets.all$PLANETDISCMETH)

#' > "We chose to use RV data only for our analysis since the planets in that data set  typically have known and relatively reliably measured eccentricities."

#'  ...

#' > "If the eccentricity of the planet was not listed or if it was given as zero, the exoplanet was excluded from our sample."

planets.selected  <-subset(planets.all,  PLANETDISCMETH == "RV" & ECC != 0.0)
cat("Planets in selected dataset:    ", nrow(planets.selected))  # Expecting 403

#' Data for the Solar System.
#' Source:   http://en.wikipedia.org/wiki/List_of_gravitationally_rounded_objects_of_the_Solar_System#Planets
#' (accessed: 2014-08-31)
mercury <- data.frame(STAR="Sun", ECC=0.20563069, NCOMP=8) 
venus   <- data.frame(STAR="Sun", ECC=0.00677323, NCOMP=8)
earth   <- data.frame(STAR="Sun", ECC=0.01671022, NCOMP=8)
mars    <- data.frame(STAR="Sun", ECC=0.09341233, NCOMP=8)
jupiter <- data.frame(STAR="Sun", ECC=0.04839266, NCOMP=8)
saturn  <- data.frame(STAR="Sun", ECC=0.05415060, NCOMP=8)
uranus  <- data.frame(STAR="Sun", ECC=0.04716771, NCOMP=8)
neptune <- data.frame(STAR="Sun", ECC=0.00858587, NCOMP=8)

#' Append Solar System to the selected data set:
planets.selected <- rbind.fill(planets.selected, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune)

#' > "The 5- and 6-planet systems, one of each, were combined into one bin so that there was sufficient data for a statical analysis."

#' Apply the bins to the data as a new multiplicity factor column:
planets.selected$mfactor <- cut(planets.selected$NCOMP, 
    breaks=c(0,1,2,3,4,6,7,8), 
    labels=c("1 Planet", "2 Planets", "3 Planets", "4 Planets", "5 or 6 Planets", "7 Planets", "8 Planets")
)



#' Table 1: No. planets in dataset for given multiplicity
aggregate(STAR~mfactor,planets.selected,FUN="length")


