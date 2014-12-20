#' ---
#' title: "Replication: Orbital Eccentricity-Multiplicity Relation"
#' author: "Richard Dallaway"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 2
#'    fig_width: 9
#'    fig_height: 7 
#' ---

# install.packages("plyr")
# install.packages("Hmisc")

#' Introduction
#' ------------
#' 
#' TODO


library(plyr)
library(Hmisc)
library(lattice)
library(ggplot2)
library(boot)

#' The Dataset
#' -----------
#' 

planets.all <- read.csv("data/exoplanets.1392162267.csv")

#' A summary of the discovery methods in this dataset. 
#' (why so many rows with no discovery method?)
table(planets.all$PLANETDISCMETH)

#' > "We chose to use RV data only for our analysis since the planets in that data set  typically have known and relatively reliably measured eccentricities."

#'  ...

#' > "If the eccentricity of the planet was not listed or if it was given as zero, the exoplanet was excluded from our sample."

planets.selected <- subset(planets.all, PLANETDISCMETH == "RV" & ECC != 0.0)
cat("Planets in selected dataset: ", nrow(planets.selected))  # Expecting 403

#' Data for the Solar System (see _References_ for sources):
mercury <- data.frame(STAR="Sun", ECC=0.20563593, A=0.38709927, NCOMP=8) 
venus   <- data.frame(STAR="Sun", ECC=0.00677672, A=0.72333566, NCOMP=8)
earth   <- data.frame(STAR="Sun", ECC=0.01671123, A=1.0,        NCOMP=8)
mars    <- data.frame(STAR="Sun", ECC=0.0933941,  A=1.523662,   NCOMP=8)
jupiter <- data.frame(STAR="Sun", ECC=0.04838624, A=5.2028870,  NCOMP=8)
saturn  <- data.frame(STAR="Sun", ECC=0.05386179, A=9.53667594, NCOMP=8)
uranus  <- data.frame(STAR="Sun", ECC=0.04725744, A=19.189165,  NCOMP=8)
neptune <- data.frame(STAR="Sun", ECC=0.00859048, A=30.069923,  NCOMP=8)

#' Append Solar System to the selected data set:
planets.selected <- rbind.fill(planets.selected, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune)

#' > "The 5- and 6-planet systems, one of each, were combined into one bin so that there was sufficient data for a statical analysis."

#' Apply the bins to the data as a new multiplicity factor column (called `mfactor`):
planets.selected$mfactor <- cut(planets.selected$NCOMP, 
    breaks=c(0,1,2,3,4,6,7,8), 
    labels=c("1 Planet", "2 Planets", "3 Planets", "4 Planets", "5 or 6 Planets", "7 Planets", "8 Planets")
)

# For plots, it is useful to have 5.5 for "5-6 Planets". This will be `$numplanets`
planets.selected$numplanetsFactor <- cut(planets.selected$NCOMP, 
     breaks=c(0,1,2,3,4,6,7,8), 
     labels=c(1,2,3,4,5.5,7,8)
)
planets.selected$numplanets <- as.numeric(as.character(planets.selected$numplanetsFactor))


#' Table 1: No. planets in dataset for given multiplicity
setNames(
  aggregate(STAR~mfactor,planets.selected,FUN="length"),
  c("Multiplicity", "Total number of planets")
)

#' A Trend in Multiplicity versus Eccentricity
#' -------------------------------------------
#' 

eccs <- planets.selected[, c("mfactor", "ECC")]

# See `?par` for a description formatting parameters such as `xaxs` and `las`
Ecdf(eccs$ECC, group=eccs$mfactor, 
     log          = "x", 
     xlim         = c(10^-3,1), 
     ylim         = c(0,1), 
     subtitle     = FALSE, 
     label.curves = FALSE,
     xlab         = "Eccentricity",
     ylab         = "Cumulative Distribution Function (CDF)",
     xaxs = "i", yaxs = "i", las = "1",
     lty = c("longdash", "solid"),
     col = c("blue", "green", "red", "cyan", "purple", "black"))

# The call to `factor` avoids plotting factors that do not occur in the data set. I.e, "7 planets"
legend("topleft", legend=levels(factor(eccs$mfactor)), 
    inset = 0.05, bty = "n",
    lty   = c("longdash", "solid"),
    col   = c("blue", "green", "red", "cyan", "purple", "black"))
title("Fig. 1: Cumulative eccentricity distributions, by multiplicity")

xyplot(ECC ~ A | mfactor, planets.selected, 
  xlim   = c(10^-2,10^2),
  scales = list(x=list(alternating=FALSE,log=TRUE)),
  ylab   = "Eccentricity", 
  xlab   = "Sem-major Axis (AU)",
  layout = c(7,1),
  drop.unused.levels = TRUE, 
  main   = "Fig. 2: 'Eccentricity verses semi-major axis going from low- (left) to high-multiplicity (right)'"
)

summaryStats <- ddply(
  planets.selected, c("numplanets"), 
  summarise, 
  N    = length(ECC), 
  mean = mean(ECC),
  median = median(ECC)
)


# boot(planets.selected, statistic=function(d,i){mean(d$ECC[i])},R=1000)
# foo <- subset(eccs, mfactor %in% c("8 Planets"))

plot(summaryStats$numplanets, summaryStats$mean, log="xy", xlab="Number of planets", ylab="Eccentricity", ylim=c(0.03,0.3), yaxs="i", type="b", pch=5, col="blue")
points(summaryStats$numplanets, summaryStats$median, type="b", col="red", lty="longdash")
grid()
legend(x=4.5, y=0.25,
  legend=c("Mean Eccentricity","Median Eccentricity"),
  lty=c("solid","longdash"),
  col=c("blue","red"),
  pch=c(5,1),
  box.col=NA
)
title("Fig. 3: Mean and median RV eccentricity by multiplicity (number of planets)")

#' References
#' ----------
#' 
#' * [Mercury, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Mercury&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Venus, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Venus&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Earth, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Earth&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Mars, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Mars&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Jupiter, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Jupiter&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Saturn, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Saturn&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Uranus, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Uranus&Display=Facts&System=Metric), accessed: 2014-10-21.
#' * [Neptune, SSE Facts & Figures, NASA](http://solarsystem.nasa.gov/planets/profile.cfm?Object=Neptune&Display=Facts&System=Metric), accessed: 2014-10-21.
#' 



