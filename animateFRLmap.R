###################################
## Animating the FRL Map ##########
###################################

# Version 0.7
# Date: 08/21/2012
# Author: Jared E. Knowles, Policy Research Advisor DPI
# Data sources: WINSS (http://data.dpi.state.wi.us/data/)
# WISEemaps (http://wisemaps.dpi.wi.gov)

library(animation)
# Need GraphicsMagick installed and in the path

setwd("plots")
# Avoid weird output in animation package
ani.options(outdir=getwd(),tempdir=getwd())

# Loop repeatedly in GIF
ani.options(loop = TRUE)
# May need to specify full path here
gm.convert("F:/FRLMap/plots/evenFRLmap*.png",output="evenFRLmap.gif")

ani.options(loop = TRUE)
# Loop once and then stop
ani.options(loop=1)
# May need to specify full path here
gm.convert("F:/FRLMap/plots/evenFRLmap*.png",output="evenFRLmap_noloop.gif")

