# Install Bioconductor core packages:
source("http://bioconductor.org/biocLite.R")
biocLite()

### this had issues- found the following fix online
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.12")

# For this class, you'll also need RTCGA and RTCGA data packages
biocLite("RTCGA")
biocLite("RTCGA.clinical")
biocLite("RTCGA.mRNA")

BiocManager::install(ask=FALSE)
library(BiocManager)
avail <- BiocManager::available()
a

install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
