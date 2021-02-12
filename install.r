# install gdal
system('sudo apt-get update')
system('sudo apt-get install libgdal-dev libproj-dev')
# install required R packages
install.packages(c("rgdal","raster","mapdata","maptools","downloader","directlabels","rasterVis","XML","ncdf4","kableExtra","rjson"), repos='http://cran.rstudio.com/')
install.packages('ggplot2')
install.packages('ggmap')
install.packages('ggrepel')
install.packages('rgeos')
install.packages('sp')
