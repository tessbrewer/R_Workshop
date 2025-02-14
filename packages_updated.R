
packages = c('readxl', 'reshape2', 'RColorBrewer', 'growthcurver', 'ggforce', 'ggsignif', 'knitr', 
             'kableExtra', 'stringr', 'ggplot2', 'ggpmisc', 'scales', 'multcompView', 'pgirmess', 'ggvenn', 'BiocManager', 'EnhancedVolcano', 'tidyverse')

#don't be alarmed, this will spam a lot of text in the console
#it just installs the above packages if they are not already installed
for (package in packages){
  if (! is.element(package, .packages(TRUE))){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}


