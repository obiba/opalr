# Load the required libraries on client side and login in Opal
library(opalr)
o <- opal.login('administrator', 'password', 'http://opal-demo.obiba.org')

# Assign some variables into a data.frame with associated *D* symbol in R on Opal server side:
opal.assign(o,'D','FNAC.FNAC',variables=c('SVUOSI','SUKUP','PITUUS','PAINO'))

# Preview the assigned data.frame:
opal.execute(o,'head(D)')

#Summary of the assigned data.frame:
opal.execute(o,'summary(D)')

# Histogram figure of the *PITUUS* variable:
plot(opal.execute(o,'hist(D$PITUUS)'))

# Loading *ggplot2* library in R on Opal server side... This will fail if *ggplot2* is not installed in R server environment.
opal.execute(o, 'library(ggplot2)')

# Plot *PITUUS* vs. *PAINO* with "lm" smoothing:
opal.execute(o,'qplot(PITUUS,PAINO, data=D) + geom_smooth(method="lm")')

# Cleaning the resources on Opal server side...
opal.logout(o)
