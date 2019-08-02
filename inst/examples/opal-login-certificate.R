library(opalr)
options(opal.opts=list(
      sslcert='certificate.pem',
      sslkey='key.pem',
      ssl.verifypeer=0,
      ssl.verifyhost=0))
o <- opal.login(url='https://localhost:8443')
opal.projects(o)
opal.logout(o)

