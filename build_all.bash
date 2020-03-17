#!/bin/bash

set -e
set -x

R --vanilla --slave -e "devtools::test()"

R --vanilla --slave -e "devtools::install(upgrade = 'never')"
R --vanilla --slave -e "devtools::document()"
R --vanilla --slave -e "devtools::build_vignettes(quiet = FALSE)"
R --vanilla --slave -e "devtools::build_site(quiet = FALSE)"