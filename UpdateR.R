#1. Before you upgrade, build a temp file with all of your old packages.

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")
# 2. Install the new version of R and let it do it’s thing.
# 
# 3. Once you’ve got the new version up and running, reload the saved packages and re-install them from CRAN.

tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()
# If you had any packages from BioConductor, you will need to reload those as well.
# 
# chooseBioCmirror()
# biocLite() 
# load("installed_old.rda")
# tmp <- installed.packages()
# installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
# missing <- setdiff(installedpkgs, installedpkgs.new)
# for (i in 1:length(missing)) biocLite(missing[i])
# All done, now you can get back to cracking out R code. This method helped me save a lot of time, hope someone else finds it useful!

