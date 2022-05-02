
Subversion commands
---------------------
```
# See differences in your local copy
git diff
# Commit your changes to the local repository
git commit -m "Explain your changes"
# Update your local copy with changes from remote repository (github)
git pull --rebase
# Send your changes to the remote repository
git push
# See differences between branches
git diff master
# Merge changes from master into your branch
git rebase -i master
# Get help
git help
```

Building, installing and testing
--------------------------------

* `make build` :   Build package

* `make install` :   Build and install the package
  
* `make cran` :   Check that the build is acceptable by CRAN

* `make check` :  More thoroughly test the build

* `make quick-install` : Do a minimal build and install that has as few
  requirements as possible. Designed for installing on machines that are not
  used for development, like IRIDIA's cluster.


Importing from packages
-----------------------

We do not use this: 

```R
#' @importFrom DT datatable
```

because we always use `DT::datatable`. By using `importFrom`, the name
`'datatable'` is imported and we cannot use it for something else.  The
criterion should be:

 * If we use a lot of functions from a package, import the whole package with `@import`.
 * If we use a lot a few functions from a package, `@importFrom` those functions.
 * Otherwise, use `package::function` so we don't have to import anything.
