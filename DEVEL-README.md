Git commands
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



RELEASE Process:
----------------

TODO: See useful release steps here: https://github.com/tidyverse/ggplot2/issues/4965

1. git status # make sure you are up to date and clean

2. make check # passes

3. make releasecheck 

4. make examples # Takes a few hours. Inspect the examples in the vignette.

5. make revdepcheck # Takes a few hours

6. make releasebuild # Inspect the output for strange files!

7. make closeversion

8. Update `cran-comments.md`

9. make submit

11.a IF the package requires further changes:

  * Make the changes.

  * Repeat the whole RELEASE process above without bumping the version number.


11.b IF the package is released in CRAN:

  * Bump the version number in DESCRIPTION and NEWS.md.

  * make build # To update other files with the new version."

  * git ci -a -m " * Bump development version to $NEW_VERSION"

12. Announce the release in the Google group:

    https://groups.google.com/d/forum/irace-package


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
