http://stackoverflow.com/questions/34291440/error-when-deploying-shiny-app-that-depends-on-non-cran-package

Actual question
----
When trying to deploy a shiny app that depends on a non-CRAN/self-authored package to [shinyapps.io](http://www.shinyapps.io/) via `rsconnect::deployApp()`, I get the following error:

    Preparing to deploy application...DONE
    Uploading bundle for application: 73721...Fehler in findLocalRepoForPkg(pkg, repos, fatal = fatal) : 
      No package 'timetrackr' found in local repositories specified
    Ruft auf: <Anonymous> ... getPackageRecordsLocalReposImpl -> findLocalRepoForPkg
    Ausf�hrung angehalten

What do I need to do in order to get a self-authored package dependency over to [shinyapps.io](http://www.shinyapps.io/)?

Details
----
Reading the [getting started manual](http://shiny.rstudio.com/articles/shinyapps.html) initially gave me the impression that package dependencies are automatically transferred/installed on [shinyapps.io](http://www.shinyapps.io/)

> Package dependencies
>
> When you deploy your application, the rsconnect package attempts to detect the packages that your application uses. rsconnect sends this list of packages and their dependencies along with your application to the shinyapps.io service. Then shinyapps.io builds and installs the packages into the R library for your application. The first time you deploy your application, it may take some time to build these packages (depending on how many packages are used). However, you will not wait for these packages to build during future deployments (unless you upgrade or downgrade a package).

After reading the error message and thinking about, I rather suspect that my package must live in a publicly accessible repository. **Is this correct?**

I did make that the package in question, [`timetrackr`](https://github.com/rappster/timetrackr/tree/stackoverflow_20151215), is in fact installed (checked the library directory returned by `.libPaths()`:
    1. Via RStudio `CTRL + STRG + B`
    2. Via `devtools::build()` and `devtools::install()`
    3. Via `devtools::install_github("rappster/timetrackr")

None of the above worked 
