# `suggests`

_Feature-Enhanced Packages_

Fearless feature enhanced packages. Give clearer messaging about features,
seamlessly integrate fallback code and clean up your suggests control-flow.

## What's so hard about suggested packages?

[`Suggests` dependencies](https://cran.r-project.org/doc/manuals/R-exts.html#Suggested-packages)
are packages that are not strictly required for a package to function.
They can be used to give more context for a package, such as in examples,
vignettes and testing - behaviors outside of package use. However, `Suggests`
serve a dual purpose of enhancing package behaviors at runtime. This is the use
case that this package aims to improve.

Let's first take a look at an example in base R from 
[the `R` manual](https://cran.r-project.org/doc/manuals/R-exts.html#Suggested-packages), 
explaining how a suggested package might enable a new feature:

```r
if (requireNamespace("rgl", quietly = TRUE)) {
  rgl::plot3d(...)
} else {
  ## do something else not involving rgl.
}
```

So far so good. If the package is able to be loaded it's used, otherwise we have
some code to fall back to. But this code has its issues - in the order of
increasing severity, it:

- is tedious to constantly re-write this boilerplate.
- forces divergent package logic.
- will quietly fail to use advanced features without any indication to the user.
- does not consider version constraints specified in the `DESCRIPTION` file.

Of course these can all be addressed in code, but it's not exactly trivial.

## Feature Overview

### Importing Suggested Package Namespaces

Importing a suggested package creates a placeholder that can be used just like a
package namespace.

```r
cli <- suggests::import(cli)
survey_question <- function() {
  cli::cli_h1("What sort of R user are you?")
  menu(c("tidy", "base", "both"))
}
```

The first time the `cli` "namespace" is accessed, `suggests` will work in the 
background to ensure the version that can be loaded will satisfy your version
constraints in your package's `DESCRIPTION`. If it isn't satisfied, your users
will automatically get an informative message:

```
Error: Suggested package 'cli' is required for this functionality, but
dependency version requirement is not met. Version 3.5.0 is installed, but must
be >=3.6.0.
```

> ***Tip***
>
> Use `suggests::enable_capabilities("create")` to automatically create a
> suggests  package object that shares the package's name in your package
> environment.
>
> ```r
> suggests::enable_capabilities("create")
> suggests::import(cli)  # creates "namespace" cli
> ```

### Simple Alternatives

Maybe you don't want to gate this feature off entirely from your users, you just
want it to look prettier for the folks that already have `cli` installed.

For one-off cases, you can use the `%?s%` operator, which is a shorthand for "if
suggests package is available, else". It works like a `tryCatch()` specifically
for errors arising from failing to load suggested packages.

```r
#' @import suggests
NULL

cli <- suggests::import(cli)
survey_question <- function() {
  msg <- "What sort of R user are you?"
  cli::cli_h1(msg) %?s% cat(msg, "\n")
  menu(c("tidy", "base", "both"))
}
```

### Package-wide Alternatives

If you only use a handful of functions from a suggested package, and you could
reasonably provide simpler versions of those behaviors yourself, then you might
consider defining package-wide fallbacks when those functions are unavailable.

```r
cli <- suggests::import(cli)

suggests::fallback(cli::cli_h1, function(text, ...) {
  cat(text, "\n")  
})

survey_question <- function() {
  cli::cli_h1("What sort of R user are you?")
  menu(c("tidy", "base", "both"))
}
```

Now, even though `cli` might not be available, when the suggested `cli`
namespace is accessed, it will automatically fall back to using the provided
function.

### Seamless Features

> :construction_worker: _Under Construction_ :construction_worker:
> 
> None of these features work! They're aspirational for now.

If you categorize your `Suggests` packages into features, `suggests` can
discover them and provide a more user-friendly interface for handling feature-
enhancing suggests packages.

```dcf
Suggests:
    cli (>= 100.0.0),
    cli (<= 100.0.1),
    ggplot2
Config/suggests/feature/cli:
    cli
Config/suggests/feature/graphics:
    ggplot2
```

Further details can be provided in `inst/FEATURES`

```dcf
Feature: cli
Title: Enhanced Command Line Interface (CLI)
Description: 
    Provides a prettier and more colorful command line interface when used
    interactively in a terminal session.

Feature: graphics
Title: Data Visualizations and other Graphical Outputs
```

Or generated using `roxygen2`

```r
#' @feature cli
#' Enhanced Command Line Interface (CLI)
#'
#' Provides a prettier and more colorful command line interface when used
#' interactively in a terminal session.
#'
#' @feature graphics
#' Data Visualizations and other Graphical Outputs
```

#### Prompt Feature-Enhancing Suggests Installation on Install

When installed interactively, users are provided with information regarding
packages they might consider installing to enable additional package features.

```r
install.packages(pkg)
#> Package `pkg` provides optional features which require additional
#> dependencies:
#>
#>   1. Enhanced Command Line Interface (CLI):
#>         cli (>= 1.0.0)
#>
#> Would you like to install suggested dependencies? 
#> (y/N/comma-separated feature numbers)
```

#### Exploring Available Features

Even if suggested packages aren't installed from the start, users can easily 
explore package features to get a report about what other features might be 
available.

```r
pkg::features()
#> Enhanced Command Line Interface (CLI)
#>     x  cli (>= 1.0.0)
```

#### Generating Feature Documentation

```r
?pkg::features
```

```
features                  package:pkg                   R Documentation

pkg Features

Usage:

    pkg::features()

Description:

    Reports package features and the status of dependencies required for each.

Features:

    Enhanced Command Line Interface (CLI):

        Provides a prettier and more colorful command line interface when used
        interactively in a terminal session.

        Requires

            cli (>= 1.0.0)
```

