# Show the win-builder queue

Check whether your package is in the win-builder queue.

## Usage

``` r
winbuilder_queue(
  pkg = NULL,
  folders = c("R-release", "R-devel", "R-oldrelease")
)
```

## Arguments

- pkg:

  Optionally provide a vector of package names to limits the results to
  these packages.

- folders:

  Which folders of the win-builder queue do you want to inspect?
  Default: the 3 architectures win-builder provides.

## Value

A `tibble` with the following columns:

- package:

  package name

- version:

  package version

- folder:

  the folder indicating the R version that will be used to perform the
  checks

- time:

  the date and time at which the package tarball was uploaded on
  win-builder

- size:

  the size of the package tarball

## Details

To check whether your package has successfully been submitted to
win-builder, or to check whether there is unusual delay in processing
packages submitted to win-builder, `winbuilder_queue` allows you to
inspect the packages that are in the queue to be processed by the
win-builder service.

## References

- Maëlle Salmon, 2020. "Everything you should know about WinBuilder"
  <https://blog.r-hub.io/2020/04/01/win-builder/>

- Uwe Ligges. Building and checking R source packages for Windows.
  <https://win-builder.r-project.org/>

## Examples

``` r
if (FALSE) { # \dontrun{
  ## Get all the packages in the win-builder queue
  winbuilder_queue()
  ## Check if the 'dplyr' package is in the win-builder queue
  winbuilder_queue(pkg = "dplyr")
  ## Check which packages are in the R-devel queue
  winbuilder_queue(folders = "R-devel")
} # }
```
