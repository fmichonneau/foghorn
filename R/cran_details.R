#' @importFrom tibble tibble
cran_details_from_web <- function(pkg, ...) {
    parsed <- read_cran_web_from_pkg(pkg)
    issue_test <- has_other_issues(parsed)

    all_p <- lapply(parsed, function(x) {
        p <- xml2::xml_find_all(x, ".//p")
        p <- strsplit(xml2::xml_text(x), "\n")
        p <- unlist(p)
        p <- p[nzchar(p)]
        p <- gsub(intToUtf8(160), " ", p)
        chk_idx <- grep("^Check:", p)
        res_idx <- grep("^Result:", p)
        flv_idx <- grep("^Flavors?:", p)
        if (!identical(length(chk_idx), length(res_idx)) &&
            !identical(length(chk_idx), length(flv_idx)))
            stop("File an issue on Github indicating the name of your package.")
        msg <- mapply(function(c, r, f) {
            tibble::tibble(
                result = gsub("^Result: ", "", p[r]),
                check = gsub("^Check: ", "", p[c]),
                flavors = gsub("^Flavors?: ", "", p[f]),
                message = paste(p[(r + 1):(f - 1)], collapse = "\n")
            )
        }, chk_idx, res_idx, flv_idx, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        dplyr::bind_rows(msg)
    })
    names(all_p) <- pkg
    res <- dplyr::bind_rows(all_p, .id = "Package")
    attr(res, "other_issues") <- issue_test
    res
}



##' Given the names of packages published on CRAN, return the output of
##' checks that return notes, warnings or errors.
##'
##' @title Get details about the CRAN check results for packages
##' @param pkg character vector of the names for the packages on CRAN
##' @param object an object created by \code{cran_details}
##' @template src
##' @template dots
##' @template details
##' @return a \code{tibble} listing the names of the packages that have non- OK
##'     check results, the nature of the result (\code{WARN}, \code{ERROR},
##'     \code{FAIL}, \code{NOTE}, or other issues).
##' @export
##' @importFrom crayon bold
cran_details <- function(pkg, src = c("website", "crandb"),
                         ...) {
    if (!is.character(pkg))
        stop(sQuote("pkg"), " is not a string.", call. = FALSE)

    src <- match.arg(src, c("website", "crandb"))

    if (identical(src, "website"))
        res <- cran_details_from_web(pkg)
    else if (identical(src, "crandb"))
        res <- cran_details_from_crandb(pkg, ...)

    class(res) <- c("cran_details", class(res))
    res
}



##' @importFrom clisymbols symbol
render_flavors <- function(x) {
    ## transform the comma separated list of platform flavors into
    ## unordered list
    res <- unlist(strsplit(x, ", "))
    paste("  ", clisymbols::symbol$pointer, res, "\n")
}


##' @param show_log Should the messages of the \dQuote{Check Details}
##'     be printed? (logical)
##' @rdname cran_details
##' @export
##' @importFrom purrr pmap
##' @importFrom crayon green
##' @importFrom clisymbols symbol
summary.cran_details <- function(object, show_log = TRUE, ...) {

    no_result <- setdiff(attr(object, "other_issues")$Package, object$Package)
    if (length(no_result) > 0) {
        message(crayon::green(clisymbols::symbol$tick, "All clear for ",
                              paste0(no_result, collapse = ", "), "!"))
    }

    if (nrow(object) < 1)
        return(invisible(object))

    purrr::pmap(attr(object, "other_issues"), function(Package, has_other_issues) {
        if (has_other_issues)
            cat(foghorn_components[["has_other_issues"]]$color(
                  paste(foghorn_components[["has_other_issues"]]$symbol,
                        crayon::bold(Package), "has other issues")), "\n")
        })

    purrr::pmap(object, function(Package, result, check, flavors, message)  {
        cmpt <- foghorn_components[[result]]
        if (show_log)
            msg <- message
        else
            msg <- character(0)
        cat(## Type of CRAN message
            cmpt$color(paste0(cmpt$symbol, " ",
                              crayon::bold(paste0(Package, " - ", result)),
                              ": ", check)), "\n",
            ## Flavors concerned
            render_flavors(flavors), "\n",
            ## Optionally the log output
            msg, "\n\n", sep = "")
    })

    invisible(object)
}
