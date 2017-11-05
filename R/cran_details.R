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
                message = paste(p[(r + 1):(f - 1)], collapse = "\n"),
                stringsAsFactors = FALSE
            )
        }, chk_idx, res_idx, flv_idx, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        dplyr::bind_rows(msg)
    })
    names(all_p) <- pkg
    res <- dplyr::bind_rows(all_p, .id = "Package")
    attr(res, "other_issues") <- issue_test
    res
}



##' Given a package name published on CRAN, print the outcomes of
##' checks that return notes, warnings or errors, and optionally print
##' the messages.
##'
##' @title Show the CRAN check results for a package
##' @param pkg name of the package on CRAN
##' @param show_log Should the messages of the \dQuote{Check Details}
##'     be printed? (logical)
##' @template src
##' @template dots
##' @template details
##' @return \code{NULL}, used for its side effect of printing the CRAN
##'     messages
##' @export
##' @importFrom crayon bold
cran_details <- function(pkg, src = c("website", "crandb"),
                         ...) {
    if (!(length(pkg) == 1 && is.character(pkg)))
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



summary.cran_details <- function(res, show_log = TRUE) {

    if (nrow(res) < 1) {
        message("All clear for ", paste(pkg, collapse = ", "))
        return(invisible(NULL))
    }


    apply(attr(res, "other_issues"), 1, function(x) {
        if (x[2])
            cat(foghorn_components[["has_other_issues"]]$color(
                  paste(foghorn_components[["has_other_issues"]]$symbol,
                        crayon::bold(x[1]), "has other issues")), "\n")
    })
    apply(res, 1, function(x)  {
        cmpt <- foghorn_components[[x[2]]]
        if (show_log)
            msg <- x[5]
        else
            msg <- character(0)
        cat(## Type of CRAN message
            cmpt$color(paste0(cmpt$symbol, " ",
                              crayon::bold(paste0(x[1], " - ", x[2])),
                              ": ", x[3])), "\n",
            ## Flavors concerned
            render_flavors(x[4]), "\n",
            ## Optionally the log output
            msg, "\n\n", sep = "")
    })
    invisible(NULL)
}
