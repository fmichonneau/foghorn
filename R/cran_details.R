##' @importFrom xml2 xml_find_all xml_text
##' @importFrom dplyr %>% bind_rows
##' @importFrom tibble tibble
has_other_issues_details <- function(parsed, ...) {
      pkg <- all_packages(parsed)

      lapply(parsed, function(x) {
          other_issues_pth <-  xml2::xml_find_all(x, ".//h3/a/following::p[1]//a")
          issue_type <- xml2::xml_text(other_issues_pth)
          issue_url <- xml2::xml_find_all(other_issues_pth, "@href") %>% xml2::xml_text()
          tibble::tibble(result = "other_issue", check = issue_type,
                         flavors = NA_character_,
                         message = paste0("See: <", issue_url, ">"))
      }) %>%
          dplyr::bind_rows(.id = "Package")

}


#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange
cran_details_from_web <- function(pkg, ...) {
    parsed <- read_cran_web_from_pkg(pkg)
    issue_test <- has_other_issues_details(parsed)

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
    res <- dplyr::bind_rows(all_p, .id = "Package") %>%
        dplyr::bind_rows(issue_test) %>%
        dplyr::arrange(.data$Package)
    res
}



add_other_issues_crandb <- function(tbl, ...) {
    issues <- get_cran_rds_file("issues", ...)
    res <- vapply(tbl[["Package"]], function(x) {
        pkg_issues <- issues[issues$Package == x, ]
        if (nrow(pkg_issues) > 0) {
            paste(pkg_issues$kind, collapse = ", ")
        } else ""
    }, character(1), USE.NAMES = FALSE)
    dplyr::mutate(tbl, "has_other_issues" = nchar(res) > 0)
}

##' @importFrom dplyr group_by ungroup distinct mutate_if select mutate bind_rows arrange
##' @importFrom tibble tibble
##' @importFrom rlang .data
cran_details_from_crandb <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    issues <- get_cran_rds_file("issues", ...)

    dt <- dt[dt[["Package"]] %in%  pkg, ]

    ## remove lines that don't have any issues
    dt <- dt[dt[["Check"]] != "*", ]

    dt$Status <- gsub("WARNING", "WARN", dt$Status)

    res <- dt %>%
        tibble::as_tibble() %>%
        dplyr::group_by(.data$Package, .data$Output) %>%
        dplyr::mutate(flavors = paste(.data$Flavor, collapse = ", ")) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(.data$Package, .data$Output, .keep_all = TRUE) %>%
        dplyr::select(Package = .data$Package,
                      result = .data$Status,
                      check = .data$Check,
                      flavors = .data$flavors,
                      message = .data$Output) %>%
        dplyr::mutate_if(is.factor, as.character)

    other_issues <- dplyr::filter(issues, .data$Package %in% pkg) %>%
        tibble::as_tibble() %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::select(Package = .data$Package,
                      check = .data$kind,
                      message = .data$href) %>%
        dplyr::mutate(
                   result = "other_issue",
                   flavors = NA_character_)

    res <- dplyr::bind_rows(res, other_issues) %>%
        dplyr::arrange(.data$Package)
    res
}


##' Given the names of packages published on CRAN, return the output of
##' checks that return notes, warnings or errors.
##'
##' @title Get details about the CRAN check results for packages
##' @param pkg character vector of the names for the packages on CRAN
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
    attr(res, "pkgs") <- pkg
    res
}



##' @importFrom clisymbols symbol
render_flavors <- function(x) {
    ## transform the comma separated list of platform flavors into
    ## unordered list
    if (!is.na(x)) {
        res <- unlist(strsplit(x, ", "))
        paste("  ", clisymbols::symbol$pointer, res, "\n")
    } else ""
}


##' @param object an object created by \code{cran_details}
##' @param show_log Should the messages of the \dQuote{Check Details}
##'     be printed? (logical)
##' @rdname cran_details
##' @export
##' @importFrom purrr pmap
##' @importFrom crayon green
##' @importFrom clisymbols symbol
summary.cran_details <- function(object, show_log = TRUE, ...) {

    no_result <- setdiff(attr(object, "pkgs"), object$Package)
    if (length(no_result) > 0) {
        message(crayon::green(clisymbols::symbol$tick, "All clear for",
                              paste0(no_result, collapse = ", "), "!"))
    }

    if (nrow(object) < 1)
        return(invisible(object))

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


##' @export
##' @rdname cran_details
summary_cran_details <- function(pkg, src = c("website", "crandb"),
                                 show_log = TRUE, ...) {
    res <- cran_details(pkg = pkg, src = src, ...)
    summary(res)
}
