##' @importFrom xml2 xml_find_all xml_text
##' @importFrom tibble tibble
has_other_issues_details <- function(parsed, ...) {
      pkg <- all_packages(parsed)

      res <- lapply(parsed, function(x) {
          other_issues_pth <-  xml2::xml_find_all(x, ".//h3/a/following::p[1]//a")
          issue_type <- xml2::xml_text(other_issues_pth)
          issue_url <- xml2::xml_text(xml2::xml_find_all(other_issues_pth, "@href"))
          tibble::tibble(result = "other_issue", check = issue_type,
                         flavors = NA_character_,
                         message = paste0("See: <", issue_url, ">"))
      })

      pkgs <- rep(unlist(pkg), vapply(res, function(x) nrow(x) %||% 0L, integer(1)))
      res <- do.call("rbind", res)
      res <- cbind(Package = pkgs, res, stringsAsFactors = FALSE)
      tibble::as.tibble(res)
}


#' @importFrom tibble tibble
cran_details_from_web <- function(pkg, ...) {
    parsed <- read_cran_web_from_pkg(pkg)
    issue_test <- has_other_issues_details(parsed)

    all_p <- mapply(function(x, pkg_nm) {
        p <- xml2::xml_find_all(x, ".//p")
        p <- strsplit(xml2::xml_text(x), "\n")
        p <- unlist(p)
        p <- p[nzchar(p)]
        p <- gsub(intToUtf8(160), " ", p)
        chk_idx <- grep("^Check:", p)
        vrs_idx <- grep("^Version:", p)
        res_idx <- grep("^Result:", p)
        flv_idx <- grep("^Flavors?:", p)
        if (!identical(length(chk_idx), length(res_idx)) &&
            !identical(length(chk_idx), length(flv_idx)))
            stop("File an issue on Github indicating the name of your package.")
        msg <- mapply(function(c, v, r, f) {
            tibble::tibble(
                version = gsub("^Version: ", "", p[v]),
                result = gsub("^Result: ", "", p[r]),
                check = gsub("^Check: ", "", p[c]),
                flavors = gsub("^Flavors?: ", "", p[f]),
                n_flavors = length(unlist(gregexpr(",", p[f]))) + 1,
                message = paste(p[(r + 1):(f - 1)], collapse = "\n")
            )
        }, chk_idx, vrs_idx, res_idx, flv_idx, SIMPLIFY = FALSE, USE.NAMES = FALSE)

        r <- do.call("rbind", msg)
        if (!is.null(r)) {
            r <- cbind(Package = rep(pkg_nm, nrow(r)), r, stringsAsFactors = FALSE)
        } else {
            r <- tibble::add_row(default_cran_details,
                                 Package = pkg_nm,
                                 version = paste(unique(get_cran_table(parsed[pkg_nm])$Version), collapse = ", "),
                                 result = "OK",
                                 check = "",
                                 flavors = "",
                                 n_flavors = n_cran_platforms,
                                 message = "")
        }
    }, parsed, names(parsed), SIMPLIFY = FALSE)
    res <- do.call("rbind", all_p)
    tibble::as.tibble(res[order(res$Package), ])
}



##' @importFrom tibble tibble
cran_details_from_crandb <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    issues <- get_cran_rds_file("issues", ...)

    col_is_factor <- vapply(dt, is.factor, logical(1))

    for (i in seq_along(col_is_factor)) {
        if (col_is_factor[i])
            dt[[i]] <- as.character(dt[[i]])
    }

    dt <- dt[dt[["Package"]] %in%  pkg, ]
    dt$Status <- gsub("WARNING", "WARN", dt$Status)
    .res <- default_cran_details

    for (.p in unique(dt$Package)) {
        dt_p <- dt[dt$Package == .p, ]
        for (.v in unique(dt_p$Version)) {
            dt_v <- dt_p[dt_p$Version == .v, ]
            for (.c in unique(dt_v$Check)) {
                .sub <- dt_v[dt_v$Check == .c, ]
                .rslt <- unique(.sub$Status)
                if (length(.rslt) > 1)
                    stop("report on github")

                .res <- tibble::add_row(.res,
                    Package = .p,
                    version = .v,
                    result = .rslt %~~% "",
                    check = .c,
                    flavors = paste(.sub$Flavor, collapse = ", ") %~~% "",
                    n_flavors = length(.sub$Flavor),
                    message = .sub$Output[1]

                    )
            }
        }
    }

    ## remove lines that don't have any issues
    .res$check <- replace(.res$check, .res$check == "*", "")

    issues <- issues[issues[["Package"]] %in% pkg, ]
    issues <- tibble::as.tibble(issues)
    .iss_pkg <- as.character(issues$Package)
    .iss_vrs <- as.character(issues$Version)
    .iss_chk <- as.character(issues$kind)
    .iss_msg <- as.character(issues$href)
    .iss_res <- rep("other_issue", nrow(issues))
    .iss_flvr <- character(nrow(issues))
    .iss_n_flvr <- integer(nrow(issues))

    .iss <- tibble::tibble(
                        Package = .iss_pkg,
                        version = .iss_vrs,
                        result = .iss_res,
                        check = .iss_chk,
                        flavors = .iss_flvr,
                        n_flavors = .iss_n_flvr,
                        message = .iss_msg
                    )

    res <- rbind(.res, .iss)
    convert_nas(res[order(res$Package), ], replace_with = "")

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
##' @template print_ok
##' @rdname cran_details
##' @export
##' @importFrom crayon green
##' @importFrom clisymbols symbol
summary.cran_details <- function(object, show_log = TRUE, print_ok = TRUE, ...) {

    res_ok <- object[object[["result"]] == "OK" & object[["n_flavors"]] == n_cran_platforms, ]
    if (nrow(res_ok) > 0 && print_ok) {
        print_all_clear(res_ok[["Package"]])
    }

    res_others <- object[object[["result"]] != "OK", ]

    if (nrow(res_others) < 1)
        return(invisible(object))

    mapply(function(Package, result, check, flavors, message)  {
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
    }, res_others$Package, res_others$result, res_others$check,
    res_others$flavors, res_others$message)

    invisible(object)
}


##' @export
##' @rdname cran_details
summary_cran_details <- function(pkg, src = c("website", "crandb"),
                                 show_log = TRUE, print_ok = TRUE, ...) {
    res <- cran_details(pkg = pkg, src = src, ...)
    summary(res, print_ok = print_ok)
}
