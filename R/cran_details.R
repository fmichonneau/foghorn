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
        do.call("rbind", msg)
    })

    pkgs <- rep(pkg, vapply(all_p, function(x) nrow(x) %||% 0L, integer(1)))

    if (length(pkgs) > 0L) {
        res <- do.call("rbind", all_p)
        res <- cbind(Package = pkgs, res, stringsAsFactors = FALSE)
        res <- rbind(res, issue_test)
        tibble::as.tibble(res[order(res$Package), , drop = FALSE])
    } else default_cran_details
}



##' @importFrom tibble tibble
cran_details_from_crandb <- function(pkg, ...) {
    dt <- get_cran_rds_file("details", ...)
    issues <- get_cran_rds_file("issues", ...)

    dt <- dt[dt[["Package"]] %in%  pkg, ]

    ## remove lines that don't have any issues
    dt <- dt[dt[["Check"]] != "*", ]

    dt$Status <- gsub("WARNING", "WARN", dt$Status)

    cnt_by <- function(x, grp) {
        as.character(tapply(x, grp, function(.x) unique(as.character(.x)), simplify = FALSE))
    }
    grps <- list(dt$Package, dt$Output)

    .res_pkg <- cnt_by(dt$Package, grps)
    .res_res <- cnt_by(dt$Status, grps)
    .res_chk <- cnt_by(dt$Check, grps)
    .res_flvr <- as.character(tapply(dt$Flavor, list(dt$Package, dt$Output),
                                     function(x) paste(as.character(x),
                                                       collapse = ", ")))
    .res_msg <- cnt_by(dt$Output, grps)
    .res <- tibble::tibble(
                        Package = .res_pkg,
                        result = .res_res,
                        check = .res_chk,
                        flavors = .res_flvr,
                        message = .res_msg
                    )

    issues <- issues[issues[["Package"]] %in% pkg, ]
    issues <- tibble::as.tibble(issues)
    .iss_pkg <- as.character(issues$Package)
    .iss_chk <- as.character(issues$kind)
    .iss_msg <- as.character(issues$href)
    .iss_res <- rep("other_issue", nrow(issues))
    .iss_flvr <- character(nrow(issues))

    .iss <- tibble::tibble(
                        Package = .iss_pkg,
                        result = .iss_res,
                        check = .iss_chk,
                        flavors = .iss_flvr,
                        message = .iss_msg
                    )

    res <- rbind(.res, .iss)
    res[order(res$Package), ]

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
    }, object$Package, object$result, object$check,
    object$flavors, object$message)

    invisible(object)
}


##' @export
##' @rdname cran_details
summary_cran_details <- function(pkg, src = c("website", "crandb"),
                                 show_log = TRUE, ...) {
    res <- cran_details(pkg = pkg, src = src, ...)
    summary(res)
}
