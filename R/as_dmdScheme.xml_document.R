#' @param useSchemeInXml if \code{TRUE}, use scheme definition in xml and raise
#'   an error if the xml does not contain a scheme definition. If False, use the
#'   scheme definition from the corresponding installed package, even if the xml
#'   contains a scheme definition. if \code{NULL} (the default), use the
#'   definition in the xml if it contains a definition, if not use the
#'   corresponding definition from the installed package.
#'
#'
#' @importFrom xml2 xml_attrs as_list xml_name xml_children
#' @importFrom rlang :=
#'
#' @rdname as_dmdScheme
#' @export
#'
#' @examples
#' xml <- as_xml(dmdScheme_example())
#' x <- as_dmdScheme(xml)
#' all.equal(dmdScheme_example(), x)
#'
as_dmdScheme.xml_document <- function(
  x,
  keepData = TRUE,
  useSchemeInXml = NULL,
  ...,
  verbose = FALSE
){

  # Helper functions --------------------------------------------------------
  xmlAttrList <- function(xml) {
    x <- as.list(xml2::xml_attrs(xml))
    for (i in grep(" #%# ", x)) {
      x[[i]] <- strsplit(x[[i]], " #%# ")[[1]]
    }
    return(x)
  }

  checkAttr <- function(xml, x) {
    xmlAtt <- xmlAttrList(xml)
    xAtt <- attributes(x)
    ###
    xmlAtt <- xmlAtt[ -which( names(xmlAtt) == "output") ]
    ###
    ok <- names(xmlAtt) %in% names(xAtt)
    if (!all(ok)) {
      stop("Attribute", names(xmlAtt)[!ok], "not in scheme definition!")
    }
    ###
    xmlAtt$fileName <- NULL
    xAtt$fileName <- NULL
    ok <- xmlAtt %in% xAtt
    if (!all(ok)) {
      stop("Value of Attribute", names(xmlAtt)[!ok], "is different than expected!")
    }
    return(TRUE)
  }

  xml_to_dmdSchemeOnly <- function(x) {

    xml <- x

    # Check if output = "complete" --------------------------------------------
    if (!(xml2::xml_attr(xml, "output") %in% c("complete", "scheme"))) {
      stop("Can not create scheme from this xml!")
    }


    # Do conversion iteratively by ...List ------------------------------------

    dmdS <- list()
    for (i in 1:xml2::xml_length(xml)) {
      List <- xml2::xml_child(xml, i)

      atr <- xmlAttrList(List)
      atr <- atr[ !(names(atr) %in% c("row.names", "output")) ]

      # Create tibble with names and type ---------------------------------------

      dmdD <- tibble()
      suppressMessages(
        for (j in 1:length(atr$names)) {
          dmdD <- tibble::add_column(dmdD, !!(atr$names[j]) := get(atr$type[j])(1))
        }
      )
      dmdD[1,] <- NA

      atr <- atr[ !(names(atr) %in% c("names")) ]

      # Set class ---------------------------------------------------------------

      class(dmdD) <- atr$class

      atr <- atr[ !(names(atr) %in% c("class")) ]

      # Add remaining attributes ------------------------------------------------

      for (a in names(atr)) {
        attr(dmdD, a) <-  atr[[a]]
      }

      # Return dmdSchemeData ----------------------------------------------------

      dmdS[[i]] <- dmdD
    }


    # Get Attributes ----------------------------------------------------------

    atr <- xmlAttrList(xml)
    atr <- atr[ !(names(atr) %in% c("row.names", "output")) ]

    # Set class ---------------------------------------------------------------

    class(dmdS) <- atr$class
    atr <- atr[ !(names(atr) %in% c("class")) ]

    # Add remaining attributes ------------------------------------------------

    for (a in names(atr)) {
      attr(dmdS, a) <-  atr[[a]]
    }

    # Return dmdSchemeData ----------------------------------------------------

    return(dmdS)

  }

  # create scheme -----------------------------------------------------------

  complete <- ifelse(
    "output" %in% names(xml2::xml_attrs(x)),
    xml2::xml_attrs(x)[["output"]] == "complete",
    FALSE
  )

  if (is.null(useSchemeInXml)) {
    useSchemeInXml <- complete
  }

  if (useSchemeInXml) {
    if (!complete) {
      stop("The xml is not exported with the option 'output = complete' and containds no scheme definition!")
    }
    result <- xml_to_dmdSchemeOnly(x)
  } else {
    result <- dmdScheme()
    # Check if result scheme exists -------------------------------------------

    if (is.null(result)) {
      stop("xml is in a not in a loaded scheme definition.\n",
           "Load the R package containing the scheme before trying again."
      )
    }

    # Check version -----------------------------------------------------------

    if (xml2::xml_attrs(x)[["dmdSchemeVersion"]] != attr(result, "dmdSchemeVersion")) {
      stop("Version conflict - can not proceed:\n",
           x, " version : ", xml2::xml_attrs(x)[["dmdSchemeVersion"]], "\n",
           "dmdScheme version : ", attr(result, "version"))
    }

    # Check remaining attributes ----------------------------------------------

    checkAttr(x, result)

    # Check if all objects are also in dmdScheme ------------------------------

    if (!all(xml_name(xml_children(x)) %in% paste0(names(result), "List"))) {
      stop(
        "Nodes of xml file not in dmdScheme.\n",
        "Nodes in xml file  : ", paste(names(x), collapse = " "), "\n",
        "Names in dmdScheme : ", paste(names(result), collapse = " "), "\n"
      )
    }
  }

  if (keepData) {

    # Do the initial conversion to list ---------------------------------------

    xmlList <- xml2::as_list(x)[[1]]

    # Do the conversion iteratively -------------------------------------------


    for (sheetList in names(xmlList)) {
      sheet <- gsub("List", "", sheetList)

      # Check names -----------------------------------------------------------

      if (!all(names(xmlList[[sheetList]][[sheet]]) %in% names(result[[sheet]]))) {
        stop(
          "Nodes of xml file not in dmdScheme.\n",
          "Nodes in xml file  : ", paste(names(x), collapse = " "), "\n",
          "Names in dmdScheme : ", paste(names(result[[sheet]]), collapse = " "), "\n"
        )
      }

      for (i in 1:length(xmlList[[sheetList]])) {

        # add data to result ------------------------------------------------------

        data <- xmlList[[sheetList]][[i]] %>%
          unlist() %>%
          t() %>%
          as.data.frame(stringsAsFactors = FALSE)

        data <- vctrs::vec_cast(data, result[[sheet]])

        result[[sheet]] <- tibble::add_row(
          result[[sheet]],
          !!!data
        )

        # As the scheme contains a row with NAs already, this needs to be deleted ----

        if (i == 1) {
          result[[sheet]] <- result[[sheet]][-1,]
        }

        # Apply types -------------------------------------------------------------

        if (verbose) message("Apply types...")
        #
        type <- attr(result[[sheet]], "type")

        for (i in 1:ncol(result[[sheet]])) {
          if (verbose) message("   Apply type '", type[i], "' to '", names(result[[sheet]])[[i]], "'...")
          #
          result[[sheet]][[i]] <- as(result[[sheet]][[i]], Class = type[i])
        }
      }
    }

    # Copy remaining attributes -----------------------------------------------

    atr <- xmlAttrList(x)
    atr <- atr[ !(names(atr) %in% c("row.names", "output", "dmdSchemeName", "dmdSchemeVersion" )) ]
    for (i in names(atr)) {
      attr(result, i) <- atr[[i]]
    }
  }

  # Return ------------------------------------------------------------------

  return(result)
}



