% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_group.R
\name{get_group}
\alias{get_group}
\title{Get Zotero Group Data}
\usage{
get_group(
  group_id = 2352922,
  path = tempfile(),
  output_format = NULL,
  api_key = NULL
)
}
\arguments{
\item{group_id}{The ID of the Zotero group.}

\item{path}{The path to save the retrieved data.}

\item{output_format}{The format of the output files. Supported are:
\itemize{
\item bibtex: BibTeX format
\item biblatex: BibLaTeX format
\item bookmarks: Firefox bookmarks in HTML format
\item coins: COinS format
\item csljson: Citation Style Language (CSL) JSON format
\item csv: Comma-separated values format
\item mods: MODS format
\item refer: Refer/BibIX format
\item rdf_bibliontology: RDF/Bibliographic Ontology format
\item rdf_dc: RDF/Dublin Core format
\item rdf_zotero: RDF/Zotero format
\item ris: RIS format
\item tei: TEI format
\item wikipedia: Wikipedia citation templates format
\item \code{NULL}: A default format (json?)
}

for a list of supported formats see
(see \@link[https://www.zotero.org/support/dev/web_api/v3/basics#item_export_formats]{here} or
\code{NULL} in which case the default format (json) is used.`}

\item{api_key}{API key for Zotero. Only needed for private groups.}
}
\value{
The \code{path} where the data is saved.
}
\description{
This function retrieves data from a Zotero group using the Zotero API.
}
\details{
This function uses the Zotero API to retrieve data from a Zotero group. It downloads
the data in the format specified in \code{output_format}
and saves it in batches of 100 records to the folder specified by the \code{path} parameter.

\strong{NB: The folder specified will always be deleted before processing.}

For further information about the allowed formats and other details on the API see
\url{https://www.zotero.org/support/dev/web_api/v3/start}.
}
\examples{
# Retrieve data from Zotero group with ID 2352922 (IPBES IAS Assessment Bibliograhy) and save it
# in the file named "zotero_data.csv"

get_group(2352922, "zotero_data")

}
