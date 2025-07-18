% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_tabular}
\alias{read_tabular}
\alias{write_tabular}
\alias{write_tabular_file}
\alias{read_tabular_file}
\title{Read from or write to tabular files, with
file format inferred from the file extension.}
\usage{
read_tabular(path_to_file, ...)

write_tabular(table, path_to_file, simplify_column_types = TRUE, ...)

write_tabular_file(table, path_to_file, simplify_column_types = TRUE, ...)

read_tabular_file(path_to_file, ...)
}
\arguments{
\item{path_to_file}{Path to the file to read/write.
Must have extension \code{.tsv}, \code{.csv}, or \code{.parquet}
(not case-sensitive).}

\item{...}{Additional keyword arguments passed to the
file reader/writer function, which will be one of
\code{\link[readr:read_delim]{readr::read_csv()}} / \code{\link[readr:write_delim]{readr::write_csv()}},
\code{\link[readr:read_delim]{readr::read_tsv()}} / \code{\link[readr:write_delim]{readr::write_tsv()}}, and
\code{\link[arrow:read_parquet]{arrow::read_parquet()}} / \code{\link[arrow:write_parquet]{arrow::write_parquet()}},
depending on the file format.}

\item{table}{Table to write (\code{write_tabular} only).}

\item{simplify_column_types}{If \code{TRUE}, attempts to convert extension arrays
(e.g. fs paths, glue strings) to plain base R vectors before writing.}
}
\value{
For \code{read_tabular}, the result of
reading in the file, as a
\code{\link[tibble:tibble]{tibble}}. For \code{write_tabular},
nothing, saving the tabular to disk as a side effect.
}
\description{
Read from or write to tabular files, with
file format inferred from the file extension.
}
