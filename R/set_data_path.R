# set_data_path.R

#' Set the path to data files
#'
#' \code{set_data_path} sets the path to data files based on the OS
#'
#' This function takes the name of a project and folder and sets the path to the
#' data based on the OS being used and availability of the network drive.
#'
#' @param folder A string, the name of the top-level directory containing the
#'   project
#' @param project An optional string, the name of the sub-folder containing an
#'   individual research project
#'
#' @return A string
#'
#' @export
set_data_path <- function(folder, project = "") {
    if (Sys.info()['sysname'] == "Windows") {
        f <- paste0("U:/Data/", folder, "/")
    } else if (Sys.info()['sysname'] == "Darwin") { # macOS
        f <- paste0("/Volumes/brgulbis/Data/", folder, "/")
    }

    if (!dir.exists(f)) {
        f <- paste0(project, "/")
    } else {
        f <- paste0(f, project, "/")
    }

    f
}
