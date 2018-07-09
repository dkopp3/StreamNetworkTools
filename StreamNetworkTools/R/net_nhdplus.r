#' NHDPlus VPU Download
#'
#' \code{net_nhdplus} downloads and extracts the .7z file from the national
#' hydrography dataset plus by the vector processing unit specified in its
#' arguments
#'
#' 7-zip \url{https://www.7-zip.org/download.html} must be downloaded and
#' installed for tool to work
#'
#' \code{dir} creates a new sub-directory "/NHDPlus" to extract data.
#'
#' acceptable options for \code{file}: c("FdrFac", "FdrNull", "FilledAreas",
#' "Hydrodem", "NEDSnapshot", "EROMExtension", "NHDPlusAttributes",
#' "NHDPlusBurnComponents", "NHDPlusCatchment", "NHDSnapshotFGDB",
#' "NHDSnapshot", "VogelExtension", "VPUAttributeExtension", "WBDSnapshot"). see
#' \url{http://www.horizon-systems.com/NHDPlus/NHDPlusV2_documentation.php} for
#' definitions
#'
#' All raster rocessing units (RPU) within the vpu will be downloaded
#'
#' Includes \code{\link[base]{system}} commands which may not work outside
#' windows OS
#'
#' @param nhdplus_path character specfiying the directory for downloaded files
#' @param download character specifying "http" or "ftp"
#' @param vpu character vector processing unit for data
#' @param files character specifying which files to download. "ALL" will
#'   download full dataset. See Details
#' @param zip_7 character specifying the location of the 7-zip download
#'
#' @return /NHDPlus and subdirectories for NHDPlus V2 data
#' @examples
#' files needed for StreamNetworkTools
#' net_nhdplus(nhdplus_path=getwd(), download = "http", vpu = "01", files =
#' c("NHDPlusAttributes", "NHDSnapshot", "NHDPlusCatchment", "VPUAttributeExtension",
#' "VogelExtension", "EROMExtension"), zip_7 = "C:/Program Files/7-Zip")
#' @export

net_nhdplus <- function (nhdplus_path=getwd(),
                         download = "http",
                         vpu = "01",
                         files = "ALL",
                         zip_7 = "C:/Program Files/7-Zip") {
  #save wd for reset
  old_wd <- getwd()
  #Othertools will look for NHDPlus folder therefore i hard coded it here
  #check if there is a NHDPlus sub-directory in the desired parent directory
  if(all(basename(list.dirs((nhdplus_path))) != "NHDPlus")){
    dir.create(paste(nhdplus_path, "/NHDPlus", sep = ""))
    nhdplus_path <- paste(nhdplus_path, "/NHDPlus", sep = "")
    } else {
      nhdplus_path <- grep("NHDPlus$", list.dirs(nhdplus_path), value = T)
      }
  if(any(files == "ALL")){
    files <- c("FdrFac", "FdrNull", "FilledAreas",
               "Hydrodem", "NEDSnapshot", "EROMExtension",
               "NHDPlusAttributes", "NHDPlusBurnComponents","NHDPlusCatchment",
               "NHDSnapshotFGDB","NHDSnapshot","VogelExtension",
               "VPUAttributeExtension", "WBDSnapshot")
    } else {
      files <- files
      }

  #NLCD Extensions is kept in a different site
  if(any(files == "VPUAttributeExtension")){
      url <- "http://www.horizon-systems.com/NHDPlus/V2NLCD2011.php"
      url2 <- RCurl::getURL(url)
      parsed <- XML::htmlParse(url2)
      link <- XML::xpathSApply(parsed,
                               path = "//a",
                               XML::xmlGetAttr,
                               "href")
    link <- unlist(link)
    link <- grep(download,
                  link,
                  value = T)
    link <- grep(paste(vpu,"_VPUAttributeExtensionNLCD",sep=""),
                  link,
                  value = T)
    filename <- unlist(strsplit(link, "/"))
    zip_name <- paste(nhdplus_path, "/",
                      filename[length(filename)],
                      sep = "")
    utils::download.file(link,
                          zip_name,
                          mode = "wb")
    setwd(zip_7)
    system(paste("7z x", paste("-o", nhdplus_path, sep=""),
                 zip_name, sep = " "))
  }

  #obtain links form vpu websites to download desired files
  url <- paste("http://www.horizon-systems.com/nhdplus/NHDPlusV2_",
               vpu, ".php", sep = "")
  url2 <- RCurl::getURL(url)
  parsed <- XML::htmlParse(url2)
  links <- XML::xpathSApply(parsed,
                            path = "//a",
                            XML::xmlGetAttr,
                            "href")
  links <- unlist(links)
  links <- grep(download,
                links,
                value = T)
#nested for loop bc some files (e.g. raster processing units have mutiple files)
  for (f in files){
    download_files <- grep(f, links, value = T)
    for (download_file in download_files){
      filename <- unlist(strsplit(download_file, "/"))
      #mode "wb" is not default but specifies binary
      zip_name <- paste(nhdplus_path, "/",
                        filename[length(filename)],
                        sep = "")
      utils:::download.file(download_file,
                            zip_name,
                            mode = "wb")
      setwd(zip_7)
      system(paste("7z x", paste("-o", nhdplus_path, sep=""),
                   zip_name, sep = " "))
    }
  }
  #reset wd
  setwd(old_wd)
}
