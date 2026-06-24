# EPW output helpers ----------------------------------------------------------

disclaimer_comment <- function(case) {
    paste0(
        "This climate change adapted weather file, which bases on ", case, " ",
        "ensemble data, has been generated using the epwshiftr tool V", utils::packageVersion("epwshiftr"), ". ",
        "The original weather file used for generating this climate change ",
        "adapted weather data may be copyrighted material. Therefore, generated ",
        "weather files can only be used by persons or entities who possess the ",
        "corresponding licensed weather file. ",
        "DISCLAIMER OF WARRANTIES: ",
        "The data is provided 'as is' without warranty of any kind, either expressed or implied. ",
        "The entire risk as to the quality and performance of the calculated climate change ",
        "weather data in this file is with you. In no event will the authors of the ",
        "weather file generation tool be liable to you for any damages, including ",
        "without limitation any lost profits, lost savings, or other incidental or ",
        "consequential damages arising out of the use or inability to use this data."
    )
}
