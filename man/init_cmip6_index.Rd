% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esgf.R
\name{init_cmip6_index}
\alias{init_cmip6_index}
\title{Build CMIP6 experiment output file index}
\usage{
init_cmip6_index(
  activity = "ScenarioMIP",
  variable = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "pr", "rsds",
    "rlds", "psl", "sfcWind", "clt"),
  frequency = "day",
  experiment = c("ssp126", "ssp245", "ssp370", "ssp585"),
  source = c("AWI-CM-1-1-MR", "BCC-CSM2-MR", "CESM2", "CESM2-WACCM", "EC-Earth3",
    "EC-Earth3-Veg", "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MPI-ESM1-2-HR",
    "MRI-ESM2-0"),
  variant = "r1i1p1f1",
  replica = FALSE,
  latest = TRUE,
  resolution = c("100 km", "50 km"),
  limit = 10000L,
  data_node = NULL,
  years = NULL,
  save = FALSE
)
}
\arguments{
\item{activity}{A character vector indicating activity identifiers. Default:
\code{"ScenarioMIP"}. Possible values:
\itemize{
\item \code{"AerChemMIP"}: Aerosols and Chemistry Model Intercomparison Project,
\item \code{"C4MIP"}: Coupled Climate Carbon Cycle Model Intercomparison Project,
\item \code{"CDRMIP"}: Carbon Dioxide Removal Model Intercomparison Project,
\item \code{"CFMIP"}: Cloud Feedback Model Intercomparison Project,
\item \code{"CMIP"}: CMIP DECK: 1pctCO2, abrupt4xCO2, amip, esm-piControl, esm-historical, historical, and piControl experiments,
\item \code{"CORDEX"}: Coordinated Regional Climate Downscaling Experiment,
\item \code{"DAMIP"}: Detection and Attribution Model Intercomparison Project,
\item \code{"DCPP"}: Decadal Climate Prediction Project,
\item \code{"DynVarMIP"}: Dynamics and Variability Model Intercomparison Project,
\item \code{"FAFMIP"}: Flux-Anomaly-Forced Model Intercomparison Project,
\item \code{"GMMIP"}: Global Monsoons Model Intercomparison Project,
\item \code{"GeoMIP"}: Geoengineering Model Intercomparison Project,
\item \code{"HighResMIP"}: High-Resolution Model Intercomparison Project,
\item \code{"ISMIP6"}: Ice Sheet Model Intercomparison Project for CMIP6,
\item \code{"LS3MIP"}: Land Surface, Snow and Soil Moisture,
\item \code{"LUMIP"}: Land-Use Model Intercomparison Project,
\item \code{"OMIP"}: Ocean Model Intercomparison Project,
\item \code{"PAMIP"}: Polar Amplification Model Intercomparison Project,
\item \code{"PMIP"}: Palaeoclimate Modelling Intercomparison Project,
\item \code{"RFMIP"}: Radiative Forcing Model Intercomparison Project,
\item \code{"SIMIP"}: Sea Ice Model Intercomparison Project,
\item \code{"ScenarioMIP"}: Scenario Model Intercomparison Project,
\item \code{"VIACSAB"}: Vulnerability, Impacts, Adaptation and Climate Services Advisory Board,
\item \code{"VolMIP"}: Volcanic Forcings Model Intercomparison Project
}}

\item{variable}{A character vector indicating variable identifiers. The 12
most related variables for EPW are set as defaults. If \code{NULL}, all
possible variables are returned. Default: \code{c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rss", "rls", "sfcWind", "pr", "clt")}, where:
\itemize{
\item \code{tas}: Near-surface (usually, 2 meter) air temperature, units: \code{K}.
\item \code{tasmax}: Maximum near-surface (usually, 2 meter) air temperature, units: \code{K}.
\item \code{tasmin}: Minimum near-surface (usually, 2 meter) air temperature, units: \code{K}.
\item \code{hurs}: Near-surface relative humidity, units: \verb{\%}.
\item \code{hursmax}: Maximum near-surface relative humidity, units: \verb{\%}.
\item \code{hursmin}: Minimum near-surface relative humidity, units: \verb{\%}.
\item \code{psl}: Sea level pressure, units: \code{Pa}.
\item \code{rsds}: Surface downwelling shortwave radiation, units: \verb{W m-2}.
\item \code{rlds}: Surface downwelling longwave radiation, units: \verb{W m-2}.
\item \code{sfcWind}: Near-surface (usually, 10 meters) wind speed, units: \verb{m s-1}.
\item \code{pr}: Precipitation, units: \verb{kg m-2 s-1}.
\item \code{clt}: Total cloud area fraction for the whole atmospheric column, as
seen from the surface or the top of the atmosphere. Units: \verb{\%}.
}}

\item{frequency}{A character vector of sampling frequency. If \code{NULL}, all
possible frequencies are returned. Default: \code{"day"}.
Possible values:
\itemize{
\item \code{"1hr"}: sampled hourly,
\item \code{"1hrCM"}: monthly-mean diurnal cycle resolving each day into 1-hour means,
\item \code{"1hrPt"}: sampled hourly, at specified time point within an hour,
\item \code{"3hr"}: sampled every 3 hours,
\item \code{"3hrPt"}: sampled 3 hourly, at specified time point within the time period,
\item \code{"6hr"}: sampled every 6 hours,
\item \code{"6hrPt"}: sampled 6 hourly, at specified time point within the time period,
\item \code{"day"}: daily mean samples,
\item \code{"dec"}: decadal mean samples,
\item \code{"fx"}: fixed (time invariant) field,
\item \code{"mon"}: monthly mean samples,
\item \code{"monC"}: monthly climatology computed from monthly mean samples,
\item \code{"monPt"}: sampled monthly, at specified time point within the time period,
\item \code{"subhrPt"}: sampled sub-hourly, at specified time point within an hour,
\item \code{"yr"}: annual mean samples,
\item \code{"yrPt"}: sampled yearly, at specified time point within the time period
}}

\item{experiment}{A character vector indicating root experiment identifiers.
The \strong{Tier-1} experiment of activity ScenarioMIP are set as defaults.
If \code{NULL}, all possible experiment are returned.
Default: \code{c("ssp126", "ssp245", "ssp370", "ssp585")}.}

\item{source}{A character vector indicating model identifiers. Defaults are
set to 11 sources which give outputs of all 4 experiment of activity
ScenarioMIP with daily frequency, i.e. \code{"AWI-CM-1-1-MR"},
\code{"BCC-CSM2-MR"}, \code{"CESM2"}, \code{"CESM2-WACCM"}, \code{"EC-Earth3"},
\code{"EC-Earth3-Veg"}, \code{"GFDL-ESM4"}, \code{"INM-CM4-8"}, \code{"INM-CM5-0"},
\code{"MPI-ESM1-2-HR"} and \code{"MRI-ESM2-0"}.
If \code{NULL}, all possible sources are returned.}

\item{variant}{A character vector indicating label constructed from 4
indices stored as global attributes in format \verb{r<k>i<l>p<m>f<n>}
described below. Default: \code{"r1i1p1f1"}.
If \code{NULL}, all possible variants are returned.
\itemize{
\item \code{r}: realization_index (\verb{<k>}) = realization number (integer >0)
\item \code{i}: initialization_index (\verb{<l>}) = index for variant of initialization method (integer >0)
\item \code{p}: physics_index (\verb{<m>}) = index for model physics variant (integer >0)
\item \code{f}: forcing_index (\verb{<n>}) = index for variant of forcing (integer >0)
}}

\item{replica}{Whether the record is the "master" copy, or a replica. Use
\code{FALSE} to return only originals and \code{TRUE} to return only replicas.
Use \code{NULL} to return both the master and the replicas.
Default: \code{FALSE}.}

\item{latest}{Whether the record is the latest available version, or a
previous version. Use \code{TRUE} to return only the latest version of all
records and \code{FALSE} to return previous versions. Default: \code{FALSE}.}

\item{resolution}{A character vector indicating approximate horizontal
resolution. Default: \code{c("50 km", "100 km")}.
If \code{NULL}, all possible resolutions are returned.}

\item{limit}{An integer indicating the maximum of matched records to return.
Should be <= 10,000. Default: \code{10000}.}

\item{data_node}{A character vector indicating data nodes to be queried.
Default to \code{NULL}, which means all possible data nodes.}

\item{years}{An integer vector indicating the target years to be include in
the data file. All other years will be excluded. If \code{NULL}, no
subsetting on years will be performed. Default: \code{NULL}.}

\item{save}{If \code{TRUE}, the results will be saved into user data directory.
Default: \code{FALSE}.}
}
\value{
A \link[data.table:data.table]{data.table::data.table} with 22 columns:\tabular{rlll}{
   No. \tab Column \tab Type \tab Description \cr
   1 \tab \code{file_id} \tab Character \tab Model output file universal identifier \cr
   2 \tab \code{dataset_id} \tab Character \tab Dataset universal identifier \cr
   3 \tab \code{mip_era} \tab Character \tab Activity's associated CMIP cycle. Will always be \code{"CMIP6"} \cr
   4 \tab \code{activity_drs} \tab Character \tab Activity DRS (Data Reference Syntax) \cr
   5 \tab \code{institution_id} \tab Character \tab Institution identifier \cr
   6 \tab \code{source_id} \tab Character \tab Model identifier \cr
   7 \tab \code{experiment_id} \tab Character \tab Root experiment identifier \cr
   8 \tab \code{member_id} \tab Character \tab A compound construction from \code{sub_experiment_id} and \code{variant_label} \cr
   9 \tab \code{table_id} \tab Character \tab Table identifier \cr
   10 \tab \code{frequency} \tab Character \tab Sampling frequency \cr
   11 \tab \code{grid_label} \tab Character \tab Grid identifier \cr
   12 \tab \code{version} \tab Character \tab Approximate date of model output file \cr
   13 \tab \code{nominal_resolution} \tab Character \tab Approximate horizontal resolution \cr
   14 \tab \code{variable_id} \tab Character \tab Variable identifier \cr
   15 \tab \code{variable_long_name} \tab Character \tab Variable long name \cr
   16 \tab \code{variable_units} \tab Character \tab Units of variable \cr
   17 \tab \code{datetime_start} \tab POSIXct \tab Start date and time of simulation \cr
   18 \tab \code{datetime_end} \tab POSIXct \tab End date and time of simulation \cr
   19 \tab \code{file_size} \tab Character \tab Model output file size in Bytes \cr
   20 \tab \code{data_node} \tab Character \tab Data node to download the model output file \cr
   21 \tab \code{dataset_pid} \tab Character \tab A unique string that helps identify the dataset \cr
   22 \tab \code{tracking_id} \tab Character \tab A unique string that helps identify the output file \cr
}
}
\description{
\code{init_cmip6_index()} will search the CMIP6 model output file using \code{\link[=esgf_query]{esgf_query()}}
, return a \code{\link[data.table:data.table]{data.table::data.table()}} containing the actual NetCDF file url
to download, and store it into user data directory for future use.
}
\details{
For details on where the file index is stored, see \code{\link[rappdirs:user_data_dir]{rappdirs::user_data_dir()}}.
}
\note{
Argument \code{limit} will only apply to \code{Dataset} query. \code{init_cmip6_index()} will
try to get all model output files which match the dataset id.
}
\examples{
\dontrun{
init_cmip6_index()
}

}
