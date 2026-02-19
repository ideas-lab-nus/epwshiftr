# OPeNDAP Remote Data Access Implementation

## Summary

Successfully implemented OPeNDAP-based remote data access for ESGF query results, allowing users to access NetCDF data without downloading entire files.

## Implementation Details

### 1. New Classes

#### EsgDataset (R/dataset.R)

A comprehensive class for remote NetCDF dataset access via OPeNDAP, providing three layers of functionality:

**Basic Layer (RNetCDF Wrappers)**
- `$open()` / `$close()`: Connection management
- `$file_inq()`: File information
- `$var_inq(var)`: Variable information
- `$dim_inq(dim)`: Dimension information
- `$att_get(var, att)`: Attribute retrieval
- `$var_get(var, start, count)`: Raw data reading

**Middle Layer (Convenient Subsetting)**
- `$get_variables()`: List all variables
- `$get_dimensions()`: List all dimensions
- `$get_time_axis()`: Time axis information
- `$get_spatial_grid()`: Spatial grid (lat/lon) information

**High Layer (Data Operations)**
- `$read_array(variable, ...)`: Read as array
- `$read_data_table(variable, ...)`: Read as data.table
- Support for multi-file aggregation along time dimension

**Features**
- Automatic connection management with finalizer
- Metadata caching for performance
- Multi-file aggregation support
- Comprehensive error handling

#### EsgFileDownloader (R/downloader.R)

A unified file downloader for fallback when OPeNDAP is unavailable:

**Core Methods**
- `$download_file(url, dest, ...)`: Single file download
- `$download_batch(urls, dest_dir, ...)`: Batch download
- `$verify_checksum(file, expected, type)`: Checksum verification

**Features**
- Retry logic with exponential backoff
- Progress tracking
- Checksum verification (MD5/SHA256)
- Timeout handling
- Overwrite control

### 2. Updated Classes

#### EsgResultFile

Added `$open_dataset()` method:
- Opens one or more files via OPeNDAP
- Supports single file or aggregation
- Automatic download fallback when OPeNDAP unavailable
- Three fallback modes: "ask", "auto", "error"

#### EsgResultAggregation

Added `$open_dataset()` method:
- Opens aggregation files via OPeNDAP
- Default behavior: aggregate all files
- Can return list of individual datasets
- Same fallback logic as EsgResultFile

### 3. Dependencies

**Added to Imports**
- `abind`: For array concatenation in aggregation
- `curl`: For file downloads

**Moved from Suggests to Imports**
- `curl`: Now required for download fallback

**Added to Suggests**
- `digest`: Optional for SHA256 checksum verification

### 4. Documentation

Generated comprehensive documentation for:
- `EsgDataset.Rd`: Complete class documentation with examples
- `EsgFileDownloader.Rd`: Downloader class documentation
- Updated `EsgResultFile.Rd`: Added `$open_dataset()` method
- Updated `EsgResultAggregation.Rd`: Added `$open_dataset()` method

### 5. Tests

Created comprehensive test suites:

**test-dataset.R**
- Dataset creation and initialization
- Connection open/close
- Basic layer methods (RNetCDF wrappers)
- Middle layer methods (convenience functions)
- High layer methods (data operations)
- Error handling
- Print method

**test-downloader.R**
- Downloader creation
- Single file download
- Batch download
- Overwrite behavior
- Checksum verification
- Error handling
- Print method

**Updated test-query-result.R**
- `EsgResultFile$open_dataset()` tests
- `EsgResultAggregation$open_dataset()` tests
- OPeNDAP availability handling
- Fallback behavior tests

## Usage Example

```r
library(epwshiftr)

# Query ESGF for data
q <- esg_query()$
    activity_id("ScenarioMIP")$
    source_id("CESM2")$
    experiment_id("ssp585")$
    variable_id("tas")$
    frequency("mon")$
    variant_label("r4i1p1f1")$
    limit(1)

# Collect file results
files <- q$collect(type = "File", all = FALSE, limit = 1)

# Open dataset via OPeNDAP
ds <- files$open_dataset(index = 1)

# Access metadata
vars <- ds$get_variables()
dims <- ds$get_dimensions()
time_info <- ds$get_time_axis()

# Read data as array
data_array <- ds$read_array("tas", start = c(1, 1, 1), count = c(12, 10, 10))

# Read data as data.table
data_dt <- ds$read_data_table("tas", start = c(1, 1, 1), count = c(12, 10, 10))

# Close connection
ds$close()

# For aggregations
datasets <- q$collect(type = "Dataset", all = FALSE, limit = 1)
aggs <- datasets$collect(type = "Aggregation", all = FALSE)

# Open and aggregate multiple files
ds_agg <- aggs$open_dataset(aggregate = TRUE)

# Access aggregated data
time_info <- ds_agg$get_time_axis()  # Combined time axis
data <- ds_agg$read_array("tas")     # Concatenated data

ds_agg$close()
```

## Key Features

1. **No Full Download Required**: Access remote data via OPeNDAP without downloading entire files
2. **Flexible Data Access**: Three layers (basic, middle, high) for different use cases
3. **Multiple Format Support**: Read data as arrays or data.tables
4. **Multi-file Aggregation**: Automatically concatenate time series across files
5. **Fallback Mechanism**: Automatic download when OPeNDAP unavailable
6. **Robust Error Handling**: Comprehensive error messages and retry logic
7. **Performance Optimized**: Metadata caching and efficient subsetting
8. **User-Friendly**: Interactive prompts for downloads, progress tracking

## Technical Details

### Multi-file Aggregation

The aggregation logic:
1. Validates that all files have compatible dimensions and variables
2. Retrieves time axis information from each file
3. Concatenates data along the time dimension (first dimension)
4. Caches metadata to avoid repeated queries

### Connection Management

- Connections are automatically opened when needed
- Private finalizer ensures connections are closed when object is garbage collected
- Explicit `$close()` method for manual cleanup
- Multiple connection handles managed for aggregated datasets

### Error Handling

- Comprehensive checks for closed datasets
- Invalid index detection
- OPeNDAP availability verification
- Network error handling with retries
- User-friendly error messages

## Files Modified/Created

### Created
- `R/dataset.R` (579 lines)
- `R/downloader.R` (367 lines)
- `tests/testthat/test-dataset.R` (162 lines)
- `tests/testthat/test-downloader.R` (126 lines)

### Modified
- `R/query-result.R`: Added `$open_dataset()` methods
- `DESCRIPTION`: Updated dependencies and Collate field
- `NAMESPACE`: Automatically updated by roxygen2
- `tests/testthat/test-query-result.R`: Added new tests

### Generated
- `man/EsgDataset.Rd`
- `man/EsgFileDownloader.Rd`
- `man/EsgResultFile.Rd` (updated)
- `man/EsgResultAggregation.Rd` (updated)

## Status

✅ Implementation complete
✅ Documentation generated
✅ Tests written
✅ No linter errors
✅ Package loads successfully
✅ Classes exported correctly

## Next Steps (Optional Enhancements)

1. Add parallel download support in `EsgFileDownloader`
2. Implement more sophisticated aggregation (spatial dimensions)
3. Add caching for downloaded files
4. Implement lazy evaluation for large datasets
5. Add support for subsetting by coordinate values (not just indices)
6. Implement data compression/optimization for large transfers

