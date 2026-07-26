# Daily CMIP6 future-weather method development

This document is the implementation guide for adding and comparing daily and
sub-daily future-weather methods in epwshiftr. It consolidates the methods
identified in the project daily CMIP6 morphing research report with subsequent
source and literature checks completed on 2026-07-25.

The document is a living development record. Every pull request that adds,
changes, validates, or rejects a method described here should update the
relevant status and decision notes.

## 1. Decisions established by the literature review

### 1.1 Daily data alone is not a methodological contribution

Daily and sub-daily climate-model data have already been used to generate
weather files for building simulation. Published precedents include daily
change factors, quantile-based change functions, QDM, analogue reconstruction,
machine-learning reconstruction, direct multi-year climate sequences, and TMY
selection from future daily data.

The package must therefore describe daily support as a software capability and
an input to controlled method comparison. It must not describe the use of daily
CMIP6 data, daily change factors, or QDM alone as a new scientific method.

### 1.2 The current epwshiftr method is a combination of prior method families

The current daily temperature implementation has two distinct parts:

1. Its climate signal is closest to Sobie and Curry (2025): historical and
   future daily climatologies are estimated on an annual cycle, smoothed, and
   used to alter a baseline hourly weather sequence.
2. Its hourly temperature projection solves the same mean/minimum/maximum
   constraint problem as the bounded temperature weighted stretch described by
   Eames et al. (2024), but it uses a different transfer family.

epwshiftr uses a different transfer function from Eames et al. For normalized
hourly temperature \(x_h\), it solves the exponent \(p\) in:

\[
T'_h = T'_{\min} +
       \left(T'_{\max} - T'_{\min}\right) x_h^p
\]

so that the projected 24-hour series closes exactly on its requested mean,
minimum, and maximum.

The accurate description of the current method is therefore:

> Sobie-Curry-style daily climatological signals combined with an independent
> bounded hourly projection using an \(x^p\) transfer function.

This statement identifies the closest prior methods without claiming that the
complete implementation is identical to either one.

### 1.3 Reliability infrastructure is not scientific novelty

The following capabilities remain necessary, but are not standalone research
contributions:

- mapping CF calendars to a common annual coordinate;
- producing a 365-day EPW-compatible target grid;
- falling back to a mean shift when extrema are unavailable or constraints
  cannot be satisfied;
- enforcing humidity and psychrometric consistency;
- reporting closure, bounds, missing data, and transformation diagnostics.

Daily climatologies, bounded fallbacks, humidity closure, and diagnostic
frameworks all have published or open-source precedents. The exact epwshiftr
implementation may provide broader compatibility or better traceability, but
that is an engineering property to demonstrate, not an assumed methodological
advance.

Any future scientific contribution must be supported by controlled evidence,
for example:

- improved preservation of climate-model quantile changes and event
  persistence at the same time;
- a multivariable hourly reconstruction that improves physical consistency
  without degrading trends;
- a reproducible comparison showing where existing methods fail and why;
- a new constrained transformation with a measurable advantage over published
  transformations;
- an auditable end-to-end workflow from live ESGF data to simulation-ready
  weather files.

## 2. Method levels

The word "method" is used at several different levels in the literature. They
must remain separate in the implementation and in comparison results.

| Level | Question answered | Examples |
|---|---|---|
| Complete recipe | How is an entire future weather file produced? | Sobie-Curry, Wang, Hosseini, Bass fTMY |
| Climate signal | How is a future change or corrected daily value estimated? | delta change, QM, QDM, CDFt |
| Sequence | Which days occur and in what order? | inherited EPW, model sequence, block, analogue |
| Hourly reconstruction | How is a daily or sub-daily target converted to 24 hourly values? | BTWS, \(x^p\), MTCLIM, KNN/RF |
| Physical post-process | How are dependent weather variables made internally consistent? | specific-humidity closure, solar closure |
| Evaluation | How are method failures and differences measured? | trend, quantile, spell, joint and EnergyPlus metrics |

A complete recipe selects one or more components from the lower levels.
Consequently, the inventory below is not a list of mutually independent
algorithms and must not be implemented as duplicated end-to-end pipelines.

As of 2026-07-25, the identified inventory contains:

- 12 complete future-weather recipes;
- the eight univariate methods implemented by ibicus;
- six additional signal, preprocessing, or multivariable method families;
- eight sequence methods;
- ten daily-to-hourly or sub-daily-to-hourly methods.

These counts deliberately retain named published recipes even when they share
an underlying algorithm. For example, Arima uses a QM-family signal and Wang
uses QDM, but each also defines a distinct end-to-end weather construction.

## 3. Current epwshiftr baseline

Status in this table refers to the repository state checked on 2026-07-26.

| Capability | Repository status | Evidence |
|---|---|---|
| Enhanced monthly Belcher workflow | Merged | PR [#126](https://github.com/ideas-lab-nus/epwshiftr/pull/126) |
| Preserve CF calendar and annual phase during daily extraction | Merged | PR [#135](https://github.com/ideas-lab-nus/epwshiftr/pull/135) |
| Calendar-neutral circular daily climatology | Merged | PR [#137](https://github.com/ideas-lab-nus/epwshiftr/pull/137) |
| Constrained daily temperature targets and \(x^p\) projection | Merged | PR [#139](https://github.com/ideas-lab-nus/epwshiftr/pull/139) |
| Daily temperature backend in the future-EPW workflow | Merged | PR [#141](https://github.com/ideas-lab-nus/epwshiftr/pull/141) |
| Seven-stage component contracts | Merged | PR [#143](https://github.com/ideas-lab-nus/epwshiftr/pull/143) |
| Inspectable complete-recipe registry | Merged | PR [#145](https://github.com/ideas-lab-nus/epwshiftr/pull/145) |
| Eames monthly temperature signal and BTWS recipe | Merged | PR [#153](https://github.com/ideas-lab-nus/epwshiftr/pull/153) |
| Ek daily temperature factors | Implemented | PR [#155](https://github.com/ideas-lab-nus/epwshiftr/pull/155) |
| Arima month-wise temperature quantile mapping | Implemented | PR [#157](https://github.com/ideas-lab-nus/epwshiftr/pull/157) |
| Package-native daily adjusted-series contract and Linear Scaling signal | Implemented | PR [#161](https://github.com/ideas-lab-nus/epwshiftr/pull/161) |

The current daily temperature path:

1. requires daily `tas`;
2. optionally consumes paired daily `tasmin` and `tasmax`;
3. maps source calendars onto an annual phase and a 365-day target grid;
4. estimates historical and future circular daily climatologies using a
   31-day window by default;
5. computes mean, minimum, maximum, and DTR changes;
6. projects each baseline EPW day onto the requested daily temperature
   statistics;
7. inherits the baseline specific humidity, clips it only at future
   saturation, and derives future RH and dew-point temperature;
8. records target closure, fallback, saturation, and daily-boundary
   diagnostics.

If `tasmin` and `tasmax` are unavailable, the temperature is shifted by the
daily mean change and the baseline DTR is inherited. A flat hourly template
also uses a mean-shift fallback.

## 4. Calendar handling in existing daily methods

Daily methods do not all solve the same date problem because many of them avoid
pairing climate-model dates with TMY or EPW dates.

| Method family | Date strategy | What is known about calendar conversion |
|---|---|---|
| Ek (2018) and Sobie-Curry (2025) | Estimate a multi-year factor for each day of the annual cycle and apply it to the baseline weather day | Same high-level idea as a daily climatology; the publications do not document a general `360_day`/`365_day`/leap-year conversion |
| Arima (2024) | Build a daily CDF within each month and select a change factor by percentile | Avoids a 365-element day-of-year mapping; calendar length affects monthly samples rather than direct date pairing |
| Wang (2023) | Retain future model decades, interpolate to hourly data, and bias-adjust the resulting sequence | Does not pair future dates to a TMY; a general conversion from every CF calendar to EnergyPlus dates is not documented |
| Hosseini (2021) | Treat each future climate-model year as a realization and reconstruct its hours | Does not pair model days with a baseline TMY day; calendar harmonization is not described in the accessible method |
| Bass fTMY (2022) | Use an already-downscaled daily dataset, reconstruct hours, and select representative months | Calendar handling is absorbed by upstream preprocessing and TMY construction |
| ibicus seasonal windows | Extract day of year from standard or `cftime` objects and use circular running windows | Current source uses a 366-day circular boundary and is not equivalent to normalizing arbitrary year lengths to one annual phase |
| epwshiftr | Preserve the source CF calendar, map each sample to annual phase, then estimate a common 365-day climatology | Explicitly supports comparison across different source-calendar lengths |

The absence of calendar details in a publication is not evidence that its
authors ignored the problem. It means only that the exact transformation
cannot be reproduced from the publication. A faithful recipe must record
whether calendar handling is described, inherited from an upstream product, or
reconstructed by epwshiftr.

The common calendar adapter belongs below all daily signal methods. It must not
be duplicated inside individual QM, QDM, analogue, or paper-specific recipes.

## 5. Complete future-weather recipes

The recipe keys below are internal design identifiers, not committed public
API names.

| Recipe key | Published or product method | Signal | Sequence and hourly construction | Implementation state |
|---|---|---|---|---|
| `belcher_monthly` | Belcher et al. (2005) | Monthly shift, stretch, or combined change | Inherit baseline hourly sequence | Available through existing morphing profiles |
| `epwshiftr_monthly` | Current enhanced epwshiftr | Enhanced monthly Belcher changes | Inherit baseline sequence; apply shared physical post-processing | Implemented |
| `future_weather_generator` | Future Weather Generator | Enhanced monthly Belcher-family changes | Product-specific smoothing, ensemble and derived-field processing | Not implemented; external comparison or clean-room implementation only |
| `eames_monthly_temperature` | Eames et al. (2024) | Monthly mean and average-daily-extrema change factors | Apply BTWS day by day to each hourly baseline month | Temperature-only CMIP6 source adaptation implemented in PR #153; published non-temperature transforms remain unimplemented |
| `ek_daily_factors` | Ek et al. (2018) | Daily climatological mean and DTR change factors | Inherit baseline hourly sequence and apply the combined daily shift-and-stretch transform | Temperature-focused implementation in PR #155 |
| `sobie_curry_daily` | Sobie and Curry (2025) | Daily mean/DTR and thermodynamic factors with 21-day smoothing | Inherit CWEC/EPW hourly sequence | Implemented in PR #147; harmonized closure in PR #149 |
| `epwshiftr_daily_btws` | Composite comparison using Eames et al. (2024) | Existing epwshiftr daily CMIP6 mean/min/max targets | Published day-wise bounded temperature weighted stretch | Implemented; tracked in #150 |
| `epwshiftr_daily_power` | Current epwshiftr daily method | Circular daily mean/min/max climatologies | Day-wise \(x^p\) constrained projection | Implemented through PR #141 |
| `arima_rank_qm` | Arima et al. (2024) | Month-wise daily CDF and percentile-dependent change function | Apply the selected daily factor to every hour of the TMY day | Temperature-focused implementation in PR #157 |
| `wang_subdaily_qdm` | Wang et al. (2023) | KDE-QDM with a three-month moving window | Preserve future decade; interpolate primarily three-hourly model data to hours | Not implemented |
| `hosseini_knn_rf` | Hosseini et al. (2021) | QQ bias correction | KNN weather-type classification and random-forest hourly reconstruction | Not implemented |
| `bass_ftmy` | Bass et al. (2022) | Downscaled daily future climate | MTCLIM hourly reconstruction followed by representative-month TMY selection | Not implemented |
| `agyei_qdm_analogue` | Agyei-Agyemang et al. (2026) | Daily QDM | Analogue hourly reconstruction and ML solar decomposition | Method parameters require full-paper and source verification |

WeatherShift and FutureWeather.co remain useful product-level comparators, but
their complete internal methods are not publicly specified. They must be
treated as black-box output comparisons rather than reproducible recipes.
Published BTWS equations may be implemented independently; proprietary source
or undocumented behavior must not be reconstructed by copying.

## 6. Univariate climate-signal methods

### 6.1 The eight ibicus methods

ibicus currently implements eight peer-reviewed univariate methods. All eight
must be available to the comparison workflow through a common epwshiftr signal
interface.

| Method | Core behavior | Important comparison note |
|---|---|---|
| `LinearScaling` | Correct the model mean additively or multiplicatively using the reference-period bias | Transforms future model output; not the same output semantics as delta change |
| `DeltaChange` | Transfer the modeled historical-to-future change onto observations | Produces modified observations and is the ibicus method closest to morphing |
| `QuantileMapping` | Map modeled reference quantiles to observed reference quantiles | May alter the modeled future trend |
| `QuantileDeltaMapping` | Correct quantiles while transferring modeled quantile changes | Additive form suits temperature; multiplicative form suits positive quantities |
| `ScaledDistributionMapping` | Parametric distribution mapping with magnitude and event-likelihood changes | Designed to preserve raw modeled changes in distribution |
| `CDFt` | Construct a future target CDF through chained distribution transforms | Includes seasonal and optional future-period windows |
| `ECDFM` | Apply equidistant quantile corrections to future values | Trend-preserving quantile-mapping family |
| `ISIMIP` | Generate pseudo-future observations and map future model values onto them | Implements the ISIMIP3BASD family with variable-specific bounds and trend rules |

`DeltaChange` is included by ibicus for comparison even though its output is a
modified observational climatology rather than a bias-adjusted future model
series.

Implementation policy:

1. implement every method as a native R kernel behind the common signal
   contract;
2. translate published equations, defaults, bounds, and variable restrictions
   into explicit method specifications;
3. create deterministic R fixtures for every supported method-variable
   combination;
4. test calendar handling, seasonal and future-period windows, bounds, and
   stochastic behavior directly in the package;
5. enable a method in an EPW recipe only after its native implementation and
   diagnostics are complete.

The first implementation uses a package-native `DailyAdjustedSeries` result
rather than an external debiaser type. It retains the transformed
`model_future` role, canonical calendar-native daily coordinates, variable
metadata, resolved settings, correction provenance, and the standard signal
diagnostics. Linear Scaling is the first producer of this contract: monthly
temperature mean bias is applied additively and monthly precipitation mean
bias multiplicatively. The signal component deliberately stops at the
corrected daily sequence; it does not invent an EPW sequence or hourly
reconstruction policy.

### 6.2 Other signal and preprocessing methods

| Method | Role | Relationship to the main inventory |
|---|---|---|
| QQ bias correction | Quantile-to-quantile reference correction | Treat as a configured QM variant when reproducing Hosseini |
| BCCAQv2 | Bias correction and constructed analogues with QDM reordering | Upstream method used to create the CanDCS-U6 temperature input used by Sobie-Curry |
| Daily BCSD | Daily bias correction and spatial disaggregation | Upstream method used by NEX-GDDP-CMIP6 |
| MBCn | N-dimensional multivariate bias correction | Separate multivariable backend; not one of the eight ibicus methods |
| Copula correction | Correct or model inter-variable dependence | Candidate multivariable backend |
| SBCK | Collection of multivariate and spatial bias-correction methods | External reference suite, not one individual method |

An input derived from BCCAQv2, BCSD, or another preprocessing method must retain
that provenance. A comparison must not present a difference caused by an
upstream dataset as a difference caused by the EPW recipe.

Every named method family in this section must eventually be executable either
natively or through a documented optional adapter before the full comparison
is marked complete:

- QQ is implemented as a declared QM configuration;
- BCCAQv2 and daily BCSD initially use reference-data or external-tool adapters
  with complete provenance;
- MBCn is verified against the R MBC package before any native optimization;
- selected copula methods receive explicit algorithm names before
  implementation rather than being represented by one ambiguous "copula"
  option;
- SBCK remains a reference suite until its individual algorithms are added to
  this inventory.

## 7. Sequence methods

| Sequence method | Behavior | Main use |
|---|---|---|
| Baseline inheritance | Keep the original EPW/TMY day order | Belcher, Ek, Sobie-Curry, Arima, current epwshiftr |
| Direct model realization | Retain the modeled future sequence | Wang and other multi-year simulation workflows |
| Rank reordering | Reorder corrected values to recover a target temporal structure | BCCAQv2 and trend/sequence-preserving workflows |
| Representative-month selection | Select and concatenate months into a future TMY | Bass fTMY |
| Block resampling | Resample multi-day event blocks | Preserve local event shapes and spell structure |
| Shared analogue | Select one historical multivariable day and its hourly template | Physically coherent daily-to-hourly reconstruction |
| Weather-regime model | Model transitions between large-scale or local weather states | Persistent and interpretable event generation |
| Stochastic weather generator | Sample occurrence, intensity, dependence, and persistence models | Produce multiple plausible realizations |

Sequence methods must expose a seed when stochastic and must record the
selected source days, blocks, regimes, or model dates in provenance.

## 8. Daily-to-hourly and sub-daily-to-hourly methods

| Hourly method | Required inputs | Preserved or constrained properties |
|---|---|---|
| Belcher shift/stretch/combined | Monthly change factors and baseline hours | Baseline order and transformed monthly properties |
| Sobie mean/DTR anomaly transform | Daily mean and DTR changes | Daily mean and DTR; baseline hourly timing |
| Eames BTWS | Mean/min/max changes and baseline day | Bounded daily profile and requested mean/min/max when feasible |
| epwshiftr \(x^p\) projection | Daily mean/min/max changes and baseline day | Exact daily mean/min/max and baseline rank/timing |
| Linear temporal interpolation | Sub-daily climate-model values | Model sequence and interpolated hourly values |
| MTCLIM | Daily meteorological inputs and site information | Model-based diurnal meteorology |
| KNN plus random forest | Corrected daily state and trained historical data | Learned historical hourly patterns and extrapolation |
| Shared hourly analogue | Daily multivariable target and observed hourly library | Real historical within-day covariance |
| Conditional hourly generator | Daily state and trained conditional model | Sampled hourly profiles conditional on daily weather |
| ML solar decomposition | Daily or global radiation and explanatory state | Learned direct/diffuse radiation components |

Every hourly method must state whether it preserves event timing, hourly ranks,
daily totals, extrema, diurnal phase, and cross-variable dependence.

## 9. Variable-specific transformations

These transformations are shared components rather than complete recipes.

### Temperature

- additive delta or additive QDM for `tas`;
- separate or derived treatment of `tasmin`, `tasmax`, DTR, and temperature
  skew;
- Belcher, Sobie, BTWS, \(x^p\), interpolation, MTCLIM, analogue, or learned
  hourly reconstruction.

### Humidity

- direct bounded transformation of `hurs`;
- correction of `huss` followed by derivation of RH and dew point from future
  temperature and pressure;
- correction of dew point or humidity ratio followed by psychrometric closure;
- saturation clipping and explicit reporting of any clipped moisture.

The paper-faithful Sobie-Curry recipe must retain its published independent
temperature, dew-point, and RH transformations. The harmonized comparison mode
may instead use the shared epwshiftr humidity closure.

### Precipitation

- wet-day occurrence modeled separately from positive intensity;
- hurdle or censored precipitation distributions;
- multiplicative QDM for positive precipitation;
- empirical, GPD, or GEV tail handling;
- shared wet-hour templates and daily/monthly water conservation;
- dry/wet spell diagnostics.

### Radiation and cloud

- solar-geometry reconstruction and night-time zero enforcement;
- direct/diffuse decomposition using published models such as RBL, Engerer, or
  Paulescu, or direct use of a model-provided diffuse component;
- closure among GHI, DHI, DNI, and solar zenith;
- joint treatment or diagnosis of cloud, precipitation, humidity, and
  radiation.

### Pressure and wind

- additive correction for surface pressure;
- explicit distinction between surface pressure and sea-level pressure;
- additive, multiplicative, or bounded wind transformations as required by the
  selected method;
- direction-aware reconstruction when vector components are available.

## 10. Software architecture

Methods must be composed through explicit contracts:

```text
climate and reference data
  -> calendar adapter
    -> climate-signal backend
      -> sequence generator
        -> hourly reconstructor
          -> physical post-process
            -> diagnostics and provenance
              -> EPW or multi-year weather output
```

### 10.1 Calendar adapter

Responsibilities:

- decode and preserve the source CF calendar;
- expose original date components and annual phase;
- define leap-day and 360-day policies explicitly;
- provide seasonal windows without silently pairing unrelated model and EPW
  dates;
- retain enough metadata to reproduce every mapping.

### 10.2 Climate-signal backend

Responsibilities:

- consume historical model, future model, and reference data according to the
  selected algorithm;
- return daily targets, corrected daily sequences, or distribution functions
  with explicit output semantics;
- record additive/multiplicative/bounded treatment, windows, distributions,
  thresholds, extrapolation, and training periods;
- never perform hourly reconstruction implicitly.

### 10.3 Sequence generator

Responsibilities:

- select or generate the target daily order;
- retain links to model dates, analogue dates, blocks, regimes, and random
  seeds;
- return one or more named realizations;
- report event and transition diagnostics.

### 10.4 Hourly reconstructor

Responsibilities:

- transform each daily or sub-daily target into complete hours;
- state its constraints and fallback behavior;
- use a shared multivariable template when the algorithm requires physical
  dependence;
- return closure and boundary diagnostics before EPW formatting.

### 10.5 Shared physical post-process

Responsibilities:

- humidity and psychrometric closure;
- solar and radiation closure;
- precipitation conservation;
- physical limits and missing-value policy;
- EPW header, design-condition, ground-temperature, and extreme-period policy.

### 10.6 Recipe registry

Each complete recipe must declare:

- source publication or product;
- required and optional climate variables;
- reference data requirements;
- calendar policy;
- signal backend;
- sequence method;
- hourly method;
- physical post-process policy;
- diagnostics;
- output type: representative year, one future year, or multiple years;
- whether the recipe is paper-faithful, harmonized, or both.

Recipe metadata must be inspectable without running the method.

### 10.7 R naming and comments

Public exported APIs use ordinary `snake_case`.

Every package-internal top-level helper uses:

```text
module__function_name()
```

Relevant module prefixes include `daily__`, `bias__`, `sequence__`,
`hourly__`, `recipe__`, and the existing `morpher__` helpers.

New equations, classes, and method definitions require an explanatory comment
immediately before the definition. Important numerical choices, constraints,
fallbacks, and physical transformations require comments at their points of
use. Tests and call sites must use the same internal names.

## 11. Comparison design

### 11.1 Two required comparison modes

Every reproducible published recipe should support:

- **paper-faithful mode**: reproduce the source method, including its original
  variable transformations, sequence assumptions, fallbacks, and known
  limitations;
- **harmonized mode**: retain the method's defining climate signal, sequence,
  and hourly algorithm while applying the same epwshiftr physical closure and
  output quality controls.

Paper-faithful mode answers whether the publication can be reproduced.
Harmonized mode isolates the effect of the statistical or sequence method from
differences in post-processing.

### 11.2 Two comparison tracks

Some methods cannot be compared fairly in only one experiment.

1. **Common-input comparison**
   - same model, member, scenario, grid, historical period, future period,
     reference data, site, and baseline EPW;
   - isolates algorithmic differences;
   - uses only methods compatible with the common data.
2. **Original-recipe reproduction**
   - uses the data products, periods, and assumptions specified by each
     publication;
   - measures reproducibility and end-to-end behavior;
   - does not attribute upstream dataset differences to the final algorithm.

Representative-year methods and direct multi-year methods must be reported in
separate primary comparisons. Their building outputs may be compared only
through clearly defined aggregate or matched-event summaries.

### 11.3 Required metrics

Climate-signal metrics:

- seasonal and annual mean;
- variance and selected quantiles;
- modeled versus transferred quantile changes;
- trend preservation;
- wet-day and threshold frequencies;
- tail sensitivity and extrapolation behavior.

Daily and hourly metrics:

- daily mean, minimum, maximum, DTR, skew, and closure error;
- hourly rank and diurnal timing;
- day-boundary discontinuity and temperature ramp;
- solar and precipitation conservation.

Sequence metrics:

- lag-1, lag-3, and lag-7 autocorrelation;
- heatwave, cold-spell, wet-spell, and dry-spell counts and durations;
- transition matrices;
- event-block and spectral properties.

Multivariable and physical metrics:

- temperature-humidity and dew-point relationships;
- enthalpy and humidity-ratio consistency;
- cloud-rain-radiation dependence;
- wind-temperature and pressure-weather-state relationships;
- physical-bound and saturation violations.

Building and software metrics:

- EnergyPlus weather parsing and simulation success;
- heating, cooling, latent load, overheating, peak demand, and natural
  ventilation indicators;
- runtime, memory, storage, and external dependency cost;
- reproducibility, provenance completeness, and diagnostic coverage.

Results must separate uncertainty from climate model, member, scenario,
baseline weather, method, and stochastic realization.

## 12. Definition of a completed method

A method is not complete merely because it returns a weather file. Each method
requires:

- a primary-source method note with equations and parameter defaults;
- a declared calendar and missing-data policy;
- a canonical small fixture;
- a paper-faithful implementation where the source is reproducible;
- a harmonized implementation where meaningful;
- unit tests for equations, bounds, monotonicity, zero values, tails, and
  fallbacks;
- property tests for conservation and physical closure;
- comparison against a reference implementation or published example;
- provenance and diagnostic output;
- a user-facing example;
- inclusion in the common comparison report;
- a recorded decision on whether it is suitable for production use, comparison
  only, or rejection.

If a method cannot be reproduced because essential equations, code, or
parameters are unavailable, record that limitation and retain it as an
external or black-box comparator. Do not invent missing behavior.

## 13. Development order

### 13.1 Establish the common method framework

1. Add the recipe registry and inspectable recipe metadata. Implemented in PR
   #145.
2. Define climate-signal, sequence, hourly, physical, and diagnostic contracts.
   Implemented in PR #143.
3. Add explicit `paper_faithful` and `harmonized` execution policies.
   Implemented in PR #145.
4. Preserve the existing monthly and daily outputs unchanged while introducing
   these abstractions. Covered by the component, pipeline, and recipe-registry
   regression tests.

### 13.2 Implement the closest published comparisons

1. Sobie-Curry daily factors and 21-day smoothing. Implemented in PR #147,
   with harmonized humidity closure added in PR #149.
2. Eames BTWS hourly projection and its documented mean-shift fallback.
   Implemented as the hourly component of the composite
   `epwshiftr_daily_btws` recipe; tracked in #150.
3. Eames monthly temperature signal. Implemented as the temperature-only
   `eames_monthly_temperature` recipe in PR #153; the paper's non-temperature
   transforms remain separate work.
4. Ek daily change factors. Implemented in PR #155.
5. Arima month-wise rank/QM change functions. Implemented in PR #157.

These methods provide the most direct tests of the current daily signal and
\(x^p\) projection.

### 13.3 Add all eight ibicus signal methods

1. Specify published equations, defaults, bounds, and supported variables.
2. Add native R implementations behind the common signal interface.
3. Add deterministic R fixtures and verify each implementation by variable,
   transformation type, seasonal window, and future-period window.
4. Combine each signal method with the same inherited-sequence and hourly
   projection configuration for a controlled comparison.

### 13.4 Reproduce complete sequence-generating workflows

1. Wang sub-daily QDM and direct future-decade output.
2. Hosseini QQ/KNN/random-forest reconstruction.
3. Bass MTCLIM and future-TMY selection.
4. Agyei-Agyemang QDM/analogue/solar workflow after its specification is
   verified.

### 13.5 Add advanced sequence and multivariable methods

1. Shared analogue with a common hourly template.
2. Block resampling and rank reordering.
3. MBCn and selected copula alternatives.
4. Weather-regime transitions.
5. Stochastic weather generation and multiple realizations.

### 13.6 Complete all weather variables and building validation

1. Precipitation occurrence, intensity, tails, spells, and hourly allocation.
2. Radiation, cloud, and solar decomposition.
3. Humidity, pressure, and wind alternatives.
4. Cross-method weather diagnostics.
5. EnergyPlus simulations and paired building-response comparisons.
6. Production recommendation for each method and use case.

## 14. Development status

Update this table as work is merged.

| Work item | Status |
|---|---|
| Enhanced monthly Belcher reference | Implemented |
| CF calendar and annual-phase extraction | Implemented |
| Circular daily climatology | Implemented |
| Current \(x^p\) daily temperature projection | Implemented |
| Daily temperature future-EPW backend | Implemented in PR #141 |
| Shared component and signal contracts | Implemented in PR #143 |
| Recipe registry and execution policies | Implemented in PR #145 |
| Sobie-Curry faithful recipe | Implemented in PR #147 |
| Sobie-Curry harmonized humidity closure | Implemented in PR #149 |
| Eames BTWS hourly component and daily CMIP6 composite recipe | Implemented in PR #151 |
| Eames monthly temperature recipe | Implemented in PR #153 |
| Eames non-temperature transformations | Not started |
| Ek daily temperature factors | Implemented in PR #155 |
| Arima rank/QM | Implemented in PR #157 |
| Native daily adjusted-series signal contract | Implemented in PR #161 |
| Linear Scaling signal | Implemented in PR #161 |
| Remaining seven native ibicus-compatible signal methods | Not started |
| QQ/QM reproduction configuration | Not started |
| BCCAQv2 reference-data/tool adapter | Not started |
| Daily BCSD reference-data/tool adapter | Not started |
| Wang workflow | Not started |
| Hosseini workflow | Not started |
| Bass fTMY workflow | Not started |
| Agyei-Agyemang workflow | Waiting for complete method verification |
| MBCn and copula backends | Not started |
| SBCK method sub-inventory and reference adapter | Not started |
| Block, analogue, regime, and stochastic sequence methods | Not started |
| Complete cross-method weather and building comparison | Not started |

The next work is to add native R signal kernels one at a time behind the common
signal interface. Each kernel should define and test its supported variables,
defaults, transformations, and calendar behavior, then be combined with the
same sequence, hourly, physics, and output components for controlled
comparison.

## 15. Primary sources

- Belcher, Hacker, and Powell (2005), *Constructing design weather data for
  future climates*: <https://doi.org/10.1191/0143624405bt112oa>
- Eames et al. (2024), *A revised morphing algorithm for creating future
  weather for building performance evaluation*:
  <https://doi.org/10.1177/01436244231218861>
- Ek et al. (2018), *Future weather files to support climate resilient
  building design in Vancouver*:
  <https://dspace.library.uvic.ca/bitstreams/2d8f1238-79ba-4ac7-8dac-3331c8a4f836/download>
- Sobie and Curry (2025), *Dataset of future-shifted weather files for Canada
  using climate projections from CMIP6*:
  <https://doi.org/10.1016/j.dib.2025.111667>
- Pacific Climate Impacts Consortium future-shifted weather files:
  <https://www.uvic.ca/pcic/data-analysis-tools/data-portal/weather-files/>
- Arima et al. (2024), *Development of Future Weather Data Using the Quantile
  Mapping Technique and its Application in Japan*:
  <https://doi.org/10.69357/asim2024.1178>
- Wang et al. (2023), *Impacts of climate change, population growth, and power
  sector decarbonization on urban building energy use*:
  <https://doi.org/10.1038/s41467-023-41458-5>
- Hosseini, Bigtashi, and Lee (2021), *Generating future weather files under
  climate change scenarios to support building energy simulation*:
  <https://doi.org/10.1016/j.enbuild.2020.110543>
- Bass et al. (2022), *Future Typical Meteorological Year US Weather Files for
  Building Simulation*: <https://doi.org/10.5281/zenodo.6939750>
- Agyei-Agyemang, Ocampo, and Bibeau (2026), *Simulation of energy performance
  of a residential building through the year 2100 and the workflow to
  integrate evolving climate projections with historical hourly
  meteorological data*:
  <https://doi.org/10.1080/19401493.2026.2689622>
- Cannon, Sobie, and Murdock (2015), QDM:
  <https://doi.org/10.1175/JCLI-D-14-00754.1>
- Lange (2019), ISIMIP3BASD:
  <https://doi.org/10.5194/gmd-12-3055-2019>
- Teutschbein and Seibert (2012), precipitation and temperature bias-correction
  methods for hydrological climate-change simulation:
  <https://doi.org/10.1016/j.jhydrol.2012.05.052>
- Cannon (2018), MBCn:
  <https://doi.org/10.1007/s00382-017-3580-6>
- Spuler et al. (2024), ibicus:
  <https://doi.org/10.5194/gmd-17-1249-2024>
- ibicus method overview:
  <https://ibicus.readthedocs.io/en/latest/getting_started/overview.html>
- Future Weather Generator documentation:
  <https://future-weather-generator.adai.pt/documentation/>
- NASA NEX-GDDP-CMIP6:
  <https://www.nccs.nasa.gov/data-collections/nex-gddp-cmip6/>
