# Daily CMIP6 metadata availability probe

This repository tool answers a question that must be settled before a daily
morphing backend is designed: which CMIP6 model/member/grid identities provide
the same required daily variables across the historical reference period and
every requested future scenario?

The probe first collects global ESGF `Dataset` metadata using
`frequency=day`, `table_id=day`, and the union of required `variable_id`
values. It then expands `File` metadata only for models that pass the core
Dataset screen. It does not download NetCDF payloads, open OPeNDAP arrays,
morph an EPW, or claim that daily data improves the final weather file. Its
output is the evidence used to choose the models and periods for that later
experiment.

ESGF treats multiple values of one facet as alternatives: a request containing
`variable_id=tas,hurs,pr` finds Dataset records for `tas` **or** `hurs` **or**
`pr`. The probe therefore groups the returned variable-specific records by
model, experiment, member, table, and grid, then applies the required AND logic
locally. Future ScenarioMIP and historical CMIP records are queried separately
before their identities are intersected.

The Dataset discovery query intentionally does not use Dataset-level time
fields because their population and calendar endpoints vary across index
nodes. Passing the Dataset screen proves only that the required variable
identity exists. The subsequent File query repairs DRS time ranges and verifies
every requested year. Its Dataset parent query is also unbounded in time; the
requested window is applied only after File records are returned. Only that
second result can establish temporal coverage.

## Requirement profiles

The `core` profile requires:

- `tas`;
- `hurs`, or a complete and consistently selected `huss + tas + ps` path;
- `pr`;
- `rsds`;
- `rlds`;
- `sfcWind`.

The `enhanced` profile adds `tasmax` and `tasmin` so a future backend can
evaluate changes in daily temperature range.

A candidate is complete only when the historical period and all future
scenarios share the same:

- model (`source_id`);
- ensemble member (`variant_label`);
- daily table/grid partition;
- humidity source path.

File time ranges are expanded into individual years. A gap inside the requested
window therefore cannot pass through a simple minimum/maximum date check.

## Run

From the package root:

```sh
Rscript tools/probe-daily-cmip6-availability.R \
  --scenarios=ssp245,ssp585 \
  --member=r1i1p1f1 \
  --future-years=2041:2070 \
  --historical-years=1995:2014 \
  --index-nodes=DKRZ,CEDA,ORNL \
  --query-timeout=45 \
  --connect-timeout=10 \
  --output=outputs/daily-cmip6-availability
```

Use `--plan` to resolve and print the configuration without contacting ESGF:

```sh
Rscript tools/probe-daily-cmip6-availability.R --plan
```

Run `--help` for all options. The default `--models=auto` leaves `source_id`
unconstrained and discovers every model that has matching Dataset records. Use
an explicit comma-separated model list only for a targeted audit. Index nodes
can be supplied as the built-in names shown above or as explicit URLs. Dataset
completeness is evaluated independently at each node so records from different
nodes cannot create a false complete identity. Candidate models then undergo
File verification on the nodes where they passed discovery; the closest
complete result is retained and verification stops early when the enhanced
profile is complete. `--query-timeout` and `--connect-timeout` bound each
remote attempt independently so one unhealthy index node cannot stall a
survey. The default locks the first realization (`r1i1p1f1`) so models with
many ensemble members do not expand into an unbounded catalog. Use
`--member=auto` only for a deliberate all-member survey.

## Receipt

The output directory contains:

- `model-summary.csv`: the deterministic best identity for each model/profile;
- `candidate-intersections.csv`: every historical/future identity
  intersection and its missing requirements;
- `period-candidates.csv`: candidates before the cross-period intersection;
- `variable-coverage.csv`: per-variable file, year, data-node, OPeNDAP, and
  HTTP coverage;
- `file-query-attempts.csv`: candidate File-query failures, counts, durations,
  and selected attempt;
- `file-catalog.csv`: the compact File metadata snapshot, without long access
  URLs;
- `dataset-discovery-summary.csv`: all model/profile results from each
  successful Dataset discovery node;
- `dataset-discovery-intersections.csv`: the historical/future Dataset identity
  intersections;
- `dataset-discovery-candidates.csv`: Dataset candidates before the
  cross-period intersection;
- `dataset-discovery-attempts.csv`: Dataset node failures, counts, and
  durations;
- `dataset-catalog.csv`: the compact Dataset evidence used for discovery;
- `manifest.json`: exact request, package commit, profile contracts, and row
  counts;
- `checksums.sha256`: SHA-256 checksums for every immutable receipt file.

The DuckDB store is kept in the receipt directory as operational query state,
but is intentionally excluded from `checksums.sha256` because it can change
when the probe is resumed. The CSV, JSON, and checksum files are the immutable
research receipt.

In `dataset-discovery-summary.csv`, `complete` means the variable and identity
contract is present and the model is eligible for File verification. In
`model-summary.csv`, `complete` additionally means File metadata covers every
requested year. Neither result verifies remote-array readability,
calendar/timestamp alignment, physical quality, bias, or improvement over
monthly morphing. Those checks belong to the subsequent daily-backend
validation experiment.
