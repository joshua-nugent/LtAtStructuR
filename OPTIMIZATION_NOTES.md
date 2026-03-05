# LtAtStructuR Optimization Notes

## Package overview
- R6-based package that transforms time-stamped clinical data into long-format analytic datasets
- Main pipeline: `construct()` → `createIntervals()` → `assignAC()` → `assignL()` → `imputeL()` → `cleanUp()`
- All internal processing uses data.table
- Source: `R/dataClass.R` (3,268 lines), `R/dataConstruction_workflow.R` (470 lines)

## Key files
- `R/dataClass.R`: 5 R6 classes — cohortData, expData, instExpData, timeDepCovData, LtAtData
- `R/dataConstruction_workflow.R`: User-facing API (setCohort, setExposure, setCovariate, construct, `+` operator)
- `data/`: cohortDT.rda, expDT.rda, a1cDT.rda, egfrDT.rda (example datasets)

## Performance bottlenecks (ranked by impact)

### 1. Per-subject future_lapply loop (BIGGEST)
- `assignAC()` line 1240: loops over every individual subject
- `assignL()` line 1926: same pattern, also loops per-covariate sequentially
- Each iteration: subsets data, merges, filters, creates columns, returns data.table
- Could be vectorized with data.table group-by and non-equi joins

### 2. Lubridate interval arithmetic inside per-subject loop
- Lines 1276-1332: `lubridate::interval()`, `int_overlaps()`, `%within%` per subject
- S4 interval objects have significant overhead
- Replace with simple date arithmetic: `a <= d & c <= b` for overlap test

### 3. assertthat checks inside per-subject loop
- Lines 1301-1302, 1343-1344: `assertthat::are_equal()` sorts and compares data.tables per subject
- Debug assertions that should be removed or guarded for production

### 4. Cartesian merge per subject → single non-equi join
- Line 1271: `merge(out_data_slice, exp_data_this_id, allow.cartesian = TRUE)`
- Does interval × episode cross product per subject
- Could be single `data.table` non-equi join across ALL subjects

### 5. Repeated get() calls
- `get(id_var)`, `get(exp_level)`, `get(eof_date)` called dozens of times per subject
- Each has evaluation overhead

### 6. Sequential covariate loop in assignL()
- Line 1916: covariates processed one at a time in for loop
- Each spawns own future_lapply over all subjects

### 7. Multiple data copies per subject
- Lines 1286, 1307: `copy(exp_overlap)` and `copy(z_exp_overlap)` per subject

## Optimization plan
1. Generate complex test data to compare rewrites against ground truth
2. Tackle bottlenecks one at a time, starting with #1 (per-subject loop in assignAC)
3. After each change: verify data correctness against ground truth, measure runtime improvement
