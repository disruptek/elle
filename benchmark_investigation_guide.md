# How to Investigate Benchmark Results

## 1. Raw JSON Data (Command Line)

### View median time and confidence intervals:
```bash
cat target/criterion/parsing/large_list_100/base/estimates.json | python3 -m json.tool
```

Key metrics:
- **median**: Most stable measurement (less affected by outliers)
- **point_estimate**: Single best estimate of the time
- **confidence_interval**: Range where true value likely lies (95% confidence)
- **standard_error**: How confident we are in the estimate

### Compare two runs:
```bash
# Base (old version)
BASE=$(cat target/criterion/parsing/large_list_100/base/estimates.json | \
  python3 -c "import json,sys; d=json.load(sys.stdin); print(d['median']['point_estimate'])")

# New (current version)  
NEW=$(cat target/criterion/parsing/large_list_100/new/estimates.json | \
  python3 -c "import json,sys; d=json.load(sys.stdin); print(d['median']['point_estimate'])")

# Calculate % change
python3 -c "print(f'Change: {((${NEW}-${BASE})/${BASE})*100:+.2f}%')"
```

## 2. Summary Reports

View all benchmarks across categories (slower/faster than baseline):
```bash
ls -la target/criterion/parsing/*/report/
cat target/criterion/parsing/large_list_100/report/summary.json | python3 -m json.tool
```

## 3. Visual Reports (HTML)

Open in a web browser:
```
file:///home/adavidoff/git/elle/target/criterion/report/index.html
```

Each report includes:
- **Time plot**: Shows distribution of measurements
- **PDF estimate**: Probability distribution function
- **Regression**: Linear trend line through the data
- **Comparison**: Against baseline if available

## 4. Detailed Sample Data

View raw individual measurements:
```bash
cat target/criterion/parsing/large_list_100/base/sample.json | python3 -m json.tool | head -50
```

Shows all measurements taken during the benchmark run.

## 5. Statistical Analysis (Tukey's Fences)

Outlier detection using Tukey's IQR method:
```bash
cat target/criterion/parsing/large_list_100/base/tukey.json | python3 -m json.tool
```

## Why Benchmarks Show No Change

The Token<'a> refactoring optimizes **lexing/symbol allocation**:
- Most benchmark time is spent in VM execution, not parsing
- To see the improvement, need benchmarks that are **parse-heavy**
- Recommend: Create a parsing-only benchmark (no VM execution)

## Key Insights

### Baseline data structure (target/criterion/parsing/large_list_100/):
```
base/           <- Previous run (baseline)
  estimates.json   <- Statistical estimates
  sample.json      <- All individual measurements
  tukey.json       <- Outlier detection
new/            <- Current run
  estimates.json
  sample.json
  tukey.json
change/         <- Comparison results
  estimates.json   <- Percent change from base to new
report/         <- HTML visualization
  index.html       <- Click for graphs and charts
```

### What changed:
- **base**: Used old lexer with Vec<char> + String allocation
- **new**: Uses new lexer with &str + borrowed references
- **change**: Relative improvement (if > 0%, it got slower; < 0%, got faster)

## Next Steps

1. **Save this baseline** as the new reference:
   ```bash
   cp -r target/criterion/parsing target/criterion/parsing.baseline
   ```

2. **Run on original code** (checkout old commit):
   ```bash
   git stash
   cargo bench --bench benchmarks
   ```

3. **Compare results** with Python script to see actual improvement
