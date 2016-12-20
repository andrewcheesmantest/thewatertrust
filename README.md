# thewatertrust

Contains scripts that process base TWT data as gathered from field surveys.

Surveys take three forms, which are recoded and merged into site_agg.csv. This site-level aggregate is then used to feed a single KPI report (comprehensive summary) and visualizations. Deeper analyses may need to descend to the individual household-, water point-, or water quality-level surveys, but the site_agg document should serve for most questions and aggregations.

Both/New denotes whether an individual site has data for at least one Baseline and at least one Monitoring survey (both) and whether its Baseline survey was conducted on or after Jan 1 2015 (new). Midterm surveys are generally disregarded, as they don't provide a uniform and methodologically distinct perspective across sites.

Status as of Dec 19 2016:

1. Site aggregation process complete
2. Draft visualizations complete
3. Comprehensive summary (KPI report) draft complete
4. Short summary complete, albeit with calculations errors that'll take some digging to resolve. Mainly reorganizes and makes pretty the calculations present in the comp_summary object

TODO: 

1. Investigate and fix logic around the functionality indicators and other counts in the short summary
2. Break functionality into bucketed counts
3. Add a site + survey level count if necessary, as it relates to functionality counts

Maybe TODO:

1. Develop a long version of site_agg that's easy to use with ggplot
2. In conjunction with #1, functionalize a handful of plotting scripts (one pie, one side-by-side bar, etc.). Will require some recoding for bucketed/count vars

Long-term TODO:

1. Develop operational status report. Goal is to present functionality and quality information that can be exposed to field managers to help them identify issues
