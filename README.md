# thewatertrust

Contains scripts that process base TWT data as gathered from field surveys.

Surveys take three forms, which are recoded and merged into site_agg.csv. This site-level aggregate is then used to feed a single KPI report (Short Summary) and visualizations. Deeper analyses may need to descend to the individual household-, water point-, or water quality-level surveys, but the site_agg document should serve for most questions and aggregations.

Both/New denotes whether an individual site has data for at least one Baseline and at least one Monitoring survey (both) and whether its Baseline survey was conducted on or after Jan 1 2015 (new). Midterm surveys are generally disregarded, as they don't provide a uniform and methodologically distinct perspective across sites.

Status as of Dec 20 2016:

1. Site aggregation process complete
2. Draft visualizations complete (for style, at least - some metrics still need to be built)
3. Comprehensive summary (KPI report) draft complete
4. Short summary complete and calculations errors resolved
5. Working on aggregation issues

TODO: 

- Break functionality into bucketed counts
- Create version of Short Summary that aggregates at the site level before rolling higher.

Maybe TODO:

- Develop a long version of site_agg that's easy to use with ggplot
- In conjunction with above, functionalize a handful of plotting scripts (one pie, one side-by-side bar, etc.). Will require some recoding for bucketed/count vars

Long-term TODO:

- Develop operational status report. Goal is to present functionality and quality information that can be exposed to field managers to help them identify issues. May be redundant given sweetdata sensors; if so, maybe focus less on functionality (which is well-covered in SD) and instead focus on health issues, management, etc.?

Finished TODO: (12/20/2016)

- Investigate and fix logic around the functionality indicators and other counts in the short summary
- Add a site + survey level count if necessary, as it relates to functionality counts

