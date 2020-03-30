# COVID19
Models for simple log linear regression of data from usafacts.org

Caveats / Comments

This is my analysis, not Stanford’s analysis. My understanding is that Stanford’s internal analysis, done to plan resource allocation at Stanford, shows substantially longer doubling times. This is reassuring for those of us working at Stanford.

The data are from usafacts.org. Here is the file: https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv. The data are collected from state and county public health departments. 

The only statistical fits I’m doing are simple log-linear regressions, guided by visual inspection of the data. Nothing more. If you graph log cases vs. time on an Etch-a-Sketch, you can produce a nearly identical result with the magnet pen. 

I’m only projecting ahead by 1 week. I dropped the second week yesterday. Since the curves are flattening, the log linear projection for the second week is probably worse than “worst case” projections.

The idiosyncratic locations chosen for the analysis reflect where Pamela and I have friends and family. I’m happy to add other graphs of interest. Also, I’m happy to add people to the blind CC distribution list. Just let me know.

I’m hoping to move this to Shiny over the next week. If so, it will be available online, and anyone can access the analysis, specify the log linear portion for regression for any state or county, and project out for a week. I’ll also move the code to GitHub. 

I’ve added two map graphs: doubling rate, and 1 week projection of cases per 10,000 population. I plotted these to see the emerging hot spots. The news is (appropriately) focused on New York. My understanding is that epidemics start in the crowded cities, but that public health measures are also most quickly implemented in big cities. Rural communities eventually fare the worst. There is a hint of that in the map graphs. The most rapid doubling times include Idaho, Indiana, West Virginia, Oklahoma, and Missouri. We hear a lot about the Northeast states (Massachusetts, Pennsylvania, and New Jersey), and they still have the most cases. However, Washington has hugely increased doubling time (> 5 days currently), and New York has also made huge progress.

Steven L. Shafer, MD
