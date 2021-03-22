# Building Data Products with R

This repository contains all files to successfully build an HTML and PDF report of a bike sales dataset. Moreover, this repository includes files of two successful application deployments. These applications are deployed via [Shiny](https://shiny.rstudio.com/) and are currently served via [shinyapps.io](https://www.shinyapps.io/)

## Bike Sales Report & Dashboard

To view the bike sales report in HTML format, you will need to download [bike-sales-report.html](https://github.com/royruizzy/DataProducts_R-Deployment/raw/main/01_reporting_tools/bike-sales-report.html) and view it locally.

You may also, view the [Bike Sales Report](01_reporting_tools/bike-sales-report.pdf) in PDF format.

Furthermore, the bike sales dataset is implemented in a dashboard format by using `flexdashboard` via [Shiny](https://shiny.rstudio.com/). The dashboard is reactive where the user can selects from set of queries pertaining to the bike sales dataset. The dashboard includes metrics that will automatically update according to the inputs selected by the user. It also includes a chloropleth map, which covers geographic boundaries based on the data. Finally, the dashboard includes a visualization of total sales by year, quarter, month, week, and day. This can be viewed by clicking on the corresponding tabs within the dashboard.

To access the dashboard, please visit the following link: [Bike Sales Dashboard](https://royruiz.shinyapps.io/bike-sales-dashboard/)*

## Stock Market Analyzer

On another note, another application was deployed without the use of `flexdashboard`. This application allows the user to select a stock from a dropdown list pulled from different stock indices (DAX, S&P 500, DOW, NASDAQ100).

The application's functionality is designed to pull the past 180 days of stock data. The user can visualize a time series representation of the selected stock and adjust short (fast) and long (slow) moving averages with the use of sliders. Moreover, the plot includes an automated commentary output, which indicates positive or negative trends based on the moving averages. The user may select date range to adjust the default time frame.

 To access the web application, please visit the following link: [Stock Market Analyzer](https://royruiz.shinyapps.io/stock-market-analyzer/)*

\***shinyapps.io**: Server has a limit of 25 active hours per month. I ask that you use it wisely to avoid from hitting such limit. Thank you!
