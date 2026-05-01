# Carolina Hurricanes Food & Beverage Shiny App

This Shiny app was created for our ST 542 Carolina Hurricanes Food & Beverage price elasticity project.

The app lets the user explore the data, run the models, and test simple pricing scenarios for three research questions:

- **RQ1:** Category-level price elasticity
- **RQ2:** Item-level price elasticity
- **RQ3:** Event-level revenue drivers

The main app file is:

```text
app.R
```

---

## 1. How the Project Folder Should Look

Put `app.R` in one folder. Inside that same folder, create a folder named `data`.

The folder should look like this:

```text
project-folder/
│
├── app.R
│
└── data/
    ├── lineitems_data_v2.csv
    ├── orders_data_v2.csv
    ├── event_data.csv
    └── ticket_data_v2.csv
```

The app expects the raw files to be inside the `data` folder. In the code, this is set as:

```r
data_dir <- "data"
```

So the `data` folder must be in the same home/project directory as `app.R`.

The required files are:

| File | What it is used for |
|---|---|
| `lineitems_data_v2.csv` | Food and beverage transaction line items |
| `orders_data_v2.csv` | Order-level data used to connect sales to games |
| `event_data.csv` | Game/event data, including attendance |
| `ticket_data_v2.csv` | Ticket data used to calculate average ticket price |

If one of these files is missing, the app will stop and show which file is missing.

---

## 2. How to Run the App

### Option 1: Run in RStudio

1. Open RStudio.
2. Open `app.R`.
3. Make sure the `data` folder is in the same folder as `app.R`.
4. Make sure the four CSV files are inside the `data` folder.
5. Click **Run App**.

### Option 2: Run from the R Console

First set your working directory to the folder where `app.R` is saved.

Example:

```r
setwd("path/to/project-folder")
shiny::runApp()
```

---

## 3. R Packages Needed

The app uses these R packages:

```r
shiny
dplyr
readr
lubridate
tidyr
ggplot2
DT
broom
scales
```

To install them, run:

```r
install.packages(c(
  "shiny", "dplyr", "readr", "lubridate", "tidyr",
  "ggplot2", "DT", "broom", "scales"
))
```

---

## 4. Main Data Preparation

All model datasets are built inside `app.R` from the raw CSV files.

The app first reads the line-item, order, event, and ticket files. Then it creates a game-level table called `event_master`.

`event_master` is important because it stores the main game information used in all three models, such as:

- event date
- attendance
- average ticket price
- playoff game flag
- weekend game flag
- season year
- season segment: Early, Middle, or Late

After this, the app builds separate datasets for RQ1, RQ2, and RQ3.

---

## 5. RQ1 Dataset and Model: Category-Level Elasticity

RQ1 uses a **category-game dataset**.

This means each row is one product category during one Hurricanes home game.

Example:

```text
Beer during one home game
Snacks during one home game
Prepared Food during one home game
```

For each category-game row, the app calculates:

- total quantity sold
- total revenue
- average category price
- quantity per attendee
- revenue per attendee
- attendance
- average ticket price
- playoff/weekend/season information

The main response variable is:

```text
quantity per attendee
```

This is used because games have different attendance. Dividing by attendance makes games easier to compare.

### RQ1 Primary Model

RQ1 uses a log-log regression model.

The basic idea is:

```text
quantity per attendee ~ category price + attendance + ticket price + event factors
```

In the app, the model uses log quantity per attendee as the response and log average category price as the main price variable.

The coefficient for log average price is treated as the category elasticity estimate.

Example:

```text
If Beer elasticity = 1.628, then a 1% increase in Beer price is associated with about a 1.628% change in Beer quantity per attendee.
```

If the estimate is positive, the app warns to interpret it carefully. Normally, we expect higher price to reduce demand. In this project, positive values may happen because playoff games and high-demand games can have both higher prices and higher sales.

### RQ1 Prediction Model

The RQ1 scenario section lets the user change the category price and event settings.

The app uses:

- selected category
- baseline average category price
- selected price change percent
- scenario attendance
- scenario ticket price
- playoff/weekend setting
- season year

The model predicts quantity per attendee first. Then the app calculates:

```text
predicted total quantity = predicted quantity per attendee x scenario attendance
predicted total revenue = predicted total quantity x scenario price
```

So RQ1 shows how the selected category may perform under the selected scenario.

---

## 6. RQ2 Dataset and Model: Item-Level Elasticity

RQ2 uses an **item-game dataset**.

This means each row is one specific item during one Hurricanes home game.

Example:

```text
R&D Storm Brew 16 oz during one home game
Large Pretzel during one home game
```

For each item-game row, the app calculates:

- item name
- category
- total quantity sold
- total revenue
- average item price
- quantity per attendee
- revenue per attendee
- attendance
- average ticket price
- playoff/weekend/season information

Item-level data is harder to model because some items have fewer sales or very little price change. Because of that, RQ2 uses a borrowing-strength approach.

### RQ2 Primary Model

RQ2 has three main parts.

First, the app fits an item-level model for the selected item.

Second, the app also uses the matching category-level result from RQ1 as a prior.

Example:

```text
R&D Storm Brew 16 oz uses Beer as the category prior.
Large Pretzel uses Prepared Food as the category prior.
```

Third, the app combines the item estimate and the category estimate into a posterior elasticity estimate.

This makes the item estimate more stable than using only the item data by itself.

The app also reports a 95% credible interval so the user can see how uncertain the item estimate is.

### RQ2 Prediction Model

The RQ2 scenario section lets the user test a price change for a selected item.

The app uses:

- selected item
- baseline average item price
- selected item price change percent
- scenario attendance
- scenario ticket price
- playoff/weekend setting
- season year
- posterior elasticity estimate

The model predicts item quantity per attendee, then calculates:

```text
predicted total quantity = predicted quantity per attendee x scenario attendance
predicted total revenue = predicted total quantity x scenario item price
```

RQ2 is useful for looking at specific items, but the results should still be read carefully because item-level data can be noisy.

---

## 7. RQ3 Dataset and Model: Event-Level Revenue Drivers

RQ3 uses an **event-game dataset**.

This means each row is one Hurricanes home game.

For each game, the app calculates:

- total F&B revenue
- total F&B quantity
- F&B revenue per attendee
- F&B quantity per attendee
- attendance
- average ticket price
- playoff status
- weekend status
- season year
- season segment

The main response variable is:

```text
F&B revenue per attendee
```

This is used because total revenue naturally goes up when attendance is higher. Revenue per attendee shows how much each fan spends on average.

### RQ3 Primary Model

RQ3 uses a game-level regression model.

The basic idea is:

```text
F&B revenue per attendee ~ ticket price + attendance + playoff + weekend + season timing
```

The ticket price coefficient shows how average ticket price is associated with F&B revenue per attendee.

Example:

```text
If the ticket price coefficient is 0.057, then a 10% increase in average ticket price is associated with about a 0.57% increase in F&B revenue per attendee.
```

Playoff and weekend variables help show how game context affects food and beverage spending.

### RQ3 Prediction Model

The RQ3 scenario section lets the user test a ticket price scenario.

The app uses:

- baseline average ticket price
- scenario ticket price
- scenario attendance
- playoff/weekend setting
- season year

The model predicts F&B revenue per attendee. Then the app calculates:

```text
predicted total F&B revenue = predicted F&B revenue per attendee x scenario attendance
```

RQ3 helps explain why RQ1 and RQ2 can have positive elasticity estimates. Some games, like playoff or weekend games, may have both higher prices and higher sales.

---

## 8. Global Filters and Scenario Controls

The left side of the app has filters and scenario controls.

The global filters change the data used in the models:

- season filter
- include or exclude playoff games
- attendance range

The scenario controls do not change the raw data. They are only used for what-if predictions:

- scenario attendance
- scenario weekend setting
- scenario playoff setting
- scenario season year

---

## 9. Notes About the Results

The app is mainly a decision-support and exploration tool. It helps the user see patterns in the data and test simple scenarios.

Positive elasticity estimates should not be treated as proof that raising prices increases demand. In this project, positive estimates may happen because high-demand games can have higher prices and higher sales at the same time.

Controlled pricing tests would be needed before making strong pricing decisions.

---

## 10. Author

Saurabh Gupta  
ST 542 Statistical Consulting Project  
Carolina Hurricanes Food & Beverage Price Elasticity Analysis
