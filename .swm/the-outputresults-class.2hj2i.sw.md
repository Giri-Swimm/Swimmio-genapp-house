---
title: The OutputResults class
---
This document covers the <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> class in <SwmPath>[base/src/lgapdb04.java](base/src/lgapdb04.java)</SwmPath>. We will cover:

1. What <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is and its purpose.
2. Variables and functions defined in <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken>, including detailed explanations of key functions in <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> that interact with <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken>.

# What is <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken>

<SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is a static inner class within the <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> class that serves as a data structure to hold the results of insurance premium calculations. It stores calculated premiums for different perils, premium components such as base amount and loadings, and rating factors used in the premium calculation process. This class is used to encapsulate all output values from the premium calculation workflow, making it easier to manage and access the results after computations.

<SwmSnippet path="/base/src/lgapdb04.java" line="124">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> is the constructor of the <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> class. It initializes the database connection used for loading rates and other data necessary for premium calculations.

```java
    public LGAPDB04(Connection connection) {
        this.connection = connection;
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="131">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> is the main entry point for premium calculation. It takes input data, coverage data, and an <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> instance to perform a series of calculation steps, updating the <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> with computed premium values and factors.

```java
    public void calculatePremium(InputData inputData, CoverageData coverageData,
                                 OutputResults outputResults) throws SQLException {
        CalculationAreas calc = new CalculationAreas();

        // Initialize calculations
        p200Init(inputData, coverageData, calc);

        // Load base rates
        p300LoadRates(inputData, calc);

        // Calculate experience modifier
        p400ExperienceMod(inputData, calc, outputResults);

        // Calculate schedule modifier
        p500ScheduleMod(inputData, calc, outputResults);

        // Calculate base premiums
        p600BasePremium(inputData, coverageData, calc, outputResults);

        // Calculate catastrophe loading
        p700CatLoading(coverageData, calc, outputResults);

        // Calculate expense and profit loading
        p800ExpenseAndProfit(calc, outputResults);

        // Calculate discounts
        p900Discounts(inputData, coverageData, calc, outputResults);

        // Calculate taxes
        p950Taxes(outputResults);

        // Final calculations
        p999Final(calc, outputResults);
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="169">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="169:5:5" line-data="    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {">`p200Init`</SwmToken> initializes exposure calculations based on input and coverage data. It computes risk adjustment factors, exposures, total insured value, and exposure density, setting up the calculation context for subsequent steps.

```java
    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {
        // Calculate risk adjustment factor
        BigDecimal riskAdjustment = new BigDecimal(input.riskScore - 100)
            .divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP)
            .add(BigDecimal.ONE);

        // Calculate exposures with risk adjustment
        calc.buildingExposure = coverage.buildingLimit.multiply(riskAdjustment);
        calc.contentsExposure = coverage.contentsLimit.multiply(riskAdjustment);
        calc.biExposure = coverage.biLimit.multiply(riskAdjustment);

        // Calculate total insured value
        calc.totalInsuredVal = calc.buildingExposure
            .add(calc.contentsExposure)
            .add(calc.biExposure);

        // Calculate exposure density (per square foot)
        if (input.squareFootage > 0) {
            calc.exposureDensity = calc.totalInsuredVal
                .divide(new BigDecimal(input.squareFootage), 4, RoundingMode.HALF_UP);
        } else {
            calc.exposureDensity = new BigDecimal("100.00");
        }

        // Initialize base rates array
        for (int i = 0; i < 4; i++) {
            calc.baseRates[i] = BigDecimal.ZERO;
        }
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="202">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="202:5:5" line-data="    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {">`p300LoadRates`</SwmToken> loads base rates for different perils from the database. It queries the database using the connection and updates the calculation areas with base rate values used in premium computations.

```java
    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {
        String[] perilCodes = {"FI", "CR", "FL", "WE"};
        BigDecimal[] defaultRates = {
            new BigDecimal("0.008500"),
            new BigDecimal("0.006200"),
            new BigDecimal("0.012800"),
            new BigDecimal("0.009600")
        };

        String sql = "SELECT BASE_RATE, MIN_PREMIUM, MAX_PREMIUM " +
                     "FROM RATE_MASTER " +
                     "WHERE TERRITORY = ? " +
                     "AND CONSTRUCTION_TYPE = ? " +
                     "AND OCCUPANCY_CODE = ? " +
                     "AND PERIL_CODE = ? " +
                     "AND EFFECTIVE_DATE <= ? " +
                     "AND EXPIRY_DATE >= ?";

        LocalDate currentDate = LocalDate.now();

        try (PreparedStatement stmt = connection.prepareStatement(sql)) {
            for (int i = 0; i < perilCodes.length; i++) {
                stmt.setString(1, input.territory);
                stmt.setString(2, input.constructionType);
                stmt.setString(3, input.occupancyCode);
                stmt.setString(4, perilCodes[i]);
                stmt.setDate(5, Date.valueOf(currentDate));
                stmt.setDate(6, Date.valueOf(currentDate));

                try (ResultSet rs = stmt.executeQuery()) {
                    if (rs.next()) {
                        calc.baseRates[i] = rs.getBigDecimal("BASE_RATE");
                    } else {
                        calc.baseRates[i] = defaultRates[i];
                    }
                }
            }
        }
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="245">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="245:5:5" line-data="    private void p400ExperienceMod(InputData input, CalculationAreas calc,">`p400ExperienceMod`</SwmToken> calculates the experience modification factor based on the input data such as years in business and claims history. It updates the <SwmToken path="base/src/lgapdb04.java" pos="275:3:3" line-data="        output.experienceMod = expMod;">`experienceMod`</SwmToken> field in <SwmToken path="base/src/lgapdb04.java" pos="246:1:1" line-data="                                   OutputResults output) {">`OutputResults`</SwmToken> accordingly.

```java
    private void p400ExperienceMod(InputData input, CalculationAreas calc,
                                   OutputResults output) {
        BigDecimal expMod = BigDecimal.ONE;

        if (input.yearsInBusiness >= 5) {
            if (input.claimsCount5Yr == 0) {
                expMod = new BigDecimal("0.8500");
            } else {
                // Calculate loss ratio impact
                BigDecimal lossRatio = input.claimsAmount5Yr
                    .divide(calc.totalInsuredVal, 6, RoundingMode.HALF_UP);

                BigDecimal adjustment = lossRatio
                    .multiply(CREDIBILITY_FACTOR)
                    .multiply(new BigDecimal("0.50"));

                expMod = BigDecimal.ONE.add(adjustment);

                // Cap between 0.5 and 2.0
                if (expMod.compareTo(new BigDecimal("2.0000")) > 0) {
                    expMod = new BigDecimal("2.0000");
                }
                if (expMod.compareTo(new BigDecimal("0.5000")) < 0) {
                    expMod = new BigDecimal("0.5000");
                }
            }
        } else {
            expMod = new BigDecimal("1.1000");
        }

        output.experienceMod = expMod;
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="281">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="281:5:5" line-data="    private void p500ScheduleMod(InputData input, CalculationAreas calc,">`p500ScheduleMod`</SwmToken> computes the schedule modification factor considering building age, protection class, occupancy hazard, and exposure density. It caps the <SwmToken path="base/src/lgapdb04.java" pos="332:3:3" line-data="        output.scheduleMod = schedMod;">`scheduleMod`</SwmToken> value within defined limits and updates the <SwmToken path="base/src/lgapdb04.java" pos="282:1:1" line-data="                                 OutputResults output) {">`OutputResults`</SwmToken>.

```java
    private void p500ScheduleMod(InputData input, CalculationAreas calc,
                                 OutputResults output) {
        BigDecimal schedMod = BigDecimal.ZERO;

        // Building age factor
        if (input.yearBuilt >= 2010) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        } else if (input.yearBuilt >= 1990) {
            // No change
        } else if (input.yearBuilt >= 1970) {
            schedMod = schedMod.add(new BigDecimal("0.100"));
        } else {
            schedMod = schedMod.add(new BigDecimal("0.200"));
        }

        // Protection class factor
        int protectionClass = Integer.parseInt(input.protectionClass);
        if (protectionClass >= 1 && protectionClass <= 3) {
            schedMod = schedMod.subtract(new BigDecimal("0.100"));
        } else if (protectionClass >= 4 && protectionClass <= 6) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        } else if (protectionClass >= 7 && protectionClass <= 9) {
            // No change
        } else {
            schedMod = schedMod.add(new BigDecimal("0.150"));
        }

        // Occupancy hazard factor
        if (input.occupancyCode.startsWith("OFF")) {
            schedMod = schedMod.subtract(new BigDecimal("0.025"));
        } else if (input.occupancyCode.startsWith("MFG")) {
            schedMod = schedMod.add(new BigDecimal("0.075"));
        } else if (input.occupancyCode.startsWith("WHS")) {
            schedMod = schedMod.add(new BigDecimal("0.125"));
        }

        // Exposure density factor
        if (calc.exposureDensity.compareTo(new BigDecimal("500.00")) > 0) {
            schedMod = schedMod.add(new BigDecimal("0.100"));
        } else if (calc.exposureDensity.compareTo(new BigDecimal("50.00")) < 0) {
            schedMod = schedMod.subtract(new BigDecimal("0.050"));
        }

        // Cap schedule mod between -0.200 and +0.400
        if (schedMod.compareTo(new BigDecimal("0.400")) > 0) {
            schedMod = new BigDecimal("0.400");
        }
        if (schedMod.compareTo(new BigDecimal("-0.200")) < 0) {
            schedMod = new BigDecimal("-0.200");
        }

        output.scheduleMod = schedMod;
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="338">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="338:5:5" line-data="    private void p600BasePremium(InputData input, CoverageData coverage,">`p600BasePremium`</SwmToken> calculates base premiums for each peril (fire, crime, flood, weather) using coverage data, calculation areas, and schedule modifiers. It updates the respective peril premiums and total premium in <SwmToken path="base/src/lgapdb04.java" pos="339:6:6" line-data="                                 CalculationAreas calc, OutputResults output) {">`OutputResults`</SwmToken>.

```java
    private void p600BasePremium(InputData input, CoverageData coverage,
                                 CalculationAreas calc, OutputResults output) {
        BigDecimal scheduleModifier = BigDecimal.ONE.add(output.scheduleMod);

        // FIRE PREMIUM
        if (coverage.firePeril > 0) {
            BigDecimal fireExposure = calc.buildingExposure.add(calc.contentsExposure);
            output.firePremium = fireExposure
                .multiply(calc.baseRates[0])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.firePremium);
        }

        // CRIME PREMIUM
        if (coverage.crimePeril > 0) {
            BigDecimal crimeExposure = calc.contentsExposure
                .multiply(new BigDecimal("0.80"));
            output.crimePremium = crimeExposure
                .multiply(calc.baseRates[1])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.crimePremium);
        }

        // FLOOD PREMIUM
        if (coverage.floodPeril > 0) {
            output.floodPremium = calc.buildingExposure
                .multiply(calc.baseRates[2])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .multiply(new BigDecimal("1.25"))
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.floodPremium);
        }

        // WEATHER PREMIUM
        if (coverage.weatherPeril > 0) {
            BigDecimal weatherExposure = calc.buildingExposure.add(calc.contentsExposure);
            output.weatherPremium = weatherExposure
                .multiply(calc.baseRates[3])
                .multiply(output.experienceMod)
                .multiply(scheduleModifier)
                .multiply(TREND_FACTOR)
                .setScale(2, RoundingMode.HALF_UP);

            output.baseAmount = output.baseAmount.add(output.weatherPremium);
        }
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="399">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="399:5:5" line-data="    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,">`p700CatLoading`</SwmToken> calculates catastrophe loading based on coverage data and updates the <SwmToken path="base/src/lgapdb04.java" pos="429:3:3" line-data="        output.catLoadAmt = calc.catLoading.setScale(2, RoundingMode.HALF_UP);">`catLoadAmt`</SwmToken> in <SwmToken path="base/src/lgapdb04.java" pos="400:1:1" line-data="                                OutputResults output) {">`OutputResults`</SwmToken>. It considers various catastrophe factors to adjust the premium accordingly.

```java
    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,
                                OutputResults output) {
        calc.catLoading = BigDecimal.ZERO;

        // Hurricane loading (wind/weather peril)
        if (coverage.weatherPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.weatherPremium.multiply(HURRICANE_FACTOR)
            );
        }

        // Earthquake loading (affects all perils)
        calc.catLoading = calc.catLoading.add(
            output.baseAmount.multiply(EARTHQUAKE_FACTOR)
        );

        // Tornado loading (weather peril primarily)
        if (coverage.weatherPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.weatherPremium.multiply(TORNADO_FACTOR)
            );
        }

        // Flood catastrophe loading
        if (coverage.floodPeril > 0) {
            calc.catLoading = calc.catLoading.add(
                output.floodPremium.multiply(FLOOD_FACTOR)
            );
        }

        output.catLoadAmt = calc.catLoading.setScale(2, RoundingMode.HALF_UP);
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="435">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="435:5:5" line-data="    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {">`p800ExpenseAndProfit`</SwmToken> computes expense and profit loadings using predefined ratios and updates the corresponding fields in <SwmToken path="base/src/lgapdb04.java" pos="435:12:12" line-data="    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {">`OutputResults`</SwmToken>.

```java
    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {
        // Expense loading
        calc.expenseLoading = output.baseAmount
            .add(output.catLoadAmt)
            .multiply(EXPENSE_RATIO);
        output.expenseLoadAmt = calc.expenseLoading.setScale(2, RoundingMode.HALF_UP);

        // Profit loading
        calc.profitLoading = output.baseAmount
            .add(output.catLoadAmt)
            .add(calc.expenseLoading)
            .multiply(PROFIT_MARGIN);
        output.profitLoadAmt = calc.profitLoading.setScale(2, RoundingMode.HALF_UP);
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="453">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="453:5:5" line-data="    private void p900Discounts(InputData input, CoverageData coverage,">`p900Discounts`</SwmToken> calculates applicable discounts based on input data, coverage, and calculation areas. It updates the <SwmToken path="base/src/lgapdb04.java" pos="499:3:3" line-data="        output.discountAmt = output.baseAmount">`discountAmt`</SwmToken> field in <SwmToken path="base/src/lgapdb04.java" pos="454:6:6" line-data="                              CalculationAreas calc, OutputResults output) {">`OutputResults`</SwmToken> to reflect premium reductions.

```java
    private void p900Discounts(InputData input, CoverageData coverage,
                              CalculationAreas calc, OutputResults output) {
        calc.totalDiscount = BigDecimal.ZERO;

        // Multi-peril discount
        calc.multiPerilDisc = BigDecimal.ZERO;
        if (coverage.firePeril > 0 && coverage.crimePeril > 0 &&
            coverage.floodPeril > 0 && coverage.weatherPeril > 0) {
            calc.multiPerilDisc = new BigDecimal("0.100");
        } else if (coverage.firePeril > 0 && coverage.weatherPeril > 0 &&
                   (coverage.crimePeril > 0 || coverage.floodPeril > 0)) {
            calc.multiPerilDisc = new BigDecimal("0.050");
        }

        // Claims-free discount
        calc.claimsFreeDisc = BigDecimal.ZERO;
        if (input.claimsCount5Yr == 0 && input.yearsInBusiness >= 5) {
            calc.claimsFreeDisc = new BigDecimal("0.075");
        }

        // Deductible credit
        calc.deductibleCredit = BigDecimal.ZERO;
        if (coverage.fireDeductible.compareTo(new BigDecimal("10000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.025"));
        }
        if (coverage.windDeductible.compareTo(new BigDecimal("25000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.035"));
        }
        if (coverage.floodDeductible.compareTo(new BigDecimal("50000")) >= 0) {
            calc.deductibleCredit = calc.deductibleCredit
                .add(new BigDecimal("0.045"));
        }

        // Total discount
        calc.totalDiscount = calc.multiPerilDisc
            .add(calc.claimsFreeDisc)
            .add(calc.deductibleCredit);

        // Cap at 25%
        if (calc.totalDiscount.compareTo(new BigDecimal("0.250")) > 0) {
            calc.totalDiscount = new BigDecimal("0.250");
        }

        // Calculate discount amount
        output.discountAmt = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .multiply(calc.totalDiscount)
            .setScale(2, RoundingMode.HALF_UP);
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="510">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="510:5:5" line-data="    private void p950Taxes(OutputResults output) {">`p950Taxes`</SwmToken> calculates taxes on the premium by applying the tax rate to the base amount and updates the <SwmToken path="base/src/lgapdb04.java" pos="511:3:3" line-data="        output.taxAmt = output.baseAmount">`taxAmt`</SwmToken> field in <SwmToken path="base/src/lgapdb04.java" pos="510:7:7" line-data="    private void p950Taxes(OutputResults output) {">`OutputResults`</SwmToken>.

```java
    private void p950Taxes(OutputResults output) {
        output.taxAmt = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .subtract(output.discountAmt)
            .multiply(TAX_RATE)
            .setScale(2, RoundingMode.HALF_UP);
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="523">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="523:5:5" line-data="    private void p999Final(CalculationAreas calc, OutputResults output) {">`p999Final`</SwmToken> performs final premium calculations by aggregating all components such as base premium, loadings, discounts, and taxes to compute the total premium. It updates the <SwmToken path="base/src/lgapdb04.java" pos="525:3:3" line-data="        output.totalPremium = output.baseAmount">`totalPremium`</SwmToken> and <SwmToken path="base/src/lgapdb04.java" pos="535:3:3" line-data="            output.finalRateFactor = output.totalPremium">`finalRateFactor`</SwmToken> fields in <SwmToken path="base/src/lgapdb04.java" pos="523:12:12" line-data="    private void p999Final(CalculationAreas calc, OutputResults output) {">`OutputResults`</SwmToken>.

```java
    private void p999Final(CalculationAreas calc, OutputResults output) {
        // Calculate total premium
        output.totalPremium = output.baseAmount
            .add(output.catLoadAmt)
            .add(output.expenseLoadAmt)
            .add(output.profitLoadAmt)
            .subtract(output.discountAmt)
            .add(output.taxAmt)
            .setScale(2, RoundingMode.HALF_UP);

        // Calculate final rate factor
        if (calc.totalInsuredVal.compareTo(BigDecimal.ZERO) > 0) {
            output.finalRateFactor = output.totalPremium
                .divide(calc.totalInsuredVal, 6, RoundingMode.HALF_UP);

            // Cap rate factor at 5%
            if (output.finalRateFactor.compareTo(new BigDecimal("0.050000")) > 0) {
                output.finalRateFactor = new BigDecimal("0.050000");
                output.totalPremium = calc.totalInsuredVal
                    .multiply(output.finalRateFactor)
                    .setScale(2, RoundingMode.HALF_UP);
            }
        }
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="551">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="551:7:7" line-data="    public static void main(String[] args) {">`main`</SwmToken> demonstrates example usage of the <SwmToken path="base/src/lgapdb04.java" pos="557:1:1" line-data="            LGAPDB04 calculator = new LGAPDB04(conn);">`LGAPDB04`</SwmToken> class, including setting up input data, coverage data, invoking premium calculations, and handling results stored in <SwmToken path="base/src/lgapdb04.java" pos="589:1:1" line-data="            OutputResults results = new OutputResults();">`OutputResults`</SwmToken>.

```java
    public static void main(String[] args) {
        try {
            // Setup database connection (adjust connection parameters as needed)
            Connection conn = DriverManager.getConnection(
                "jdbc:db2://localhost:50000/INSURANCE", "user", "password");

            LGAPDB04 calculator = new LGAPDB04(conn);

            // Create sample input data
            InputData input = new InputData();
            input.customerNum = "CUST123456";
            input.riskScore = 105;
            input.propertyType = "Commercial";
            input.territory = "TX001";
            input.constructionType = "CON";
            input.occupancyCode = "OFF01";
            input.protectionClass = "03";
            input.yearBuilt = 2015;
            input.squareFootage = 50000;
            input.yearsInBusiness = 10;
            input.claimsCount5Yr = 1;
            input.claimsAmount5Yr = new BigDecimal("25000.00");

            // Create sample coverage data
            CoverageData coverage = new CoverageData();
            coverage.buildingLimit = new BigDecimal("2000000.00");
            coverage.contentsLimit = new BigDecimal("500000.00");
            coverage.biLimit = new BigDecimal("1000000.00");
            coverage.fireDeductible = new BigDecimal("10000.00");
            coverage.windDeductible = new BigDecimal("25000.00");
            coverage.floodDeductible = new BigDecimal("50000.00");
            coverage.otherDeductible = new BigDecimal("5000.00");
            coverage.firePeril = 1;
            coverage.crimePeril = 1;
            coverage.floodPeril = 1;
            coverage.weatherPeril = 1;

            // Calculate premium
            OutputResults results = new OutputResults();
            calculator.calculatePremium(input, coverage, results);

            // Print results
            System.out.println("=== Premium Calculation Results ===");
            System.out.println("Fire Premium: $" + results.firePremium);
            System.out.println("Crime Premium: $" + results.crimePremium);
            System.out.println("Flood Premium: $" + results.floodPremium);
            System.out.println("Weather Premium: $" + results.weatherPremium);
            System.out.println("\n--- Premium Components ---");
            System.out.println("Base Amount: $" + results.baseAmount);
            System.out.println("Catastrophe Loading: $" + results.catLoadAmt);
            System.out.println("Expense Loading: $" + results.expenseLoadAmt);
            System.out.println("Profit Loading: $" + results.profitLoadAmt);
            System.out.println("Discount Amount: -$" + results.discountAmt);
            System.out.println("Tax Amount: $" + results.taxAmt);
            System.out.println("\n--- Rating Factors ---");
            System.out.println("Experience Modifier: " + results.experienceMod);
            System.out.println("Schedule Modifier: " + results.scheduleMod);
            System.out.println("Final Rate Factor: " + results.finalRateFactor);
            System.out.println("\n=== TOTAL PREMIUM: $" + results.totalPremium + " ===");

            conn.close();

        } catch (SQLException e) {
            System.err.println("Database error: " + e.getMessage());
            e.printStackTrace();
        }
    }
```

---

</SwmSnippet>

# Usage

## <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken>

<SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is used to hold the results of various premium calculations, including premiums by peril such as fire and crime. It acts as a data structure that accumulates calculated values throughout the premium calculation process.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> method

In the main calculation entry point method, <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is passed as a parameter to collect the results of the premium calculations. This method initializes calculation areas and updates the <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> instance with computed premium values.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="142:1:1" line-data="        p400ExperienceMod(inputData, calc, outputResults);">`p400ExperienceMod`</SwmToken> method

<SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is used to store the experience modification factor calculated based on input data such as years in business. This factor influences the overall premium calculation and is updated within this method.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="145:1:1" line-data="        p500ScheduleMod(inputData, calc, outputResults);">`p500ScheduleMod`</SwmToken> method

Within the schedule modification calculation method, <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> holds the schedule modification factor. This factor is computed considering elements like building age and is added to the <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> to affect the final premium.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="148:1:1" line-data="        p600BasePremium(inputData, coverageData, calc, outputResults);">`p600BasePremium`</SwmToken> method

<SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> is used to store the base premiums for each peril after applying schedule modifiers. The method calculates the fire premium and other peril premiums, updating the <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> accordingly to reflect the final premium amounts.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
