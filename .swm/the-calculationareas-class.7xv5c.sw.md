---
title: The CalculationAreas class
---
This document explains the <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> class in <SwmPath>[base/src/LGAPDB04.java](base/src/LGAPDB04.java)</SwmPath>. We will cover:

1. What <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> is and its purpose
2. Variables and functions defined in <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> and related methods used in premium calculation

# What is <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken>

<SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> is a private static inner class within <SwmPath>[base/src/LGAPDB04.java](base/src/LGAPDB04.java)</SwmPath> that serves as a container for intermediate calculation values used during the insurance premium calculation process. It holds various <SwmToken path="base/src/LGAPDB04.java" pos="171:1:1" line-data="        BigDecimal riskAdjustment = new BigDecimal(input.riskScore - 100)">`BigDecimal`</SwmToken> fields representing exposures, base rates, discounts, and intermediate loading values. This class helps organize and maintain the state of these values throughout the multi-step premium calculation workflow.

<SwmSnippet path="/base/src/LGAPDB04.java" line="124">

---

The constructor function <SwmToken path="base/src/LGAPDB04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> initializes the <SwmToken path="base/src/LGAPDB04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> class with a database connection. This connection is used in premium calculations to retrieve base rates and other data.

```java
    public LGAPDB04(Connection connection) {
        this.connection = connection;
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPDB04.java" line="131">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> is the main entry point for calculating insurance premiums. It creates an instance of <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> to hold intermediate values and sequentially calls various private methods to perform steps such as initialization, loading rates, applying modifiers, calculating premiums, discounts, taxes, and finalizing the premium.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="169">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="169:5:5" line-data="    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {">`p200Init`</SwmToken> initializes exposure calculations within <SwmToken path="base/src/LGAPDB04.java" pos="169:17:17" line-data="    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {">`CalculationAreas`</SwmToken> based on input data and coverage limits. It calculates risk adjustment factors, exposures for building, contents, and business interruption, total insured value, and exposure density.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="202">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="202:5:5" line-data="    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {">`p300LoadRates`</SwmToken> loads base rates for different perils (fire, crime, flood, weather) from the database or uses default rates if not found. These rates are stored in the <SwmToken path="base/src/LGAPDB04.java" pos="233:3:3" line-data="                        calc.baseRates[i] = rs.getBigDecimal(&quot;BASE_RATE&quot;);">`baseRates`</SwmToken> array within <SwmToken path="base/src/LGAPDB04.java" pos="202:12:12" line-data="    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {">`CalculationAreas`</SwmToken>.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="245">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="245:5:5" line-data="    private void p400ExperienceMod(InputData input, CalculationAreas calc,">`p400ExperienceMod`</SwmToken> calculates the experience modification factor based on the input data such as years in business and claims history. This factor adjusts the premium to reflect the risk profile of the insured.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="281">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="281:5:5" line-data="    private void p500ScheduleMod(InputData input, CalculationAreas calc,">`p500ScheduleMod`</SwmToken> calculates the schedule modification factor considering building age, protection class, occupancy hazard, and exposure density. It caps the modifier within a specified range and stores it in the output results.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="338">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="338:5:5" line-data="    private void p600BasePremium(InputData input, CoverageData coverage,">`p600BasePremium`</SwmToken> calculates the base premiums for each peril (fire, crime, flood, weather) using coverage data, base rates, and schedule modifiers. It updates the output results with calculated premiums.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="399">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="399:5:5" line-data="    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,">`p700CatLoading`</SwmToken> calculates catastrophe loading based on coverage data and updates the <SwmToken path="base/src/LGAPDB04.java" pos="401:3:3" line-data="        calc.catLoading = BigDecimal.ZERO;">`catLoading`</SwmToken> field in <SwmToken path="base/src/LGAPDB04.java" pos="399:12:12" line-data="    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,">`CalculationAreas`</SwmToken> and the output results accordingly.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="435">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="435:5:5" line-data="    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {">`p800ExpenseAndProfit`</SwmToken> calculates expense and profit loadings using constants and updates the corresponding fields in <SwmToken path="base/src/LGAPDB04.java" pos="435:7:7" line-data="    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {">`CalculationAreas`</SwmToken> and output results.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="453">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="453:5:5" line-data="    private void p900Discounts(InputData input, CoverageData coverage,">`p900Discounts`</SwmToken> calculates various discounts such as multi-peril discount, claims-free discount, and deductible credits. These discounts are stored in <SwmToken path="base/src/LGAPDB04.java" pos="454:1:1" line-data="                              CalculationAreas calc, OutputResults output) {">`CalculationAreas`</SwmToken> and applied to the output results.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="510">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="510:5:5" line-data="    private void p950Taxes(OutputResults output) {">`p950Taxes`</SwmToken> calculates taxes on the base amount and updates the tax amount in the output results.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="523">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="523:5:5" line-data="    private void p999Final(CalculationAreas calc, OutputResults output) {">`p999Final`</SwmToken> performs the final premium calculation by summing all components including base premium, catastrophe loading, expense, profit, discounts, and taxes to produce the total premium.

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

<SwmSnippet path="/base/src/LGAPDB04.java" line="551">

---

The function <SwmToken path="base/src/LGAPDB04.java" pos="551:7:7" line-data="    public static void main(String[] args) {">`main`</SwmToken> provides an example usage of the <SwmToken path="base/src/LGAPDB04.java" pos="557:1:1" line-data="            LGAPDB04 calculator = new LGAPDB04(conn);">`LGAPDB04`</SwmToken> class, demonstrating how to instantiate it, prepare input data, coverage data, and output results, and invoke the premium calculation.

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

## <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken>

<SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> is instantiated within the <SwmToken path="base/src/LGAPDB04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> method to hold intermediate values for exposure and other calculation metrics. It acts as a shared data structure that is passed to multiple private methods responsible for different stages of the premium calculation process.

For example, in the <SwmToken path="base/src/LGAPDB04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> method, a new <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> object is created and then passed to the <SwmToken path="base/src/LGAPDB04.java" pos="136:1:1" line-data="        p200Init(inputData, coverageData, calc);">`p200Init`</SwmToken> method, which initializes exposure calculations and risk adjustment factors based on input data and coverage information.

Subsequently, <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> is also passed to methods like <SwmToken path="base/src/LGAPDB04.java" pos="139:1:1" line-data="        p300LoadRates(inputData, calc);">`p300LoadRates`</SwmToken> and <SwmToken path="base/src/LGAPDB04.java" pos="142:1:1" line-data="        p400ExperienceMod(inputData, calc, outputResults);">`p400ExperienceMod`</SwmToken>, which respectively load base rates from the database and calculate experience modification factors. This shows that <SwmToken path="base/src/LGAPDB04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken> serves as a central data holder that accumulates and carries calculation results through the various steps of the premium calculation workflow.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
