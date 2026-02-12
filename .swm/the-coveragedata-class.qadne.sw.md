---
title: The CoverageData class
---
This document explains the <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> class in <SwmPath>[base/src/lgapdb04.java](base/src/lgapdb04.java)</SwmPath>. We will cover:

1. What <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> is and its purpose
2. Variables defined in <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken>
3. Functions related to <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> and their roles in premium calculation

# What is <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken>

<SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> is a static inner class in <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> that represents the insurance coverage details for a commercial property. It holds various coverage limits, deductibles, and peril indicators that are used as inputs in the premium calculation process. This class acts as a data container to encapsulate coverage-related parameters needed for actuarial computations.

<SwmSnippet path="/base/src/lgapdb04.java" line="124">

---

The constructor function <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> initializes the <SwmToken path="base/src/lgapdb04.java" pos="124:3:3" line-data="    public LGAPDB04(Connection connection) {">`LGAPDB04`</SwmToken> class with a database connection. This connection is used in premium calculations to load rates and other data.

```java
    public LGAPDB04(Connection connection) {
        this.connection = connection;
    }
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/lgapdb04.java" line="131">

---

The function <SwmToken path="base/src/lgapdb04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> is the main entry point for premium calculation. It takes <SwmToken path="base/src/lgapdb04.java" pos="131:7:7" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`InputData`</SwmToken>, <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken>, and <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken> objects and orchestrates the calculation by calling a sequence of private methods that perform specific actuarial computations.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="169:5:5" line-data="    private void p200Init(InputData input, CoverageData coverage, CalculationAreas calc) {">`p200Init`</SwmToken> initializes exposure calculations based on input data and coverage limits. It computes risk adjustment factors and calculates exposures such as building, contents, and business interruption exposures.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="202:5:5" line-data="    private void p300LoadRates(InputData input, CalculationAreas calc) throws SQLException {">`p300LoadRates`</SwmToken> loads base insurance rates for different perils from the database using the connection. It retrieves base rates, minimum and maximum premiums for fire, crime, flood, and weather perils.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="245:5:5" line-data="    private void p400ExperienceMod(InputData input, CalculationAreas calc,">`p400ExperienceMod`</SwmToken> calculates the experience modification factor based on the insured's years in business and claims history. This factor adjusts the premium to reflect the risk profile.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="281:5:5" line-data="    private void p500ScheduleMod(InputData input, CalculationAreas calc,">`p500ScheduleMod`</SwmToken> computes the schedule modification factor considering building age, protection class, occupancy hazard, and exposure density. This factor further adjusts the premium based on property characteristics.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="338:5:5" line-data="    private void p600BasePremium(InputData input, CoverageData coverage,">`p600BasePremium`</SwmToken> calculates the base premiums for each peril (fire, crime, flood, weather) using coverage limits, base rates, and schedule modifiers. It sums these to form the base premium amount.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="399:5:5" line-data="    private void p700CatLoading(CoverageData coverage, CalculationAreas calc,">`p700CatLoading`</SwmToken> calculates catastrophe loading based on coverage perils and applies catastrophe model factors to adjust the premium for catastrophic risk.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="435:5:5" line-data="    private void p800ExpenseAndProfit(CalculationAreas calc, OutputResults output) {">`p800ExpenseAndProfit`</SwmToken> computes expense and profit loadings as percentages of the base premium to cover operational costs and profit margin.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="453:5:5" line-data="    private void p900Discounts(InputData input, CoverageData coverage,">`p900Discounts`</SwmToken> calculates applicable discounts such as multi-peril discounts, claims-free discounts, and deductible credits, reducing the premium accordingly.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="510:5:5" line-data="    private void p950Taxes(OutputResults output) {">`p950Taxes`</SwmToken> applies tax calculations on the premium amount based on a fixed tax rate, adding the tax amount to the total premium.

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

The function <SwmToken path="base/src/lgapdb04.java" pos="523:5:5" line-data="    private void p999Final(CalculationAreas calc, OutputResults output) {">`p999Final`</SwmToken> performs final premium calculations by summing all components including base premium, catastrophe loading, expense, profit, discounts, and taxes to produce the total premium.

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

The <SwmToken path="base/src/lgapdb04.java" pos="551:7:7" line-data="    public static void main(String[] args) {">`main`</SwmToken> function demonstrates example usage of the <SwmToken path="base/src/lgapdb04.java" pos="557:1:1" line-data="            LGAPDB04 calculator = new LGAPDB04(conn);">`LGAPDB04`</SwmToken> class, including setting up input data, coverage data, and invoking the premium calculation process.

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

## <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken>

<SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> is used as a container for insurance coverage limits such as <SwmToken path="base/src/lgapdb04.java" pos="176:9:9" line-data="        calc.buildingExposure = coverage.buildingLimit.multiply(riskAdjustment);">`buildingLimit`</SwmToken>, <SwmToken path="base/src/lgapdb04.java" pos="177:9:9" line-data="        calc.contentsExposure = coverage.contentsLimit.multiply(riskAdjustment);">`contentsLimit`</SwmToken>, and <SwmToken path="base/src/lgapdb04.java" pos="178:9:9" line-data="        calc.biExposure = coverage.biLimit.multiply(riskAdjustment);">`biLimit`</SwmToken>. It is instantiated and passed to various methods that perform calculations related to insurance premiums and risk adjustments.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken>

The <SwmToken path="base/src/lgapdb04.java" pos="131:5:5" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`calculatePremium`</SwmToken> method takes <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> as an input parameter along with <SwmToken path="base/src/lgapdb04.java" pos="131:7:7" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`InputData`</SwmToken> and <SwmToken path="base/src/lgapdb04.java" pos="132:1:1" line-data="                                 OutputResults outputResults) throws SQLException {">`OutputResults`</SwmToken>. This method acts as the main entry point for premium calculations, utilizing the coverage limits stored in <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> to compute the insurance premium.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="136:1:1" line-data="        p200Init(inputData, coverageData, calc);">`p200Init`</SwmToken>

In the <SwmToken path="base/src/lgapdb04.java" pos="136:1:1" line-data="        p200Init(inputData, coverageData, calc);">`p200Init`</SwmToken> method, <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> is passed along with <SwmToken path="base/src/lgapdb04.java" pos="131:7:7" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`InputData`</SwmToken> and <SwmToken path="base/src/lgapdb04.java" pos="133:1:1" line-data="        CalculationAreas calc = new CalculationAreas();">`CalculationAreas`</SwmToken>. This method initializes exposure calculations, where <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> likely provides the coverage limits necessary for calculating risk adjustment factors.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="148:1:1" line-data="        p600BasePremium(inputData, coverageData, calc, outputResults);">`p600BasePremium`</SwmToken>

The <SwmToken path="base/src/lgapdb04.java" pos="148:1:1" line-data="        p600BasePremium(inputData, coverageData, calc, outputResults);">`p600BasePremium`</SwmToken> method uses <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> to calculate base premiums for each peril. <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> supplies the coverage limits that influence the premium calculations, combined with other inputs such as schedule modifiers and calculation areas.

## Usage in <SwmToken path="base/src/lgapdb04.java" pos="151:1:1" line-data="        p700CatLoading(coverageData, calc, outputResults);">`p700CatLoading`</SwmToken>

In the <SwmToken path="base/src/lgapdb04.java" pos="151:1:1" line-data="        p700CatLoading(coverageData, calc, outputResults);">`p700CatLoading`</SwmToken> method, <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> is used to calculate catastrophe loading. This method uses the coverage limits from <SwmToken path="base/src/lgapdb04.java" pos="131:12:12" line-data="    public void calculatePremium(InputData inputData, CoverageData coverageData,">`CoverageData`</SwmToken> to determine additional premium loadings related to catastrophic risks.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
