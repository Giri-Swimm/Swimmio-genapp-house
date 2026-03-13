---
title: new
---
# Introduction

This document explains the design and implementation of a daily premium summary report generator. It covers:

1. How the program reads and processes input data.
2. How it accumulates and categorizes statistics.
3. How it formats and writes the report output.
4. How it handles file operations and error conditions.

# Program purpose and structure

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="4">

---

The program LGAPRPT1 reads a premium output file line by line, processes each record to accumulate statistics on premiums and risk scores, and generates a formatted management report with breakdowns by underwriting status and risk levels. This is described in the program header comments and the overall structure of the program.

```
      * PROGRAM: LGAPRPT1 - DAILY PREMIUM SUMMARY REPORT GENERATOR    *
      * PURPOSE: READS PREMIUM OUTPUT FILE AND GENERATES FORMATTED    *
      *          MANAGEMENT REPORTS WITH STATISTICS AND BREAKDOWNS    *
      * AUTHOR:  LGAP DEVELOPMENT TEAM                                 *
      *================================================================*
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="1">

---

The program is structured into divisions typical for COBOL: environment, data, and procedure. The procedure division organizes the workflow into clear steps: initialization, file handling, processing, reporting, and cleanup.

```
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGAPRPT1.
      *================================================================*
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="122">

---

&nbsp;

```
       PROCEDURE DIVISION.

       P100-MAIN.
           PERFORM P200-INIT
           PERFORM P300-OPEN-FILES
           PERFORM P400-WRITE-HEADERS
           PERFORM P500-PROCESS-RECORDS
           PERFORM P600-WRITE-SUMMARY
           PERFORM P700-CLOSE-FILES
           STOP RUN.
```

---

</SwmSnippet>

# File handling and data input

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="13">

---

The input file is assigned and opened as a line sequential file, with file status codes monitored for error handling. Similarly, the report output file is opened for writing with status checks.

```
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-IN-STAT.
           
           SELECT REPORT-FILE ASSIGN TO 'REPORT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-RPT-STAT.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="152">

---

&nbsp;

```
       P300-OPEN-FILES.
           OPEN INPUT INPUT-FILE
           IF NOT INPUT-OK
               DISPLAY 'ERROR: Cannot open input file: ' WS-IN-STAT
               STOP RUN
           END-IF
           
           OPEN OUTPUT REPORT-FILE
           IF NOT REPORT-OK
               DISPLAY 'ERROR: Cannot open report file: ' WS-RPT-STAT
               CLOSE INPUT-FILE
               STOP RUN
           END-IF.
```

---

</SwmSnippet>

<SwmSnippet path="/base/src/LGAPRPT1.cbl" line="23">

---

The input file structure is defined in the FILE SECTION, with a copybook used for the record layout. The report file is defined with a single line record for output.

```
       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-FILE.
           COPY OUTPUTREC.
```

---

</SwmSnippet>

```16~~

```

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
