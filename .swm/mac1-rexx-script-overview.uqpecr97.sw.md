---
title: Mac1 Rexx script overview
---
# What the script does

The mac1 Rexx script is designed to perform a series of text substitutions within an ISPF editor session. It replaces placeholder tokens enclosed in angle brackets with corresponding predefined symbolic names or values. This process effectively updates the content of the current member in the editor by substituting these placeholders with their actual values, streamlining the editing and configuration tasks. For example, if the script encounters the token <SwmToken path="base/exec/mac1.rexx" pos="16:6:8" line-data="&quot;Isredit Change &#39;&lt;CICSHLQ&gt;&#39; &#39;&quot;CICSHLQ&quot;&#39; All&quot;">`<CICSHLQ>`</SwmToken>, it replaces it with the string "CICSHLQ" throughout the member.

# Script flow

The script flow can be broken down into the following main steps:

- Initialize the ISPF environment and prepare for editing commands.
- Retrieve a set of predefined symbolic names or variables from the ISPF environment.
- For each placeholder token, perform a global replacement in the current editor member, substituting the token with its corresponding symbolic name.
- Determine the current member name and prepend a special character based on its value.
- Replace specific markers in the member with the modified member name.
- Cancel the ISPF edit session and exit the script.

```mermaid
flowchart TD
    A[Initialize ISPF environment] --> B[Retrieve symbolic names]
    B --> C[Replace placeholders with symbolic names]
    C --> D[Modify member name based on condition]
    D --> E[Replace markers with modified member name]
    E --> F[Cancel ISPF edit session and exit]
```

<SwmSnippet path="/base/exec/mac1.rexx" line="6">

---

First, the script sets the address to ISPEXEC to enable ISPF command execution and issues a trace command for debugging purposes.

```rexx
Address Ispexec
"Isredit Macro (TRACE)"

```

---

</SwmSnippet>

<SwmSnippet path="/base/exec/mac1.rexx" line="9">

---

Next, the script retrieves multiple symbolic names from the ISPF environment using the <SwmToken path="base/exec/mac1.rexx" pos="9:1:3" line-data="&quot;ISPEXEC VGET (CICSHLQ CPSMHLQ CICSLIC USRHLQ COBOLHLQ DB2HLQ CEEHLQ)&quot;">`ISPEXEC VGET`</SwmToken> command. These names represent various system or application-related identifiers that will be used for substitution in the editor content.

```rexx
"ISPEXEC VGET (CICSHLQ CPSMHLQ CICSLIC USRHLQ COBOLHLQ DB2HLQ CEEHLQ)"
"ISPEXEC VGET (CSDNAME DB2RUN SQLID DB2SSID DB2DBID DB2CCSID DB2PLAN)"
"ISPEXEC VGET (TORAPPL AORAPPL DORAPPL TORSYSID AORSYSID DORSYSID)"
"ISPEXEC VGET (CMASAPPL CMASYSID WUIAPPL WUISYSID WSIMHLQ)"
"ISPEXEC VGET (PDSDBRM PDSMACP PDSLOAD PDSMSGS WSIMLOG WSIMSTL)"
"ISPEXEC VGET (KSDSCUS KSDSPOL SOURCEX LOADX MAPCOPX DBRMLIX)"
"ISPEXEC VGET (WSIMLGX WSIMWSX WSIMMSX ZFSHOME)"
```

---

</SwmSnippet>

<SwmSnippet path="/base/exec/mac1.rexx" line="16">

---

Then, the script performs a series of global replacements in the current ISPF editor member. For each placeholder token enclosed in angle brackets, it replaces all occurrences with the corresponding symbolic name retrieved earlier. This is done using the <SwmToken path="base/exec/mac1.rexx" pos="16:1:3" line-data="&quot;Isredit Change &#39;&lt;CICSHLQ&gt;&#39; &#39;&quot;CICSHLQ&quot;&#39; All&quot;">`Isredit Change`</SwmToken> command for each token-symbolic name pair.

```rexx
"Isredit Change '<CICSHLQ>' '"CICSHLQ"' All"
"Isredit Change '<CPSMHLQ>' '"CPSMHLQ"' All"
"Isredit Change '<CICSLIC>' '"CICSLIC"' All"
"Isredit Change '<USRHLQ>' '"USRHLQ"' All"
"Isredit Change '<COBOLHLQ>' '"COBOLHLQ"' All"
"Isredit Change '<DB2HLQ>' '"DB2HLQ"' All"
"Isredit Change '<CEEHLQ>' '"CEEHLQ"' All"
"Isredit Change '<CSDNAME>' '"CSDNAME"' All"
"Isredit Change '<DB2RUN>' '"DB2RUN"' All"
"Isredit Change '<SQLID>' '"SQLID"' All"
"Isredit Change '<DB2SSID>' '"DB2SSID"' All"
"Isredit Change '<DB2DBID>' '"DB2DBID"' All"
"Isredit Change '<DB2CCSID>' '"DB2CCSID"' All"
"Isredit Change '<DB2PLAN>' '"DB2PLAN"' All"
"Isredit Change '<TORAPPL>' '"TORAPPL"' All"
"Isredit Change '<AORAPPL>' '"AORAPPL"' All"
"Isredit Change '<DORAPPL>' '"DORAPPL"' All"
"Isredit Change '<TORSYSID>' '"TORSYSID"' All"
"Isredit Change '<AORSYSID>' '"AORSYSID"' All"
"Isredit Change '<DORSYSID>' '"DORSYSID"' All"
"Isredit Change '<CMASAPPL>' '"CMASAPPL"' All"
"Isredit Change '<CMASYSID>' '"CMASYSID"' All"
"Isredit Change '<WUIAPPL>' '"WUIAPPL"' All"
"Isredit Change '<WUISYSID>' '"WUISYSID"' All"
"Isredit Change '<WSIMHLQ>' '"WSIMHLQ"' All"
"Isredit Change '<PDSDBRM>' '"WSIMHLQ"' All"
"Isredit Change '<PDSMACP>' '"WSIMHLQ"' All"
"Isredit Change '<PDSLOAD>' '"WSIMHLQ"' All"
"Isredit Change '<PDSMSGS>' '"WSIMHLQ"' All"
"Isredit Change '<WSIMLOG>' '"WSIMHLQ"' All"
"Isredit Change '<WSIMSTL>' '"WSIMHLQ"' All"
"Isredit Change '<KSDSPOL>' '"KSDSPOL"' All"
"Isredit Change '<KSDSCUS>' '"KSDSCUS"' All"
"Isredit Change '<SOURCEX>' '"SOURCEX"' All"
"Isredit Change '<LOADX>' '"LOADX"' All"
"Isredit Change '<MAPCOPX>' '"MAPCOPX"' All"
"Isredit Change '<DBRMLIX>' '"DBRMLIX"' All"
"Isredit Change '<WSIMWSX>' '"WSIMWSX"' All"
"Isredit Change '<WSIMMSX>' '"WSIMMSX"' All"
"Isredit Change '<WSIMLGX>' '"WSIMLGX"' All"
"Isredit Change '<ZFSHOME>' '"ZFSHOME"' All"
```

---

</SwmSnippet>

<SwmSnippet path="/base/exec/mac1.rexx" line="58">

---

After completing the substitutions, the script retrieves the current member name into a variable. It then checks if the member name equals a specific value ('ONCICS'). If so, it prepends a '#' character; otherwise, it prepends an '@' character. This modified member name is then used to replace certain markers in the editor content.

```rexx
"Isredit (memnme) = MEMBER"
If memnme = 'ONCICS' Then memnme = '#' || memnme
                     Else memnme = '@' || memnme
```

---

</SwmSnippet>

<SwmSnippet path="/base/exec/mac1.rexx" line="61">

---

Finally, the script replaces the markers in the editor with the modified member name, cancels the ISPF edit session to apply changes, and exits with a success code.

```rexx
"Isredit Replace" memnme ".zfirst .zlast"
"Isredit CANCEL"

Exit 0
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
