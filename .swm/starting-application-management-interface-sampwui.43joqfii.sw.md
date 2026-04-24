---
title: Starting Application Management Interface (SAMPWUI)
---
This document explains the SAMPWUI job flow that starts the Application Management Interface. It configures the management region using system parameters and connection details, enabling administrators to monitor and manage the application environment. The flow receives configuration inputs and produces logs and reports summarizing the startup process.

## Start Application Management Interface

Step in this section: `GENAPP1`.

This section launches the management interface environment and sets up system connections, regions, and controls necessary for application administrators to use and monitor the application within the CICSplex.

1. The system reads parameters from the SYSIN input to configure system resources, define region characteristics, and grant access rights for the management interface environment.
2. The connection details and interface configuration are taken from the EYUWUI input to establish TCP/IP host, port, and timeout settings for the management interface.
3. The program uses all application definitions, startup values, and port allocations to initialize the management region, set up connections with the CICSplex, and start relevant services allowing administrative access.
4. As the management region starts, logs are generated in EYULOG capturing the progress, errors, and status messages.
5. Reporting output is created in EYUWREP to summarize the startup status, region initialization results, and any relevant operational details for administrators.

### Input

**SYSIN**

System parameters and application definitions to configure the management interface.

Sample:

```
GRPLIST=(DFHLIST)
APPLID=<WUIAPPL>
SYSIDNT=IWUI
CPSMCONN=WUI
INITPARM=(EYU9VKEC='ENU',EYU9VWAN='ENU1')
TCPIP=YES
PARMERR=IGNORE
GMTEXT='WELCOME TO CICS TS'
CHKSTSK=CURRENT
CHKSTRM=CURRENT
USSHOME=NONE
SEC=NO
AICONS=YES
PGAIPGM=INACTIVE
EDSALIM=500M,
MXT=600
GMTRAN=CESN,
DFLTUSER=<SQLID>
MN=OFF
MNPER=OFF
MNEXC=OFF
RLS=YES
NCPLDFT=GENA
XCMD=NO
XDCT=NO
XFCT=NO
XJCT=NO
XPCT=NO
XPPT=NO
XPSB=NO
XTST=NO
XRES=NO
XTRAN=NO
STGPROT=YES
TRANISO=YES
CICSSVC=224
SPOOL=YES
STATRCD=ON
STATINT=003000
EDSALIM=900M,
MAXOPENTCBS=400
BMS=FULL
MCT=NO
IRCSTRT=YES
ISC=YES
DB2CONN=NO
/*
```

**EYUWUI**

Management interface connection and port information for HTTP and CMCIP.

Sample:

```
TCPIPHTTPHOST(YES)
TCPIPHOSTNAME(127.0.0.1)
TCPIPPORT(6345)
CMCIPORT(6346)
INACTIVETIMEOUT(3600)
DEFAULTCONTEXT(GNAPPLEX)
DEFAULTSCOPE(GNAPPLEX)
DEFAULTCMASCTXT(<CMASAPPL>)
AUTOIMPORTDSN(<CPSMHLQ>.SEYUVIEW)
AUTOIMPORTMEM(EYUEA*)
/*
```

### Output

**EYULOG**

Log output for management interface startup and activity.

**EYUWREP**

Management interface reporting file with status and results from startup.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtaG91c2UlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-house"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
