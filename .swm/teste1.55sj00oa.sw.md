---
title: Teste1
---
# Introduction

This document will walk you through the implementation of the job control language (JCL) script located at <SwmPath>[app/jcl/CARDFILE.jcl](/app/jcl/CARDFILE.jcl)</SwmPath>. The purpose of this script is to manage the VSAM files for card data, including deleting, defining, and copying data, as well as creating and managing alternate indexes.

We will cover:

1. Closing files in the CICS region.
2. Deleting existing VSAM files.
3. Defining new VSAM files.
4. Copying data from a flat file to the VSAM file.
5. Creating an alternate index.
6. Defining a path to relate the alternate index to the base cluster.
7. Building the alternate index cluster.
8. Opening files in the CICS region.

# Closing files in the CICS region

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="1">

---

First, we need to close the files in the CICS region to ensure they are not in use during the delete and define operations.

```
//CARDFILE JOB 'Delete define card data',CLASS=A,MSGCLASS=0,
// NOTIFY=&SYSUID              
//******************************************************************
//* Copyright Amazon.com, Inc. or its affiliates.                   
//* All Rights Reserved.                                            
//*                                                                 
//* Licensed under the Apache License, Version 2.0 (the "License"). 
//* You may not use this file except in compliance with the License.
//* You may obtain a copy of the License at                         
//*                                                                 
//*    http://www.apache.org/licenses/LICENSE-2.0                   
//*                                                                 
//* Unless required by applicable law or agreed to in writing,      
//* software distributed under the License is distributed on an     
//* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
//* either express or implied. See the License for the specific     
//* language governing permissions and limitations under the License
//******************************************************************       
//*********************************************************************         
//* Close files in CICS region                                                  
//*********************************************************************         
//CLCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) CLO'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) CLO'                                       
/*                                                                              
//*                                                                             
//* *******************************************************************         
```

---

</SwmSnippet>

# Deleting existing VSAM files

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="31">

---

Next, we delete any existing VSAM files to avoid conflicts when defining new ones. This step ensures that the environment is clean before creating new files.

```
//* DELETE CARD DATA VSAM FILE IF ONE ALREADY EXISTS                            
//* *******************************************************************         
//STEP05 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DELETE AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS -                                  
          CLUSTER                                                               
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
   DELETE AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX -                                   
          ALTERNATEINDEX                                                        
   IF MAXCC LE 08 THEN SET MAXCC = 0                                            
/*                                                                              
//*                                                                             
//* *******************************************************************         
//* DEFINE CARD DATA VSAM FILE                                                  
//* *******************************************************************         
//STEP10 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
   DEFINE CLUSTER (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS) -                   
          CYLINDERS(1 5) -                                                      
          VOLUMES(AWSHJ1 -                                                      
          ) -                                                                   
          KEYS(16 0) -                                                          
          RECORDSIZE(150 150) -                                                 
          SHAREOPTIONS(2 3) -                                                   
          ERASE -                                                               
          INDEXED -                                                             
          ) -                                                                   
          DATA (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS.DATA) -                 
          ) -                                                                   
```

---

</SwmSnippet>

# Defining new VSAM files

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="62">

---

We then define the new VSAM files. This step includes specifying the cluster, data, and index components of the VSAM file.

```
          INDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS.INDEX) -               
          )                                                                     
/*                                                                              
```

---

</SwmSnippet>

# Copying data from a flat file to the VSAM file

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="65">

---

After defining the VSAM files, we copy data from a flat file to the newly created VSAM file. This step ensures that the VSAM file contains the necessary data for further processing.

```
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//CARDDATA DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.PS                                      
//CARDVSAM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(CARDDATA) OUTFILE(CARDVSAM)                                     
/*                                                                              
//*-------------------------------------------------------------------*         
//* CREATE ALTERNATE INDEX ON ACCT ID                                           
//*-------------------------------------------------------------------*         
//STEP40  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   DEFINE ALTERNATEINDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)-              
   RELATE(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                    -              
   KEYS(11 16)                                                   -              
   NONUNIQUEKEY                                                  -              
   UPGRADE                                                       -              
   RECORDSIZE(150,150)                                           -              
   VOLUMES(AWSHJ1)                                               -              
   CYLINDERS(5,1))                                               -              
   DATA (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.DATA))           -              
   INDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.INDEX))                        
/*                                                                              
//*-------------------------------------------------------------------*         
```

---

</SwmSnippet>

# Creating an alternate index

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="65">

---

We create an alternate index on the VSAM file to allow for efficient data retrieval based on alternate keys. This step is crucial for optimizing data access patterns.

```
//* *******************************************************************         
//* COPY DATA FROM FLAT FILE TO VSAM FILE                                       
//* *******************************************************************         
//STEP15 EXEC PGM=IDCAMS                                                        
//SYSPRINT DD   SYSOUT=*                                                        
//CARDDATA DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.PS                                      
//CARDVSAM DD DISP=SHR,                                                         
//         DSN=AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS                               
//SYSIN    DD   *                                                               
   REPRO INFILE(CARDDATA) OUTFILE(CARDVSAM)                                     
/*                                                                              
//*-------------------------------------------------------------------*         
//* CREATE ALTERNATE INDEX ON ACCT ID                                           
//*-------------------------------------------------------------------*         
//STEP40  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   DEFINE ALTERNATEINDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)-              
   RELATE(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                    -              
   KEYS(11 16)                                                   -              
   NONUNIQUEKEY                                                  -              
   UPGRADE                                                       -              
   RECORDSIZE(150,150)                                           -              
   VOLUMES(AWSHJ1)                                               -              
   CYLINDERS(5,1))                                               -              
   DATA (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.DATA))           -              
   INDEX (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.INDEX))                        
/*                                                                              
//*-------------------------------------------------------------------*         
```

---

</SwmSnippet>

# Defining a path to relate the alternate index to the base cluster

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="95">

---

We define a path to relate the alternate index to the base cluster. This step establishes the relationship between the alternate index and the base VSAM file.

```
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*-------------------------------------------------------------------*         
//STEP50  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX))                               
/*                                                                              
//*------------------------------------------------------------------           
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*-------------------------------------------------------------------*         
//STEP60  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)                                
/*                                                                              
//*                                                                             
//*********************************************************************         
//* Open files in CICS region                                                   
//*********************************************************************         
//OPCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) OPE'                                       
/*                                                                              
//                                                                              
```

---

</SwmSnippet>

# Building the alternate index cluster

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="95">

---

Finally, we build the alternate index cluster to complete the setup of the alternate index. This step ensures that the alternate index is fully functional and ready for use.

```
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*-------------------------------------------------------------------*         
//STEP50  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX))                               
/*                                                                              
//*------------------------------------------------------------------           
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*-------------------------------------------------------------------*         
//STEP60  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)                                
/*                                                                              
//*                                                                             
//*********************************************************************         
//* Open files in CICS region                                                   
//*********************************************************************         
//OPCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) OPE'                                       
/*                                                                              
//                                                                              
```

---

</SwmSnippet>

# Opening files in the CICS region

<SwmSnippet path="/app/jcl/CARDFILE.jcl" line="95">

---

After all operations are complete, we open the files in the CICS region to make them available for use.

```
//* DEFINE PATH IS USED TO RELATE THE ALTERNATE INDEX TO BASE CLUSTER           
//*-------------------------------------------------------------------*         
//STEP50  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
  DEFINE PATH                                           -                       
   (NAME(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX.PATH)        -                       
    PATHENTRY(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX))                               
/*                                                                              
//*------------------------------------------------------------------           
//* BUILD ALTERNATE INDEX CLUSTER                                               
//*-------------------------------------------------------------------*         
//STEP60  EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
   BLDINDEX                                                      -              
   INDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS)                 -              
   OUTDATASET(AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX)                                
/*                                                                              
//*                                                                             
//*********************************************************************         
//* Open files in CICS region                                                   
//*********************************************************************         
//OPCIFIL EXEC PGM=SDSF                                                         
//ISFOUT DD SYSOUT=*                                                            
//CMDOUT DD SYSOUT=*                                                            
//ISFIN  DD *                                                                   
 /F CICSAWSA,'CEMT SET FIL(CARDDAT ) OPE'                                       
 /F CICSAWSA,'CEMT SET FIL(CARDAIX ) OPE'                                       
/*                                                                              
//                                                                              
```

---

</SwmSnippet>

# Epic

**Title:** Manage VSAM files for card data

**Description:** As a system administrator, I need to manage the VSAM files for card data, including deleting, defining, and copying data, as well as creating and managing alternate indexes, to ensure the data is properly organized and accessible.

# User Story

**Title:** Delete and define VSAM files for card data

**Description:** As a system administrator, I need to delete existing VSAM files and define new ones to ensure the environment is clean and the data is properly organized.

**Acceptance Criteria:**

1. Existing VSAM files are deleted if they exist.
2. New VSAM files are defined with the specified parameters.
3. Data is copied from a flat file to the newly created VSAM file.
4. An alternate index is created and related to the base cluster.
5. The alternate index cluster is built.
6. Files are opened in the CICS region after all operations are complete.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBYXdzLW1haW5mcmFtZS1tb2Rlcm5pemF0aW9uLWNhcmRkZW1vJTNBJTNBdmFsZGVjaXJjYXJ2YWxobw==" repo-name="aws-mainframe-modernization-carddemo"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
