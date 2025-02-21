&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gMain
{adecomm/appserv.i}
DEFINE VARIABLE h_xtools                   AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gMain 
/* Input Help Window for Client */
/* Author: Y, Date: 20240823 */

/*
Input Parameter:
Verbose              : Show Verbose Debug Output message to programmer.
Parent Field Handle  : Assign Screen-Value of caller's field directly within this window
Pagination           : If TRUE, data retrieval will be divided into several pages to retrival time. If FALSE, all data will be retrieved at once.  
Page Offset          : Integer, how many records is displayed per page of pagination view.
Filter First         : If TRUE, data will be retrieved after user applies the filter. If FALSE, data will be retrived immediately.
Window Name          : Window Title for Input Help Window (Optional, Default : "Input Help")
Table Name           : Database Table Name for Browse Data (Required) (Multiple Items, Comma Separated)
Table Field          : Database Table Field for Browse Column (Required) (Multiple Items, Comma Separated for List Table, Hash Sign (#) Separated for Different Table Field)
Table Label          : Label for Browse Column. Need to have same items count with Table Field (Optional, Default : Table Field) (Multiple Items, Comma Separated)
Table Format         : Table data format for each Fields. (Optional, Default : Inherited Table Format) (Multiple Items, Comma Separated)
Table Query          : Query for Database Table (Optional for single table, Required For Multiple Table, Default : "FOR EACH " + Table Name) (Multiple Items, Comma Separated)
Table Where          : Where Clause of the Query (Optional, Default : None) (NOTE : IF YOU USE WHERE CLAUSE IN 'TABLE QUERY' LAST TABLE, PLEASE USE 'AND TRUE' TO PREVENT DOUBLE WHERE QUERY)
Table Index          : Specify USE-INDEX and NO-LOCK here (Optional, Default : None).
Table Order          : Sort Clause of the Query (Optional, Default : None)
Field to Return      : Field of Record Value need to be returned (Optional, Default: First Table Field) (Single Value)
*/

CREATE WIDGET-POOL.

{src/adm2/widgetprto.i}
{sharevar.i}

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iplVerbose      AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER iphParentField  AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER iplPagination   AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipiPageOffset   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iplFilterFirst  AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipcWindowName   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableField   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableLabel   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcFieldFormat  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableQuery   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableWhere   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableIndex   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableOrder   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcReturnField  AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ix              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iy              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPage           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageLow        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageTop        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRecordCount    AS INTEGER     NO-UNDO.
DEFINE VARIABLE lError          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFilterFirstDefault    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTableWhere     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTableOrder     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInitialOrder   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWhereType      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWhereClause    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOrderClause    AS CHARACTER   NO-UNDO.

/* Handle Definitions */
DEFINE VARIABLE hDynTable       AS HANDLE      NO-UNDO.
DEFINE VARIABLE btt             AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE      NO-UNDO.
DEFINE VARIABLE dhBrowse        AS HANDLE      NO-UNDO.
DEFINE VARIABLE dhBrowseCol     AS HANDLE      NO-UNDO.

/* Dynamic Widget Handle */
DEFINE VARIABLE hFilterWidget   AS HANDLE      EXTENT 6 NO-UNDO.
DEFINE VARIABLE hWidgets        AS HANDLE      EXTENT 2 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-7 RECT-8 RECT-A RECT-235 ~
btnPageFirst btnPagePrev btnPageNext btnChoose cbFilter cbSort cbSortOrder ~
btnDefault btnApply fiStatus FILL-IN-3 FILL-IN-6 
&Scoped-Define DISPLAYED-OBJECTS cbFilter cbSort cbSortOrder fiStatus ~
FILL-IN-3 FILL-IN-6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     LABEL "Apply" 
     SIZE 16 BY 1.14 TOOLTIP "or press Enter to apply filters."
     FONT 6.

DEFINE BUTTON btnChoose 
     LABEL "Choose" 
     SIZE 15 BY 1.14 TOOLTIP "or Double-Click the record to choose"
     FONT 6.

DEFINE BUTTON btnDefault 
     LABEL "Default" 
     SIZE 15 BY 1.14
     FONT 6.

DEFINE BUTTON btnPageFirst 
     LABEL "First" 
     SIZE 9 BY 1.14 TOOLTIP "First Records Page".

DEFINE BUTTON btnPageNext 
     LABEL "Next" 
     SIZE 9 BY 1.14 TOOLTIP "Next Records Page".

DEFINE BUTTON btnPagePrev 
     LABEL "Prev" 
     SIZE 9 BY 1.14 TOOLTIP "Previous Records Page".

DEFINE VARIABLE cbFilter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "x","x"
     DROP-DOWN-LIST
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE cbSort AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "x","x"
     DROP-DOWN-LIST
     SIZE 17.8 BY 1 NO-UNDO.

DEFINE VARIABLE cbSortOrder AS CHARACTER FORMAT "X(256)":U INITIAL "asc" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ASC","DESC" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter:" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Sort:" 
      VIEW-AS TEXT 
     SIZE 5 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 54 BY .62
     BGCOLOR 15 FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 108 BY 13.57.

DEFINE RECTANGLE RECT-235
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 1
     BGCOLOR 13 FGCOLOR 13 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 108 BY 4.81
     BGCOLOR 11 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY .05.

DEFINE RECTANGLE RECT-A
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 108 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gMain
     btnPageFirst AT ROW 13.24 COL 59.6 WIDGET-ID 352
     btnPagePrev AT ROW 13.24 COL 69.8 WIDGET-ID 356
     btnPageNext AT ROW 13.24 COL 80 WIDGET-ID 354
     btnChoose AT ROW 13.24 COL 93.4 WIDGET-ID 350
     cbFilter AT ROW 15.52 COL 10.8 COLON-ALIGNED NO-LABEL WIDGET-ID 358
     cbSort AT ROW 16.71 COL 10.8 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     cbSortOrder AT ROW 16.71 COL 29.8 COLON-ALIGNED NO-LABEL WIDGET-ID 362
     btnDefault AT ROW 18.38 COL 76.4 WIDGET-ID 348
     btnApply AT ROW 18.38 COL 92.4 WIDGET-ID 346
     fiStatus AT ROW 13.52 COL 4.4 NO-LABEL WIDGET-ID 368
     FILL-IN-3 AT ROW 15.71 COL 4.4 NO-LABEL WIDGET-ID 364
     FILL-IN-6 AT ROW 16.91 COL 4.4 NO-LABEL WIDGET-ID 366
     RECT-1 AT ROW 1.24 COL 2.4 WIDGET-ID 370
     RECT-7 AT ROW 15.05 COL 2.4 WIDGET-ID 374
     RECT-8 AT ROW 18.1 COL 4.4 WIDGET-ID 376
     RECT-A AT ROW 1.24 COL 2.4 WIDGET-ID 382
     RECT-235 AT ROW 13.33 COL 91.2 WIDGET-ID 384
     SPACE(19.99) SKIP(5.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gMain 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gMain
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gMain:SCROLLABLE       = FALSE
       FRAME gMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME gMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME gMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiStatus IN FRAME gMain
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gMain
/* Query rebuild information for DIALOG-BOX gMain
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gMain gMain
ON HELP OF FRAME gMain
ANYWHERE
DO:
  APPLY 'entry' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gMain gMain
ON WINDOW-CLOSE OF FRAME gMain
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
    IF VALID-HANDLE(hDynTable) THEN
        DELETE OBJECT hDynTable.

    DELETE WIDGET-POOL NO-ERROR.
    APPLY "END-ERROR":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply gMain
ON CHOOSE OF btnApply IN FRAME gMain /* Apply */
DO:
    /* Escaping Some characters that might cause error (" and ') */
    DO ix = 1 TO EXTENT(hFilterWidget):
        IF NOT VALID-HANDLE(hFilterWidget[ix]) OR
            hFilterWidget[ix]:TYPE = 'TEXT' THEN NEXT.

        hFilterWidget[ix]:SCREEN-VALUE = REPLACE(hFilterWidget[ix]:SCREEN-VALUE, CHR(34),"").
        hFilterWidget[ix]:SCREEN-VALUE = REPLACE(hFilterWidget[ix]:SCREEN-VALUE, CHR(39),"").

    END.

    /* Jika user menggunakan Filter Value */
    IF cbFilter:SCREEN-VALUE <> ? AND VALID-HANDLE(hFilterWidget[1]) THEN DO:
        DEFINE VARIABLE cDateClause     AS CHARACTER   EXTENT 3 NO-UNDO.
        DEFINE VARIABLE cDateClauseList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE dtFilterDate    AS DATE      NO-UNDO.

        ASSIGN ipcTableWhere = cTableWhere.

        IF ipcTableWhere = '' THEN ASSIGN ipcTableWhere = 'WHERE'.
        ELSE ASSIGN ipcTableWhere = ipcTableWhere + ' AND'.  

        /*------- Filter Type Conditioning -------*/
        IF cWhereType = 'character' THEN DO:
            IF hFilterWidget[2]:SCREEN-VALUE = 'YES' THEN
                ASSIGN cWhereClause = "&1 &2 MATCHES '&3'".
            ELSE ASSIGN cWhereClause = "&1 &2 MATCHES '*&3*'".
            
            ASSIGN ipcTableWhere = SUBSTITUTE(
                cWhereClause, ipcTableWhere, cbFilter:SCREEN-VALUE, hFilterWidget[1]:SCREEN-VALUE
                ). 
        END.
        ELSE IF cWhereType = 'date' THEN DO:
            /* Date Validation to prevent invalid full date  */
            IF  hFilterWidget[2]:SCREEN-VALUE <> '' AND
                hFilterWidget[1]:SCREEN-VALUE <> '' THEN 
            DO:
                DEFINE VARIABLE iYear AS INTEGER     NO-UNDO.
                IF hFilterWidget[3]:SCREEN-VALUE = '' THEN
                    ASSIGN iYear = INT(YEAR(TODAY)).
                ELSE ASSIGN iYear = INT(hFilterWidget[3]:SCREEN-VALUE).

                ASSIGN dtFilterDate = DATE(
                    INT(hFilterWidget[2]:SCREEN-VALUE),
                    INT(hFilterWidget[1]:SCREEN-VALUE),
                    iYear
                ) NO-ERROR.

                IF ERROR-STATUS:ERROR THEN DO:
                    MESSAGE "You entered invalid date." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
                    APPLY 'ENTRY' TO hFilterWidget[1].
                    RETURN NO-APPLY.
                END.
            END.

            /* &2 is cbFilter Screen-Value */
            ASSIGN 
                cDateClauseList = " AND DAY(&2) = , AND MONTH(&2) = , AND YEAR(&2) = ".
            ASSIGN cWhereClause = "&1 TRUE".

            DO ix = 1 TO EXTENT(cDateClause):
                ASSIGN cDateClause[ix] = " AND TRUE".
                IF hFilterWidget[ix]:SCREEN-VALUE <> '' THEN
                ASSIGN cDateClause[ix] = 
                    ENTRY(ix, cDateClauseList) + 
                    hFilterWidget[ix]:SCREEN-VALUE.
                ASSIGN cWhereClause = cWhereClause + cDateClause[ix].
            END.
            
            ASSIGN ipcTableWhere = SUBSTITUTE(
                cWhereClause, ipcTableWhere, cbFilter:SCREEN-VALUE
                ). 
        END.

        ELSE IF cWhereType = 'logical' THEN DO:
            ASSIGN cWhereClause = "&1 &2 = &3".
            
            ASSIGN ipcTableWhere = SUBSTITUTE(
                cWhereClause, ipcTableWhere, cbFilter:SCREEN-VALUE, hFilterWidget[1]:SCREEN-VALUE
                ). 
        END.

        ELSE IF cWhereType = 'integer' OR cWhereType = 'decimal' THEN DO:
            ASSIGN 
                cWhereClause = "&1 &2 &3 &4"
                
                ipcTableWhere = SUBSTITUTE(
                cWhereClause, 
                ipcTableWhere, 
                cbFilter:SCREEN-VALUE, 
                hFilterWidget[1]:SCREEN-VALUE,
                DECIMAL(hFilterWidget[2]:SCREEN-VALUE)
                ). 
        END.

        ELSE
            MESSAGE "Unknown Filter Error Occured!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    /* Jika user menggunakan sort */
    IF cbSort:SCREEN-VALUE <> ? THEN DO:
        ASSIGN ipcTableOrder = SUBSTITUTE(
            "BY &1 &2",
            cbSort:SCREEN-VALUE,
            REPLACE(cbSortOrder:SCREEN-VALUE, 'ASC', '')
            ).
    END.

    /* condition if user need to filter first to show data */
    IF iplFilterFirst THEN
        ASSIGN
            btnChoose:SENSITIVE = TRUE
            btnPageNext:SENSITIVE = TRUE
            iplFilterFirst = FALSE.

    IF (cbFilter:SCREEN-VALUE <> ? AND VALID-HANDLE(hFilterWidget[1])) OR 
    cbSort:SCREEN-VALUE <> ? THEN DO:
        ASSIGN iPage = 0.
        RUN getData.
        RUN displayData.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnChoose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChoose gMain
ON CHOOSE OF btnChoose IN FRAME gMain /* Choose */
DO:
  ASSIGN iphParentField:SCREEN-VALUE = btt:BUFFER-FIELD(ipcReturnField):BUFFER-VALUE.
  APPLY 'WINDOW-CLOSE' TO FRAME gMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault gMain
ON CHOOSE OF btnDefault IN FRAME gMain /* Default */
DO:
    DO ix = 1 TO EXTENT(hFilterWidget):
        IF VALID-HANDLE(hFilterWidget[ix]) THEN
            DELETE OBJECT hFilterWidget[ix].
    END.

    ASSIGN
        hWidgets[1]:VISIBLE = TRUE
        hWidgets[1]:SENSITIVE = TRUE
        cbSortOrder:SENSITIVE = FALSE
        cbSortOrder:VISIBLE = FALSE
        btnApply:SENSITIVE = FALSE
        ipcTableWhere = cTableWhere
        ipcTableOrder = cTableOrder
        iPage = 0
        iplFilterFirst = lFilterFirstDefault
        .

    RUN populateFilter.
    RUN getData.
    RUN displayData.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPageFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageFirst gMain
ON CHOOSE OF btnPageFirst IN FRAME gMain /* First */
DO:
    IF iPage >= 1 THEN DO:
        ASSIGN iPage = 0.
    
        RUN getData.
        RUN displayData.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPageNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageNext gMain
ON CHOOSE OF btnPageNext IN FRAME gMain /* Next */
DO:
    IF btt:AVAILABLE THEN
    DO:
        ASSIGN iPage = iPage + 1.
    
        RUN getData.
        RUN displayData.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPagePrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPagePrev gMain
ON CHOOSE OF btnPagePrev IN FRAME gMain /* Prev */
DO:
    IF iPage >= 1 THEN DO:
        ASSIGN iPage = iPage - 1.
    
        RUN getData.
        RUN displayData.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFilter gMain
ON VALUE-CHANGED OF cbFilter IN FRAME gMain
DO:
    ASSIGN 
        cWhereType = btt:buffer-field(cbFilter:SCREEN-VALUE):DATA-TYPE
        btnApply:SENSITIVE = TRUE.

    DO ix = 1 TO EXTENT(hFilterWidget):
        IF VALID-HANDLE(hFilterWidget[ix]) THEN
            DELETE OBJECT hFilterWidget[ix].
    END.
    
    IF cWhereType = 'character' THEN 
    DO:
        /* string field */
        CREATE FILL-IN hFilterWidget[1] ASSIGN
            FORMAT      = "x(55)"
            COLUMN      = cbFilter:COLUMN + cbFilter:WIDTH + 2
            ROW         = cbFilter:ROW     
            WIDTH       = 50
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            .
        /* Adv Filter (user can use wildcard) toggle box */    
        CREATE TOGGLE-BOX hFilterWidget[2] ASSIGN
            COLUMN      = hFilterWidget[1]:COLUMN + hFilterWidget[1]:WIDTH + 2
            ROW         = hFilterWidget[1]:ROW      
            WIDTH       = 20
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            BGCOLOR     = 11
            LABEL       = 'Use Wildcards (*)'
            TOOLTIP     = 
                'Use the asterisk (*) as a wildcard ' + 
                'to search for multiple variations of a word.~n' +
                'For example, search "cat*" to find "cat," "cats," "catfish," and more.'
            .
    END.

    ELSE IF cWhereType = 'date' THEN 
    DO:
        CREATE TEXT hFilterWidget[4] ASSIGN
            COLUMN      = cbFilter:COLUMN + cbFilter:WIDTH + 2
            ROW         = cbFilter:ROW     
            WIDTH       = 5
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            BGCOLOR     = 11
            SCREEN-VALUE = 'Day:'
            .

        /* Day field */
        CREATE FILL-IN hFilterWidget[1] ASSIGN
            DATA-TYPE   = 'INTEGER'
            FORMAT      = ">>"
            COLUMN      = hFilterWidget[4]:COLUMN + hFilterWidget[4]:WIDTH + 1
            ROW         = hFilterWidget[4]:ROW     
            WIDTH       = 4
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            TOOLTIP     = "DAY (DD)"
            .

        CREATE TEXT hFilterWidget[5] ASSIGN
            COLUMN      = hFilterWidget[1]:COLUMN + hFilterWidget[1]:WIDTH + 1
            ROW         = hFilterWidget[1]:ROW     
            WIDTH       = 8
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            BGCOLOR     = 11
            SCREEN-VALUE = 'Month:'
            .

        /* Month field */
        CREATE FILL-IN hFilterWidget[2] ASSIGN
            DATA-TYPE   = 'INTEGER'
            FORMAT      = ">>"
            COLUMN      = hFilterWidget[5]:COLUMN + hFilterWidget[5]:WIDTH + 1
            ROW         = hFilterWidget[5]:ROW     
            WIDTH       = 4
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            TOOLTIP     = "MONTH (MM)"
            .

        CREATE TEXT hFilterWidget[6] ASSIGN
            COLUMN      = hFilterWidget[2]:COLUMN + hFilterWidget[2]:WIDTH + 1
            ROW         = hFilterWidget[2]:ROW     
            WIDTH       = 6
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            BGCOLOR     = 11
            SCREEN-VALUE = 'Year:'
            .

        /* Year field */
        CREATE FILL-IN hFilterWidget[3] ASSIGN
            DATA-TYPE   = 'INTEGER'
            FORMAT      = ">>>>"
            COLUMN      = hFilterWidget[6]:COLUMN + hFilterWidget[6]:WIDTH + 1
            ROW         = hFilterWidget[6]:ROW     
            WIDTH       = 8
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            TOOLTIP     = "YEAR (YYYY)"
            .
    END.
    
    ELSE IF cWhereType = 'logical' THEN DO:
        CREATE RADIO-SET hFilterWidget[1] ASSIGN
            RADIO-BUTTONS = "TRUE,TRUE,FALSE,FALSE"
            BGCOLOR     = 11
            HORIZONTAL  = TRUE
            COLUMN      = cbFilter:COLUMN + cbFilter:WIDTH + 1
            ROW         = cbFilter:ROW     
            WIDTH       = 24
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            .
    END.

    ELSE IF cWhereType = 'integer' OR cWhereType = 'decimal' THEN DO:
        CREATE COMBO-BOX hFilterWidget[1] ASSIGN
            COLUMN      = cbFilter:COLUMN + cbFilter:WIDTH + 2
            ROW         = cbFilter:ROW     
            WIDTH       = 10
            INNER-LINES = 6
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            LIST-ITEMS  = "=,>,<,>=,<=,<>"
            .

        CREATE FILL-IN hFilterWidget[2] ASSIGN
            DATA-TYPE   = 'DECIMAL'
            FORMAT      = "->>>,>>>,>>>,>>9"
            COLUMN      = hFilterWidget[1]:COLUMN + hFilterWidget[1]:WIDTH + 2
            ROW         = hFilterWidget[1]:ROW     
            WIDTH       = 46
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            .

        ASSIGN hFilterWidget[1]:SCREEN-VALUE = hFilterWidget[1]:ENTRY(1).
    END.

    /* For not implemented data type */
    ELSE DO:
        CREATE TEXT hFilterWidget[1] ASSIGN
            COLUMN      = cbFilter:COLUMN + cbFilter:WIDTH + 2
            ROW         = cbFilter:ROW     
            WIDTH       = 57
            HEIGHT      = 1
            FRAME       = FRAME gMain:HANDLE
            SENSITIVE   = TRUE
            VISIBLE     = TRUE
            BGCOLOR     = 11
            FORMAT      = "x(135)"
            SCREEN-VALUE = "You can't filter this column"
            .
        ASSIGN btnApply:SENSITIVE = FALSE.
    END.

    /* Move Widget Tab Order after SELF and entry to it, 
    also If user press enter, apply the filter */
    DO ix = EXTENT(hFilterWidget) TO 1 BY -1:
        IF NOT VALID-HANDLE(hFilterWidget[ix]) OR
            hFilterWidget[ix]:TYPE = 'TEXT'
            THEN NEXT.
        
        hFilterWidget[ix]:MOVE-AFTER-TAB-ITEM(SELF).
        APPLY 'ENTRY' TO hFilterWidget[ix].

        ON 'RETURN' OF hFilterWidget[ix]
            PERSISTENT RUN widgetEvent IN THIS-PROCEDURE.

        ON 'LEAVE' OF hFilterWidget[ix]
            PERSISTENT RUN widgetEvent4 IN THIS-PROCEDURE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSort gMain
ON RETURN OF cbSort IN FRAME gMain
DO:
  APPLY 'ENTRY' TO cbSortOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSort gMain
ON VALUE-CHANGED OF cbSort IN FRAME gMain
DO:
    ASSIGN
        hWidgets[1]:VISIBLE = FALSE
        hWidgets[1]:SENSITIVE = FALSE
        cbSortOrder:VISIBLE = TRUE
        cbSortOrder:SENSITIVE = TRUE
        btnApply:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSortOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSortOrder gMain
ON VALUE-CHANGED OF cbSortOrder IN FRAME gMain
OR 'RETURN' OF cbSortOrder
DO:
  APPLY 'CHOOSE' TO btnApply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gMain 


/* ***************************  Main Block  *************************** */
RUN ENABLE_ui.
DO WITH FRAME gMain:
    RUN initVar.

    DO WITH FRAME gMain. END.
    ASSIGN 
        FRAME gMain:TITLE = ipcWindowName
        cbSortOrder:VISIBLE = FALSE
        cbSortOrder:SENSITIVE = FALSE
        btnApply:SENSITIVE = FALSE
        iPage = 0
        lFilterFirstDefault = iplFilterFirst.

    RUN getData.
    RUN displayData.
    RUN populateFilter.
END.

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gMain  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkPaging gMain 
PROCEDURE checkPaging :
/**/
DO WITH FRAME gMain: 
    
    IF iPageLow = 1 THEN ASSIGN btnPageFirst:SENSITIVE = FALSE.
    ELSE ASSIGN btnPageFirst:SENSITIVE = TRUE.
    
    IF iPageLow < ipiPageOffset THEN ASSIGN btnPagePrev:SENSITIVE = FALSE.
    ELSE ASSIGN btnPagePrev:SENSITIVE = TRUE.
    
    IF iPageTop >= iRecordCount THEN ASSIGN btnPageNext:SENSITIVE = FALSE.
    ELSE ASSIGN btnPageNext:SENSITIVE = TRUE.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gMain  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayData gMain 
PROCEDURE displayData :
/* Untuk menampilkan data ke browse */

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(btt).
hQuery:QUERY-PREPARE("FOR EACH tt").
hQuery:QUERY-OPEN().

/* Construct Dynamic Browse */
DO WITH FRAME gMain: END.
IF VALID-HANDLE(dhBrowse) THEN DELETE WIDGET dhBrowse.
CREATE BROWSE dhBrowse
    ASSIGN
        ROW                 = RECT-A:ROW
        COL                 = RECT-A:COL
        WIDTH               = RECT-A:WIDTH
        HEIGHT              = RECT-A:HEIGHT
        FRAME               = FRAME gMain:HANDLE
        QUERY               = hQuery
        SENSITIVE           = TRUE
        SEPARATORS          = TRUE
        ROW-MARKERS         = FALSE
        VISIBLE             = TRUE
        READ-ONLY           = TRUE
        COLUMN-MOVABLE      = FALSE
        SCROLLBAR-VERTICAL  = TRUE
        COLUMN-RESIZABLE    = TRUE
        COLUMN-SCROLLING    = TRUE
        ALLOW-COLUMN-SEARCHING = YES
        TOOLTIP             = 
            "Double-Click or Enter to choose a Record.~n" + 
            "Click browse header to sort results."
        .

ON "MOUSE-SELECT-DBLCLICK" OF dhBrowse 
OR "RETURN" OF dhBrowse
        PERSISTENT RUN widgetEvent2 IN THIS-PROCEDURE.

/* You need to turn off COLUMN-MOVEABLE for using COLUMN-SEARCHING */
ON "START-SEARCH" OF dhBrowse
    PERSISTENT RUN widgetEvent3 IN THIS-PROCEDURE.

/* Constructing Browse Column */
dhBrowse:ADD-COLUMNS-FROM(btt).
ASSIGN dhBrowse:FIT-LAST-COLUMN = YES.

IF btt:AVAILABLE THEN ASSIGN btnChoose:SENSITIVE = TRUE.
ELSE ASSIGN btnChoose:SENSITIVE = FALSE.

APPLY 'ENTRY' TO dhBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gMain  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cbFilter cbSort cbSortOrder fiStatus FILL-IN-3 FILL-IN-6 
      WITH FRAME gMain.
  ENABLE RECT-1 RECT-7 RECT-8 RECT-A RECT-235 btnPageFirst btnPagePrev 
         btnPageNext btnChoose cbFilter cbSort cbSortOrder btnDefault btnApply 
         fiStatus FILL-IN-3 FILL-IN-6 
      WITH FRAME gMain.
  VIEW FRAME gMain.
  {&OPEN-BROWSERS-IN-QUERY-gMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getData gMain 
PROCEDURE getData :
/* Untuk mengambil data dari server */

    /* Display Status Message */
    ASSIGN fiStatus:SCREEN-VALUE IN FRAME gMain = 'Retrieving Data...'.
    /*DISABLE btnChoose btnApply btnDefault cbFilter cbSort
        btnPageFirst btnPagePrev btnPageNext 
        WITH FRAME gMain.*/
    PAUSE 0.001 NO-MESSAGE.

    ETIME(YES).

    /* jalankan script server */
    /*RUN serverhelp.p ON hApp (*/
    RUN C:\Users\yordan\Desktop\Cortisol\Programs\inputhelp\serverhelp.p (
        INPUT iplVerbose,
        INPUT iplPagination,
        INPUT ipiPageOffset,
        INPUT iplFilterFirst,
        INPUT ipcTableName,
        INPUT ipcTableField,
        INPUT ipcTableLabel,
        INPUT ipcFieldFormat,
        INPUT ipcTableQuery,
        INPUT ipcTableWhere,
        INPUT ipcTableIndex,
        INPUT ipcTableOrder,
        INPUT iPage,
        OUTPUT iRecordCount,
        OUTPUT lError,
        INPUT-OUTPUT TABLE-HANDLE hDynTable
        ).   
   
    IF lError OR NOT VALID-HANDLE(hDynTable) THEN DO:
        MESSAGE 'Unexpected error occured from server while retrieving data.' SKIP
            'Please contact IT Department for assistance.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'WINDOW-CLOSE' TO FRAME gMain.
        RETURN ERROR.
    END.

    ASSIGN 
        btt = hDynTable:DEFAULT-BUFFER-HANDLE
        iPageLow = ipiPageOffset * iPage + 1
        iPageTop = ipiPageOffset * iPage + ipiPageOffset
        .

    IF iRecordCount < iPageTop THEN
        ASSIGN iPageTop = iRecordCount.

    IF iplPagination THEN DO:
        ASSIGN fiStatus:SCREEN-VALUE = 
            SUBSTITUTE(
                "Showing &1-&2 of &3 records, &4 sec",
                iPageLow,
                iPageTop,
                iRecordCount,
                TRIM(STRING(ETIME / 1000, ">9.999"))
                ).
        RUN checkPaging.
    END.

    ELSE
    ASSIGN fiStatus:SCREEN-VALUE = 
        SUBSTITUTE(
            '&1 records retrieved in &2 seconds',
            iRecordCount,TRIM(STRING(ETIME / 1000, ">9.999"))
            ).

    IF iplFilterFirst THEN DO:
        ASSIGN 
            fiStatus:SCREEN-VALUE = "Use filter to retrive data" 
            btnChoose:SENSITIVE = FALSE
            btnPageNext:SENSITIVE = FALSE
        .
        APPLY 'ENTRY' TO cbFilter.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gMain 
PROCEDURE initializeObject :
/* dont delete this even it's blank */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initVar gMain 
PROCEDURE initVar :
/* Backup Variable's Value */

    ASSIGN 
        cTableWhere = ipcTableWhere
        cInitialOrder = ipcTableOrder
        cTableOrder = ipcTableOrder.

    /* Assign Default Value if some input variable ommited */
    IF ipcWindowName = '' THEN ASSIGN ipcWindowName = 'Input Help'.
    IF ipcTableLabel = '' THEN DO: 
        ASSIGN ipcTableLabel = ipcTableField.
        DO ix = 1 TO NUM-ENTRIES(ipcTableName):
            ipcTableLabel = REPLACE(ipcTableLabel, ENTRY(ix, ipcTableName) + '.', '').
        END.
    END.
    IF ipcTableQuery = '' THEN ASSIGN ipcTableQuery = 'PRESELECT EACH ' + ipcTableName + ' NO-LOCK'.
    IF ipcReturnField = '' THEN ASSIGN ipcReturnField = ENTRY(1, ipcTableField).

    /* Jika jumlah field dan label tidak sesuai */
    IF NUM-ENTRIES(ipcTableField) <> NUM-ENTRIES(ipcTableLabel) THEN DO: 
        MESSAGE 'Jumlah Field dan Label tidak sesuai'.
        RETURN ERROR.
    END.

    DO ix = 1 TO NUM-ENTRIES(ipcTableField):
        ASSIGN cInitialOrder = REPLACE(cInitialOrder, ENTRY(ix, ipcTableField), ENTRY(ix, ipcTableLabel)).
    END.

    DO WITH FRAME gMain. END.
    IF cInitialOrder <> '' THEN
        ASSIGN cInitialOrder = "Initially Sorted " + cInitialOrder.

    CREATE TEXT hWidgets[1] ASSIGN
        COLUMN      = cbSort:COLUMN + 2 + 17.80        
        ROW         = cbSort:ROW        
        HEIGHT      = 1
        WIDTH       = 57
        FRAME       = FRAME gMain:HANDLE
        SENSITIVE   = TRUE
        VISIBLE     = TRUE
        BGCOLOR     = 11
        FORMAT      = "x(135)"
        SCREEN-VALUE = cInitialOrder.

    IF NOT iplPagination THEN
    DO:
        ASSIGN
            btnPageFirst:VISIBLE = FALSE
            btnPagePrev:VISIBLE = FALSE
            btnPageNext:VISIBLE = FALSE.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE populateFilter gMain 
PROCEDURE populateFilter :
/* populate filter field with browse column label */

DO WITH FRAME gMain. END.
ASSIGN
    cbFilter:LIST-ITEM-PAIRS = '1,1'
    cbSort:LIST-ITEM-PAIRS = '1,1'.

DO ix = 1 TO btt:NUM-FIELDS:
    cbFilter:ADD-LAST(btt:BUFFER-FIELD(ix):LABEL, btt:BUFFER-FIELD(ix):NAME).
    cbSort:ADD-LAST(btt:BUFFER-FIELD(ix):LABEL, btt:BUFFER-FIELD(ix):NAME).
END.

cbFilter:DELETE(1).
cbSort:DELETE(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetEvent gMain 
PROCEDURE widgetEvent :
/* Persistent Procedure to trigger an event */
    DO ON ERROR UNDO, LEAVE:
        RUN widgetEvent4.
        APPLY 'CHOOSE' TO btnApply IN FRAME gMain.
        RETURN NO-APPLY.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetEvent2 gMain 
PROCEDURE widgetEvent2 :
/**/
    IF btt:AVAILABLE THEN
        APPLY 'CHOOSE' TO btnChoose IN FRAME gMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetEvent3 gMain 
PROCEDURE widgetEvent3 :
/* Event for start search in browse */

    DO WITH FRAME gMain. END.

    IF cbSort:SCREEN-VALUE = dhBrowse:CURRENT-COLUMN:NAME AND cbSortOrder:SCREEN-VALUE <> 'DESC' THEN
        ASSIGN cbSortOrder:SCREEN-VALUE = 'DESC'.
    ELSE ASSIGN cbSortOrder:SCREEN-VALUE = 'ASC'.

    ASSIGN cbSort:SCREEN-VALUE = dhBrowse:CURRENT-COLUMN:NAME.
    APPLY 'VALUE-CHANGED' TO cbSort.
    APPLY 'CHOOSE' TO btnApply.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgetEvent4 gMain 
PROCEDURE widgetEvent4 :
/**/
IF cWhereType = 'date' THEN DO:
    IF SELF:SCREEN-VALUE <> '' AND SELF:TOOLTIP = 'DAY (DD)' AND
        (INT(SELF:SCREEN-VALUE) > 31 OR INT(SELF:SCREEN-VALUE) <= 0) THEN DO:
        MESSAGE 'Invalid Day' VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    IF SELF:SCREEN-VALUE <> '' AND SELF:TOOLTIP = 'MONTH (MM)' AND
        (INT(SELF:SCREEN-VALUE) > 12 OR INT(SELF:SCREEN-VALUE) <= 0) THEN DO:
        MESSAGE 'Invalid Month' VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.

    IF SELF:SCREEN-VALUE <> '' AND SELF:TOOLTIP = 'YEAR (YYYY)' AND
        (INT(SELF:SCREEN-VALUE) >= 3000 OR INT(SELF:SCREEN-VALUE) <= 1000) THEN DO:
        MESSAGE 'Invalid Year' VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

