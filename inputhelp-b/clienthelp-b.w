&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS gDialog 
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
DEFINE VARIABLE h_xtools                   AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/* N012 - Client Program untuk utiliti input help V2 */
/* Author: YR, Date: 2024-12-06 */

CREATE WIDGET-POOL.

{src/adm2/widgetprto.i}
/*{sharevar.i}*/

/* Parameters
---------------------------------------------------------------------------- */
DEFINE INPUT  PARAMETER oJsonIn  AS JsonObject   NO-UNDO.
DEFINE OUTPUT PARAMETER oJsonOut AS JsonObject   NO-UNDO.

/* Variables
---------------------------------------------------------------------------- */
DEFINE VARIABLE ix              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLimit          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iOffset         AS INTEGER     NO-UNDO.
DEFINE VARIABLE hData           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hData2          AS HANDLE      NO-UNDO.

DEFINE VARIABLE hProc           AS HANDLE      NO-UNDO.

DEFINE VARIABLE hBrowse         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBuffer         AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BASE-1 RECT-4 fiSearch btnSearch btnLoad ~
btnLoadAll BtnCancel BtnOK 
&Scoped-Define DISPLAYED-OBJECTS fiSearch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLoad 
     LABEL "Load More" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLoadAll 
     LABEL "Load All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BtnOK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSearch 
     LABEL "Search" 
     SIZE 15.2 BY 1.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE RECTANGLE BASE-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 79 BY 10.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.8 BY .62
     BGCOLOR 10 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     fiSearch AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 2
     btnSearch AT ROW 1.24 COL 65.8 WIDGET-ID 4
     btnLoad AT ROW 13.29 COL 2 WIDGET-ID 18
     btnLoadAll AT ROW 13.29 COL 18.4 WIDGET-ID 26
     BtnCancel AT ROW 13.29 COL 50
     BtnOK AT ROW 13.29 COL 66
     BASE-1 AT ROW 2.43 COL 2 WIDGET-ID 6
     RECT-4 AT ROW 13.57 COL 34.6 WIDGET-ID 20
     SPACE(43.99) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Input Help"
         DEFAULT-BUTTON BtnOK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiSearch IN FRAME gDialog
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON END-ERROR OF FRAME gDialog /* Input Help */
DO:
  APPLY 'WINDOW-CLOSE' TO FRAME gDialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON GO OF FRAME gDialog /* Input Help */
DO:
  RUN deleteObject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Input Help */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
    oJsonOut:AddNull('Return').
    RUN deleteObject.
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel gDialog
ON CHOOSE OF BtnCancel IN FRAME gDialog /* Cancel */
DO:
    APPLY 'WINDOW-CLOSE' TO FRAME gDialog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad gDialog
ON CHOOSE OF btnLoad IN FRAME gDialog /* Load More */
DO:
    ASSIGN iOffset = iOffset + iLimit. 
    RUN LoadMore IN hProc(INPUT iOffset, OUTPUT TABLE-HANDLE hData2).

    hData:COPY-TEMP-TABLE(hData2, TRUE).
    RUN DisplayData.
    hQuery:REPOSITION-TO-ROW(iOffset).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoadAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoadAll gDialog
ON CHOOSE OF btnLoadAll IN FRAME gDialog /* Load All */
DO:
    ASSIGN iOffset = iOffset + iLimit. 
    RUN LoadAll IN hProc(INPUT iOffset, OUTPUT TABLE-HANDLE hData2).

    hData:COPY-TEMP-TABLE(hData2, TRUE).
    RUN DisplayData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK gDialog
ON CHOOSE OF BtnOK IN FRAME gDialog /* OK */
DO:
    oJsonOut:ADD('Return', 
        hBuffer:BUFFER-FIELD(oJsonIn:GetInteger('Return')):BUFFER-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch gDialog
ON CHOOSE OF btnSearch IN FRAME gDialog /* Search */
DO:
    oJsonIn:SET('Search', fiSearch:SCREEN-VALUE).
     
    RUN SearchAll IN hProc (INPUT oJsonIn, OUTPUT TABLE-HANDLE hData).
    RUN DisplayData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch gDialog
ON RETURN OF fiSearch IN FRAME gDialog
DO:
  APPLY 'CHOOSE' TO btnSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
RUN "C:\Users\yordan\Desktop\Cortisol\Programs\serverhelp-b.p"
    SINGLETON SET hProc /*ON hApp*/.

ASSIGN 
    iOffset = 1  /* Start the data from 1 and gradually increase when user hit load more */
    iLimit = 12. /* Limit data displayed per batch */

oJsonIn:ADD('Offset', iOffset).
oJsonIn:ADD('Limit', iLimit).
oJsonIn:AddNull('Search').

RUN InitInputHelp IN hProc (INPUT oJsonIn, OUTPUT TABLE-HANDLE hData).
hBuffer = hData:DEFAULT-BUFFER-HANDLE.

RUN DisplayData.

/* for output to caller client */
oJsonOut = NEW JsonObject().

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteObject gDialog 
PROCEDURE deleteObject :
/* Delete Dynamic Object */
IF VALID-HANDLE(hData)      THEN DELETE OBJECT hData.
IF VALID-HANDLE(hBrowse)    THEN DELETE OBJECT hBrowse.
IF VALID-HANDLE(hBuffer)    THEN DELETE OBJECT hBuffer.
IF VALID-HANDLE(hQuery)     THEN DELETE OBJECT hQuery.

/* Clean Singleton procedure */
RUN DELETE-PROCEDURE IN hProc.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayData gDialog 
PROCEDURE DisplayData :
/* Display Retrieved data to browse  */

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
    hQuery:QUERY-OPEN().
    
    DO WITH FRAME gDialog. END. /* provoke frame for base-1 */
    IF VALID-HANDLE(hBrowse) THEN DELETE WIDGET hBrowse.
    CREATE BROWSE hBrowse
        ASSIGN
            ROW                     = BASE-1:ROW
            COL                     = BASE-1:COL
            WIDTH                   = BASE-1:WIDTH
            HEIGHT                  = BASE-1:HEIGHT
            FRAME                   = FRAME gDialog:HANDLE
            QUERY                   = hQuery
            SENSITIVE               = TRUE
            SEPARATORS              = TRUE
            ROW-MARKERS             = FALSE
            VISIBLE                 = TRUE
            READ-ONLY               = TRUE
            COLUMN-MOVABLE          = FALSE
            SCROLLBAR-VERTICAL      = TRUE
            COLUMN-RESIZABLE        = TRUE
            COLUMN-SCROLLING        = TRUE
            ALLOW-COLUMN-SEARCHING  = FALSE
        .
    
    /* Constructing Browse Column */
    hBrowse:MOVE-AFTER-TAB-ITEM(btnSearch:HANDLE).
    hBrowse:ADD-COLUMNS-FROM(hBuffer).
    ASSIGN hBrowse:FIT-LAST-COLUMN = YES.
    APPLY 'ENTRY' TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY fiSearch 
      WITH FRAME gDialog.
  ENABLE BASE-1 RECT-4 fiSearch btnSearch btnLoad btnLoadAll BtnCancel BtnOK 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

