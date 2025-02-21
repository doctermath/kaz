/* N012 - Server Script untuk proses data ke window clienthelp-b.w */
/* Author: YR, Date: 2024-12-06 */

/* Class Declarations 
---------------------------------------------------------------------------- */
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

/* Variable Definitions
---------------------------------------------------------------------------- */
DEFINE VARIABLE oJsonIn  AS JsonObject   NO-UNDO.
DEFINE VARIABLE oJsonOut AS JsonObject   NO-UNDO.

DEFINE VARIABLE ix      AS INTEGER     NO-UNDO.

DEFINE VARIABLE hData    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hRealTableBuffer    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hTempTableBuffer    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery              AS HANDLE      NO-UNDO.

DEFINE VARIABLE cTableName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldList  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLabelList  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cQuery      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iOffset     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLimit      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLimitCount AS INTEGER     NO-UNDO.

DEFINE VARIABLE cFieldName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLabelName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullField  AS CHARACTER   NO-UNDO.

/* Internal Procedures
---------------------------------------------------------------------------- */
PROCEDURE DELETE-PROCEDURE:
    /* Delete unused dynamic object to prevent memory leaks */
    IF VALID-HANDLE(hRealTableBuffer) THEN DELETE OBJECT hRealTableBuffer.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE InitInputHelp:
    /* First Procedure to run, to initialize the input help data to browse */
    DEFINE INPUT  PARAMETER ipJsonIn  AS JsonObject   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE-HANDLE opData.

    /* Move Json Input to Variables */
    ASSIGN
        oJsonIn     = ipJsonIn
        cTableName  = oJsonIn:GetCharacter('Table')
        cFieldList  = oJsonIn:GetCharacter('Field')
        cLabelList  = oJsonIn:GetCharacter('Label')
        cQuery      = oJsonIn:GetCharacter('Query')
        iOffset     = oJsonIn:GetInteger('Offset')
        iLimit      = oJsonIn:GetInteger('Limit')
        .

    /* Create Dynamic Table and its column based on specified table name and its fields */
    CREATE TEMP-TABLE hData.
    DO ix = 1 TO NUM-ENTRIES(cFieldList):
        ASSIGN cFieldName = ENTRY(ix, cFieldList).
        ASSIGN cFullField = cTableName + '.' + cFieldName.
        hData:ADD-LIKE-FIELD(cFullField, cFullField). 
    END.

    /* Prepare Temp-Table and its buffer */
    hData:TEMP-TABLE-PREPARE(cTableName).
    ASSIGN hTempTableBuffer = hData:DEFAULT-BUFFER-HANDLE.
    
    /* Assign Temp-Table Buffer based on label input */
    DO ix = 1 TO hTempTableBuffer:NUM-FIELDS:
       ASSIGN hTempTableBuffer:BUFFER-FIELD(ix):LABEL = ENTRY(ix, cLabelList).
    END.

    /* Create buffer for real table in database */
    CREATE BUFFER hRealTableBuffer FOR TABLE cTableName.

    /* Create dynamic query to query data from real table */
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hRealTableBuffer).
    hQuery:QUERY-PREPARE(cQuery). /* Specified on input help run */

    RUN QueryData(INPUT iOffset, INPUT FALSE, OUTPUT TABLE-HANDLE opData).

END PROCEDURE.

PROCEDURE LoadMore:
    DEFINE INPUT  PARAMETER iOffset AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE-HANDLE opData.

    RUN QueryData(INPUT iOffset, INPUT FALSE, OUTPUT TABLE-HANDLE opData).
END PROCEDURE.

PROCEDURE LoadAll:
    DEFINE INPUT  PARAMETER iOffset AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE-HANDLE opData.

    RUN QueryData(INPUT iOffset, INPUT TRUE, OUTPUT TABLE-HANDLE opData).
END PROCEDURE.

PROCEDURE QueryData:
    /* Query and send data per batch, determined by offset and limit */
    DEFINE INPUT  PARAMETER iOffset  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER lNoLimit AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE-HANDLE opData.

    hTempTableBuffer:EMPTY-TEMP-TABLE.
    hQuery:QUERY-OPEN().
    hQuery:REPOSITION-TO-ROW(iOffset).

    /* start assigning real table data to temp table based on query result */
    ASSIGN iLimitCount = 1.
    DO WHILE hQuery:GET-NEXT() AND (iLimitCount <= iLimit OR lNoLimit):
        ASSIGN iLimitCount = iLimitCount + 1.
        hTempTableBuffer:BUFFER-CREATE().
        DO ix = 1 TO NUM-ENTRIES(cFieldList):
            ASSIGN
                hTempTableBuffer:BUFFER-FIELD(ix):BUFFER-VALUE = 
                    hRealTableBuffer:BUFFER-FIELD(ENTRY(ix, cFieldList)):BUFFER-VALUE.
        END. 
    END.

    hQuery:QUERY-CLOSE().
    opData = hData.

END PROCEDURE.

PROCEDURE SearchAll:
    DEFINE INPUT  PARAMETER oJsonIn AS JsonObject   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE-HANDLE opData.

    DEFINE VARIABLE cClause AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCondition AS CHARACTER   NO-UNDO.

    DO ix = 1 TO NUM-ENTRIES(cFieldList):
        ASSIGN cCondition = SUBSTITUTE(
            "STRING(&1) MATCHES '*&2*'",
            ENTRY(ix, cFieldList),
            oJsonIn:GetCharacter('Search')    
            ).

        ASSIGN cClause = 
            cClause + ' ' +
            (IF cClause = '' THEN 'WHERE' ELSE 'OR') + ' ' +
            cCondition.
    END.

    hQuery:QUERY-PREPARE(cQuery + '' + cClause).
    RUN QueryData(INPUT iOffset, INPUT FALSE, OUTPUT TABLE-HANDLE opData).
END PROCEDURE.






