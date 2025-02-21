/* Server Script untuk proses data ke window clienthelp.w */
/* Author: Y, Date: 20240827 */

/* Parameters Definitions */
DEFINE INPUT  PARAMETER iplVerbose          AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER iplPagination       AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipiPageOffset       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iplFilterFirst      AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableField       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableLabel       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcFieldFormat      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableQuery       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableWhere       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableIndex       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableOrder       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPage             AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER opiRecordCount      AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER oplError            AS LOGICAL     NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE iohDynTable.

/* Local Definitions */
DEFINE VARIABLE ix                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iy                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iz                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTopRecord          AS INTEGER     NO-UNDO.

DEFINE VARIABLE cFullQuery          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTableDotField      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE btt                 AS HANDLE      NO-UNDO.
DEFINE VARIABLE bTargetTable        AS HANDLE      EXTENT 6 NO-UNDO.
DEFINE VARIABLE hQuery              AS HANDLE      NO-UNDO.

DO ON ERROR UNDO, THROW:
    /* Create and assign temp-table field */
    CREATE TEMP-TABLE iohDynTable.
    DO ix = 1 TO NUM-ENTRIES(ipcTableField):
        ASSIGN cTableDotField = ENTRY(ix, ipcTableField).
        iohDynTable:ADD-LIKE-FIELD(cTableDotField, cTableDotField).
    END.

    /* prepare dynamic temp-table and assign its buffer*/
    iohDynTable:TEMP-TABLE-PREPARE("tt").
    ASSIGN btt = iohDynTable:DEFAULT-BUFFER-HANDLE.

    /* Assign Label to temp-table field */
    DO ix = 1 TO btt:NUM-FIELDS WITH FRAME fMain:
       ASSIGN btt:BUFFER-FIELD(ix):LABEL = ENTRY(ix, ipcTableLabel).
       IF ipcFieldFormat <> '' AND ENTRY(ix, ipcFieldFormat) <> '' THEN
           ASSIGN btt:BUFFER-FIELD(ix):FORMAT = ENTRY(ix, ipcFieldFormat).
    END.

    /* Create Dynamic Query and add corresponding target table's Buffer */
    CREATE QUERY hQuery.
    DO ix = 1 TO NUM-ENTRIES(ipcTableName):
        CREATE BUFFER bTargetTable[ix] FOR TABLE ENTRY(ix, ipcTableName).
        hQuery:ADD-BUFFER(bTargetTable[ix]).
    END.

    /* LEAVE ALL IF uses filter first to show data */
    IF iplFilterFirst THEN LEAVE.

    /* Construct QUERY STRING
    ====================================================================== */
    DO ix = 1 TO NUM-ENTRIES(ipcTableName):
        ASSIGN cFullQuery = cFullQuery + (IF cFullQuery = '' THEN '' ELSE ', ') + SUBSTITUTE(
            "&1 &2 &3 &4",
            ENTRY(ix, ipcTableQuery),
            ENTRY(ix, ipcTableWhere),
            ENTRY(ix, ipcTableIndex),
            ENTRY(ix, ipcTableOrder)
            ).
    END.

    IF iplVerbose THEN
    MESSAGE 'fullquery :' SKIP cFullQuery.

    /* Prepare and Open the Query */
    hQuery:QUERY-PREPARE(cFullQuery).
    hQuery:QUERY-OPEN().

    /* if pagination on */
    IF iplPagination THEN DO:
        ASSIGN iTopRecord = ipiPageOffset.
        hQuery:REPOSITION-TO-ROW(ipiPage * iTopRecord + 1).
    END.
    ELSE
        ASSIGN iTopRecord = hQuery:NUM-RESULTS.

    /* Loop records and assign it to corresponding temp-table fields */
    DO iz = 1 TO iTopRecord:
        hQuery:GET-NEXT().
        IF hQuery:QUERY-OFF-END THEN LEAVE.

        btt:BUFFER-CREATE().
        DO ix = 1 TO NUM-ENTRIES(ipcTableField):
            ASSIGN
                /* Dynamic Table Corresponding Field */
                btt:BUFFER-FIELD(ENTRY(ix, ipcTableField)):BUFFER-VALUE = 
                hQuery:GET-BUFFER-HANDLE(ENTRY(1, ENTRY(ix, ipcTableField), '.'))
                    :BUFFER-FIELD(ENTRY(2, ENTRY(ix, ipcTableField), '.'))
                    :BUFFER-VALUE.
        END.
    END.

    ASSIGN opiRecordCount = hQuery:NUM-RESULTS.

    /* Catch if any error happened in runtime */
    CATCH e AS Progress.Lang.Error:
        MESSAGE "Unexpected error occurred on server" SKIP e:GetMessage(1).
        ASSIGN oplError = TRUE.
    END CATCH.

    FINALLY:
        /* Memory Cleanup for Dynamic Object to prevent memory leaks */
        DO ix = 1 TO NUM-ENTRIES(ipcTableName):
            IF VALID-HANDLE(bTargetTable[ix]) THEN DO:
                bTargetTable[ix]:BUFFER-RELEASE().
                DELETE OBJECT bTargetTable[ix].
            END.
        END.
        IF VALID-HANDLE(hQuery) THEN DO:
            hQuery:QUERY-CLOSE.
            DELETE OBJECT hQuery.
        END.
        RETURN.
    END FINALLY.
END.


