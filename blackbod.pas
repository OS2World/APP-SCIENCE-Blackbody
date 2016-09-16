PROGRAM BlackBodyCurve;
{   Plot the Planck black body radiation curve for given temperature list.
    This program illustrates several OS/2 programming techniques:
      1) text I/O and graphics in the same window with one presentation space,
      2) menu items appended to the system menu,
      3) device independent graphics printing.
    Robert D. Miller, 11/13/95.
    Copyright Robert D. Miller, 1997. All Rights Reserved.
    Modified for SpeedSoft Pascal for OS/2, February 6, 1996.
    Compiled with Virtual Pascal, Dec 2, 1996.
    Last revised: May 15, 1997, Added menu items to frame window.
                  May 16, 1997, Added printing option.
                  June 4, 1997, Combined text & graphics presentation spaces.
                  Sep 10, 1997, Correct extreme temperature plotting. }
{$X+,V-}
{$IFDEF VIRTUALPASCAL }
{&PMType PM }
USES OS2BASE, OS2DEF, OS2PMAPI, STRINGS;
{$ELSE }
USES BSEDOS, PMWIN, PMGPI, OS2DEF;
{$ENDIF }

TYPE STRING80 = STRING[80];
     CSTRING34= ARRAY[0.. 34] OF CHAR;
     CSTRING  = ARRAY[0..256] OF CHAR;
     CSTRING7 = ARRAY[0..  7] OF CHAR;
     CSTRING12= ARRAY[0.. 12] OF CHAR;
     CSTRING40= ARRAY[0.. 40] OF CHAR;

CONST szClientClass : PCHAR = 'Planck Blackbody Radiation Curve' ;
      IDMPRINT = 10;
      IDMPRINTL= 11;
      IDMABOUT = 12;
   ProgramName :
{$IFDEF VIRTUALPASCAL }
   PCHAR  = 'Planck Blackbody Radiation Curve';
{$ELSE }
   PSZ    = 'Planck Blackbody Radiation Curve';
{$ENDIF }

CONST ProgInfo   : CSTRING40 = 'Copyright (c), Robert D. Miller, 1997.';
      szMenuText : ARRAY[0..3] OF CSTRING12 = ('', '~Print', '~Large Print', '~About');
      MenuChoice : ARRAY[0..3] OF MENUITEM =
           ((iPosition:MIT_END; afStyle: MIS_SEPARATOR; afAttribute:0;
             id:0; hwndSubMenu:0; hItem:0),
            (iPosition:MIT_END; afStyle: MIS_TEXT; afAttribute:0;
             id:IDMPRINT;  hwndSubMenu:0; hItem:0),
            (iPosition:MIT_END; afStyle: MIS_TEXT; afAttribute:0;
             id:IDMPRINTL; hwndSubMenu:0; hItem:0),
            (iPosition:MIT_END; afStyle: MIS_TEXT; afAttribute:0;
             id:IDMABOUT;  hwndSubMenu:0; hItem:0));

      flFrameFlags  : ULONG = FCF_TitleBar      OR FCF_SYSMENU OR
                              FCF_SizeBorder    OR FCF_MinMax  OR
                              FCF_ShellPosition OR FCF_TaskList ;

      YValXAxis = 1500; YAxisTop = 7000; YRange = YAxisTop -YValXAxis;
      XValYAxis = 1500; XRight   = 9500; XRange = XRight -XValYAxis;
      LabelFontSize = 14;  // Points.
      CXClient: INTEGER = 0;
      CYClient: INTEGER = 0;
      TextFontBaseX : INTEGER = 20; // 18;
      TextFontWidth : INTEGER =  9; // 10;
      NULL : ULONG = 0;
{$IFDEF VIRTUALPASCAL }
      LabelFontName : PCHAR = 'Helvetica Bold';
      TextFontName  : PCHAR = 'System Monospaced';  // 'System VIO';
{$ELSE }
      LabelFontName : PSZ =   'Helvetica Bold';
      TextFontName  : PSZ =   'System Monospaced';
{$ENDIF }

VAR
  hwndSysMenu, hwndSysSubMenu: HWND;
  PSId,
  idSysMenu : LONGINT;
  miSysMenu : MENUITEM;

  Ahab           : HAB;
  Ahmq           : HMQ;
  HFrame,HClient : HWND;
  AHPS           : HPS;
  Aqmsg          : QMSG;
  ViewPort, RECT : RECTL;
  PT             : POINTL;
  L, L1, L2      : DOUBLE;
  T, TK, TMin, TMax, YMaxT: SINGLE;
  K, N, ScreenPixelsX  : INTEGER;
  NewScreen, PMTextMode,
  AllowKBD, AxesDrawn,
  PSAlloc, SkipFirstPaint, KeyWait,
  ShowingCursor, PrintingGraph,
  PrintLandscape,
  LineEntered    : BOOLEAN;

  TextFont       : FATTRS;
  FontMet        : FONTMETRICS;
  TMatrix        : MATRIXLF;

  Keyboard       : RECORD
       InputLine : STRING;
       CharPos, BCPos, LineLength : INTEGER;
       InsertMode :BOOLEAN END;

  PMVScreen: RECORD
      CurX, CurY, TopLine: INTEGER;  // N.B: CurX begins at 1!
      ScrnLine: ARRAY[0..24] OF STRING; END;

  TempList,
  TList          : ARRAY[0..9] OF SINGLE;
  TUnits         : ARRAY[0..9] OF CHAR;
  Pts            : ARRAY[0..299] OF POINTL;
  PrintDriver, PrintDevice,
     PrintLogAddress : CSTRING;

FUNCTION IMax(A, B: INTEGER): INTEGER;
BEGIN
    IMax:= A; IF B > A THEN IMax:= B
END;  { IMax. }

FUNCTION Min(A, B: LONGINT): LONGINT;
BEGIN
    Min:= A; IF B < A THEN Min:= B
END;  { Min. }

FUNCTION RSET(VAR A: STRING; LEN : INTEGER): STRING;
{ Returns a string with A right justified in a field of LEN bytes.}
VAR TEMP : STRING; K,L : INTEGER;
BEGIN
    L:=LENGTH(A);
    IF L >= LEN THEN RSET:=COPY(A,1,LEN)
    ELSE BEGIN
        L:=LEN-L; TEMP[0]:=CHR(L);
        FOR K:=1  TO L DO TEMP[K]:=' ';
        RSET:=TEMP+A
    END
END; { RSET.}

FUNCTION EFormat(VAR N: SINGLE; Size: INTEGER): STRING;
{ Get rid of leading zeroes in exponent part of number and
  right justify number in field. }
VAR T: STRING[32];
VAR P : INTEGER;
BEGIN
    STR(N:Size:-2, T); P:= POS('E', T);
    WHILE T[P+2] = '0' DO DELETE(T, P+2, 1);
    P:= 0;
    EFormat:=RSET(T, Size)
END; { EFormat. }

PROCEDURE PMCls;
VAR K: INTEGER;
BEGIN
    GPIErase(AHPS);
    WITH PMVScreen DO
      BEGIN
      CurX:= 1; CurY:= 0; TopLine:=0;
      FOR K:= 0 TO 24 DO ScrnLine[K]:= ''
      END
END;  { PMCls. }

PROCEDURE SetTextFontSize;
BEGIN
    ScreenPixelsX:= WinQuerySysValue(HWND_DESKTOP, SV_CXSCREEN);
    IF ScreenPixelsX = 640 THEN
        BEGIN TextFontBaseX:= 12; TextFontWidth:=  8 END
    ELSE IF ScreenPixelsX = 800 THEN
         BEGIN TextFontBaseX:= 16; TextFontWidth:=  8 END
    ELSE BEGIN TextFontBaseX:= 20; TextFontWidth:=  9 END;
END;  { SetTextFontSize. }

PROCEDURE PMTextWinInit;
BEGIN
    PMCls;
    PMTextMode:= TRUE;  ShowingCursor:= FALSE;
    WITH TextFont DO
        BEGIN
        usRecordLength:=SIZEOF(FATTRS);
        lMaxBaseLineExt:= TextFontBaseX;
        lAveCharWidth:=   TextFontWidth;
        fsFontUse:= FATTR_FONTUSE_NOMIX;
{$IFDEF VIRTUALPASCAL }
        StrCopy(szFaceName, TextFontName)
{$ELSE }
        szFaceName:= PSZ(TextFontName)
{$ENDIF }
        END;
    GPICreateLogFont(AHPS, NIL, 2, TextFont);
    GPISetCharSet(AHPS, 2)
END;  { PMTextWinInit. }

PROCEDURE PMTextPaint;
VAR K, L, Y: INTEGER;
    T : STRING;
BEGIN
    GPIErase(AHPS);
    PT.Y:= CYClient -TextFontBaseX; PT.X:= 0;
    WITH PMVScreen DO
      BEGIN
      L:=TopLine; Y:= CYClient -TextFontBaseX;
      FOR K:= 0 TO 24 DO
          BEGIN
          IF L > 24 THEN L:= 0;
          T:= ScrnLine[L];
          IF T <> '' THEN GPICharStringAt(AHPS, PT, LENGTH(T), CSTRING(T[1]));
          INC(L);
          DEC(PT.Y, TextFontBaseX)
          END
      END;
    IF ShowingCursor THEN
        BEGIN
        PT.Y:= CYClient -TextFontBaseX*(PMVScreen.CurY+1);
        WinShowCursor(HClient, TRUE)
        END
END;  { PMTextPaint. }

PROCEDURE PMWrite(S: STRING);
VAR K, Y:INTEGER;
    P : ARRAY[0..255] OF CHAR;
BEGIN
    WITH PMVScreen DO ScrnLine[CurY]:= S;
    IF S <> '' THEN
       BEGIN
       PT.Y:= CYClient -TextFontBaseX*(PMVScreen.CurY+1);
       PT.X:= (PMVScreen.CurX-1)*FontMet.lAveCharWidth;
       GPICharStringAt(AHPS, PT, LENGTH(S), CSTRING(S[1]))
       END;
    PMVScreen.CurX:= LENGTH(S)+1
END;  { PMWrite. }

PROCEDURE PMWriteLn(S: STRING);
VAR K, Y:INTEGER;
BEGIN
    PMWrite(S);
    INC(PMVScreen.CurY);  PMVScreen.CurX:= 1;
    IF PMVScreen.CurY >= 25 THEN PMVScreen.CurY:= 24; // PMScrollUp1Line
END;  { PMWriteLn.}

PROCEDURE PMReadLn(VAR Response: STRING);
VAR K: INTEGER; RC: BOOLEAN;
BEGIN
    PT.Y:= CYClient -TextFontBaseX*(PMVScreen.CurY+1); PT.X:= 0;
    WITH Keyboard DO
      BEGIN InputLine:= ''; CharPos:= 0; LineLength:= 0; BCPos:= PMVScreen.CurX END;
    LineEntered:= FALSE;
    AllowKBD:= TRUE;

    RC:=WinCreateCursor(HClient, PT.X, PT.Y-2, TextFontWidth, 0,
                    CURSOR_SOLID + CURSOR_FLASH, NIL);
    WinShowCursor(HClient, TRUE);  ShowingCursor:= TRUE;

    WHILE WinGetMsg(Ahab, Aqmsg, NULLHANDLE, 0, 0) DO
        BEGIN
        WinDispatchMsg(Ahab, Aqmsg);
        IF LineEntered THEN BREAK
        END;

    WinDestroyCursor(HClient);
    ShowingCursor:= FALSE; AllowKBD:= FALSE;
    Response:= Keyboard.InputLine;
    WITH PMVScreen DO
       BEGIN CurX:= 1; INC(CurY); IF CurY > 24 THEN DEC(CurY, 24) END
END;  { PMReadLn. }

PROCEDURE DoAKey(M1, M2: ULONG);
CONST SPC : STRING[1] = ' ';
      TT  : POINTL = (X:0; Y:0);
VAR CP, VC,J,K, LK, L: INTEGER;
    CursorHt  : LONGINT;
    T, TMP, S : STRING;  S1 : STRING[1];
    PrevCH, CH: CHAR;

    ChangedInsertMode: BOOLEAN;
LABEL DONE;
BEGIN
    WinShowCursor(HClient, FALSE);  ChangedInsertMode:= FALSE;
    WITH Keyboard DO
    IF ODD(M1) THEN  // Normal ASCII char
        BEGIN
        CH:= CHAR(SHORT1FROMMP(M2)); // IF INTEGER(CH) < 0 THEN EXIT;
        CP:= PMVScreen.CurX;

        IF (CH >= ' ') THEN
          IF CharPos < 255 THEN
             BEGIN
             INC(PMVScreen.CurX);
             PT.X:= (CP-1) *FontMet.lAveCharWidth;
             INC(CharPos);
             IF InsertMode THEN
                BEGIN
                INC(LineLength);
                S1:= CH; InputLine[0]:= CHR(LineLength);
                INSERT(S1, InputLine, CharPos);
                L:= LineLength-CharPos+1;
                TMP:= COPY(InputLine, CharPos, L);
                GPICharStringAt(AHPS, PT, L, CSTRING(TMP[1]))
                END
              ELSE
                BEGIN   // Get char and echo to keyboard.
                IF CharPos > LineLength THEN Linelength:= CharPos;
                InputLine[CharPos]:= CH;  TMP:= CH;
                GPICharStringAt(AHPS, PT, 1, CSTRING(TMP[1]))
                END
             END
          ELSE LineEntered:= TRUE
        ELSE IF (M2 AND 255) = 13 THEN LineEntered:= TRUE
             ELSE IF (M2 AND 255) = 8 THEN         // Backspace
                IF CharPos >= 1 THEN
                    BEGIN
                    DEC(CharPos); DEC(LineLength);
                    DEC(PMVScreen.CurX);
                    TT.Y:= PT.Y;
                    TT.X:= (PMVScreen.CurX-1)*FontMet.lAveCharWidth;
                    IF CharPos = LineLength THEN    // EOL, backup one char.
                         GPICharStringAt(AHPS, TT, 1, CSTRING(SPC[1]))
                    ELSE BEGIN                      // Backup, pull line left.
                         S:= COPY(InputLine, CharPos+1, LineLength-CharPos);
                         TT.X:= (PMVScreen.CurX-1)*FontMet.lAveCharWidth;
                         GPICharStringAt(AHPS, TT, LENGTH(S), CSTRING(S[1]))
                         END;

                    END
        END
    ELSE BEGIN
        IF (M1 AND $42) = $42 THEN  // Process virtual keys.
        BEGIN
        VC:= SHORT2FROMMP(M2) AND 255;
        CASE VC OF
          15 : BEGIN        // Escape
               FillChar(S[1], 80, ' ');  S[0]:= CHR(80);
               PMVScreen.CurX:= Keyboard.BCPos;
               TT.X:=(PMVScreen.CurX-1)*FontMet.lAveCharWidth;
               TT.Y:= PMVScreen.CurY;
               GPICharStringAt(AHPS, TT, 80, CSTRING(S[1]));
               CharPos:= 0; InputLine:= ''; LineLength:= 0;
               LineEntered:= TRUE
               END;

          19 : BEGIN        // End
               PMVScreen.CurX:= LineLength+1; CharPos:= LineLength
               END;

          20 : BEGIN        // Home
               PMVScreen.CurX:= BCPos; CharPos:= 0
               END;

          21 : BEGIN        // Left arrow
               IF CharPos > 0 THEN BEGIN DEC(CharPos); DEC(PMVScreen.CurX) END
               END;

          23 : IF CharPos < 255 THEN  // Right arrow
                   BEGIN
                   INC(CharPos);
                   IF CharPos > LineLength THEN
                      BEGIN InputLine[CharPos]:= ' '; LineLength:= CharPos END;
                   INC(PMVScreen.CurX)
                   END;

          26 : BEGIN    // Insert key
               InsertMode:= NOT InsertMode; ChangedInsertMode:= TRUE
               END;

          27 : BEGIN  // Delete
               DELETE(InputLine, CharPos+1, 1); DEC(LineLength);
               S:= COPY(InputLine, CharPos+1, LineLength-CharPos+1) +' ';
               PT.X:= CharPos*FontMet.lAveCharWidth;
               GPICharStringAt(AHPS, PT, LENGTH(S), CSTRING(S[1]))
               END;
          ELSE ;
          END  // CASE
        END;   // IF (M1 AND 2) = 2
        END;

  DONE:
    IF ChangedInsertMode THEN
       BEGIN
       CursorHt:= 0;
       IF Keyboard.InsertMode THEN CursorHt:= 4;
       WinCreateCursor(HClient, (PMVScreen.CurX-1)*FontMet.lAveCharWidth, PT.Y-2,
          FontMet.lAveCharWidth, CursorHt, CURSOR_SOLID+CURSOR_FLASH, NIL)
       END
    ELSE
       WinCreateCursor(HClient, (PMVScreen.CurX-1)*FontMet.lAveCharWidth, PT.Y-2, 0, 0,
                        CURSOR_SETPOS, NIL);
    WinShowCursor(HClient, TRUE);
    WITH Keyboard DO InputLine[0]:= CHAR(LineLength)
END;  { DoAKey. }

FUNCTION TRIM(A : STRING):STRING;
{ Returns a string with blanks removed from both ends. }
VAR J, K, L : INTEGER;
BEGIN
    J:=0; L:=LENGTH(A);
    REPEAT INC(J) UNTIL (J=L) OR (A[J] <> ' '); K:=L+1;
    REPEAT DEC(K) UNTIL (K=0) OR (A[K] <> ' ');
    TRIM:=COPY(A,J,K-J+1)
END;  { TRIM. }

FUNCTION Log10(X: EXTENDED): EXTENDED;
{   Log base 10 of x. }
CONST LogE : EXTENDED = 0.4342944819032518276511;
BEGIN
    Log10:= LN(X)*LogE
END;  { Log10. }

FUNCTION TENTOI(I: INTEGER): EXTENDED;  //  Returns 10^I.
VAR R,T: EXTENDED; N: INTEGER;
BEGIN
    IF I = 0 THEN TENTOI:=1
    ELSE BEGIN
        N:=ABS(I);  T:=10;  R:=1;
        WHILE N > 0 DO
            BEGIN
            IF ODD(N) THEN R:=R*T;
            IF N > 1  THEN T:=SQR(T);
            N:=N SHR 1
            END;
        IF I > 0 THEN TenToI:= R
        ELSE TenToI:=1/R
    END
END; { TENTOI.}

FUNCTION XToI(X: EXTENDED; I: INTEGER): EXTENDED;
{   Compute X^I. }
VAR R, T: EXTENDED; N: INTEGER;
BEGIN
    R:=1; T:=X; N:=ABS(I);
    WHILE N > 0 DO
        BEGIN
        IF ODD(N) THEN R:=R*T;
        IF N > 0  THEN T:=T*T;
        N:=N SHR 1
        END;
    IF I > 0 THEN XToI:= R
    ELSE XToI:=1/R
END;  { XToI. }

FUNCTION XToY(X, Y: EXTENDED): EXTENDED;
{   Compute X^Y. }
VAR N : INTEGER;
BEGIN
    IF ABS(Y) < 32767 THEN
       IF Y = TRUNC(Y) THEN
            BEGIN XTOY:=XTOI(X, TRUNC(Y)); EXIT END;
    IF X = 0 THEN XTOY:=0
    ELSE XTOY:=EXP(Y*LN(X))
END;  { XToY. }

FUNCTION LeadDigit(X: EXTENDED): INTEGER;
{   Returns leading digit of floating point number. }
VAR N: EXTENDED; DecExpon: INTEGER;
BEGIN
    IF X = 0 THEN LeadDigit:= 0
    ELSE BEGIN
        DecExpon:= TRUNC(Log10(ABS(X)));
        N:= X/TenToI(DecExpon);
        WHILE N >= 10 DO N:= N/10;
        WHILE N <  1  DO N:= N*10;
        LeadDigit:=TRUNC(N)
    END
END;  { LeadDigit. }

FUNCTION DecimalExponent(X: EXTENDED): INTEGER;
{   Returns the decimal exponent of x. The while loops normalize
    the number to insure the closest approximation is found. }
VAR D: INTEGER;
BEGIN
    IF X = 0 THEN DecimalExponent:= 0
    ELSE BEGIN
         X:= ABS(X);
         D:= TRUNC(Log10(X));
         WHILE X/TenToI(D) >= 10 DO INC(D);
         WHILE X/TenToI(D) < 1   DO DEC(D);
         DecimalExponent:= D
         END
END; { DecimalExponent. }

FUNCTION WeinDisplacement(T: DOUBLE): DOUBLE;
{   Return wavelength of maximum energy for black body curve
    for given temperature T (Kelvin). See Motz: Astrophysics ... p 77.
    Wavelength meters, e.g. 5E-7m = 0.5 micron = 5000 Angstroms. }
BEGIN
    WeinDisplacement:= 0.00289715/T
END;  { WeinDisplacement. }

FUNCTION PlanckBB(Lambda: DOUBLE; T:SINGLE): DOUBLE;
{ Returns the intensity on the Planck black body radiation curve,
  given wavelength, lambda (meters) and temperature, T degrees Kelvin.
  Result is in units of W/m^2 *d Lambda. For example at:
  Lambda = 1 micron = 10,000 Angstroms = 1E-6 m, T=3000K is 3.1153E12 W/m^2 }
CONST C: SINGLE = 2.99792458E8;       { c in m/s }
      H: DOUBLE = 6.6260755E-34;      { Joule Second }
      K: DOUBLE = 1.380658E-23;       { Boltzman const. Joule/Kelvin }
      TwoPiHCC : DOUBLE = 3.741774873362E-16;
      HCOverK  : DOUBLE = 0.0143876866033339;
      Stefan   : DOUBLE = 5.67051E-8; { W Meter^-2 Kelvin^-4 }
VAR L5 : DOUBLE;
BEGIN
    L5:= SQR(SQR(Lambda))*Lambda;
    PlanckBB:= TwoPiHCC/(L5*(EXP(HCOverK/(Lambda*T)) -1))
END;  { PlanckBB. }

FUNCTION GETREAL(VAR INS:STRING; VAR OK:BOOLEAN): DOUBLE;
{   Returns next real number from input string. Leading characters
    up to the end of the numeric substring are deleted. }
VAR I, LEN, K : INTEGER; CODE : LONGINT;
    R               : DOUBLE;
    WS              : STRING[20];
CONST  NUM:   SET OF CHAR=['0'..'9', '+', '-', '.', 'E', 'e'];
BEGIN
    WS:='';  OK:=FALSE;  GETREAL:=0;
    LEN:=LENGTH(INS);  I:=1;  K:= 1;
    WHILE (I <= LEN) AND NOT(INS[I] IN NUM) DO INC(I);

    IF I <= LEN THEN
    REPEAT WS[K]:= UPCASE(INS[I]); INC(I); INC(K);
    UNTIL NOT(INS[I] IN NUM) OR (I > LEN) OR (K > 20);
    WS[0]:= CHR(K-1);

    IF LENGTH(WS) > 0 THEN
      BEGIN
{$IFDEF VIRTUALPASCAL }
      VAL(WS,R,CODE);
{$ELSE }
      VAL(WS,R, INTEGER(CODE));
{$ENDIF }
      OK:=(INTEGER(CODE)=0); IF OK THEN GETREAL:=R;
      DELETE(INS,1,I-1)
      END
END;  { GETREAL.}

PROCEDURE SortTempList(NumItems: INTEGER);
{  Bubble Sort list of y vertices into decreasing order. }
VAR J, K, Bound, Interchange: INTEGER;
    T: SINGLE;
BEGIN
    Bound:= NumItems -1;
    REPEAT Interchange:=0;
      FOR J:= 0 TO Bound-1 DO
        IF TempList[J] < TempList[J+1] THEN
            BEGIN T:= TempList[J]; TempList[J]:= TempList[J+1];
                  TempList[J+1]:= T; Interchange:= J
            END
    UNTIL Interchange = 0
END;  { SortTempList. }

PROCEDURE BlackbodyGraph(PS: HPS);
VAR SizePS: SIZEL; RCL : RECTL; CP, PTL: POINTL;
    K  : INTEGER;
    RB : BOOLEAN;

  PROCEDURE ALine(X1, Y1, X2, Y2:INTEGER);
  VAR PT : POINTL;
  BEGIN
    PT.X:= X1; PT.Y:= Y1; GPIMove(PS, PT);
    PT.X:= X2; PT.Y:= Y2; GPILine(PS, PT)
  END;  { Line. }

  PROCEDURE PlotBlackBodyCurve(VAR L1, L2: DOUBLE; TK: SINGLE);
  CONST  XStep = XRange DIV 200;
  VAR K, N, XP, YP, W, Expon, LD, LogRange : INTEGER;
      Intensity, L, LStep, LabelStepSize : DOUBLE;
      R, D, LS, Y, WaveRange : SINGLE;
      TEMPS   : ARRAY[0..9] OF SINGLE;
      PT, PE  : POINTL;
      APTLPoints : ARRAY[0..TXTBOX_COUNT-1] OF POINTL;
      FontMet : FONTMETRICS;
      FSize   : SIZEF;
      FSizeWC,
      LC2     : LONGINT;
      FontAttr: FATTRS;
      AnnotateGraph : BOOLEAN;
      S       : STRING[5];
      LabelText : STRING[40];

    PROCEDURE DrawLegend;
    VAR K : INTEGER; T: SINGLE;
        S : STRING[12];
        PC: ARRAY[0..12] OF CHAR;
        LabelText: STRING[19];
        PT: POINTL;
    BEGIN
      IF TempList[1] = 0 THEN LabelText:= 'Temperature ' +#$F8 +'K:'
      ELSE LabelText:= 'Temperatures ' +#$F8 +'K:' ;
      PT.X:= 6000; PT.Y:= 6000;

      GPICharStringAt(PS, PT, LENGTH(LabelText), CSTRING(LabelText[1]));
      LC2:= TXTBOX_COUNT;

{$IFDEF VIRTUALPASCAL }
      GPIQueryTextBox(PS, LONG(LENGTH(LabelText)), CSTRING(LabelText[1]),
         TXTBOX_COUNT, APTLPoints[0]);
{$ELSE }
      GPIQueryTextBox(PS, LONG(LENGTH(LabelText)), PCHAR(LabelText[1]),
         TXTBOX_COUNT, APTLPoints[0]);
{$ENDIF }
      PE.X:= APTLPoints[TXTBOX_CONCAT].X;

      FOR K:=0 TO 9 DO
          BEGIN
          T:= TempList[K]; IF T = 0 THEN EXIT;
          IF (T >= 10) AND (T <= 1.E6) THEN STR(ROUND(T):10, S)
          ELSE IF (T >= 0.01) AND (T < 10) THEN STR(T:10:2, S)
               ELSE S:= EFormat(T, 10);
{$IFDEF VIRTUALPASCAL }
          GPIQueryTextBox(PS, LENGTH(S), CSTRING(S[1]), TXTBOX_COUNT, APTLPoints[0]);
{$ELSE }
          GPIQueryTextBox(PS, LONG(LENGTH(S)), PCHAR(S[1]), TXTBOX_COUNT, APTLPoints[0]);
{$ENDIF }
          PT.X:= 6000 +PE.X -APTLPoints[TXTBOX_CONCAT].X;
          PT.Y:= 5900-180*(K+1);
          GPICharStringAt(PS, PT, LENGTH(S), CSTRING(S[1]))
          END
    END;  { DrawLegend. }

BEGIN
    GPISetColor(PS, CLR_BLACK);
    IF NOT AxesDrawn THEN
        BEGIN
        GPIBeginPath(PS, 1);
        PT.X:= XvalYAxis; PT.Y:= YAxisTop;   GPIMove(PS, PT);
                          PT.Y:= YValXAxis;  GPILine(PS, PT);
        PT.X:= XRight;    PT.Y:= YValXAxis;  GPILine(PS, PT);
        GPISetLineWidthGeom(PS, 40);
        GPIEndPath(PS); GPIStrokePath(PS, 1, 0);

        Expon:= DecimalExponent(YMaxT);  STR(Expon-6, S);

        AnnotateGraph:= LONGINT(CXClient)* CYClient > 90000;
        IF AnnotateGraph THEN
          BEGIN
          FillChar(FontAttr, SizeOf(FontAttr), 0);
          WITH FontAttr DO
            BEGIN
            usRecordLength:= SIZEOF(FontAttr);
            lMaxBaseLineExt:= 0;  lAveCharWidth:= 0;
{$IFDEF VIRTUALPASCAL }
            STRCOPY(szFaceName, LabelFontName);
{$ELSE }
            szFaceName:= PSZ(LabelFontName);
{$ENDIF }
            fsFontUse:= FATTR_FONTUSE_OUTLINE OR FATTR_FONTUSE_TRANSFORMABLE;
            END;

          GPICreateLogFont(PS, NIL, 1, FontAttr);
          GPISetCharSet(PS, 1);
          GPIQueryFontMetrics(AHPS, SizeOf(FontMet), FontMet);
          FSizeWC:= LabelFontSize *1000 DIV 72;   // Size (points)*PU_HIEnglish/Pts/In
          FSize.CX:= FSizeWC SHL 16;  FSize.CY:= FSize.CX;
          GPISetCharBox(PS, FSize);

          LabelText:= 'Radiance W/m' +#$FD +'/' +#$E6 + ' *10^' +S;
          PT.X:= 0; PT.Y:= 10;
          GPISetCharAngle(PS, GRADIENTL(PT));
          GPISetCharMode(PS, CM_MODE3);
          PT.X:= 700; PT.Y:= 3400;
          GPICharStringAt(PS, PT, LENGTH(LabelText), CSTRING(LabelText[1]));
          PT.X:= 10; PT.Y:= 0;
          GPISetCharAngle(PS, GRADIENTL(PT))
          END;

        LD:= LeadDigit(YMaxT);  S:= ' ';
        WHILE LD > 0 DO
          BEGIN
          Y:= LD* TenTOI(Expon)/YMaxT*YRange;
          K:= TRUNC(Y) +YValXaxis;
          PT.X:= XValYAxis-120; PT.Y:= K; GPIMove(PS, PT);
          PT.X:= XValYAxis;               GPILine(PS, PT);
          IF AnnotateGraph THEN
            BEGIN
            S[1]:= CHR(LD + ORD('0'));
            PT.X:= XValYAxis-140-168;  PT.Y:=K -80;
            GPICharStringAt(PS, PT, LENGTH(S), CSTRING(S[1]))
            END;
          DEC(LD)
          END;
                                         // Draw wavelength scale
      WaveRange:= L2 -L1;                // Label in units, tens or hundreds
      IF (WaveRange >= 1) AND (WaveRange <= 20) THEN LabelStepSize:= 1
      ELSE IF (WaveRange > 20) AND (WaveRange < 300) THEN LabelStepSize:= 10
      ELSE BEGIN
           LogRange:= TRUNC(LOG10(WaveRange/3));
           LabelStepSize:= TenToI(LogRange)
           END;

      R:= L1;
      WHILE R <= L2 DO
          BEGIN
          W:= ROUND(R/LabelStepSize); K:= W;
          XP:= TRUNC(R/WaveRange*XRange)+XValYAxis;
          PT.X:= XP; PT.Y:= YValXAxis;
          GPIMove(PS, PT);
          DEC(PT.Y, 120);
          GPILine(PS, PT);
          ALine(XP, YValXAxis, XP, YValXAxis-120);
          IF K < 10 THEN
               BEGIN S:= ' '; S[1]:= CHR(K +ORD('0')) END
          ELSE STR(W, S);
          R:= R +LabelStepSize;
          IF AnnotateGraph THEN
              BEGIN
              PT.X:= XP- 48; PT.Y:= YValXAxis -120 -168;
              GPICharStringAt(PS, PT, LENGTH(S), CSTRING(S[1]))
              END
          END;

    IF AnnotateGraph THEN
        BEGIN
        LabelText:= 'Wavelength Microns';  // Label along x-axis.
        IF LabelStepSize <> 1 THEN
          IF (LabelStepSize >= 10) AND (LabelStepSize <= 1000) THEN
            BEGIN
            STR(TRUNC(LabelStepSize), S);
            LabelText:= LabelText + ' *' + S
            END
          ELSE BEGIN STR(LogRange,S); LabelText:= LabelText + ' *1.E' +S END;

        PT.X:= 4000; PT.Y:= 800;
        GPICharStringAt(PS, PT, LENGTH(LabelText), CSTRING(LabelText[1]))
        END;

    GPISetLineWidth(PS, LineWidth_Normal);
                                         // Indicators for visual range
    IF (L1 < 0.38) AND (L2 > 0.7) AND (WaveRange < 60) THEN
        BEGIN
        GPISetColor(PS, Clr_Blue);
        XP:= ROUND((0.38-L1)/WaveRange*XRange)+XValYAxis;
        ALine(XP, YValXAxis+1, XP, YAxisTop-240);
        GPISetColor(PS, Clr_Red);
        XP:= ROUND((0.7 -L1)/WaveRange*XRange)+XValYAxis;
        ALine(XP, YValXAxis+1, XP, YAxisTop-240);

        IF WaveRange <= 5 THEN
            BEGIN
            GPISetColor(PS, CLR_Green);
            XP:= ROUND((0.56 -L1)/WaveRange*XRange)+XValYAxis;
            ALine(XP, YValXAxis+1, XP, YAxisTop-240)
            END
        END;

    GPISetColor(PS, CLR_Black);
    IF AnnotateGraph THEN DrawLegend;
    AxesDrawn:= TRUE
    END;  { IF NOT AxesDrawn }

    K:= 0;  N:= 0;
    L:= L1;
    LStep:= WaveRange/XRange*XStep;
    WHILE K <= XRange DO
        BEGIN
        IF L = 0 THEN Intensity:= 0
        ELSE Intensity:= PlanckBB(L*1E-6, TK);
        Y:= Intensity/YMaxT;
        YP:= TRUNC(Y*YRange) +YValXAxis;
        Pts[N].X:= XValYAxis+K; Pts[N].Y:= YP;
        L:= L+ LStep;
        INC(N); INC(K, XStep)
        END;

     GPIBeginPath(PS, 1);
     GPIMove(PS, Pts[0]); GPIPolyLine(PS, N-1, Pts[1]);
     GPISetLineWidthGeom(PS, 20);
     GPIEndPath(PS); GPIStrokePath(PS, 1, 0)
  END;  { PlotBlackBodyCurve. }

VAR ScaleXY : ARRAY[0..1] OF FIXED;
BEGIN
    SizePS.CX:= 11000;  SizePS.CY:=8500;
    GPIErase(PS);
    GPISetPS(PS, SizePS, PU_ARBITRARY);

    PTL.X:= CXClient;           PTL.Y:= CYClient;
    ViewPort.XLeft:= 0;         ViewPort.YBottom:= 0;
    ViewPort.XRight:= CXClient; ViewPort.YTop:= CYClient;
    GPISetPageViewPort(PS, ViewPort);

    IF PrintingGraph THEN
      IF PrintLandscape THEN // DrawLandscape
        BEGIN
        FillChar(TMatrix, SIZEOF(TMatrix), 0);
        TMatrix.fxM11:= 65536; TMatrix.fxM22:= 65536; TMatrix.lm33:= 1;
        CP.X:= CXClient DIV 2; CP.Y:= CYClient DIV 2;
        CP.X:= 6500; CP.Y:= 6000;
        RB:=GPIRotate(PS, TMatrix, TRANSFORM_REPLACE, LONGINT(90) SHL 16, @CP);
        ScaleXY[0]:= 90000; ScaleXY[1]:= 90000;  // ~1.37
        PT.X:=0; PT.Y:= 0;
        RB:=GPIScale(PS, TMatrix, TRANSFORM_ADD, ScaleXY[0], PT);
        TMatrix.lM31:= 11500; TMatrix.lM32:= 0;
        RB:=GPISetModelTransformMatrix(PS, 9, TMatrix, TRANSFORM_REPLACE)
        END;

    AxesDrawn:= FALSE;
    K:= 0;
    WHILE (K < 9) AND (TempList[K] <> 0) DO
        BEGIN PlotBlackBodyCurve(L1, L2, TempList[K]); INC(K) END;
END;  { BlackbodyGraph. }

FUNCTION CharScan(VAR S: CSTRING; C: CHAR; Limit, Start: INTEGER): INTEGER;
VAR K: INTEGER;
BEGIN
    CharScan:= -1;
    FOR K:= Start TO Limit DO
      IF S[K] = C THEN BEGIN CharScan:= K; EXIT END
END;  { ScanFor. }

FUNCTION OpenDefaultPrinterDC(VAR AB: HAB): HDC;
{   Setup printer device context.
    Algorithm after Petzold OS/2 PM Programming, Ch. 18. }
CONST PMPRINTER : PCHAR = 'PM_SPOOLER_PRINTER';
VAR DefPrinterName : CSTRING34;
    PrnData : CSTRING;
    PPrfProfile: PRFPROFILE;
    Dop : DEVOPENSTRUC;
    Drv : DRIVDATA;
    C,P,J,K : INTEGER;
    H   : HDC;
    PT  : POINTER;
    PC  : PCHAR;
BEGIN
    PrfQueryProfileString(HINI_PROFILE, PCHAR('PM_SPOOLER'), PCHAR('PRINTER'),
        PCHAR(';'), @DefPrinterName, SIZEOF(DefPrinterName));

    FOR K:= 0 TO 34 DO IF DefPrinterName[K] = ';' THEN
        BEGIN DefPrinterName[K]:= #0; BREAK END;

    PrfQueryProfileString(HINI_PROFILE, PCHAR('PM_SPOOLER_PRINTER'), DefPrinterName,
       PCHAR(';;;;'), @PrnData, SizeOf(PrnData));

    J:= CharScan(PrnData, ';', SizeOf(PrnData)-1, 0);
    IF J < 0 THEN BEGIN OpenDefaultPrinterDC:= DEV_ERROR; EXIT END;
    K:= CharScan(PrnData, ';', SizeOf(PrnData)-1, J+1);
    IF K < 0 THEN BEGIN OpenDefaultPrinterDC:= DEV_ERROR; EXIT END;

    C:= 0;  // Driver name is second item in PrnData.
    FOR P:= J+1 TO K-1 DO BEGIN PrintDriver[C]:= CSTRING(PrnData)[P]; INC(C) END;
    PrintDriver[C]:= #0;
    DOP.PSZDriverName:= @PrintDriver;

    J:= K+1;  // Logical address is third item in PrnData.
    K:= CharScan(PrnData, ';', SizeOf(PrnData)-1, J+1);
    IF K < 0 THEN BEGIN OpenDefaultPrinterDC:= DEV_ERROR; EXIT END;
    C:= 0;
    FOR P:= J TO K-1 DO
        BEGIN PrintLogAddress[C]:= PrnData[P]; INC(C) END;
    PrintLogAddress[C]:= #0;
    Dop.pszLogAddress:= @PrintLogAddress;

    FOR K:= 0 TO StrLen(PrintLogAddress) DO
        IF PrintLogAddress[K] = ',' THEN BEGIN PrintLogAddress[K]:= #0; BREAK END;
    Dop.pszLogAddress:= @PrintLogAddress;

    Drv.cb:= SizeOf(Drv); Drv.lVersion:= 0; Drv.abGeneralData:= #0;
    WITH Dop DO
       BEGIN
       K:= CharScan(PrintDriver, '.', SizeOf(PrintDriver), 0);
       IF K > 0 THEN
           BEGIN
           PrintDriver[K]:= #0; Dop.pszDriverName:= @PrintDriver;
           INC(K); C:= 0;
           WHILE (K <= SizeOf(PrintDriver)) AND (PrintDriver[K] <> #0)
              AND (PrintDriver[K] <> ',') DO
              BEGIN Drv.szDeviceName[C]:= PrintDriver[K]; INC(K); INC(C); END;
           Drv.szDeviceName[C]:= #0;
           pDriv:=@DRV;
           END
       ELSE pDriv:= NIL;
       pszDataType:= 'PM_Q_STD'
       END;

    H:= DevOpenDC(AHAB, OD_QUEUED, '*', 4, @DOP, 0);
    OpenDefaultPrinterDC:= H
END;  { OpenDefaultPrinterDC. }

PROCEDURE PrintGraph;
{   Draw Blackbody graph on printer. }
CONST JobID : PCHAR = 'BLACKBODY';
VAR HDCPtr : HDC;
    HPSPtr : HPS;
    SizePg : SIZEL;
    pbOutCount : LONG;
    pbDataBuf: POINTER;
BEGIN
    SizePg.CX:= 11000;  SizePg.CY:= 8500;
    HDCPtr:= OpenDefaultPrinterDC(AHAB);
    PrintingGraph:= TRUE;
    pbDataBuf:= NIL;
    IF HDCPtr <> DEV_ERROR THEN
       BEGIN
       HPSPtr:= GPICreatePS(AHAB, HDCPtr, SizePg,
          PU_ARBITRARY OR GPIF_DEFAULT OR GPIT_MICRO OR GPIA_ASSOC);

       GPIQueryPS(HPSPtr, SizePg);
       IF DEVESCAPE(HDCPtr, DEVESC_STARTDOC, 9, JobID, pbOutCount, pbDataBuf) <>
          DEVESC_ERROR THEN
          BEGIN
          BlackBodyGraph(hpsPtr);
          DevEscape(hpsPtr, DEVESC_ENDDOC, 0, NIL, pbOutCount, pbDataBuf)
          END;

       GPIDestroyPS(HPSPtr);
       DevCloseDC(HDCPtr)
       END;
    PrintingGraph:= FALSE
END;  { PrintGraph. }

PROCEDURE SetMenuPrintOption(Active: BOOLEAN);
{  Activate (for graphics) or deactivate (text) print options
   in window menu.}
VAR M : LONGINT;
BEGIN
    IF Active THEN M:= 0
    ELSE M:= MIA_DISABLED;
    WinSendMsg(hwndSysMenu, MM_SETITEMATTR,
               MPFROM2SHORT(IDMPRINT, ORD(TRUE)),
               MPFROM2SHORT(MIA_DISABLED, M));
    WinSendMsg(hwndSysMenu, MM_SETITEMATTR,
               MPFROM2SHORT(IDMPRINTL, ORD(TRUE)),
               MPFROM2SHORT(MIA_DISABLED, M))
END;  { SetMenuPrintOption. }

FUNCTION ClientWndProc(Ahwnd:HWND; MSG:ULONG; MP1, MP2:MPARAM):MRESULT; CDECL;
CONST Init : STRING[9] = 'INITMENU!';
VAR   PT   : POINTL;
      SizePS: SIZEL;  M : LONGINT;
BEGIN
    ClientWndProc:= 0;
    CASE MSG OF
    WM_CREATE: BEGIN // Attach Print and About Options to window menu
               hwndSysMenu:= WinWindowFromID(WinQueryWindow(Ahwnd, QW_PARENT),
                   FID_SYSMENU);
               idSysMenu:= SHORT1FROMMR(
                    WinSendMsg(hwndSysMenu, MM_ITEMIDFROMPOSITION, 0,0));
                    WinSendMsg(hwndSysMenu, MM_QUERYITEM,
                        MPFROM2SHORT(idSysMenu, 0), MPFROMP(@miSysMenu)) ;
               hwndSysSubMenu:= miSysMenu.hwndSubMenu;
               FOR K:= 0 TO 3 DO
                   WinSendMsg(hwndSysSubMenu, MM_INSERTITEM,
                              MPFROMP(@MenuChoice[K]),
                              MPFROMP(@szMenuText[K]));
               SetMenuPrintOption(FALSE)
               END;
 {  WM_SYSCOMMAND,
    WM_INITMENU: SetMenuPrintOption(NOT PMTextMode);
 }
    WM_COMMAND:BEGIN
                 CASE LO(MP1) OF
                 IDMPRINT:  IF NOT PMTextMode THEN
                            BEGIN PrintLandscape:= FALSE; PrintGraph END;
                 IDMPRINTL: IF NOT PMTextMode THEN
                            BEGIN PrintLandscape:= TRUE;  PrintGraph END;
                 IDMABOUT:
                     BEGIN
                     WinMessageBox(HWND_DESKTOP, Ahwnd, ProgInfo,
                        szClientClass, 0, MB_OK OR MB_INFORMATION);
                     END;
                END;  { CASE LO(MP1) }
              ClientWndProc:= 0
              END;
    WM_CHAR:  BEGIN
              IF AllowKBD THEN DoAKey(MP1, MP2)
              ELSE IF KeyWait AND ((MP1 AND $40) = 0) THEN KeyWait:= FALSE
              END;
    WM_SIZE:  BEGIN
              CXClient:= SHORT1FROMMP(MP2);
              CYClient:= SHORT2FROMMP(MP2)
              END;
    WM_PAINT: BEGIN
              WinBeginPaint(AHwnd, NULLHANDLE, NIL);
              IF PMTextMode THEN PMTextPaint
              ELSE BlackbodyGraph(AHPS);
              WinEndPaint(AHwnd)
              END;
    ELSE ClientWndProc:= WinDefWindowProc(Ahwnd, MSG, MP1, MP2)
    END { CASE }
END;  { ClientWndProc. }

PROCEDURE Set87ControlWord;
{  Set 8087 control word to NOT trap on overflow. }
VAR CWMask87 : WORD;
BEGIN
    ASM  finit
         fstcw   CWMask87
         fwait
         or      CWMask87, $0018
         fldcw   CWMask87
    END; { asm }
END;  { Set87ControlWord. }

VAR TRESP, RESP : STRING[80];
    S, SS: STRING[12];
    SF   : STRING[32];
    LineOut : STRING;
    OK   : BOOLEAN;
    RC   : INTEGER;
    WAIT : CHAR;
    Q    : EXTENDED;
LABEL RESTART, REPT, NEXT, DONE, ALLDONE;
BEGIN
    Ahab:= WinInitialize(0);
    Ahmq:= WinCreateMsgQueue(Ahab, 0);
{$IFDEF VIRTUALPASCAL }
    WinRegisterClass(Ahab, szClientClass, ClientWndProc, CS_SIZEREDRAW, 0);

    HFrame:= WinCreateStdWindow(HWND_DESKTOP, WS_VISIBLE, flFrameFlags,
                                szClientClass, NIL, 0,0,0, @HClient);
    WinSetWindowText(HFrame, ProgramName);
{$ELSE }
    WinRegisterClass(Ahab, CSTRING(szClientClass), @ClientWndProc, CS_SIZEREDRAW, 0);

    HFrame:= WinCreateStdWindow(HWND_DESKTOP, WS_VISIBLE, flFrameFlags,
                                CSTRING(szClientClass), NIL, 0,0,0, HClient);
    WinSetWindowText(HFrame, PSZ(ProgramName));
{$ENDIF }

    Set87ControlWord;

    KeyWait:= FALSE;

    AHPS:= WinGetPS(HClient);
    SetTextFontSize;

  RESTART:
    GPISetBackMix(AHPS, BM_OVERPAINT);
    GPISetBackColor(AHPS, Clr_White);

    PMTextWinInit;
    GPIQueryFontMetrics(AHPS, SIZEOF(FontMet), FontMet);

    AxesDrawn:= FALSE;
    FOR K:= 0 TO 9 DO BEGIN TempList[K]:= 0; TList[K]:= 0 END;
    FOR K:= 0 TO 9 DO TUnits[K]:= 'K';

    PMWriteln('Temperatures are assumed Kelvin unless followed immediately');
    PMWriteln('  by C or F for Celsius or Fahrenheit.');
    PMWriteln('Enter list of temperatures to be shown, (up to 10), or blank line to quit.');

    PMREADLN(TResp);  TResp:= TRIM(TResp);
    IF TResp = '' THEN GOTO ALLDONE
    ELSE IF UPCASE(TResp[1]) = 'Q' THEN GOTO ALLDONE;

    PMWriteln('');  N:= 0;
    WHILE (TResp <> '') AND (N <= 9) DO
        BEGIN
        TList[N]:= GetReal(TResp, OK);
        IF TList[N] < 0.99E-6 THEN
           BEGIN
           PMWriteLn('Temperatures must be >= 1E-6ø');
           TList[N]:= 0
           END;
        FOR K:= 1 TO LENGTH(TResp) DO
            IF TResp[K] IN ['0'..'9'] THEN GOTO NEXT
            ELSE IF TResp[K] IN ['K', 'C', 'F', 'k', 'c', 'f'] THEN
                BEGIN
                TUnits[N]:= UPCASE(TResp[K]);
                DELETE(TResp, 1, K); K:= LENGTH(TResp);
                GOTO NEXT
                END;
        NEXT: INC(N)
        END;

  DONE: K:= 0;  TMax:= 0;  TMin:= 1.0E29;
      WHILE (K < 9) DO
          BEGIN
          TK:= TList[K];
          IF TUnits[K] = 'C' THEN TK:= TK + 273.15
          ELSE IF TUnits[K] = 'F' THEN TK:= 5/9*(TK -32) +273.15;
          IF TK > TMax THEN TMax:= TK;
          IF (TK < TMin) AND (TK <> 0) THEN TMin:= TK;
          TempList[K]:= TK;
          IF TK <> 0 THEN
              BEGIN
              IF (TK >= 10) AND (TK < 32000) THEN STR(TK:6, S)
              ELSE IF (TK >= 0.01) AND (TK < 10) THEN STR(TK:6:2, S)
                   ELSE S:=EFormat(TK, 10);
              LineOut:= 'T=' +S +CHR($F8) + ', wavelength at peak intensity =';
              STR(WeinDisplacement(TK)*1.E6:8:4, S);
              LineOut:= LineOut + S + ' microns.';
              PMWRITELN(LineOut)
              END;
          INC(K)
          END;

    IF TMax <= 0 THEN
       BEGIN
       PMWriteLn('There is nothing to plot. Press Enter to continue...');
       PMReadLn(RESP);
       GOTO RESTART
       END;

    PMWRITELN('');
    PMWRITELN('Enter beginning and ending wavelength (microns) for plot range.');
    PMWRITELN('  Use blank line for a default scale.');

  REPT: L1:= 0; L2:= 0;
    PMREADLN(RESP); RESP:= TRIM(RESP);
    IF RESP = '' THEN
       BEGIN
       L1:=0; L2:= XTOY(10, 4.3-Log10(TMin));
       END
    ELSE BEGIN L1:= GETREAL(RESP, OK); L2:= GETREAL(RESP, OK) END;

    IF L2 = 0 THEN
       IF L1 > 0 THEN BEGIN L2:= L1; L1:= 0 END;
    IF (L1 < 0) OR (L2 < L1) THEN
       BEGIN PMWRITELN('Wavelengths must be > 0 and ending wavelength');
             PMWRITELN('  must be greater than beginning.');
       GOTO REPT
       END;

    SortTempList(10);
    YMaxT:= PlanckBB(WeinDisplacement(TMax), TMax) *1.25;

    PMWriteln('Press Enter to continue...');
    PMReadln(S);
    AxesDrawn:= FALSE; PMTextMode:= FALSE; PrintingGraph:= FALSE;

    SetMenuPrintOption(TRUE);
    PSId:= GPISavePS(AHPS);
    GPISetBackColor(AHPS, Clr_White);
    KeyWait:= TRUE;

    WinSendMsg(HClient, WM_PAINT, 0, 0);

    WHILE WinGetMsg(Ahab, Aqmsg, NULLHANDLE, 0,0) DO
        BEGIN
        WinDispatchMsg(Ahab, Aqmsg);
        IF NOT KeyWait THEN
           BEGIN
           GPIRestorePS(AHPS, PSId);
           SetMenuPrintOption(FALSE);
           GOTO RESTART
           END
        END;

 ALLDONE:
    WinReleasePS(AHPS);
    WinDestroyWindow(HFrame);
    WinDestroyMsgQueue(Ahmq);
    WinTerminate(Ahab)
END.
