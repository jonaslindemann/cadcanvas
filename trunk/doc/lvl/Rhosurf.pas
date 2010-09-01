PROGRAM RHOSURF;
  { Changes resistivity limits in .LVL file for SURFER }
  { Torleif Dahlin, Engineering Geology, University of Lund }
  { 1995-09-23 }
USES
  Res2Type, Res2File, Res2Matr, TDMath,
  TPCrt, TPString, TPPick, TPDir,
  Dos, Break;
CONST
  ProgName = 'RHOSURF';
  LvlExt = '.LVL';
  Grey:boolean=false;
  RoundFactor = 1.5;                        { determines no of digits in rho }
  Linear : Boolean = False;
  GoThroughTens : Boolean = True;
  FileFound : Boolean = False;
  x1 = 1; y1 = 1; columns = 5; CrtY = 25;
  {col1 = $0F;                             col2 = $0F; col3 = $0F;}
  Colors : PickColorArray =
  ($0F,                                     { Unselected item colour }
   $0F,                                     { Window frame colour }
   $70,                                     { Window title colour }
   $70,                                     { Selected item colour }
   $07,                                     { Alternate unselected item colour }
   $0F                                      { Alternate selected item colour }
   );
  Rad1 : STRING = 'LVL2';
  Rad2 : STRING = #39+'Level Flags LColor LStyle LWidth FFGColor FBGColor FPattern FMode';
  ColRGB : ARRAY[-1..maxmaxcol] OF STRING = (
                                         ' 0 "Black" "Solid" 0 "R0 G0 B113" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R0 G32 B194" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R0 G101 B113" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R0 G130 B77" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R0 G154 B53" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R174 G154 B0" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R255 G202 B0" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G178 B0" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G130 B0" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R255 G77 B0" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R243 G0 B0" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R194 G0 B0" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2');
  GreyRGB : ARRAY[-1..maxmaxcol] OF STRING = (
                                         ' 0 "Black" "Solid" 0 "R16 G16 B16" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R36 G36 B36" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R57 G57 B57" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R81 G81 B81" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R101 G101 B101" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R125 G125 B125" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R146 G146 B146" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R170 G170 B170" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R190 G190 B190" "White" "Solid" 2',
                                         ' 1 "Black" "Solid" 0 "R215 G215 B215" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R235 G235 B235" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R247 G247 B247" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2',
                                         ' 0 "Black" "Solid" 0 "R255 G255 B255" "White" "Solid" 2');
VAR
  datafil : STRING;
  Status : Word;
  col : {Word} Integer;
  svar : Char;
  rho : Real; rhostr : STRING;              {err : Integer;}
  rhoi, StartRho, StopRho, xi, ncount : Integer;
  limitrho, RhoRound, xx, rho_min, rho_max, dum : Real;
  ok : Boolean;
  ProgPath : PathStr;
  SistaBokst : Byte;



  PROCEDURE ReadRhoLimitsSurfer(rho_min, rho_max : Real; DataExist : Boolean);
  CONST
    RoundFactor = 1.5;                      { determines no of digits in rho }
    TrimFactor = 1.1;                       { affects start of rho intervals }
    Compress = 3.5;                         { compresses total rho interval }
  VAR
    col : Byte;
    rho : Real; rhostr : STRING; dum : Integer;
    rhoi, xi : Integer;
    limitrho, RhoRound, xx : Real;
    FileOK : Boolean;
  BEGIN
    WriteLn('Reading/determining resistivity limits ...');
    FileOK := False;
    WITH GraphSet DO BEGIN
      WITH Options DO Assign(fil, DataDir+filename+LvlExt);
      {$I-} Reset(fil); {$i+}
      IF IoResult = 0 THEN BEGIN
        FileOK := True;
        ReadLn(fil, Rad1);
        ReadLn(fil, Rad2);
        col:=mincol-2;
        repeat
          col:=col+1;
          {$I-}
          ReadLn(fil, RhoLimit[col], ColRGB[col]);
          {$I+}
          FileOK := FileOK AND (IoResult = 0);
      until eof(fil);
      maxcol:=col;
        Close(fil);
      END;
      Write('Rholimits: ');
      FOR col := mincol TO maxcol-1 DO Write(RhoLimit[col]:1:0, ' ');
      WriteLn;
    END;
  END;                                      { ReadRhoLimitsSurfer }


  PROCEDURE WriteRhoLimitsSurfer(GraphSet : graphtype);
  VAR
    xx : Real;
    xi : Integer;
  BEGIN
    WITH GraphSet DO BEGIN
  WITH GraphSet DO BEGIN
    RhoLimit[mincol-1] := 1e-9;
    RhoLimit[maxcol] := 1e9;
  END;
      WITH Options DO Assign(fil, DataDir+RhoLimitFil+LvlExt); Rewrite(fil);
      WriteLn(fil, Rad1);
      WriteLn(fil, Rad2);
      FOR col := mincol-1 TO maxcol DO BEGIN
        xx := RhoLimit[col]/10; xi := 0;
        REPEAT xx := xx*10; xi := xi+1; UNTIL Frac(xx) < 0.1;
        if Grey then
          WriteLn(fil, RhoLimit[col]:xi:xi-1, GreyRGB[col])
        else
          WriteLn(fil, RhoLimit[col]:xi:xi-1, ColRGB[col]);
      END;
      Close(fil);
    END;
  END;                                      { WriteRhoLimitsSurfer }


BEGIN
  WITH GraphSet DO BEGIN
    RhoLimit[mincol-1] := 1e-9;
    RhoLimit[maxcol] := 1e9;
  END;
  clrscr;
  IF ParamCount > 0 THEN BEGIN
    {SistaBokst:=Pos('.',Paramstr(1))-1;
    if SistaBokst=0 then SistaBokst:=Length(Paramstr(1));
    Options.filename:=Copy(Paramstr(1),1,SistaBokst); }
    IF StUpCase(ParamStr(1)) = '?' THEN BEGIN
      WriteLn(ProgName+' - Call parameter options:');
      WriteLn('');
      WriteLn('            * = file selection menu');
      WriteLn('            no parameter = file selection menu');
      WriteLn('');
      WriteLn('The following options must be placed after filename or *:');
      WriteLn('            lin = linear resistivity limit distribution');
      WriteLn('            alt = alternative logarithmic limit distribution');
      WriteLn('            g   = output RGB values for greyscale');
      WriteLn('');
      WriteLn('');
      svar := readkey;
      Halt;
    END;
    Options.filename := ParamStr(1);
    IF Pos('.', Options.filename) <> 0 THEN
      Options.filename := Copy(Options.filename, 1, Pos('.', Options.filename)-1);
    FileFound := Options.filename <> '*';
    FOR ncount := 2 TO ParamCount DO BEGIN
      IF StUpCase(ParamStr(ncount)) = 'LIN' THEN Linear := True;
      IF StUpCase(ParamStr(ncount)) = 'ALT' THEN GoThroughTens := False;
      IF StUpCase(ParamStr(ncount)) = 'G' THEN Grey := True;
    END;
  END ELSE BEGIN
    IF NOT FileFound THEN BEGIN
      {Clrscr;}
      FastWrite(Pad(ProgName+': Choose data file:', 80), y1, x1, Hattr);
      EnablePickMouse;                      {columns:= 5;  }
      WITH Options DO Status := GetFileName(DataDisk+DataDir+'*'+LvlExt, 0, x1, y1+1, CrtY, columns, Colors, datafil);
      {Clrscr;}
      FileFound := Status = 0;              {ReadInfoDisk := ok and (Length(datafil)>0);}
      IF NOT FileFound THEN BEGIN
        WriteLn('No file found! The data directory is empty!'); {svar := readkey;} {Exit;}
      END;
      DisablePickMouse;
      FileFound := FileFound AND (Length(datafil) > 0);
      IF FileFound THEN BEGIN
        datafil := JustFilename(datafil);
        Options.filename := Copy(datafil, 1, Pos('.', datafil)-1);
      END ELSE BEGIN
        FastWrite(Pad(ProgName+': Give new filename:', 80), y1, x1, Hattr);
        gotoxy(1, 2);
        Write('Filename? '); ReadLn(Options.filename);
      END;
    END;
  END;
  CheckPath(ProgName, OrgPath, ProgPath);   {svar:=readkey;}
 (* IF FileFound THEN BEGIN
    WITH Options DO Assign(fil, DataDir+filename+DataExt);
    {$I-} Reset(fil); {$I+}
    IF IoResult = 0 THEN
      Close(fil)
    ELSE BEGIN
      WITH Options DO WriteLn('ERROR: File ', DataDir, filename, DataExt, ' not found!');
      svar := readkey;
      Halt;
    END;
    ok := ScanInPutData(Options, dum, dum, dum, dum, dum, dum, rho_min, rho_max, False, False);
  END;   *)
  {if ok then }
  ReadRhoLimitsSurfer(rho_min, rho_max, FileFound);
  {if ParamCount then } GraphSet.RhoLimitFil := Options.filename;
  WITH GraphSet DO BEGIN
    (*FOR col := mincol TO maxcol-1 DO begin
      xx:=RhoLimit[col]/10;xi:=0;
      repeat xx:=xx*10;xi:=xi+1; until Frac(xx)<0.1;
      WriteLn('Rholimit[', col, '] = ', RHOLIMIT[col]:xi:xi-1);
    end;    *)
    {WriteLn('These are the present resistivity limits. '); }
    Write('Press <C> to change or <Esc> to leave! ');
    REPEAT svar := Upcase(readkey) UNTIL svar IN ['C', #27]; WriteLn(svar);
    IF svar = #27 THEN Halt;
      {Write('Linear (1) or logarithmic (2) resistivity limit distribution? (1/2) ');
      REPEAT svar := Upcase(ReadKey) UNTIL svar IN ['1', '2']; WriteLn(svar);
      Linear := svar = '1';
      IF NOT Linear THEN BEGIN
        Write('Force resistivity limits to pass 10, 100, 1000 etc.? (Y/N) ');
        REPEAT svar := Upcase(ReadKey) UNTIL svar IN ['Y', 'N']; WriteLn(svar);
        GoThroughTens := svar = 'Y';
      END;     }
    REPEAT
      Write('Lowest resistivity limit? '); ReadLn(rhomin);
      IF Linear THEN BEGIN
        Write('Resistivity limit increment? '); ReadLn(rholabel);
        rhomax := rhomin; RhoLimit[0] := Round(rhomin);
        FOR col := mincol+1 TO maxcol-1 DO BEGIN
          rhomax := rhomax+rholabel;
          RhoLimit[col] := Round(rhomax); 
        END;
      END ELSE BEGIN
        Write('Resistivity limits per decade? '); ReadLn(rholabel);
        IF GoThroughTens THEN BEGIN
          StartRho := Round(TenLog(rhomin)*rholabel);
          rhoi := StartRho;
          col := mincol;                    { rholimit in converted integer format }
          REPEAT
            rho := TenExp(rhoi/rholabel);   { real rholimit }
            RhoRound := TenExp(Round(TenLog(rho)-RoundFactor)); { gives nice even rholimit steps }
            rho := Round(rho/RhoRound)*RhoRound; { gives nice even rholimit steps }
            RhoLimit[col] := Round(rho);
            col := col+1; rhoi := rhoi+1;
          UNTIL col > maxcol-1;
        END ELSE BEGIN
          rho := rhomin;
          FOR col := mincol TO maxcol-1 DO BEGIN
            RhoRound := TenExp(Round(TenLog(rho)-RoundFactor)); { gives nice even rholimit steps }
            RhoLimit[col] := Round(Round(rho/RhoRound)*RhoRound); { gives nice even rholimit steps }
            {rholimit[col]:=round(rho);}
            rho := TenExp(TenLog(rho)+1/rholabel);
          END;
        END;
      END;
      WriteLn('Rholimits:');
      FOR col := mincol TO maxcol-1 DO BEGIN
        xi := DecPlaces(RhoLimit[col]);
        Write(' ', RhoLimit[col]:xi+1:xi);
      END;
      WriteLn;
      Write('Are the limits OK? (Y/N) ');
      REPEAT svar := Upcase(readkey) UNTIL svar IN ['Y', 'N', #27]; WriteLn(svar);
      ok := svar = 'Y';
      Esc := svar = #27;
    UNTIL ok OR Esc;
  END;
  IF Esc THEN Halt;
  WriteRhoLimitsSurfer(GraphSet);
END.                                        { RHOSURF }
