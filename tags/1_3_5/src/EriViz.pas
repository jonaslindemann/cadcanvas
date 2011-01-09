unit EriViz;

interface

uses
  SysUtils, Classes, Generics.Collections;

type

  TFileBase = class
  private
    FFloatFormat : string;
  public
    constructor Create;

    function FloatToStr(value : double) : string;
    procedure Write(var f : TextFile); virtual;
    procedure Read(var f : TextFile); virtual;
  end;

  TCoord3D = class(TFileBase)
  private
    FZ: double;
    FX: double;
    FY: double;
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
    procedure SetZ(const Value: double);

  public
    constructor Create;

    procedure Write(var f : TextFile); override;
    procedure Read(var f : TextFile); override;

    property X : double read FX write SetX;
    property Y : double read FY write SetY;
    property Z : double read FZ write SetZ;
  end;

  TFence = class(TFileBase)
  private
    FLine : TObjectList<TCoord3D>;
    FImageFilename: string;
    FDescriptionPos: integer;
    FDescription: string;
    procedure SetImageFilename(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetDescriptionPos(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPoint(x, y, z : double);
    procedure RemovePoint(idx : integer);
    procedure ClearPoints;

    procedure Write(var f : TextFile); override;
    procedure Read(var f : TextFile); override;

    property ImageFilename : string read FImageFilename write SetImageFilename;
    property Description : string read FDescription write SetDescription;
    property DescriptionPos : integer read FDescriptionPos write SetDescriptionPos;
  end;

  TCamera = class(TFileBase)
  private
    FFieldOfView: double;
    FTarget: TCoord3D;
    FFarLimit: double;
    FPosition: TCoord3D;
    FNearLimit: double;
    procedure SetFarLimit(const Value: double);
    procedure SetFieldOfView(const Value: double);
    procedure SetNearLimit(const Value: double);
    procedure SetPosition(const Value: TCoord3D);
    procedure SetTarget(const Value: TCoord3D);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(var f : TextFile); override;
    procedure Read(var f : TextFile); override;

    property Position : TCoord3D read FPosition write SetPosition;
    property Target : TCoord3D read FTarget write SetTarget;
    property NearLimit : double read FNearLimit write SetNearLimit;
    property FarLimit : double read FFarLimit write SetFarLimit;
    property FieldOfView : double read FFieldOfView write SetFieldOfView;
  end;

  TLegend = class(TFileBase)
  private
    FLegendPosX: integer;
    FLegendPosY: integer;
    FLegendMarginX: double;
    FLegendMarginY: double;
    FRelativeLegendSize: double;
    FLegendFilename: string;
    procedure SetLegendFilename(const Value: string);
    procedure SetLegendMarginX(const Value: double);
    procedure SetLegendMarginY(const Value: double);
    procedure SetLegendPosX(const Value: integer);
    procedure SetLegendPosY(const Value: integer);
    procedure SetRelativeLegendSize(const Value: double);

  public
    constructor Create;

    procedure Write(var f : TextFile); override;
    procedure Read(var f : TextFile); override;

    property LegendFilename : string read FLegendFilename write SetLegendFilename;
    property LegendMarginX : double read FLegendMarginX write SetLegendMarginX;
    property LegendMarginY : double read FLegendMarginY write SetLegendMarginY;
    property LegendPosX : integer read FLegendPosX write SetLegendPosX;
    property LegendPosY : integer read FLegendPosY write SetLegendPosY;
    property RelativeLegendSize : double read FRelativeLegendSize write SetRelativeLegendSize;
  end;

  TEriVizFile = class(TFileBase)
  private
    FVersion: string;
    FFilename: string;
    FFences : TObjectList<TFence>;
    FCamera : TCamera;
    FLegend : TLegend;
    procedure SetFilename(const Value: string);
    procedure SetVersion(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(var f : TextFile); override;
    procedure Read(var f : TextFile); override;

    procedure Save;
    procedure Load;
    procedure Clear;

    function AddFence : TFence;
    procedure RemoveFence(idx : integer);
    procedure ClearFences;

    property Filename : string read FFilename write SetFilename;
    property Version : string read FVersion write SetVersion;

    property Fences : TObjectList<TFence> read FFences;
    property Camera : TCamera read FCamera;
    property Legend : TLegend read FLegend;
  end;

  TEriViz = class(TComponent)
  private
    FSearchPath: string;
    FAutoDetect: boolean;
    FRunning : boolean;
    FExecuteAndWait : boolean;
    FEriVizFile : TEriVizFile;
    procedure SetAutoDetect(const Value: boolean);
    procedure SetSearchPath(const Value: string);
    { Private declarations }

    function FindEriViz : boolean;
    procedure SetExecuteAndWait(const Value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EditDialog;
    function Execute : boolean;

    property InputFile : TEriVizFile read FEriVizFile;
  published
    { Published declarations }
    property SearchPath : string read FSearchPath write SetSearchPath;
    property AutoDetect : boolean read FAutoDetect write SetAutoDetect;
    property ExecuteAndWait : boolean read FExecuteAndWait write SetExecuteAndWait;
  end;

procedure Register;

implementation

uses ShlObj, Windows, Dialogs, ActiveX, Messages, Forms;

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TEriViz]);
end;

function GetSystemPath(Folder: Integer): string;
var
  Path: LPWSTR;
begin
  Path := StrAlloc(MAX_PATH);
  SHGetFolderPath(Application.Handle, Folder, 0, SHGFP_TYPE_CURRENT, Path);
  Result:=Path;
  StrDispose(Path);
end;

function WinExecAndWait32(Path: PChar; Visibility: Word; ExecAndWait : boolean): integer;
var
  WaitResult : integer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  iResult : integer;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
	  { you could pass sw_show or sw_hide as parameter: }
    wShowWindow := visibility;
  end;
  if CreateProcess(nil,path,nil, nil, False,
		NORMAL_PRIORITY_CLASS, nil, nil,
		StartupInfo, ProcessInfo) then
  begin
    if ExecAndWait then
    begin
      repeat
        Application.ProcessMessages;
        WaitResult := WaitForSingleObject(ProcessInfo.hProcess, 100);
        { timeout is in miliseconds or INFINITE if
        you want to wait forever }
        result := WaitResult;
      until WaitResult<>WAIT_TIMEOUT;
    end;
  end
  else
  { error occurs during CreateProcess see help for details }
  result:=GetLastError;
end;

{ TEriVizFile }

function TEriVizFile.AddFence: TFence;
var
  Fence : TFence;
begin
  Fence:=TFence.Create;
  FFences.Add(Fence);
  Result:=Fence;
end;

procedure TEriVizFile.Clear;
begin
  Self.ClearFences;
end;

procedure TEriVizFile.ClearFences;
begin
  FFences.Clear;
end;

constructor TEriVizFile.Create;
begin
  inherited Create;
  FFences:=TObjectList<TFence>.Create;
  FCamera:=TCamera.Create;
  FLegend:=TLegend.Create;
  FVersion:='#ERIVIZ1';
end;

destructor TEriVizFile.Destroy;
begin
  FFences.Free;
  FCamera.Free;
  FLegend.Free;
  inherited;
end;

procedure TEriVizFile.Load;
var
  f : TextFile;
begin
  if (Self.Filename<>'') and FileExists(Self.Filename) then
  begin
    AssignFile(f, Self.Filename);
    Reset(f);
    Self.Read(f);
    CloseFile(f);
  end;
end;

procedure TEriVizFile.Read(var f: TextFile);
var
    Fence : TFence;
    nFences : integer;
    i : integer;
begin
  ReadLn(f, FVersion);
  ReadLn(f, nFences);

  for i := 0 to nFences - 1 do
  begin
    Fence:=TFence.Create;
    Fence.Read(f);
    FFences.Add(Fence);
  end;

  FCamera.Read(f);
  if not eof(f) then
    FLegend.Read(f);
end;

procedure TEriVizFile.RemoveFence(idx: integer);
begin
  if (idx>=0) and (idx<FFences.Count) then
    FFences.Delete(idx);
end;

procedure TEriVizFile.Save;
var
  f : TextFile;
  OldSep : char;
begin
  OldSep:=DecimalSeparator;
  DecimalSeparator:='.';
  if Self.Filename<>'' then
  begin
    AssignFile(f, Self.Filename);
    Rewrite(f);
    Self.Write(f);
    CloseFile(f);
  end;
  DecimalSeparator:=OldSep;
end;

procedure TEriVizFile.SetFilename(const Value: string);
begin
  FFilename := Value;
end;

procedure TEriVizFile.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

procedure TEriVizFile.Write(var f: TextFile);
var
    Fence : TFence;
begin
  WriteLn(f, FVersion);
  Writeln(f, FFences.Count);
  for Fence in FFences do
    Fence.Write(f);

  FCamera.Write(f);
  FLegend.Write(f);
end;

{ TEriViz }

constructor TEriViz.Create(AOwner: TComponent);
begin
  inherited;
  FRunning:=false;
  FSearchPath:='';
  FEriVizFile:=TEriVizFile.Create;
end;

destructor TEriViz.Destroy;
begin
  FEriVizFile.Free;
  inherited;
end;

procedure TEriViz.EditDialog;
begin

end;

function TEriViz.FindEriViz: boolean;
begin
  if AutoDetect then
    FSearchPath:=GetSystemPath(CSIDL_PROGRAM_FILES) + '\LundSoft\EriViz\eriviz.exe';

  if FileExists(FSearchPath) then
    begin
      Result:=true;
      exit;
    end
  else
    begin
      Result:=false;
    end;
end;

function TEriViz.Execute: boolean;
begin
  if FindEriViz then
  begin
    if FRunning then
      exit;

    FRunning:=true;
    WinExecAndWait32(PWideChar(FSearchPath), SW_SHOW, Self.ExecuteAndWait);
    FRunning:=false;
  end;
end;

procedure TEriViz.SetAutoDetect(const Value: boolean);
begin
  FAutoDetect := Value;
end;

procedure TEriViz.SetExecuteAndWait(const Value: boolean);
begin
  FExecuteAndWait := Value;
end;

procedure TEriViz.SetSearchPath(const Value: string);
begin
  FSearchPath := Value;
end;

{ TCamera }

constructor TCamera.Create;
begin
  inherited Create;
  FPosition:=TCoord3D.Create;
  FTarget:=TCoord3D.Create;
end;

destructor TCamera.Destroy;
begin
  FPosition.Free;
  FTarget.Free;
  inherited;
end;

procedure TCamera.Read(var f: TextFile);
begin
  Self.Position.Read(f);
  Self.Target.Read(f);
  ReadLn(f, FFieldOfView, FNearLimit, FFarLimit);
end;

procedure TCamera.SetFarLimit(const Value: double);
begin
  FFarLimit := Value;
end;

procedure TCamera.SetFieldOfView(const Value: double);
begin
  FFieldOfView := Value;
end;

procedure TCamera.SetNearLimit(const Value: double);
begin
  FNearLimit := Value;
end;

procedure TCamera.SetPosition(const Value: TCoord3D);
begin
  FPosition := Value;
end;

procedure TCamera.SetTarget(const Value: TCoord3D);
begin
  FTarget := Value;
end;

procedure TCamera.Write(var f: TextFile);
begin
  Self.Position.Write(f);
  Self.Target.Write(f);
  WriteLn(f, Self.FloatToStr(FFieldOfView), ' ', Self.FloatToStr(FNearLimit), ' ', Self.FloatToStr(FFarLimit));
end;

{ TFence }

procedure TFence.AddPoint(x, y, z: double);
var
  P : TCoord3D;
begin
  P:=TCoord3D.Create;
  Self.FLine.Add(P);
end;

procedure TFence.ClearPoints;
begin
  FLine.Clear;
end;

constructor TFence.Create;
begin
  inherited Create;
  FLine:=TObjectList<TCoord3D>.Create;
end;

destructor TFence.Destroy;
begin
  FLine.Free;
  inherited;
end;

procedure TFence.Read(var f: TextFile);
var
  nPoints : integer;
  i : integer;
  Coord : TCoord3D;
begin
  FLine.Clear;
  ReadLn(f, nPoints);
  for i := 0 to nPoints - 1 do
  begin
    Coord:=TCoord3D.Create;
    Coord.Read(f);
    FLine.Add(Coord);
  end;
  ReadLn(f, FImageFilename);
  ReadLn(f, FDescription);
  ReadLn(f, FDescriptionPos);
end;

procedure TFence.RemovePoint(idx: integer);
begin
  if (idx>=0) and (idx<FLine.Count) then
    FLine.Delete(idx);
end;

procedure TFence.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TFence.SetDescriptionPos(const Value: integer);
begin
  FDescriptionPos := Value;
end;

procedure TFence.SetImageFilename(const Value: string);
begin
  FImageFilename := Value;
end;

procedure TFence.Write(var f: TextFile);
var
  nPoints : integer;
  i : integer;
  Coord : TCoord3D;
begin
  WriteLn(f, FLine.Count);
  for Coord in FLine do
    Coord.Write(f);
  WriteLn(f, FImageFilename);
  WriteLn(f, FDescription);
  WriteLn(f, FDescriptionPos);
end;

{ TFileBase }

constructor TFileBase.Create;
begin
  inherited Create;
  FFloatFormat:='%g';
end;

function TFileBase.FloatToStr(value: double): string;
begin
  Result:=format(FFloatFormat, [value]);
end;

procedure TFileBase.Read(var f: TextFile);
begin

end;

procedure TFileBase.Write(var f: TextFile);
begin

end;

{ TCoord3D }

constructor TCoord3D.Create;
begin
  inherited Create;
  FX:=0;
  FY:=0;
  FZ:=0;
end;

procedure TCoord3D.Read(var f: TextFile);
begin
  ReadLn(f, FX, FY, FZ);
end;

procedure TCoord3D.SetX(const Value: double);
begin
  FX := Value;
end;

procedure TCoord3D.SetY(const Value: double);
begin
  FY := Value;
end;

procedure TCoord3D.SetZ(const Value: double);
begin
  FZ := Value;
end;

procedure TCoord3D.Write(var f: TextFile);
begin
  Writeln(f, Self.FloatToStr(FX), ' ',Self.FloatToStr(FY), ' ', Self.FloatToStr(FZ));
end;

{ TLegend }

constructor TLegend.Create;
begin
  inherited Create;
  FLegendFilename:='';
  FLegendMarginX:=0;
  FLegendMarginY:=0;
  FLegendPosX:=0;
  FLegendPosY:=0;
  FRelativeLegendSize:=0.5;
end;

procedure TLegend.Read(var f: TextFile);
begin
  inherited;
  ReadLn(f, Self.FLegendFilename);
  ReadLn(f, Self.FLegendMarginX, Self.FLegendMarginY);
  ReadLn(f, Self.FLegendPosX, Self.FLegendPosY);
  ReadLn(f, Self.FRelativeLegendSize);
end;

procedure TLegend.SetLegendFilename(const Value: string);
begin
  FLegendFilename := Value;
end;

procedure TLegend.SetLegendMarginX(const Value: double);
begin
  FLegendMarginX := Value;
end;

procedure TLegend.SetLegendMarginY(const Value: double);
begin
  FLegendMarginY := Value;
end;

procedure TLegend.SetLegendPosX(const Value: integer);
begin
  FLegendPosX := Value;
end;

procedure TLegend.SetLegendPosY(const Value: integer);
begin
  FLegendPosY := Value;
end;

procedure TLegend.SetRelativeLegendSize(const Value: double);
begin
  FRelativeLegendSize := Value;
end;

procedure TLegend.Write(var f: TextFile);
begin
  inherited;
  WriteLn(f, Self.FLegendFilename);
  WriteLn(f, Self.FloatToStr(FLegendMarginX), ' ', Self.FloatToStr(FLegendMarginY));
  WriteLn(f, FLegendPosX, ' ', FLegendPosY);
  WriteLn(f, Self.FloatToStr(FRelativeLegendSize));
end;

end.
