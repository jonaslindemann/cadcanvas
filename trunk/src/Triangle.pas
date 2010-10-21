unit Triangle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, Delaunay, FastGeo, Generics.Collections;

type

  TRefCounted = class
  private
    FRefCount: integer;
  public
    constructor Create;

    procedure AddRef;
    procedure DelRef;
    function IsReferenced: boolean;
  end;

  TRefList = class
  private
    FList: TList;
    function GetItem(index: integer): TRefCounted;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Item: TRefCounted);
    procedure Remove(Item: TRefCounted);
    procedure Delete(Idx: integer);
    procedure Clear;
    procedure ClearUnreferenced;

    property Items[index: integer]: TRefCounted read GetItem;
    property Count: integer read GetCount;
  end;

  TTrianglePoint = class(TRefCounted)
  private
    FX: double;
    FY: double;
    FValue: double;
  public
    constructor Create;
    procedure AssignFrom(P: TTrianglePoint);

    property X: double read FX write FX;
    property Y: double read FY write FY;
    property Value: double read FValue write FValue;
  end;

  TTriangleLine = class(TRefCounted)
  private
    FEndPoint: TTrianglePoint;
    FStartPoint: TTrianglePoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Line: TTriangleLine);

    procedure Scale(Factor: double);

    function Length: double;

    function Intersection(L: TTriangleLine): TTrianglePoint;

    property StartPoint: TTrianglePoint read FStartPoint;
    property EndPoint: TTrianglePoint read FEndPoint;
  end;

  TTrianglePolyline = class(TRefCounted)
  private
    FPoints: TRefList;

    function GetPoints(Idx: integer): TTrianglePoint;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddPoint(X, Y: double);

    procedure AssignLine(Idx: integer; Line: TTriangleLine);

    function Interpolate(X: double): double;

    property Points[Idx: integer]: TTrianglePoint read GetPoints;
    property Count: integer read GetCount;

  end;

  TTriangleElement = class(TRefCounted)
  private
    FTriPoints: array [1 .. 3] of TTrianglePoint;
    function GetPoint(index: integer): TTrianglePoint;
    procedure SetPoint(index: integer; Point: TTrianglePoint);
  public
    constructor Create;
    destructor Destroy; override;

    function Split(Line: TTriangleLine): TRefList; overload;
    function Split(PolyLine: TTrianglePolyline): TRefList; overload;
    function SplitLeft(Line: TTriangleLine): TRefList; overload;
    function SplitRight(Line: TTriangleLine): TRefList; overload;
    function SplitLeft(PolyLine: TTrianglePolyline): TRefList; overload;
    function SplitRight(PolyLine: TTrianglePolyline): TRefList; overload;

    function GetIntersectionPoints(Value: double; var p1, p2: TTrianglePoint)
      : boolean;

    property Point[index: integer]: TTrianglePoint read GetPoint write SetPoint;
  end;

  TTriangleType = (ttInside, ttCrossing, ttOutside);

  TTriangleClipRect = class
  private
    FLeft: double;
    FRight: double;
    FTop: double;
    FBottom: double;
  public
    constructor Create;

    function Check(Triangle: TTriangleElement): TTriangleType;

    property Left: double read FLeft write FLeft;
    property Right: double read FRight write FRight;
    property Top: double read FTop write FTop;
    property Bottom: double read FBottom write FBottom;
  end;

  TTriangleImplementation = (tiInternal, tiExternal);

  TTriangleClipProcessing = (tpDoNothing, tpClip, tpRemove, tpRemoveReverse,
    tpRemoveCrossing, tpRemoveCrossingReverse);

  TTriangleDuplicateProcessing = (tdIgnore, tdReplace, tdMean);

  TTriangle = class(TComponent)
  private
    { Private declarations }
    FTriangleImplementation: TTriangleImplementation;
    FTriangleExecutable: string;
    FModelname: string;
    FPoints: TRefList;
    FCoordHash: TDictionary<double, integer>;
    FDuplicateProcessing : TTriangleDuplicateProcessing;
    FElements: TRefList;
    FClippedElements: TRefList;
    FCrossingElements: TRefList;
    FVisibleElements: TRefList;
    FQualityMesh: boolean;
    FMinAngle: double;
    FMaxValue: double;
    FMinValue: double;

    FMaxPoint: TTrianglePoint;
    FMinPoint: TTrianglePoint;

    FTriangleClipRect: TTriangleClipRect;
    FClipActive: boolean;

    FClipPolylines: TRefList;

    FOffsetPolyline: TTrianglePolyline;

    FTriangleClipProcessing: TTriangleClipProcessing;
    FClipPolyline: boolean;

    procedure ClearPoints;
    procedure ClearElements;
    procedure ClearClipPolylines;
    procedure CreateNodeFile;
    procedure ReadElementFile;

    function GetPoints(index: integer): TTrianglePoint;
    function GetPointCount: integer;
    function GetElementCount: integer;
    function GetElements(index: integer): TTriangleElement;
    function GetMaxPoint: TTrianglePoint;
    function GetMinPoint: TTrianglePoint;
    function CoordHash(x, y : double) : double;

    procedure UpdateExtents(Triangle: TTriangleElement);
    procedure ResetExtents;
    procedure SetClipActive(const Value: boolean);
    function GetPolylines(index: integer): TTrianglePolyline;
    procedure SetTriangleClipProcessing(const Value: TTriangleClipProcessing);
    procedure SetClipPolyline(const Value: boolean);
    procedure SetDuplicateProcessing(const Value: TTriangleDuplicateProcessing);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;

    procedure Execute;
    procedure AddPoint(X, Y, Value: double);
    procedure AddClipPolyline(PolyLine: TTrianglePolyline);

    procedure UnClip;
    procedure Clip;
    procedure PolygonClip;

    procedure Offset;

    property PointCount: integer read GetPointCount;
    property Points[index: integer]: TTrianglePoint read GetPoints;

    property ElementCount: integer read GetElementCount;
    property Elements[index: integer]: TTriangleElement read GetElements;

    property ClipPolylines[index: integer]: TTrianglePolyline read GetPolylines;
    property OffsetPolyline
      : TTrianglePolyline read FOffsetPolyline write FOffsetPolyline;

    property MaxValue: double read FMaxValue;
    property MinValue: double read FMinValue;
    property MaxPoint: TTrianglePoint read GetMaxPoint;
    property MinPoint: TTrianglePoint read GetMinPoint;

    property TriangleClipRect: TTriangleClipRect read FTriangleClipRect;
    property ClipActive: boolean read FClipActive write SetClipActive;

  published
    { Published declarations }
    property TriangleExecutable
      : string read FTriangleExecutable write FTriangleExecutable;
    property Modelname: string read FModelname write FModelname;

    property QualityMesh: boolean read FQualityMesh write FQualityMesh;
    property MinAngle: double read FMinAngle write FMinAngle;
    property TriangleImplementation
      : TTriangleImplementation read FTriangleImplementation write
      FTriangleImplementation;
    property ClipPolyline: boolean read FClipPolyline write SetClipPolyline;
    property TriangleClipProcessing
      : TTriangleClipProcessing read FTriangleClipProcessing write
      SetTriangleClipProcessing;
    property DuplicateProcessing : TTriangleDuplicateProcessing read FDuplicateProcessing write SetDuplicateProcessing;
  end;

function Distance(p1, p2: TTrianglePoint): double;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TTriangle]);
end;

function ExecAndWait(Parameter: string; nCmdShow: integer): longword;
var
  zParameter: array [0 .. 255] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  // StrPCopy(zAppName,FileName);
  StrPCopy(zParameter, Parameter);
  // StrPCopy(zCurDir,GetCurrentDir());
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := nCmdShow;
  if not CreateProcess(nil, zParameter, nil, nil, false,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
    Result := 0
  else
  begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
  end;
end;

function Distance(p1, p2: TTrianglePoint): double;
begin
  Result := Sqrt(Power(p2.X - p1.X, 2) + Power(p2.Y - p1.Y, 2));
end;

function Interpolate(p1, p2: TTrianglePoint; Value: double;
  var P3: TTrianglePoint): boolean;
var
  v1, v2: double;
  k: double;
  Inside: boolean;

begin
  v1 := p1.Value;
  v2 := p2.Value;

  if (v1 = v2) then
  begin
    Result := false;
    exit;
  end;

  if p2.Value >= p1.Value then
  begin
    if (Value >= v1) and (Value < v2) then
      Inside := true
    else
      Inside := false;
  end
  else
  begin
    if (Value >= v2) and (Value < v1) then
      Inside := true
    else
      Inside := false;
  end;

  if not Inside then
  begin
    Result := false;
    exit;
  end;

  k := (Value - v1) / (v2 - v1);

  P3.X := p1.X * (1 - k) + p2.X * k;
  P3.Y := p1.Y * (1 - k) + p2.Y * k;
  P3.Value := Value;

  Result := true;

end;

{ TTriangle }

constructor TTriangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FModelname := 'temp';
  FPoints := TRefList.Create;
  FElements := TRefList.Create;
  FClippedElements := TRefList.Create;
  FCrossingElements := TRefList.Create;
  FVisibleElements := TRefList.Create;

  FCoordHash:=TDictionary<double, integer>.Create;
  FDuplicateProcessing:=tdMean;

  FQualityMesh := false;
  FMinAngle := -1;

  FTriangleImplementation := tiInternal;

  FMaxPoint := TTrianglePoint.Create;
  FMinPoint := TTrianglePoint.Create;
  FMaxPoint.X := -1E300;
  FMaxPoint.Y := -1E300;
  FMinPoint.X := 1E300;
  FMinPoint.Y := 1E300;

  FTriangleClipRect := TTriangleClipRect.Create;
  FClipActive := false;

  FClipPolylines := TRefList.Create;
  FOffsetPolyline := TTrianglePolyline.Create;
  FTriangleClipProcessing := tpClip;
  FClipPolyline := false;
end;

destructor TTriangle.Destroy;
begin
  // ClearElements;
  FElements.Free;

  // ClearPoints;
  FPoints.Free;
  FCoordHash.Free;

  FClippedElements.Free;
  FVisibleElements.Free;
  FCrossingElements.Free;

  FClipPolylines.Free;
  FOffsetPolyline.Free;

  FMaxPoint.Free;
  FMinPoint.Free;
  FTriangleClipRect.Free;
  inherited;
end;

procedure TTriangle.AddPoint(X, Y, Value: double);
var
  TriPoint: TTrianglePoint;
  HashValue : double;
begin

  HashValue:=CoordHash(X, Y);

  if FCoordHash.ContainsKey(HashValue) then
    begin
      TriPoint:=FPoints.Items[FCoordHash.Items[HashValue]] as TTrianglePoint;

      if Self.DuplicateProcessing = tdReplace then
        TriPoint.Value:=Value
      else if Self.DuplicateProcessing = tdMean then
        TriPoint.Value:=0.5*(TriPoint.Value + Value);
    end
  else
    begin
      TriPoint := TTrianglePoint.Create;
      TriPoint.X := X;
      TriPoint.Y := Y;
      TriPoint.Value := Value;
      FPoints.Add(TriPoint);

      if Value > FMaxValue then
        FMaxValue := Value;

      if Value < FMinValue then
        FMinValue := Value;

      FCoordHash.Add(CoordHash(x, y), FPoints.Count-1);
    end;

end;

procedure TTriangle.Clear;
begin
  ClearPoints;
  ClearElements;
  ClearClipPolylines;
end;

procedure TTriangle.ClearElements;
begin
  FElements.Clear;
end;

procedure TTriangle.ClearPoints;
begin
  FPoints.Clear;
  FCoordHash.Clear;
  FMaxValue := -1E300;
  FMinValue := 1E300;
end;

procedure TTriangle.CreateNodeFile;
var
  f: TextFile;
  i: integer;
  TriPoint: TTrianglePoint;

  X, Y, Value: double;
  CurrentPath: String;
begin
  CurrentPath := ExtractFilePath(Application.ExeName);
  if (FModelname <> '') then
  begin
    AssignFile(f, CurrentPath + FModelname + '.node');
    // Ändrat till app-katalogen /RJ
    Rewrite(f);

    writeln(f, format('%d %d %d %d', [FPoints.Count, 2, 1, 0]));

    for i := 0 to FPoints.Count - 1 do
    begin
      TriPoint := TTrianglePoint(FPoints.Items[i]);
      X := TriPoint.X;
      Y := TriPoint.Y;
      Value := TriPoint.Value;
      writeln(f, i + 1, ' ', X, ' ', Y, ' ', Value);
    end;

    CloseFile(f);
  end;
end;

procedure TTriangle.Execute;
var
  CurrentPath: string;
  TriParam: string;
  Delaunay: TDelaunay;
  i: integer;
  TriElement: TTriangleElement;
begin

  ResetExtents;

  if FTriangleImplementation = tiExternal then
  begin

    CurrentPath := ExtractFilePath(Application.ExeName);
    if (not FileExists(CurrentPath + FTriangleExecutable)) then
    // Ändrat till app-katalogen /RJ
    begin
      ShowMessage(FTriangleExecutable + ' could not be found.');
      exit;
    end;

    TriParam := '';
    if (FQualityMesh) then
    begin
      TriParam := TriParam + 'q';
      if (FMinAngle > 0) then
        TriParam := TriParam + format('%g', [FMinAngle]);
    end;

    if TriParam <> '' then
      TriParam := '-' + TriParam;

    ClearElements;
    CreateNodeFile;
    // Ändrat till app-katalogen /RJ
    ExecAndWait(CurrentPath + FTriangleExecutable + ' ' + TriParam + ' "' +
        CurrentPath + FModelname + '.node"', SW_SHOWMINIMIZED);
    ReadElementFile;

  end
  else
  begin
    Self.ClearElements;

    Delaunay := TDelaunay.Create;

    for i := 0 to Self.PointCount - 1 do
      Delaunay.AddPoint(Self.Points[i].X, Self.Points[i].Y,
        Self.Points[i].Value);

    Delaunay.Execute;

    ClearPoints;

    for i := 1 to Delaunay.VertexCount - 1 do
      Self.AddPoint(Delaunay.Vertices[i].X, Delaunay.Vertices[i].Y,
        Delaunay.Vertices[i].Value);

    for i := 1 to Delaunay.TriangleCount do
    begin
      TriElement := TTriangleElement.Create;
      TriElement.Point[1] := TTrianglePoint
        (FPoints.Items[Delaunay.Triangles[i].VertexIndex[0] - 1]);
      TriElement.Point[2] := TTrianglePoint
        (FPoints.Items[Delaunay.Triangles[i].VertexIndex[1] - 1]);
      TriElement.Point[3] := TTrianglePoint
        (FPoints.Items[Delaunay.Triangles[i].VertexIndex[2] - 1]);
      UpdateExtents(TriElement);
      FElements.Add(TriElement);
    end;

    Delaunay.Free;
  end;
end;

procedure TTriangle.UnClip;
begin

  // Add all clipped elements back to the standard
  // element list.

  FClippedElements.Clear;
  FVisibleElements.Clear;
  FCrossingElements.Clear;
  FClipActive := true;
end;

procedure TTriangle.Clip;
var
  i, j : integer;
  T, ST: TTriangleElement;
  LeftClip: TTriangleLine;
  RightClip: TTriangleLine;
  TopClip: TTriangleLine;
  BottomClip: TTriangleLine;
  SplitLeft: TRefList;
  SplitRight: TRefList;
  SplitTop: TRefList;
  SplitBottom: TRefList;

  Rect2D : TRectangle;
  Tri2D  : TTriangle2D;

begin

  Rect2D[1].x:=TriangleClipRect.Left;
  Rect2D[1].y:=TriangleClipRect.Bottom;
  Rect2D[2].x:=TriangleClipRect.Right;
  Rect2D[2].y:=TriangleClipRect.Top;

  // Remove all clipped elements

  UnClip;

  // Move clipped elements to clipped list

  for i := 0 to FElements.Count - 1 do
  begin
    T := TTriangleElement(FElements.Items[i]);

    Tri2D[1].x:=T.Point[1].X;
    Tri2D[2].x:=T.Point[2].X;
    Tri2D[3].x:=T.Point[3].X;
    Tri2D[1].y:=T.Point[1].Y;
    Tri2D[2].y:=T.Point[2].Y;
    Tri2D[3].y:=T.Point[3].Y;

    if TriangleInRectangle(Tri2D, Rect2D) then
      FVisibleElements.Add(T)
    else if TriangleOutsideRectangle(Tri2D, Rect2D) then
      FClippedElements.Add(T)
    else
      FCrossingElements.Add(T);
  end;

  // Create lines for splitting triangles on the crossing
  // clip rect

  LeftClip := TTriangleLine.Create;
  LeftClip.StartPoint.X := Self.TriangleClipRect.Left;
  LeftClip.EndPoint.X := Self.TriangleClipRect.Left;
  LeftClip.StartPoint.Y := -1E300;
  LeftClip.EndPoint.Y := 1E300;

  RightClip := TTriangleLine.Create;
  RightClip.StartPoint.X := Self.TriangleClipRect.Right;
  RightClip.EndPoint.X := Self.TriangleClipRect.Right;
  RightClip.StartPoint.Y := -1E300;
  RightClip.EndPoint.Y := 1E300;

  TopClip := TTriangleLine.Create;
  TopClip.StartPoint.Y := Self.TriangleClipRect.Top;
  TopClip.EndPoint.Y := Self.TriangleClipRect.Top;
  TopClip.StartPoint.X := -1E300;
  TopClip.EndPoint.X := 1E300;

  BottomClip := TTriangleLine.Create;
  BottomClip.StartPoint.Y := Self.TriangleClipRect.Bottom;
  BottomClip.EndPoint.Y := Self.TriangleClipRect.Bottom;
  BottomClip.StartPoint.X := -1E300;
  BottomClip.EndPoint.X := 1E300;

  // Lets split the atoms !

  for i := 0 to FCrossingElements.Count - 1 do
  begin
    T := TTriangleElement(FCrossingElements.Items[i]);

    SplitLeft := T.Split(LeftClip);
    SplitRight := T.Split(RightClip);
    SplitTop := T.Split(TopClip);
    SplitBottom := T.Split(BottomClip);

    if assigned(SplitLeft) then
    begin
      for j := 0 to SplitLeft.Count - 1 do
      begin
        ST:=TTriangleElement(SplitLeft.Items[j]);
        Tri2D[1].x:=ST.Point[1].X;
        Tri2D[2].x:=ST.Point[2].X;
        Tri2D[3].x:=ST.Point[3].X;
        Tri2D[1].y:=ST.Point[1].Y;
        Tri2D[2].y:=ST.Point[2].Y;
        Tri2D[3].y:=ST.Point[3].Y;
        if TriangleInRectangle(Tri2D, Rect2D) then
          FVisibleElements.Add(ST);
      end;
      SplitLeft.Free;
    end;

    if assigned(SplitRight) then
    begin
      for j := 0 to SplitRight.Count - 1 do
      begin
        ST:=TTriangleElement(SplitRight.Items[j]);
        Tri2D[1].x:=ST.Point[1].X;
        Tri2D[2].x:=ST.Point[2].X;
        Tri2D[3].x:=ST.Point[3].X;
        Tri2D[1].y:=ST.Point[1].Y;
        Tri2D[2].y:=ST.Point[2].Y;
        Tri2D[3].y:=ST.Point[3].Y;
        if TriangleInRectangle(Tri2D, Rect2D) then
          FVisibleElements.Add(ST);
      end;
      SplitRight.Free;
    end;

    if assigned(SplitTop) then
    begin
      for j := 0 to SplitTop.Count - 1 do
      begin
        ST:=TTriangleElement(SplitTop.Items[j]);
        Tri2D[1].x:=ST.Point[1].X;
        Tri2D[2].x:=ST.Point[2].X;
        Tri2D[3].x:=ST.Point[3].X;
        Tri2D[1].y:=ST.Point[1].Y;
        Tri2D[2].y:=ST.Point[2].Y;
        Tri2D[3].y:=ST.Point[3].Y;
        if TriangleInRectangle(Tri2D, Rect2D) then
          FVisibleElements.Add(ST);
      end;
      SplitTop.Free;
    end;

    if assigned(SplitBottom) then
    begin
      for j := 0 to SplitBottom.Count - 1 do
      begin
        ST:=TTriangleElement(SplitBottom.Items[j]);
        Tri2D[1].x:=ST.Point[1].X;
        Tri2D[2].x:=ST.Point[2].X;
        Tri2D[3].x:=ST.Point[3].X;
        Tri2D[1].y:=ST.Point[1].Y;
        Tri2D[2].y:=ST.Point[2].Y;
        Tri2D[3].y:=ST.Point[3].Y;
        if TriangleInRectangle(Tri2D, Rect2D) then
          FVisibleElements.Add(ST);
      end;
      SplitBottom.Free;
    end;

  end;

  FCrossingElements.Clear;
  FClippedElements.Clear;

  // Clip agains polylines

  // Clean up

  LeftClip.Free;
  RightClip.Free;
  TopClip.Free;
  BottomClip.Free;

  FClipActive := true;
end;

function TTriangle.CoordHash(x, y: double): double;
begin
  Result:=x*13123121232.0+y;
end;

procedure TTriangle.PolygonClip;
var
  i, j, k: integer;
  T: TTriangleElement;
  ST: TTriangleElement;
  Test: array [1 .. 3] of boolean;
  TestPolyline: TPolygon2D;
  CP: TTrianglePolyline;
  RemoveList: TRefList;
  SplitList: TRefList;
  CrossingElements: TRefList;
  Line: TTriangleLine;
begin

  // Removes triangles inside a polygon.
  // Crossing triangles are clipped.

  RemoveList := TRefList.Create;
  CrossingElements := TRefList.Create;
  Line := TTriangleLine.Create;

  // Loop over all clip polylines

  for i := 0 to FClipPolylines.Count - 1 do
  begin

    // Create polygons for use with FastGeo

    CP := TTrianglePolyline(FClipPolylines.Items[i]);

    SetLength(TestPolyline, CP.Count);

    for j := 0 to CP.Count - 1 do
    begin
      TestPolyline[j].X := CP.Points[j].X;
      TestPolyline[j].Y := CP.Points[j].Y;
    end;

    // Determine what triangles are crossing the
    // polyline boundary and which should be removed

    for j := 0 to FVisibleElements.Count - 1 do
    begin
      T := TTriangleElement(FVisibleElements.Items[j]);

      for k := 1 to 3 do
      begin
        Test[k] := FastGeo.PointInPolygon(T.Point[k].X, T.Point[k].Y,
          TestPolyline)
      end;

      if (FTriangleClipProcessing = tpRemove) or
        (FTriangleClipProcessing = tpRemoveCrossing) then
      begin
        if (Test[1]) and (Test[2]) and (Test[3]) then
        begin
          RemoveList.Add(T);
        end
        else if (Test[1]) or (Test[2]) or (Test[3]) then
        begin
          CrossingElements.Add(T);
        end;
      end
      else if (FTriangleClipProcessing = tpRemoveReverse) or
        (FTriangleClipProcessing = tpRemoveCrossingReverse) then
      begin
        if (not Test[1]) and (not Test[2]) and (not Test[3]) then
        begin
          RemoveList.Add(T);
        end
        else if (Test[1]) or (Test[2]) or (Test[3]) then
        begin
          CrossingElements.Add(T);
        end;
      end;
    end;

    // Process crossing triangles

    // if (FTriangleClipProcessing = tpClip) then
    // begin
    for j := 0 to CrossingElements.Count - 1 do
    begin
      T := TTriangleElement(CrossingElements.Items[j]);
      SplitList := T.SplitRight(CP);

      if assigned(SplitList) then
      begin
        for k := 0 to SplitList.Count - 1 do
        begin
          ST := TTriangleElement(SplitList.Items[k]);
          FVisibleElements.Add(ST);
        end;
        SplitList.Free;
      end;

      RemoveList.Add(T);
    end;
    // end;

  end;

  // Remove clipped triangles from visible list

  for i := 0 to RemoveList.Count - 1 do
  begin
    T := TTriangleElement(RemoveList.Items[i]);
    FVisibleElements.Remove(T);
  end;

  if (FTriangleClipProcessing = tpRemoveCrossing) or
    (FTriangleClipProcessing = tpRemoveCrossingReverse) then
  begin
    for i := 0 to CrossingElements.Count - 1 do
    begin
      T := TTriangleElement(CrossingElements.Items[i]);
      FVisibleElements.Remove(T);
    end;
  end;

  RemoveList.Free;
  CrossingElements.Free;
  Line.Free;
end;

function TTriangle.GetElementCount: integer;
begin
  if FClipActive then
    Result := FVisibleElements.Count
  else
    Result := FElements.Count;
end;

function TTriangle.GetElements(index: integer): TTriangleElement;
begin
  if FClipActive then
  begin
    if (index >= 0) and (index < FVisibleElements.Count) then
    begin
      Result := TTriangleElement(FVisibleElements.Items[index]);
    end
    else
      Result := nil;
  end
  else
  begin
    if (index >= 0) and (index < FElements.Count) then
    begin
      Result := TTriangleElement(FElements.Items[index]);
    end
    else
      Result := nil;
  end;
end;

function TTriangle.GetMaxPoint: TTrianglePoint;
begin
  Result := FMaxPoint;
end;

function TTriangle.GetMinPoint: TTrianglePoint;
begin
  Result := FMinPoint;
end;

function TTriangle.GetPointCount: integer;
begin
  Result := FPoints.Count;
end;

function TTriangle.GetPoints(index: integer): TTrianglePoint;
begin
  if (index >= 0) and (index < FPoints.Count) then
  begin
    Result := TTrianglePoint(FPoints.Items[index]);
  end
  else
    Result := nil;
end;

procedure TTriangle.ReadElementFile;
var
  f: TextFile;
  ElementFilename: string;
  nElements: integer;
  ni1: integer;
  ni2: integer;
  i: integer;
  p1, p2, P3: integer;
  elNbr: integer;
  TriElement: TTriangleElement;
  CurrentPath: String;
begin
  CurrentPath := ExtractFilePath(Application.ExeName);
  if FileExists(CurrentPath + FModelname + '.node') then
  // Ändrat till app-katalogen /RJ
  begin
    ElementFilename := CurrentPath + FModelname + '.1.ele';
    // Ändrat till app-katalogen /RJ
    if FileExists(ElementFilename) then
    begin
      AssignFile(f, ElementFilename);
      Reset(f);

      readln(f, nElements, ni1, ni2);

      for i := 1 to nElements do
      begin
        readln(f, elNbr, p1, p2, P3);
        TriElement := TTriangleElement.Create;
        TriElement.Point[1] := TTrianglePoint(FPoints.Items[p1 - 1]);
        TriElement.Point[2] := TTrianglePoint(FPoints.Items[p2 - 1]);
        TriElement.Point[3] := TTrianglePoint(FPoints.Items[P3 - 1]);
        UpdateExtents(TriElement);
        FElements.Add(TriElement);
      end;

      CloseFile(f);
    end;
  end;
end;

procedure TTriangle.ResetExtents;
begin
  FMaxPoint.X := -1E300;
  FMaxPoint.Y := -1E300;
  FMinPoint.X := 1E300;
  FMinPoint.Y := 1E300;
end;

procedure TTriangle.UpdateExtents(Triangle: TTriangleElement);
var
  i: integer;
begin
  for i := 1 to 3 do
  begin
    if Triangle.Point[i].X > FMaxPoint.X then
      FMaxPoint.X := Triangle.Point[i].X;

    if Triangle.Point[i].Y > FMaxPoint.Y then
      FMaxPoint.Y := Triangle.Point[i].Y;

    if Triangle.Point[i].X < FMinPoint.X then
      FMinPoint.X := Triangle.Point[i].X;

    if Triangle.Point[i].Y < FMinPoint.Y then
      FMinPoint.Y := Triangle.Point[i].Y;
  end;
end;

procedure TTriangle.SetClipActive(const Value: boolean);
begin
  FClipActive := Value;
end;

function TTriangle.GetPolylines(index: integer): TTrianglePolyline;
begin
  if (index >= 0) and (index < FClipPolylines.Count) then
    Result := TTrianglePolyline(FClipPolylines.Items[index])
  else
    Result := nil;
end;

procedure TTriangle.Offset;
var
  TP: TTrianglePoint;
  i: integer;
begin

  ResetExtents;

  for i := 0 to FPoints.Count - 1 do
  begin
    TP := Self.Points[i];
    TP.Y := TP.Y + OffsetPolyline.Interpolate(TP.X);

    if TP.Y > FMaxPoint.Y then
      FMaxPoint.Y := TP.Y;

    if TP.Y < FMinPoint.Y then
      FMinPoint.Y := TP.Y;

    if TP.X > FMaxPoint.X then
      FMaxPoint.X := TP.X;

    if TP.X < FMinPoint.X then
      FMinPoint.X := TP.X;
  end;
end;

procedure TTriangle.AddClipPolyline(PolyLine: TTrianglePolyline);
begin
  FClipPolylines.Add(PolyLine);
end;

procedure TTriangle.ClearClipPolylines;
begin
  FClipPolylines.Clear;
end;

procedure TTriangle.SetTriangleClipProcessing
  (const Value: TTriangleClipProcessing);
begin
  FTriangleClipProcessing := Value;
end;

procedure TTriangle.SetClipPolyline(const Value: boolean);
begin
  FClipPolyline := Value;
end;

procedure TTriangle.SetDuplicateProcessing(
  const Value: TTriangleDuplicateProcessing);
begin
  FDuplicateProcessing := Value;
end;

{ TTriangleElement }

constructor TTriangleElement.Create;
begin
  inherited;

  FTriPoints[1] := nil;
  FTriPoints[2] := nil;
  FTriPoints[3] := nil;
end;

destructor TTriangleElement.Destroy;
begin
  Point[1] := nil;
  Point[2] := nil;
  Point[3] := nil;
  inherited;
end;

function TTriangleElement.GetIntersectionPoints
  (Value: double; var p1, p2: TTrianglePoint): boolean;
var
  I12: TTrianglePoint;
  I13: TTrianglePoint;
  I23: TTrianglePoint;
  f12: boolean;
  f13: boolean;
  f23: boolean;
begin
  I12 := TTrianglePoint.Create;
  I13 := TTrianglePoint.Create;
  I23 := TTrianglePoint.Create;
  f12 := Interpolate(FTriPoints[1], FTriPoints[2], Value, I12);
  f13 := Interpolate(FTriPoints[1], FTriPoints[3], Value, I13);
  f23 := Interpolate(FTriPoints[2], FTriPoints[3], Value, I23);

  if (not f12) and (not f13) and (not f23) then
  begin
    Result := false;
    I12.Free;
    I13.Free;
    I23.Free;
  end
  else
  begin
    if f12 then
    begin
      p1.AssignFrom(I12);
      if f13 then
        p2.AssignFrom(I13)
      else
        p2.AssignFrom(I23);
    end
    else if f13 then
    begin
      p1.AssignFrom(I13);
      if f12 then
        p2.AssignFrom(I12)
      else
        p2.AssignFrom(I23);
    end
    else
    begin
      p1.AssignFrom(I23);
      if f12 then
        p2.AssignFrom(I12)
      else
        p2.AssignFrom(I13);
    end;

    I12.Free;
    I13.Free;
    I23.Free;
    Result := true;
  end;
end;

function TTriangleElement.GetPoint(index: integer): TTrianglePoint;
begin
  Result := FTriPoints[index];
end;

procedure TTriangleElement.SetPoint(index: integer; Point: TTrianglePoint);
begin
  if assigned(FTriPoints[index]) then
  begin
    FTriPoints[index].DelRef;
    if not FTriPoints[index].IsReferenced then
      FTriPoints[index].Free;
  end;
  FTriPoints[index] := Point;
  if assigned(Point) then
    Point.AddRef;
end;

function TTriangleElement.Split(PolyLine: TTrianglePolyline): TRefList;
var
  i: integer;
  L: TTriangleLine;
  SplitList: TRefList;
begin
  L := TTriangleLine.Create;

  for i := 0 to PolyLine.Count - 2 do
  begin
    PolyLine.AssignLine(i, L);
    SplitList := Self.Split(L);
    if assigned(SplitList) then
    begin
      Result := SplitList;
      L.Free;
      exit;
    end;
  end;

  Result := nil;

  L.Free;
end;

function TTriangleElement.Split(Line: TTriangleLine): TRefList;
var
  L1: TTriangleLine;
  L2: TTriangleLine;
  L3: TTriangleLine;

  I1: TTrianglePoint;
  I2: TTrianglePoint;
  I3: TTrianglePoint;

  L: TRefList;
  i: TRefList;
  T: TRefList;

  T1: TTriangleElement;
  T2: TTriangleElement;
  T3: TTriangleElement;

  HaveTriangles: boolean;

begin

  L := TRefList.Create;
  i := TRefList.Create;
  T := TRefList.Create;

  L1 := TTriangleLine.Create;
  L2 := TTriangleLine.Create;
  L3 := TTriangleLine.Create;

  L1.StartPoint.AssignFrom(Self.Point[1]);
  L1.EndPoint.AssignFrom(Self.Point[2]);

  L2.StartPoint.AssignFrom(Self.Point[1]);
  L2.EndPoint.AssignFrom(Self.Point[3]);

  L3.StartPoint.AssignFrom(Self.Point[3]);
  L3.EndPoint.AssignFrom(Self.Point[2]);

  // Check for intersections

  I1 := L1.Intersection(Line);
  I2 := L2.Intersection(Line);
  I3 := L3.Intersection(Line);

  if assigned(I1) then
    i.Add(I1);
  if assigned(I2) then
    i.Add(I2);
  if assigned(I3) then
    i.Add(I3);

  // Create triangles

  T1 := TTriangleElement.Create;
  T2 := TTriangleElement.Create;
  T3 := TTriangleElement.Create;

  T.Add(T1);
  T.Add(T2);
  T.Add(T3);

  HaveTriangles := false;

  if assigned(I1) and assigned(I3) then
  begin

    // Primary triangle

    T1.Point[1] := Self.Point[2];
    T1.Point[2] := I3;
    T1.Point[3] := I1;

    // Secondary triangles

    T2.Point[1] := Self.Point[1];
    T2.Point[2] := I1;
    T2.Point[3] := I3;

    T3.Point[1] := Self.Point[3];
    T3.Point[2] := Self.Point[1];
    T3.Point[3] := I3;

    HaveTriangles := true;

  end;

  if assigned(I1) and assigned(I2) then
  begin

    // Primary triangle

    T1.Point[1] := Self.Point[1];
    T1.Point[2] := I1;
    T1.Point[3] := I2;

    // Secondary triangles

    T2.Point[1] := Self.Point[2];
    T2.Point[2] := I2;
    T2.Point[3] := I1;

    T3.Point[1] := Self.Point[2];
    T3.Point[2] := Self.Point[3];
    T3.Point[3] := I2;

    HaveTriangles := true;

  end;

  if assigned(I2) and assigned(I3) then
  begin

    // Primary triangle

    T1.Point[1] := Self.Point[3];
    T1.Point[2] := I2;
    T1.Point[3] := I3;

    // Secondary triangles

    T2.Point[1] := Self.Point[2];
    T2.Point[2] := I3;
    T2.Point[3] := Self.Point[1];

    T3.Point[1] := I3;
    T3.Point[2] := I2;
    T3.Point[3] := Self.Point[1];

    HaveTriangles := true;

  end;

  if HaveTriangles then
  begin
    L.Add(T1);
    L.Add(T2);
    L.Add(T3);
    i.Free;
    T.Free;
    Result := L;
  end
  else
  begin
    L.Free;
    i.Free;
    T.Free;
    Result := nil;
  end;


  // Clean up

  L1.Free;
  L2.Free;
  L3.Free;
end;

function TTriangleElement.SplitLeft(Line: TTriangleLine): TRefList;
var
  SplitList: TRefList;
  i, j: integer;
  T: TTriangleElement;
  alfa: double;
  beta: double;
  gamma: double;
  MaxAngle: double;
  NewSplitList: TRefList;
  TempLine: TTriangleLine;
begin
  SplitList := Self.Split(Line);

  if not assigned(SplitList) then
  begin
    Result := nil;
    exit;
  end;

  NewSplitList := TRefList.Create;
  TempLine := TTriangleLine.Create;
  TempLine.AssignFrom(Line);
  TempLine.Scale(1E3);

  // Calculate line orientation

  alfa := Arctan2(TempLine.EndPoint.Y - TempLine.StartPoint.Y,
    TempLine.EndPoint.X - TempLine.StartPoint.X);

  // Sort out left lying triangles

  for i := 0 to SplitList.Count - 1 do
  begin
    T := TTriangleElement(SplitList.Items[i]);
    MaxAngle := 1E-300;
    for j := 1 to 3 do
    begin
      beta := Arctan2(T.Point[j].Y - TempLine.StartPoint.Y,
        T.Point[j].X - TempLine.StartPoint.X);
      gamma := alfa - beta;
      if abs(gamma) > abs(MaxAngle) then
        MaxAngle := gamma;
    end;

    if MaxAngle < 0 then
      NewSplitList.Add(T);
  end;

  SplitList.Free;
  TempLine.Free;
  Result := NewSplitList;
end;

function TTriangleElement.SplitRight(Line: TTriangleLine): TRefList;
var
  SplitList: TRefList;
  i, j: integer;
  T: TTriangleElement;
  alfa: double;
  beta: double;
  gamma: double;
  MaxAngle: double;
  NewSplitList: TRefList;
begin
  SplitList := Self.Split(Line);

  if not assigned(SplitList) then
  begin
    Result := nil;
    exit;
  end;

  NewSplitList := TRefList.Create;

  // Calculate line orientation

  alfa := Arctan2(Line.EndPoint.Y - Line.StartPoint.Y,
    Line.EndPoint.X - Line.StartPoint.X);

  // Sort out left lying triangles

  for i := 0 to SplitList.Count - 1 do
  begin
    T := TTriangleElement(SplitList.Items[i]);
    MaxAngle := 1E-300;
    for j := 1 to 3 do
    begin
      beta := Arctan2(T.Point[j].Y - Line.StartPoint.Y,
        T.Point[j].X - Line.StartPoint.X);
      gamma := alfa - beta;
      if abs(gamma) > abs(MaxAngle) then
        MaxAngle := gamma;
    end;

    if MaxAngle > 0 then
      NewSplitList.Add(T);
  end;

  SplitList.Free;
  Result := NewSplitList;
end;

function TTriangleElement.SplitLeft(PolyLine: TTrianglePolyline): TRefList;
var
  i: integer;
  L: TTriangleLine;
  SplitList: TRefList;
begin
  L := TTriangleLine.Create;

  for i := 0 to PolyLine.Count - 2 do
  begin
    PolyLine.AssignLine(i, L);
    SplitList := Self.SplitLeft(L);
    if assigned(SplitList) then
    begin
      Result := SplitList;
      L.Free;
      exit;
    end;
  end;

  Result := nil;

  L.Free;
end;

function TTriangleElement.SplitRight(PolyLine: TTrianglePolyline): TRefList;
var
  i: integer;
  L: TTriangleLine;
  SplitList: TRefList;
begin
  L := TTriangleLine.Create;

  for i := 0 to PolyLine.Count - 2 do
  begin
    PolyLine.AssignLine(i, L);
    SplitList := Self.SplitRight(L);
    if assigned(SplitList) then
    begin
      Result := SplitList;
      L.Free;
      exit;
    end;
  end;

  Result := nil;

  L.Free;
end;

{ TTrianglePoint }

procedure TTrianglePoint.AssignFrom(P: TTrianglePoint);
begin
  FX := P.X;
  FY := P.Y;
  FValue := P.Value;
end;

constructor TTrianglePoint.Create;
begin
  inherited;
  FX := 0;
  FY := 0;
end;

{ TTriangleClipRect }

function ArcTanFull(Y, X : double) : double;
var
    angle : double;
begin
  angle:=ArcTan2(Y, X);
  if angle<0 then
    angle:=2*pi-angle;
  Result:=angle;
end;

function TTriangleClipRect.Check(Triangle: TTriangleElement): TTriangleType;
var
  i : integer;
  PointInside: array [1 .. 3] of boolean;

  Inside: boolean;
  //Outside : boolean;
  Crossing: boolean;
  eps: double;

  OutsideCount : integer;

begin

  // Test against left side

  eps := 0.0;
  for i := 1 to 3 do
  begin
    PointInside[i] :=
      (Triangle.Point[i].X > FLeft-eps) and
      (Triangle.Point[i].X < FRight+eps ) and
      (Triangle.Point[i].Y > FBottom-eps ) and
      (Triangle.Point[i].Y < FTop+eps );
  end;

  OutsideCount := 0;

  for i := 1 to 3 do
    if not PointInside[i] then
      inc(OutsideCount);

  Inside := (OutsideCount=0);
  //Outside := (OutsideCount=3);
  Crossing := (OutsideCount>0) and (OutSideCount<3);

  if Crossing then
    Result := ttCrossing
  else if Inside then
    Result := ttInside
  else
    Result := ttOutside;
end;

constructor TTriangleClipRect.Create;
begin
  inherited Create;
  FLeft := -1;
  FRight := 1;
  FTop := 1;
  FBottom := -1;
end;

{ TRefCounted }

procedure TRefCounted.AddRef;
begin
  inc(FRefCount);
end;

constructor TRefCounted.Create;
begin
  inherited;
  FRefCount := 0;
end;

procedure TRefCounted.DelRef;
begin
  if FRefCount > 0 then
    dec(FRefCount);
end;

function TRefCounted.IsReferenced: boolean;
begin
  Result := FRefCount > 0;
end;

{ TRefList }

constructor TRefList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TRefList.Destroy;
begin
  Self.Clear;
  FList.Free;
  inherited;
end;

procedure TRefList.Clear;
var
  i: integer;
  Item: TRefCounted;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Item := FList.Items[i];
    Item.DelRef;

    // Do not free any referenced items

    if not Item.IsReferenced then
      Item.Free;
  end;

  FList.Clear;
end;

procedure TRefList.Add(Item: TRefCounted);
begin
  Item.AddRef;
  FList.Add(Item)
end;

procedure TRefList.Remove(Item: TRefCounted);
begin
  FList.Remove(Item);
  Item.DelRef;
  if not Item.IsReferenced then
    Item.Free;
end;

procedure TRefList.Delete(Idx: integer);
var
  Item: TRefCounted;
begin
  Item := FList.Items[Idx];
  FList.Delete(Idx);
  if not Item.IsReferenced then
    Item.Free;
end;

function TRefList.GetItem(index: integer): TRefCounted;
begin
  if (index >= 0) and (index < FList.Count) then
    Result := FList.Items[index]
  else
    Result := nil;
end;

function TRefList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TRefList.ClearUnreferenced;
var
  i: integer;
  Item: TRefCounted;
  Referenced: TList;
begin

  // Create a list for the referenced items

  Referenced := TList.Create;

  // Move all referenced items refcount > 1 to
  // the referenced list. Unreferenced items
  // are freed.

  for i := 0 to FList.Count - 1 do
  begin
    Item := FList.Items[i];
    Item.DelRef;

    if Item.IsReferenced then
      Referenced.Add(Item)
    else
      Item.Free;
  end;

  // Clear list

  FList.Clear;

  // Add back all referenced items

  for i := 0 to Referenced.Count - 1 do
  begin
    Self.Add(Referenced.Items[i]);
  end;

  Referenced.Free;
end;

{ TTriangleLine }

procedure TTriangleLine.AssignFrom(Line: TTriangleLine);
begin
  FStartPoint.X := Line.StartPoint.X;
  FStartPoint.Y := Line.StartPoint.Y;
  FEndPoint.X := Line.EndPoint.X;
  FEndPoint.Y := Line.EndPoint.Y;
end;

constructor TTriangleLine.Create;
begin
  inherited;

  FStartPoint := TTrianglePoint.Create;
  FEndPoint := TTrianglePoint.Create;
end;

destructor TTriangleLine.Destroy;
begin
  FStartPoint.Free;
  FEndPoint.Free;
  inherited;
end;

function TTriangleLine.Intersection(L: TTriangleLine): TTrianglePoint;
var
  X, Y: double;
  ISect: TTrianglePoint;

  ltot, lp: double;
  v: double;

  s1, s2 : TSegment2D;
begin

  s1[1].x:=Self.FStartPoint.X;
  s1[1].y:=Self.FStartPoint.Y;

  s1[2].x:=Self.FEndPoint.X;
  s1[2].y:=Self.FEndPoint.Y;

  s2[1].x:=L.StartPoint.X;
  s2[1].y:=L.StartPoint.Y;

  s2[2].x:=L.EndPoint.X;
  s2[2].y:=L.EndPoint.Y;

  if Intersect(s1, s2, x, y) then
    begin
      ISect := TTrianglePoint.Create;
      ISect.X := x;
      ISect.Y := y;

      ltot := Distance(StartPoint, EndPoint);
      lp := Distance(StartPoint, ISect);
      v := StartPoint.Value + (EndPoint.Value - StartPoint.Value)
        * lp / ltot;
      ISect.Value := v;

      Result:=ISect;
      exit;
    end
  else
    begin
      Result:=nil;
      exit;
    end;
end;

function TTriangleLine.Length: double;
begin
  Result := Sqrt(sqr(FEndPoint.X - FStartPoint.X) + sqr
      (FEndPoint.Y - FStartPoint.Y));
end;

procedure TTriangleLine.Scale(Factor: double);
var
  MP: TTrianglePoint;

  vx, vy: double;
  L: double;
begin
  MP := TTrianglePoint.Create;
  MP.X := (FStartPoint.X + FEndPoint.X) * 0.5;
  MP.Y := (FStartPoint.Y + FEndPoint.Y) * 0.5;

  L := Self.Length;

  vx := (FEndPoint.X - FStartPoint.X) / L;
  vy := (FEndPoint.Y - FStartPoint.Y) / L;

  FEndPoint.X := vx * L * 0.5 * Factor;
  FEndPoint.Y := vy * L * 0.5 * Factor;
  FStartPoint.X := -vx * L * 0.5 * Factor;
  FStartPoint.Y := -vy * L * 0.5 * Factor;
end;

{ TTrianglePolyline }

procedure TTrianglePolyline.Clear;
begin
  FPoints.Clear;
end;

constructor TTrianglePolyline.Create;
begin
  inherited;
  FPoints := TRefList.Create;
end;

destructor TTrianglePolyline.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TTrianglePolyline.AddPoint(X, Y: double);
var
  P: TTrianglePoint;
begin
  P := TTrianglePoint.Create;
  P.X := X;
  P.Y := Y;
  FPoints.Add(P);
end;

function TTrianglePolyline.GetPoints(Idx: integer): TTrianglePoint;
begin
  if (Idx >= 0) and (Idx < FPoints.Count) then
  begin
    Result := TTrianglePoint(FPoints.Items[Idx]);
  end
  else
    Result := nil;
end;

function TTrianglePolyline.Interpolate(X: double): double;
var
  i: integer;
  xStart, xEnd: double;
  yStart, yEnd: double;
  x1, x2, y1, y2: double;
begin
  xStart := Self.Points[0].X;
  yStart := Self.Points[0].Y;
  xEnd := Self.Points[Self.Count - 1].X;
  yEnd := Self.Points[Self.Count - 1].Y;

  Result:=yEnd;

  if X <= xStart then
    begin
      Result := yStart;
      exit;
    end
  else if X >= xEnd then
    begin
      Result := yEnd;
      exit;
    end
  else
    begin
      for i := 0 to Self.Count - 2 do
      begin
        x1 := Self.Points[i].X;
        y1 := Self.Points[i].Y;
        x2 := Self.Points[i + 1].X;
        y2 := Self.Points[i + 1].Y;

        if (x1 <= X) and (X < x2) then
        begin
          Result := y1 + (y2 - y1) * (X - x1) / (x2 - x1);
          exit;
        end;
      end;
    end;
end;

function TTrianglePolyline.GetCount: integer;
begin
  Result := FPoints.Count;
end;

procedure TTrianglePolyline.AssignLine(Idx: integer; Line: TTriangleLine);
begin
  if (Idx >= 0) and (Idx < FPoints.Count - 1) then
  begin
    Line.StartPoint.X := TTrianglePoint(FPoints.Items[Idx]).X;
    Line.StartPoint.Y := TTrianglePoint(FPoints.Items[Idx]).Y;
    Line.EndPoint.X := TTrianglePoint(FPoints.Items[Idx + 1]).X;
    Line.EndPoint.Y := TTrianglePoint(FPoints.Items[Idx + 1]).Y;
  end;
end;

end.
