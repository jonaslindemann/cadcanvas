unit Triangle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, Delaunay, FastGeo;

type

  TRefCounted = class
  private
    FRefCount : integer;
  public
    constructor Create;

    procedure AddRef;
    procedure DelRef;
    function IsReferenced : boolean;
  end;

  TRefList = class
  private
    FList : TList;
    function GetItem(index: integer): TRefCounted;
    function GetCount : integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Item : TRefCounted);
    procedure Remove(Item : TRefCounted);
    procedure Delete(Idx : integer);
    procedure Clear;
    procedure ClearUnreferenced;

    property Items[index : integer] : TRefCounted read GetItem;
    property Count : integer read GetCount;
  end;

  TTrianglePoint = class(TRefCounted)
  private
    FX : double;
    FY : double;
    FValue : double;
  public
    constructor Create;
    procedure AssignFrom(P : TTrianglePoint);

    property X : double read FX write FX;
    property Y : double read FY write FY;
    property Value : double read FValue write FValue;
  end;

  TTriangleLine = class(TRefCounted)
  private
    FEndPoint: TTrianglePoint;
    FStartPoint: TTrianglePoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Line : TTriangleLine);

    procedure Scale(Factor : double);

    function Length : double;

    function Intersection(L: TTriangleLine): TTrianglePoint;

    property StartPoint : TTrianglePoint read FStartPoint;
    property EndPoint : TTrianglePoint read FEndPoint;
  end;

  TTrianglePolyline = class(TRefCounted)
  private
    FPoints : TRefList;

    function GetPoints(idx: integer): TTrianglePoint;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddPoint(x, y : double);

    procedure AssignLine(idx : integer; Line : TTriangleLine);

    function Interpolate(x : double) : double;

    property Points[ idx : integer ] : TTrianglePoint read GetPoints;
    property Count : integer read GetCount;

  end;

  TTriangleElement = class(TRefCounted)
  private
    FTriPoints : array [1..3] of TTrianglePoint;
    function GetPoint(index : integer) : TTrianglePoint;
    procedure SetPoint(index : integer; Point : TTrianglePoint);
  public
    constructor Create;
    destructor Destroy; override;

    function Split(Line : TTriangleLine) : TRefList; overload;
    function Split(PolyLine : TTrianglePolyLine) : TRefList; overload;
    function SplitLeft(Line : TTriangleLine) : TRefList; overload;
    function SplitRight(Line : TTriangleLine) : TRefList; overload;
    function SplitLeft(Polyline : TTrianglePolyline) : TRefList; overload;
    function SplitRight(Polyline : TTrianglePolyline) : TRefList; overload;

    function GetIntersectionPoints(value : double; var p1, p2 : TTrianglePoint) : boolean;

    property Point[index : integer] : TTrianglePoint read GetPoint write SetPoint;
  end;

  TTriangleType = (ttInside, ttCrossing, ttOutside);

  TTriangleClipRect = class
  private
    FLeft : double;
    FRight : double;
    FTop : double;
    FBottom : double;
  public
    constructor Create;

    function Check(Triangle : TTriangleElement) : TTriangleType;

    property Left : double read FLeft write FLeft;
    property Right : double read FRight write FRight;
    property Top : double read FTop write FTop;
    property Bottom : double read FBottom write FBottom;
  end;

  TTriangleImplementation = (tiInternal, tiExternal);

  TTriangleClipProcessing = (tpDoNothing, tpClip, tpRemove, tpRemoveReverse, tpRemoveCrossing, tpRemoveCrossingReverse);

  TTriangle = class(TComponent)
  private
    { Private declarations }
    FTriangleImplementation : TTriangleImplementation;
    FTriangleExecutable : string;
    FModelname : string;
    FPoints : TRefList;
    FElements : TRefList;
    FClippedElements : TRefList;
    FCrossingElements : TRefList;
    FVisibleElements : TRefList;
    FQualityMesh: boolean;
    FMinAngle: double;
    FMaxValue : double;
    FMinValue : double;

    FMaxPoint : TTrianglePoint;
    FMinPoint : TTrianglePoint;

    FTriangleClipRect : TTriangleClipRect;
    FClipActive : boolean;

    FClipPolylines : TRefList;

    FOffsetPolyline : TTrianglePolyline;

    FTriangleClipProcessing : TTriangleClipProcessing;
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

    procedure UpdateExtents(Triangle : TTriangleElement);
    procedure ResetExtents;
    procedure SetClipActive(const Value: boolean);
    function GetPolylines(index: integer): TTrianglePolyline;
    procedure SetTriangleClipProcessing(
      const Value: TTriangleClipProcessing);
    procedure SetClipPolyline(const Value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;

    procedure Execute;
    procedure AddPoint(x, y, value : double);
    procedure AddClipPolyline(Polyline : TTrianglePolyline);

    procedure UnClip;
    procedure Clip;
    procedure PolygonClip;

    procedure Offset;

    property PointCount : integer read GetPointCount;
    property Points[index : integer] : TTrianglePoint read GetPoints;

    property ElementCount : integer read GetElementCount;
    property Elements[index : integer] : TTriangleElement read GetElements;

    property ClipPolylines[index : integer] : TTrianglePolyline read GetPolylines;
    property OffsetPolyline : TTrianglePolyline read FOffsetPolyline write FOffsetPolyline;

    property MaxValue : double read FMaxValue;
    property MinValue : double read FMinValue;
    property MaxPoint : TTrianglePoint read GetMaxPoint;
    property MinPoint : TTrianglePoint read GetMinPoint;

    property TriangleClipRect : TTriangleClipRect read FTriangleClipRect;
    property ClipActive : boolean read FClipActive write SetClipActive;

  published
    { Published declarations }
    property TriangleExecutable : string read FTriangleExecutable write FTriangleExecutable;
    property Modelname : string read FModelname write FModelname;

    property QualityMesh : boolean read FQualityMesh write FQualityMesh;
    property MinAngle : double read FMinAngle write FMinAngle;
    property TriangleImplementation : TTriangleImplementation read FTriangleImplementation write FTriangleImplementation;
    property ClipPolyline : boolean read FClipPolyline write SetClipPolyline;
    property TriangleClipProcessing : TTriangleClipProcessing read FTriangleClipProcessing write SetTriangleClipProcessing;
  end;

function Distance(P1, P2 : TTrianglePoint) : double;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TTriangle]);
end;

function ExecAndWait(Parameter: string; nCmdShow: integer): longword;
var
    zParameter: array[0..255] of char;
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
begin
  //StrPCopy(zAppName,FileName);
  StrPCopy(zParameter,Parameter);
  //StrPCopy(zCurDir,GetCurrentDir());
  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb:=Sizeof(StartupInfo);
  StartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow:=nCmdShow;
  if not CreateProcess(nil,zParameter,nil,nil,false,CREATE_NEW_CONSOLE or  NORMAL_PRIORITY_CLASS,nil,nil,StartupInfo,ProcessInfo) then
    Result:=0
    else
    begin
      WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess,Result);
    end;
end;


function Distance(P1, P2 : TTrianglePoint) : double;
begin
  Result:=Sqrt(Power(P2.X-P1.X,2)+Power(P2.Y-P1.Y,2));
end;

function Interpolate(P1, P2 : TTrianglePoint; value : double;
  var P3 : TTrianglePoint) : boolean;
var
    v1, v2 : double;
    k : double;
    Inside : boolean;


begin
  v1:=P1.Value;
  v2:=P2.Value;

  if (v1=v2) then
  begin
    Result:=false;
    exit;
  end;

  if P2.Value>=P1.Value then
    begin
      if (value>=v1) and (value<v2) then
        Inside:=true
      else
        Inside:=false;
    end
  else
    begin
      if (value>=v2) and (value<v1) then
        Inside:=true
      else
        Inside:=false;
    end;

  if not Inside then
  begin
    Result:=false;
    exit;
  end;

  k:=(value-v1)/(v2-v1);

  P3.X:=P1.X*(1-k) + P2.X*k;
  P3.Y:=P1.Y*(1-k) + P2.Y*k;
  P3.Value:=value;

  Result:=true;

end;


{ TTriangle }

constructor TTriangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FModelname:='temp';
  FPoints:=TRefList.Create;
  FElements:=TRefList.Create;
  FClippedElements:=TRefList.Create;
  FCrossingElements:=TRefList.Create;
  FVisibleElements:=TRefList.Create;

  FQualityMesh:=false;
  FMinAngle:=-1;

  FTriangleImplementation:=tiExternal;

  FMaxPoint:=TTrianglePoint.Create;
  FMinPoint:=TTrianglePoint.Create;
  FMaxPoint.X:=-1e300;
  FMaxPoint.Y:=-1e300;
  FMinPoint.X:=1e300;
  FMinPoint.Y:=1e300;

  FTriangleClipRect:=TTriangleClipRect.Create;
  FClipActive:=false;

  FClipPolylines:=TRefList.Create;
  FOffsetPolyline:=TTrianglePolyline.Create;
  FTriangleClipProcessing:=tpClip;
  FClipPolyline:=false;
end;

destructor TTriangle.Destroy;
begin
  //ClearElements;
  FElements.Free;

  //ClearPoints;
  FPoints.Free;

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


procedure TTriangle.AddPoint(x, y, value: double);
var
    TriPoint : TTrianglePoint;
begin
  TriPoint:=TTrianglePoint.Create;
  TriPoint.X:=x;
  TriPoint.Y:=y;
  TriPoint.Value:=value;
  FPoints.Add(TriPoint);

  if value>FMaxValue then
    FMaxValue:=value;

  if value<FMinValue then
    FMinValue:=value;
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
  FMaxValue:=-1e300;
  FMinValue:=1e300;
end;


procedure TTriangle.CreateNodeFile;
var
    f : TextFile;
    i : integer;
    TriPoint : TTrianglePoint;

    x, y, value : double;
    CurrentPath: String;
begin
  CurrentPath:=ExtractFilePath(Application.ExeName);
  if (FModelname<>'') then
  begin
    AssignFile(f, CurrentPath + FModelname+'.node');  // Ändrat till app-katalogen /RJ
    Rewrite(f);

    writeln(f, format('%d %d %d %d',[FPoints.Count,2,1,0]));

    for i:=0 to FPoints.Count-1 do
    begin
      TriPoint:=TTrianglePoint(FPoints.Items[i]);
      x:=TriPoint.X;
      y:=TriPoint.Y;
      value:=TriPoint.Value;
      writeln(f, i+1, ' ', x, ' ', y, ' ', value);
    end;

    CloseFile(f);
  end;
end;


procedure TTriangle.Execute;
var
    CurrentPath : string;
    TriParam : string;
    Delaunay : TDelaunay;
    i        : integer;
    TriElement : TTriangleElement;
begin

  ResetExtents;

  if FTriangleImplementation=tiExternal then
    begin

      CurrentPath:=ExtractFilePath(Application.ExeName);
      if (not FileExists(CurrentPath + FTriangleExecutable)) then  // Ändrat till app-katalogen /RJ
      begin
        ShowMessage(FTriangleExecutable+' could not be found.');
        exit;
      end;

      TriParam:='';
      if (FQualityMesh) then
      begin
        TriParam:=TriParam+'q';
        if (FMinAngle>0) then
          TriParam:=TriParam+format('%g',[FMinAngle]);
      end;

      if TriParam<>'' then
        TriParam:='-'+TriParam;

      ClearElements;
      CreateNodeFile;
      // Ändrat till app-katalogen /RJ
      ExecAndWait(CurrentPath + FTriangleExecutable + ' ' + TriParam + ' "' + CurrentPath + FModelName + '.node"', SW_SHOWMINIMIZED);
      ReadElementFile;

    end
  else
    begin
      Self.ClearElements;

      Delaunay:=TDelaunay.Create;

      for i:=0 to Self.PointCount-1 do
        Delaunay.AddPoint(Self.Points[i].X, Self.Points[i].Y, Self.Points[i].Value);

      Delaunay.Execute;

      ClearPoints;

      for i:=1 to Delaunay.VertexCount-1 do
        Self.AddPoint(Delaunay.Vertices[i].X, Delaunay.Vertices[i].Y, Delaunay.Vertices[i].Value);

      for i:=1 to Delaunay.TriangleCount do
      begin
        TriElement:=TTriangleElement.Create;
        TriElement.Point[1]:=TTrianglePoint(FPoints.Items[Delaunay.Triangles[i].VertexIndex[0]-1]);
        TriElement.Point[2]:=TTrianglePoint(FPoints.Items[Delaunay.Triangles[i].VertexIndex[1]-1]);
        TriElement.Point[3]:=TTrianglePoint(FPoints.Items[Delaunay.Triangles[i].VertexIndex[2]-1]);
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
  FClipActive:=true;
end;


procedure TTriangle.Clip;
var
    i, j : integer;
    T : TTriangleElement;
    TriangleType : TTriangleType;
    LeftClip : TTriangleLine;
    RightClip : TTriangleLine;
    TopClip : TTriangleLine;
    BottomClip : TTriangleLine;
    SplitLeft : TRefList;
    SplitRight : TRefList;
    SplitTop : TRefList;
    SplitBottom : TRefList;
begin

  // Remove all clipped elements

  UnClip;

  // Move clipped elements to clipped list

  for i:=0 to FElements.Count-1 do
  begin
    T:=TTriangleElement(FElements.Items[i]);

    TriangleType:=TriangleClipRect.Check(T);

    if (TriangleType=ttOutside) then
      FClippedElements.Add(T);

    if (TriangleType=ttCrossing) then
      FCrossingElements.Add(T);

    if (TriangleType=ttInside) then
      FVisibleElements.Add(T);
  end;

  // Create lines for splitting triangles on the crossing
  // clip rect

  LeftClip:=TTriangleLine.Create;
  LeftClip.StartPoint.X:=Self.TriangleClipRect.Left;
  LeftClip.EndPoint.X:=Self.TriangleClipRect.Left;
  LeftClip.StartPoint.Y:=-1e300;
  LeftClip.EndPoint.Y:=1e300;

  RightClip:=TTriangleLine.Create;
  RightClip.StartPoint.X:=Self.TriangleClipRect.Right;
  RightClip.EndPoint.X:=Self.TriangleClipRect.Right;
  RightClip.StartPoint.Y:=-1e300;
  RightClip.EndPoint.Y:=1e300;

  TopClip:=TTriangleLine.Create;
  TopClip.StartPoint.Y:=Self.TriangleClipRect.Top;
  TopClip.EndPoint.Y:=Self.TriangleClipRect.Top;
  TopClip.StartPoint.X:=-1e300;
  TopClip.EndPoint.X:=1e300;

  BottomClip:=TTriangleLine.Create;
  BottomClip.StartPoint.Y:=Self.TriangleClipRect.Bottom;
  BottomClip.EndPoint.Y:=Self.TriangleClipRect.Bottom;
  BottomClip.StartPoint.X:=-1e300;
  BottomClip.EndPoint.X:=1e300;

  // Lets split the atoms !

  for i:=0 to FCrossingElements.Count-1 do
  begin
    T:=TTriangleElement(FCrossingElements.Items[i]);

    SplitLeft:=T.Split(LeftClip);
    SplitRight:=T.Split(RightClip);
    SplitTop:=T.Split(TopClip);
    SplitBottom:=T.Split(BottomClip);

    if assigned(SplitLeft) then
    begin
      for j:=0 to SplitLeft.Count-1 do
      begin
        if (FTriangleClipRect.Check(TTriangleElement(SplitLeft.Items[j]))=ttInside) then
          FVisibleElements.Add(TTriangleElement(SplitLeft.Items[j]));
      end;
      SplitLeft.Free;
    end;

    if assigned(SplitRight) then
    begin
      for j:=0 to SplitRight.Count-1 do
      begin
        if (FTriangleClipRect.Check(TTriangleElement(SplitRight.Items[j]))=ttInside) then
          FVisibleElements.Add(TTriangleElement(SplitRight.Items[j]));
      end;
      SplitRight.Free;
    end;

    if assigned(SplitTop) then
    begin
      for j:=0 to SplitTop.Count-1 do
      begin
        if (FTriangleClipRect.Check(TTriangleElement(SplitTop.Items[j]))=ttInside) then
          FVisibleElements.Add(TTriangleElement(SplitTop.Items[j]));
      end;
      SplitTop.Free;
    end;

    if assigned(SplitBottom) then
    begin
      for j:=0 to SplitBottom.Count-1 do
      begin
        if (FTriangleClipRect.Check(TTriangleElement(SplitBottom.Items[j]))=ttInside) then
          FVisibleElements.Add(TTriangleElement(SplitBottom.Items[j]));
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

  FClipActive:=true;
end;

procedure TTriangle.PolygonClip;
var
    i, j, k : integer;
    T : TTriangleElement;
    ST : TTriangleElement;
    Test : array [1..3] of boolean;
    TestPolyline : TPolygon2D;
    CP : TTrianglePolyline;
    RemoveList : TRefList;
    SplitList : TRefList;
    CrossingElements : TRefList;
    Line : TTriangleLine;
begin

  // Removes triangles inside a polygon.
  // Crossing triangles are clipped.

  RemoveList:=TRefList.Create;
  CrossingElements:=TRefList.Create;
  Line:=TTriangleLine.Create;

  // Loop over all clip polylines

  for i:=0 to FClipPolylines.Count-1 do
  begin

    // Create polygons for use with FastGeo

    CP:=TTrianglePolyline(FClipPolylines.Items[i]);

    SetLength(TestPolyline, CP.Count);

    for j:=0 to CP.Count-1 do
    begin
      TestPolyline[j].x:=CP.Points[j].X;
      TestPolyline[j].y:=CP.Points[j].Y;
    end;

    // Determine what triangles are crossing the
    // polyline boundary and which should be removed

    for j:=0 to FVisibleElements.Count-1 do
    begin
      T:=TTriangleElement(FVisibleElements.Items[j]);

      for k:=1 to 3 do
      begin
        Test[k]:=FastGeo.PointInPolygon(T.Point[k].X, T.Point[k].Y, TestPolyline)
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

    //if (FTriangleClipProcessing = tpClip) then
    //begin
      for j:=0 to CrossingElements.Count-1 do
      begin
        T:=TTriangleElement(CrossingElements.Items[j]);
        SplitList:=T.SplitRight(CP);

        if assigned(SplitList) then
        begin
          for k:=0 to SplitList.Count-1 do
          begin
            ST:=TTriangleElement(SplitList.Items[k]);
            FVisibleElements.Add(ST);
          end;
          SplitList.Free;
        end;

        RemoveList.Add(T);
      end;
    //end;

  end;

  // Remove clipped triangles from visible list

  for i:=0 to RemoveList.Count-1 do
  begin
    T:=TTriangleElement(RemoveList.Items[i]);
    FVisibleElements.Remove(T);
  end;

  if (FTriangleClipProcessing=tpRemoveCrossing) or
     (FTriangleClipProcessing=tpRemoveCrossingReverse) then
  begin
    for i:=0 to CrossingElements.Count-1 do
    begin
      T:=TTriangleElement(CrossingElements.Items[i]);
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
    Result:=FVisibleElements.Count
  else
    Result:=FElements.Count;
end;

function TTriangle.GetElements(index: integer): TTriangleElement;
begin
  if FClipActive then
    begin
      if (index>=0) and (index<FVisibleElements.Count) then
        begin
         Result:=TTriangleElement(FVisibleElements.Items[index]);
        end
      else
        Result:=nil;
    end
  else
    begin
      if (index>=0) and (index<FElements.Count) then
        begin
         Result:=TTriangleElement(FElements.Items[index]);
        end
      else
        Result:=nil;
    end;
end;

function TTriangle.GetMaxPoint: TTrianglePoint;
begin
  Result:=FMaxPoint;
end;

function TTriangle.GetMinPoint: TTrianglePoint;
begin
  Result:=FMinPoint;
end;

function TTriangle.GetPointCount: integer;
begin
  Result:=FPoints.Count;
end;

function TTriangle.GetPoints(index: integer): TTrianglePoint;
begin
  if (index>=0) and (index<FPoints.Count) then
    begin
     Result:=TTrianglePoint(FPoints.Items[index]);
    end
  else
    Result:=nil;
end;

procedure TTriangle.ReadElementFile;
var
    f : TextFile;
    ElementFilename : string;
    nElements : integer;
    ni1 : integer;
    ni2 : integer;
    i : integer;
    p1, p2, p3 : integer;
    elNbr : integer;
    TriElement : TTriangleElement;
    CurrentPath: String;
begin
  CurrentPath:=ExtractFilePath(Application.ExeName);
  if FileExists(CurrentPath + FModelname+'.node') then  // Ändrat till app-katalogen /RJ
  begin
    ElementFilename:=CurrentPath + FModelname+'.1.ele';  // Ändrat till app-katalogen /RJ
    if FileExists(ElementFilename) then
    begin
      AssignFile(f, ElementFilename);
      Reset(f);

      readln(f, nElements, ni1, ni2);

      for i:=1 to nElements do
      begin
        readln(f, elNbr, p1, p2, p3);
        TriElement:=TTriangleElement.Create;
        TriElement.Point[1]:=TTrianglePoint(FPoints.Items[p1-1]);
        TriElement.Point[2]:=TTrianglePoint(FPoints.Items[p2-1]);
        TriElement.Point[3]:=TTrianglePoint(FPoints.Items[p3-1]);
        UpdateExtents(TriElement);
        FElements.Add(TriElement);
      end;

      CloseFile(f);
    end;
  end;
end;

procedure TTriangle.ResetExtents;
begin
  FMaxPoint.X:=-1e300;
  FMaxPoint.Y:=-1e300;
  FMinPoint.X:=1e300;
  FMinPoint.Y:=1e300;
end;

procedure TTriangle.UpdateExtents(Triangle: TTriangleElement);
var
    i : integer;
begin
  for i:=1 to 3 do
  begin
    if Triangle.Point[i].X>FMaxPoint.X then
      FMaxPoint.X:=Triangle.Point[i].X;

    if Triangle.Point[i].Y>FMaxPoint.Y then
      FMaxPoint.Y:=Triangle.Point[i].Y;

    if Triangle.Point[i].X<FMinPoint.X then
      FMinPoint.X:=Triangle.Point[i].X;

    if Triangle.Point[i].Y<FMinPoint.Y then
      FMinPoint.Y:=Triangle.Point[i].Y;
  end;
end;

procedure TTriangle.SetClipActive(const Value: boolean);
begin
  FClipActive := Value;
end;

function TTriangle.GetPolylines(index: integer): TTrianglePolyline;
begin
  if (index>=0) and (index<FClipPolylines.Count) then
    Result:=TTrianglePolyline(FClipPolylines.Items[index])
  else
    Result:=nil;
end;

procedure TTriangle.Offset;
var
    TP : TTrianglePoint;
    i : integer;
begin
  FMaxPoint.Y:=-1e300;
  FMinPoint.Y:=1e300;
  for i := 0 to FPoints.Count - 1 do
  begin
    TP := Self.Points[i];
    TP.Y:=TP.Y + OffsetPolyline.Interpolate(TP.X);

    if TP.Y>FMaxPoint.Y then
      FMaxPoint.Y:=TP.Y;

    if TP.Y<FMinPoint.Y then
      FMinPoint.Y:=TP.Y;
  end;
end;

procedure TTriangle.AddClipPolyline(Polyline: TTrianglePolyline);
begin
  FClipPolylines.Add(Polyline);
end;

procedure TTriangle.ClearClipPolylines;
begin
  FClipPolylines.Clear;
end;

procedure TTriangle.SetTriangleClipProcessing(
  const Value: TTriangleClipProcessing);
begin
  FTriangleClipProcessing := Value;
end;

procedure TTriangle.SetClipPolyline(const Value: boolean);
begin
  FClipPolyline := Value;
end;

{ TTriangleElement }

constructor TTriangleElement.Create;
begin
  inherited;

  FTriPoints[1]:=nil;
  FTriPoints[2]:=nil;
  FTriPoints[3]:=nil;
end;


destructor TTriangleElement.Destroy;
begin
  Point[1]:=nil;
  Point[2]:=nil;
  Point[3]:=nil;
  inherited;
end;

function TTriangleElement.GetIntersectionPoints(value: double; var p1,
  p2: TTrianglePoint): boolean;
var
    I12 : TTrianglePoint;
    I13 : TTrianglePoint;
    I23 : TTrianglePoint;
    f12 : boolean;
    f13 : boolean;
    f23 : boolean;
begin
  I12:=TTrianglePoint.Create;
  I13:=TTrianglePoint.Create;
  I23:=TTrianglePoint.Create;
  f12:=Interpolate(FTriPoints[1], FTriPoints[2], value, I12);
  f13:=Interpolate(FTriPoints[1], FTriPoints[3], value, I13);
  f23:=Interpolate(FTriPoints[2], FTriPoints[3], value, I23);

  if (not f12) and (not f13) and (not f23) then
    begin
      Result:=false;
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
      Result:=true;
    end;
end;

function TTriangleElement.GetPoint(index: integer): TTrianglePoint;
begin
  Result:=FTriPoints[index];
end;

procedure TTriangleElement.SetPoint(index: integer; Point: TTrianglePoint);
begin
  if assigned(FTriPoints[index]) then
  begin
    FTriPoints[index].DelRef;
    if not FTriPoints[index].IsReferenced then
      FTriPoints[index].Free;
  end;
  FTriPoints[index]:=Point;
  if assigned(Point) then
    Point.AddRef;
end;

function TTriangleElement.Split(PolyLine : TTrianglePolyline) : TRefList;
var
    i : integer;
    L : TTriangleLine;
    SplitList : TRefList;
begin
  L:=TTriangleLine.Create;

  for i:=0 to Polyline.Count-2 do
  begin
    Polyline.AssignLine(i,L);
    SplitList:=Self.Split(L);
    if assigned(SplitList) then
    begin
      Result:=SplitList;
      L.Free;
      exit;
    end;
  end;

  Result:=nil;

  L.Free;
end;

function TTriangleElement.Split(Line: TTriangleLine): TRefList;
var
    L1 : TTriangleLine;
    L2 : TTriangleLine;
    L3 : TTriangleLine;

    I1 : TTrianglePoint;
    I2 : TTrianglePoint;
    I3 : TTrianglePoint;

    L : TRefList;
    I : TRefList;
    T : TRefList;

    T1 : TTriangleElement;
    T2 : TTriangleElement;
    T3 : TTriangleElement;

    HaveTriangles : boolean;

begin

  L:=TRefList.Create;
  I:=TRefList.Create;
  T:=TRefList.Create;

  L1:=TTriangleLine.Create;
  L2:=TTriangleLine.Create;
  L3:=TTriangleLine.Create;

  L1.StartPoint.AssignFrom(Self.Point[1]);
  L1.EndPoint.AssignFrom(Self.Point[2]);

  L2.StartPoint.AssignFrom(Self.Point[1]);
  L2.EndPoint.AssignFrom(Self.Point[3]);

  L3.StartPoint.AssignFrom(Self.Point[3]);
  L3.EndPoint.AssignFrom(Self.Point[2]);

  // Check for intersections

  I1:=L1.Intersection(Line);
  I2:=L2.Intersection(Line);
  I3:=L3.Intersection(Line);

  if assigned(I1) then I.Add(I1);
  if assigned(I2) then I.Add(I2);
  if assigned(I3) then I.Add(I3);

  // Create triangles

  T1:=TTriangleElement.Create;
  T2:=TTriangleElement.Create;
  T3:=TTriangleElement.Create;

  T.Add(T1);
  T.Add(T2);
  T.Add(T3);

  HaveTriangles:=false;

  if assigned(I1) and assigned(I3) then
    begin

      // Primary triangle

      T1.Point[1]:=Self.Point[2];
      T1.Point[2]:=I3;
      T1.Point[3]:=I1;

      // Secondary triangles

      T2.Point[1]:=Self.Point[1];
      T2.Point[2]:=I1;
      T2.Point[3]:=I3;

      T3.Point[1]:=Self.Point[3];
      T3.Point[2]:=Self.Point[1];
      T3.Point[3]:=I3;

      HaveTriangles:=true;

    end;


  if assigned(I1) and assigned(I2) then
    begin

      // Primary triangle

      T1.Point[1]:=Self.Point[1];
      T1.Point[2]:=I1;
      T1.Point[3]:=I2;

      // Secondary triangles

      T2.Point[1]:=Self.Point[2];
      T2.Point[2]:=I2;
      T2.Point[3]:=I1;

      T3.Point[1]:=Self.Point[2];
      T3.Point[2]:=Self.Point[3];
      T3.Point[3]:=I2;

      HaveTriangles:=true;

    end;

  if assigned(I2) and assigned(I3) then
    begin

      // Primary triangle

      T1.Point[1]:=Self.Point[3];
      T1.Point[2]:=I2;
      T1.Point[3]:=I3;

      // Secondary triangles

      T2.Point[1]:=Self.Point[2];
      T2.Point[2]:=I3;
      T2.Point[3]:=Self.Point[1];

      T3.Point[1]:=I3;
      T3.Point[2]:=I2;
      T3.Point[3]:=Self.Point[1];

      HaveTriangles:=true;

    end;

  if HaveTriangles then
    begin
      L.Add(T1);
      L.Add(T2);
      L.Add(T3);
      I.Free;
      T.Free;
      Result:=L;
    end
  else
    begin
      L.Free;
      I.Free;
      T.Free;
      Result:=nil;
    end;


  // Clean up

  L1.Free;
  L2.Free;
  L3.Free;
end;

function TTriangleElement.SplitLeft(Line: TTriangleLine): TRefList;
var
  SplitList : TRefList;
  i, j : integer;
  T : TTriangleElement;
  alfa : double;
  beta : double;
  gamma : double;
  MaxAngle : double;
  NewSplitList : TRefList;
  TempLine : TTriangleLine;
begin
  SplitList:=Self.Split(Line);

  if not assigned(SplitList) then
  begin
    Result:=nil;
    exit;
  end;

  NewSplitList:=TRefList.Create;
  TempLine:=TTriangleLine.Create;
  TempLine.AssignFrom(Line);
  TempLine.Scale(1e3);

  // Calculate line orientation

  alfa:=Arctan2(
    TempLine.EndPoint.Y-TempLine.StartPoint.Y,
    TempLine.EndPoint.X-TempLine.StartPoint.X
  );

  // Sort out left lying triangles

  for i:=0 to SplitList.Count-1 do
  begin
    T:=TTriangleElement(SplitList.Items[i]);
    MaxAngle:=1e-300;
    for j:=1 to 3 do
    begin
      beta:=Arctan2(
        T.Point[j].Y-TempLine.StartPoint.Y,
        T.Point[j].X-TempLine.StartPoint.X
      );
      gamma:=alfa - beta;
      if abs(gamma)>abs(MaxAngle) then
        MaxAngle:=gamma;
    end;

    if MaxAngle<0 then
      NewSplitList.Add(T);
  end;

  SplitList.Free;
  TempLine.Free;
  Result:=NewSplitList;
end;

function TTriangleElement.SplitRight(Line: TTriangleLine): TRefList;
var
  SplitList : TRefList;
  i, j : integer;
  T : TTriangleElement;
  alfa : double;
  beta : double;
  gamma : double;
  MaxAngle : double;
  NewSplitList : TRefList;
begin
  SplitList:=Self.Split(Line);

  if not assigned(SplitList) then
  begin
    Result:=nil;
    exit;
  end;

  NewSplitList:=TRefList.Create;

  // Calculate line orientation

  alfa:=Arctan2(
    Line.EndPoint.Y-Line.StartPoint.Y,
    Line.EndPoint.X-Line.StartPoint.X
  );

  // Sort out left lying triangles

  for i:=0 to SplitList.Count-1 do
  begin
    T:=TTriangleElement(SplitList.Items[i]);
    MaxAngle:=1e-300;
    for j:=1 to 3 do
    begin
      beta:=Arctan2(
        T.Point[j].Y-Line.StartPoint.Y,
        T.Point[j].X-Line.StartPoint.X
      );
      gamma:=alfa - beta;
      if abs(gamma)>abs(MaxAngle) then
        MaxAngle:=gamma;
    end;

    if MaxAngle>0 then
      NewSplitList.Add(T);
  end;

  SplitList.Free;
  Result:=NewSplitList;
end;

function TTriangleElement.SplitLeft(Polyline: TTrianglePolyline): TRefList;
var
    i : integer;
    L : TTriangleLine;
    SplitList : TRefList;
begin
  L:=TTriangleLine.Create;

  for i:=0 to Polyline.Count-2 do
  begin
    Polyline.AssignLine(i,L);
    SplitList:=Self.SplitLeft(L);
    if assigned(SplitList) then
    begin
      Result:=SplitList;
      L.Free;
      exit;
    end;
  end;

  Result:=nil;

  L.Free;
end;

function TTriangleElement.SplitRight(
  Polyline: TTrianglePolyline): TRefList;
var
    i : integer;
    L : TTriangleLine;
    SplitList : TRefList;
begin
  L:=TTriangleLine.Create;

  for i:=0 to Polyline.Count-2 do
  begin
    Polyline.AssignLine(i,L);
    SplitList:=Self.SplitRight(L);
    if assigned(SplitList) then
    begin
      Result:=SplitList;
      L.Free;
      exit;
    end;
  end;

  Result:=nil;

  L.Free;
end;

{ TTrianglePoint }

procedure TTrianglePoint.AssignFrom(P: TTrianglePoint);
begin
  FX:=P.X;
  FY:=P.Y;
  FValue:=P.Value;
end;

constructor TTrianglePoint.Create;
begin
  inherited;
  FX:=0;
  FY:=0;
end;

{ TTriangleClipRect }

function TTriangleClipRect.Check(Triangle : TTriangleElement): TTriangleType;
var
    i : integer;
    PointInside : array [1..3] of boolean;
    Inside : boolean;
    //Outside : boolean;
    Crossing : boolean;
    eps : double;
begin
  eps := 1e-6;
  for i:=1 to 3 do
  begin
    PointInside[i]:=
      (Triangle.Point[i].X>=FLeft) and (Triangle.Point[i].X<=FRight) and
      (Triangle.Point[i].Y>=FBottom-eps) and (Triangle.Point[i].Y<=FTop+eps);
  end;

  Inside:=PointInside[1] and PointInside[2] and PointInside[3];
  Crossing:=PointInside[1] or PointInside[2] or PointInside[3];
  //Outside:=(not PointInside[1]) and (not PointInside[2]) and (not PointInside[1]);

  if Inside then
    Result:=ttInside
  else if Crossing then
    Result:=ttCrossing
  else
    Result:=ttOutside;
end;

constructor TTriangleClipRect.Create;
begin
  inherited Create;
  FLeft:=-1;
  FRight:=1;
  FTop:=1;
  FBottom:=-1;
end;

{ TRefCounted }

procedure TRefCounted.AddRef;
begin
  inc(FRefCount);
end;

constructor TRefCounted.Create;
begin
  inherited;
  FRefCount:=0;
end;

procedure TRefCounted.DelRef;
begin
  if FRefCount>0 then
    dec(FRefCount);
end;

function TRefCounted.IsReferenced: boolean;
begin
  Result:=FRefCount>0;
end;

{ TRefList }

constructor TRefList.Create;
begin
  inherited;
  FList:=TList.Create;
end;

destructor TRefList.Destroy;
begin
  Self.Clear;
  FList.Free;
  inherited;
end;

procedure TRefList.Clear;
var
    i : integer;
    Item : TRefCounted;
begin
  for i:=0 to FList.Count-1 do
  begin
    Item:=FList.Items[i];
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
    Item : TRefCounted;
begin
  Item:=FList.Items[Idx];
  FList.Delete(idx);
  if not Item.IsReferenced then
    Item.Free;
end;

function TRefList.GetItem(index: integer): TRefCounted;
begin
  if (index>=0) and (index<FList.Count) then
    Result:=FList.Items[index]
  else
    Result:=nil;
end;

function TRefList.GetCount: integer;
begin
  Result:=FList.Count;
end;

procedure TRefList.ClearUnreferenced;
var
    i : integer;
    Item : TRefCounted;
    Referenced : TList;
begin

  // Create a list for the referenced items

  Referenced:=TList.Create;

  // Move all referenced items refcount > 1 to
  // the referenced list. Unreferenced items
  // are freed.

  for i:=0 to FList.Count-1 do
  begin
    Item:=FList.Items[i];
    Item.DelRef;

    if Item.IsReferenced then
      Referenced.Add(Item)
    else
      Item.Free;
  end;

  // Clear list

  FList.Clear;

  // Add back all referenced items

  for i:=0 to Referenced.Count-1 do
  begin
    Self.Add(Referenced.Items[i]);
  end;

  Referenced.Free;
end;

{ TTriangleLine }

procedure TTriangleLine.AssignFrom(Line: TTriangleLine);
begin
  FStartPoint.X:=Line.StartPoint.X;
  FStartPoint.Y:=Line.StartPoint.Y;
  FEndPoint.X:=Line.EndPoint.X;
  FEndPoint.Y:=Line.EndPoint.Y;
end;

constructor TTriangleLine.Create;
begin
  inherited;

  FStartPoint:=TTrianglePoint.Create;
  FEndPoint:=TTrianglePoint.Create;
end;

destructor TTriangleLine.Destroy;
begin
  FStartPoint.Free;
  FEndPoint.Free;
  inherited;
end;

function TTriangleLine.Intersection(L: TTriangleLine): TTrianglePoint;
var
  k1, k2 : double;
  m1, m2 : double;
  x, y : double;
  x1, x2 : double;
  ISect : TTrianglePoint;
  dx, dy : double;

  ltot, lp : double;
  v : double;

  TmpStart1, TmpStart2, TmpEnd1, TmpEnd2 : TTrianglePoint;
  TmpPoint : TTrianglePoint;

  vert1 : boolean;
  vert2 : boolean;

  Done : boolean;
begin

  Result:=nil;
  Done:=false;

  TmpStart1:=TTrianglePoint.Create;
  TmpStart2:=TTrianglePoint.Create;
  TmpEnd1:=TTrianglePoint.Create;
  TmpEnd2:=TTrianglePoint.Create;
  TmpPoint:=TTrianglePoint.Create;

  k1:=0;
  k2:=0;
  m1:=0;
  m2:=0;
  x1:=0;
  x2:=0;

  try

    vert1:=false;
    vert2:=false;

    if (FStartPoint.X>FEndPoint.X) then
      begin
        TmpStart1.AssignFrom(FEndPoint);
        TmpEnd1.AssignFrom(FStartPoint);
      end
    else
      begin
        TmpStart1.AssignFrom(FStartPoint);
        TmpEnd1.AssignFrom(FEndPoint);
      end;

    dx:=TmpEnd1.X-TmpStart1.X;
    dy:=TmpEnd1.Y-TmpStart1.Y;

    if (dx<>0) then
      begin
        k1:=dy/dx; // y = kx + m, m = y - kx
        m1:=TmpEnd1.Y-k1*TmpEnd1.X;
      end
    else
      begin
        x1:=TmpEnd1.X;
        vert1:=true;
      end;

    if (L.StartPoint.X>L.EndPoint.X) then
      begin
        TmpStart2.AssignFrom(L.EndPoint);
        TmpEnd2.AssignFrom(L.StartPoint);
      end
    else
      begin
        TmpStart2.AssignFrom(L.StartPoint);
        TmpEnd2.AssignFrom(L.EndPoint);
      end;

    dx:=TmpEnd2.X-TmpStart2.X;
    dy:=TmpEnd2.Y-TmpStart2.Y;

    if (dx<>0) then
      begin
        k2:=dy/dx;
        m2:=TmpStart2.Y-k2*TmpStart2.X;
      end
    else
      begin
        x2:=TmpEnd2.X;
        vert2:=true;
      end;

    if (k1-k2=0) then
    begin
      Result:=nil;
      Done:=true;
    end;

    if (not vert1) and (not vert2) and (not Done) then
    begin
      //x*k1+m1 = x*k2+m2, x*(k1-k2)=m2-m1
      x:=(m2-m1)/(k1-k2);
      y:=k1*x+m1;

      if (x>=TmpStart1.X) and (x<=TmpEnd1.X) then
        if (x>=TmpStart2.X) and (x<=TmpEnd2.X) then
          begin
            ISect:=TTrianglePoint.Create;
            ISect.X:=x;
            ISect.Y:=y;

            // Calcuate interpolated value

            ltot:=Distance(StartPoint, EndPoint);
            lp:=Distance(StartPoint, Isect);
            v:=StartPoint.Value + (EndPoint.Value-StartPoint.Value)*lp/ltot;
            Isect.Value:=v;

            Result:=ISect;
            Done:=true;
          end
        else
          begin
            Result:=nil;
            Done:=true;
          end;
    end;

    if (vert1) and (vert2) and (not Done) then
    begin
      Result:=nil;
      Done:=true;
    end;

    if (vert1) and (not vert2) and (not Done) then
    begin
      x:=x1;
      y:=k2*x + m2;

      if (TmpStart1.Y>TmpEnd1.Y) then
      begin
        TmpPoint.AssignFrom(TmpStart1);
        TmpStart1.AssignFrom(TmpEnd1);
        TmpEnd1.AssignFrom(TmpPoint);
      end;

      if (TmpStart2.Y>TmpEnd2.Y) then
      begin
        TmpPoint.AssignFrom(TmpStart2);
        TmpStart2.AssignFrom(TmpEnd2);
        TmpEnd2.AssignFrom(TmpPoint);
      end;

      if (y>=TmpStart1.Y) and (y<=TmpEnd1.Y) then
        if (y>=TmpStart2.Y) and (y<=TmpEnd2.Y) then
          begin
            ISect:=TTrianglePoint.Create;
            ISect.X:=x;
            ISect.Y:=y;

            // Calcuate interpolated value

            ltot:=Distance(StartPoint, EndPoint);
            lp:=Distance(StartPoint, Isect);
            v:=StartPoint.Value + (EndPoint.Value-StartPoint.Value)*lp/ltot;
            Isect.Value:=v;

            Result:=ISect;
          end
        else
          begin
            Result:=nil;
          end;

      Done:=true;
    end;

    if (vert2) and (not vert1) and (not Done) then
    begin
      x:=x2;
      y:=k1*x + m1;

      if (TmpStart1.Y>TmpEnd1.Y) then
      begin
        TmpPoint.AssignFrom(TmpStart1);
        TmpStart1.AssignFrom(TmpEnd1);
        TmpEnd1.AssignFrom(TmpPoint);
      end;

      if (TmpStart2.Y>TmpEnd2.Y) then
      begin
        TmpPoint.AssignFrom(TmpStart2);
        TmpStart2.AssignFrom(TmpEnd2);
        TmpEnd2.AssignFrom(TmpPoint);
      end;

      if (y>=TmpStart1.Y) and (y<=TmpEnd1.Y) then
        if (y>=TmpStart2.Y) and (y<=TmpEnd2.Y) then
          begin
            ISect:=TTrianglePoint.Create;
            ISect.X:=x;
            ISect.Y:=y;

            // Calcuate interpolated value

            ltot:=Distance(StartPoint, EndPoint);
            lp:=Distance(StartPoint, Isect);
            v:=StartPoint.Value + (EndPoint.Value-StartPoint.Value)*lp/ltot;
            Isect.Value:=v;

            Result:=ISect;
          end
        else
          begin
            Result:=nil;
          end;

      //Done:=true;
    end;
  finally
    TmpStart1.Free;
    TmpStart2.Free;
    TmpEnd1.Free;
    TmpEnd2.Free;
    TmpPoint.Free;
  end;
end;


function TTriangleLine.Length: double;
begin
  Result:=sqrt(sqr(FEndPoint.X-FStartPoint.X)+sqr(FEndPoint.Y-FStartPoint.Y));
end;

procedure TTriangleLine.Scale(Factor: double);
var
  MP : TTrianglePoint;

  vx, vy : double;
  l : double;
begin
  MP:=TTrianglePoint.Create;
  MP.X:=(FStartPoint.X + FEndPoint.X) * 0.5;
  MP.Y:=(FStartPoint.Y + FEndPoint.Y) * 0.5;

  l:=Self.Length;

  vx:=(FEndPoint.X-FStartPoint.X)/l;
  vy:=(FEndPoint.Y-FStartPoint.Y)/l;

  FEndPoint.X:=vx*l*0.5*Factor;
  FEndPoint.Y:=vy*l*0.5*Factor;
  FStartPoint.X:=-vx*l*0.5*Factor;
  FStartPoint.Y:=-vy*l*0.5*Factor;
end;

{ TTrianglePolyline }

procedure TTrianglePolyline.Clear;
begin
  FPoints.Clear;
end;

constructor TTrianglePolyline.Create;
begin
  inherited;
  FPoints:=TRefList.Create;
end;

destructor TTrianglePolyline.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TTrianglePolyline.AddPoint(x, y: double);
var
    P : TTrianglePoint;
begin
  P:=TTrianglePoint.Create;
  P.X:=x;
  P.Y:=y;
  FPoints.Add(P);
end;

function TTrianglePolyline.GetPoints(idx: integer): TTrianglePoint;
begin
  if (idx>=0) and (idx<FPoints.Count) then
    begin
      Result:=TTrianglePoint(FPoints.Items[idx]);
    end
  else
    Result:=nil;
end;

function TTrianglePolyline.Interpolate(x : double): double;
var
    i : integer;
    xStart, xEnd : double;
    yStart, yEnd : double;
    x1, x2, y1, y2 : double;
begin
  xStart:=Self.Points[0].X;
  yStart:=Self.Points[0].Y;
  xEnd:=Self.Points[Self.Count-1].X;
  yEnd:=Self.Points[Self.Count-1].Y;

  if x<=xStart then
  begin
    Result:=yStart;
    exit;
  end;

  if x>=xEnd then
  begin
    Result:=yEnd;
  end;

  for i := 0 to Self.Count-2 do
  begin
    x1:=Self.Points[i].X;
    y1:=Self.Points[i].Y;
    x2:=Self.Points[i+1].X;
    y2:=Self.Points[i+1].Y;

    if (x1<=x) and (x<x2) then
    begin
      Result:=y1+(y2-y1)*(x-x1)/(x2-x1);
      exit;
    end;
  end;
end;

function TTrianglePolyline.GetCount: integer;
begin
  Result:=FPoints.Count;
end;

procedure TTrianglePolyline.AssignLine(idx: integer; Line: TTriangleLine);
begin
  if (idx>=0) and (idx<FPoints.Count-1) then
  begin
    Line.StartPoint.X:=TTrianglePoint(FPoints.Items[idx]).X;
    Line.StartPoint.Y:=TTrianglePoint(FPoints.Items[idx]).Y;
    Line.EndPoint.X:=TTrianglePoint(FPoints.Items[idx+1]).X;
    Line.EndPoint.Y:=TTrianglePoint(FPoints.Items[idx+1]).Y;
  end;
end;

end.
