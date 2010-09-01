unit CadMesh;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CadCanvas, Triangle, Math;

type

  TIsoLine = class
  private
    FValue : double;
    FColor : integer;
  public
    constructor Create;
    property Value : double read FValue write FValue;
    property Color : integer read FColor write FColor;
  end;

  TInterval = class
  private
    FColor : integer;
  public
    constructor Create;
    property Color : integer read FColor write FColor;
  end;

  TIsoLines = class
  private
    FIsoLines : TList;
    FIntervals : TList;
    FSize: integer;
    FMax : double;
    FMin : double;
    FAutoUpdate: boolean;
    procedure SetSize(const Value: integer);
    function GetValue(idx : integer) : double;
    procedure SetValue(idx : integer; Value : double);
    procedure SetMax(const Value: double);
    procedure SetMin(const Value: double);
    procedure UpdateLines;
    function GetColor(idx: integer): integer;
    procedure SetColor(idx: integer; const Value: integer);
    function GetIntervalColor(idx: integer): integer;
    procedure SetIntervalColor(idx: integer; const Value: integer);
    function GetIntervalSize: integer;
    procedure SetAutoUpdate(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function InsideInterval(idx : integer; var P : TTrianglePoint) : boolean;

    property Values[index : integer] : double read GetValue write SetValue;
    property Colors[index : integer] : integer read GetColor write SetColor;
    property IntervalColors[index : integer] : integer read GetIntervalColor write SetIntervalColor;
    property Size : integer read FSize write SetSize;
    property IntervalSize : integer read GetIntervalSize;

    property Max : double read FMax write SetMax;
    property Min : double read FMin write SetMin;
    property AutoUpdate : boolean read FAutoUpdate write SetAutoUpdate;
  end;

  TMeshType = (mtTriangles, mtIsolines, mtFilled);

  TCadMesh = class(TComponent)
  private
    FCadCanvas: TCadCanvas;
    FTriangle: TTriangle;
    FMeshType : TMeshType;
    FIsoLines : TIsoLines;
    FTriangleLayer : TCadLayer;
    FIntervalLayers : TList;
    FIsoLayers : TList;

    FDebugOutput : TMemo;
    FDryRun: boolean;

    { Private declarations }

    procedure CreateIsoLines;
    procedure CreateTriangles;
    procedure CreateFilledMesh;
    procedure Triangulate(PointList : TRefList);

    //function AngleCompare(Item1, Item2 : pointer) : integer;
    //function FindClosest(Point : TTrianglePoint; PointList : TList) : TTrianglePoint;
    //function FindNextClosest(Point : TTrianglePoint; PointList : TList) : TTrianglePoint;
    function FindLeftMost(Point : TTrianglePoint; PointList : TList) : TTrianglePoint;
    procedure SetDryRun(const Value: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute;
    procedure Clear;

    procedure PrintLn(txt : string);

    property IsoLines : TIsoLines read FIsoLines;
    property TriangleLayer : TCadLayer read FTriangleLayer;
    property IntervalLayers : TList read FIntervalLayers;
  published
    { Published declarations }
    property CadCavnas : TCadCanvas read FCadCanvas write FCadCanvas;
    property Triangle : TTriangle read FTriangle write FTriangle;
    property MeshType : TMeshType read FMeshType write FMeshType;
    property DebugOutput : TMemo read FDebugOutput write FDebugOutput;
    property DryRun : boolean read FDryRun write SetDryRun;
  end;

procedure Register;

implementation

var
  GPivotPoint : TTrianglePoint;
  GClosestPoint : TTrianglePoint;
  GCadMesh : TCadMesh;

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadMesh]);
end;

{ TCadMesh }

constructor TCadMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCadCanvas:=nil;
  FTriangle:=nil;
  FMeshType:=mtTriangles;
  FIsoLines:=TIsoLines.Create;

  // Layers used

  FTriangleLayer:=nil;
  FIntervalLayers:=TList.Create;
  FIsoLayers:=TList.Create;

  FDebugOutput:=nil;

  FDryRun:=false;
end;

procedure TCadMesh.CreateFilledMesh;
var
    i, j : integer;
    TriElement : TTriangleElement;
    P1, P2, P3, P4 : TTrianglePoint;

    Layer : TCadLayer;

    Intervals : TList;
    PointList : TRefList;

    TempList : TList;

    TP1, TP2, TP3 : TTrianglePoint;

begin

  Intervals:=TList.Create;
  TempList:=TList.Create;

  for i:=0 to FIsoLines.IntervalSize-1 do
  begin
    PointList:=TRefList.Create;
    Intervals.Add(PointList);
  end;

  for i:=0 to FIsoLines.IntervalSize-1 do
  begin
    Layer:=FCadCanvas.AddLayer(format('ZMINTERVAL_%d',[i]));
    Layer.Color:=FIsoLines.IntervalColors[i];
    FIntervalLayers.Add(Layer);
  end;

  for i:=0 to FTriangle.ElementCount-1 do
  begin
    TriElement:=FTriangle.Elements[i];

    // Clear interval lists

    for j:=0 to FIsoLines.IntervalSize-1 do
    begin
      PointList:=Intervals.Items[j];
      PointList.Clear;
    end;

    // Create list of points sorted for each interval

    for j:=0 to FIsoLines.IntervalSize-1 do
    //for j:=0 to 0 do
    begin

      PointList:=Intervals.Items[j];

      TP1:=TriElement.Point[1];
      TP2:=TriElement.Point[2];
      TP3:=TriElement.Point[3];

      // First we add triangle nodes to list

      if FIsoLines.InsideInterval(j, TP1) then
          PointList.Add(TriElement.Point[1]);

      if FIsoLines.InsideInterval(j, TP2) then
          PointList.Add(TriElement.Point[2]);

      if FIsoLines.InsideInterval(j, TP3) then
          PointList.Add(TriElement.Point[3]);

      //if (PointList.Count=3) then PointList.Clear; // REMOVE!

      if (PointList.Count<3) then
      begin

        P1:=TTrianglePoint.Create;
        P2:=TTrianglePoint.Create;

        if TriElement.GetIntersectionPoints(FIsoLines.Values[j], P1, P2) then
          begin
            if (FIsoLines.InsideInterval(j, P1)) then
              PointList.Add(P1);

            if (FIsoLines.InsideInterval(j, P2)) then
              PointList.Add(P2);
          end;

        if not P1.IsReferenced then
          P1.Free;

        if not P2.IsReferenced then
          P2.Free;

        P3:=TTrianglePoint.Create;
        P4:=TTrianglePoint.Create; // Memory leak

        if (j>0) then
          begin
            if TriElement.GetIntersectionPoints(FIsoLines.Values[j-1], P3, P4) then
              begin
                if (FIsoLines.InsideInterval(j, P3)) then
                  PointList.Add(P3);

                if (FIsoLines.InsideInterval(j, P4)) then
                  PointList.Add(P4);
              end;
          end;

        if not P3.IsReferenced then
          P3.Free;

        if not P4.IsReferenced then
          P4.Free;

      end;
    end;

    // Create meshes for each interval

    for j:=0 to FIsoLines.IntervalSize-1 do
    begin
      PointList:=Intervals.Items[j];
      FCadCanvas.CurrentLayer:=FIntervalLayers.Items[j];
      if (PointList.Count<>0) then
        Triangulate(PointList);
    end;
  end;

  for i:=0 to Intervals.Count-1 do
  begin
    TRefList(Intervals.Items[i]).Free;
  end;

  Intervals.Free;
  TempList.Free;
end;

//function TCadMesh.FindClosest(Point: TTrianglePoint;
//  PointList: TList): TTrianglePoint;
//var
//    i : integer;
//    ClosestPoint : TTrianglePoint;
//    P : TTrianglePoint;
//    lmin,l : double;
//begin
//  lmin:=1e300;
//  ClosestPoint:=nil;
//  for i:=0 to PointList.Count-1 do
//  begin
//    P:=PointList.Items[i];
//    if (P<>Point) then
//    begin
//      l:=Distance(P,Point);
//      if (l<lmin) and (l<>0) then
//      begin
//        ClosestPoint:=P;
//        lmin:=l;
//      end;
//    end;
//  end;
//  Result:=ClosestPoint;
//end;
//
//function TCadMesh.FindNextClosest(Point: TTrianglePoint;
//  PointList: TList): TTrianglePoint;
//var
//    i : integer;
//    ClosestPoint : TTrianglePoint;
//    NextClosestPoint : TTrianglePoint;
//    P : TTrianglePoint;
//    lmin,l : double;
//begin
//  lmin:=1e300;
//  ClosestPoint:=nil;
//  NextClosestPoint:=nil;
//  for i:=0 to PointList.Count-1 do
//  begin
//    P:=PointList.Items[i];
//    if (P<>Point) then
//    begin
//      l:=Distance(P,Point);
//      if l<lmin then
//      begin
//        NextClosestPoint:=ClosestPoint;
//        ClosestPoint:=P;
//        lmin:=l;
//      end;
//    end;
//  end;
//  Result:=NextClosestPoint;
//end;

function AngleCompare(Item1, Item2 : pointer) : integer;
var
    P1, P2 : TTrianglePoint;
    angle1, angle2 : double;
    refAngle : double;
begin
  P1:=Item1;
  P2:=Item2;

  refAngle:=Arctan2(GClosestPoint.Y-GPivotPoint.Y, GClosestPoint.X-GPivotPoint.X)+0.0001;
  if refAngle<0 then
    refAngle:=2*pi+refAngle;

  angle1:=Arctan2(P1.Y-GPivotPoint.Y, P1.X-GPivotPoint.X);
  if angle1<0 then
    angle1:=2*pi+angle1;

  angle2:=Arctan2(P2.Y-GPivotPoint.Y, P2.X-GPivotPoint.X);
  if angle2<0 then
    angle2:=2*pi+angle2;

  if refAngle-angle1<0 then
    angle1:=refAngle+(2*pi-angle1)
  else
    angle1:=refAngle-angle1;

  if refAngle-angle2<0 then
    angle2:=refAngle+(2*pi-angle2)
  else
    angle2:=refAngle-angle2;

  //GCadMesh.PrintLn(format('%g %g',[angle1, angle2]));

  if (angle1<angle2) then
    Result:=1
  else if (angle1>angle2) then
    Result:=-1
  else
    Result:=0;
end;

function TCadMesh.FindLeftMost(Point: TTrianglePoint;
  PointList: TList): TTrianglePoint;
var
    Q : TCadVector;
    V : TCadVector;
    P1 : TTrianglePoint;
    P2 : TTrianglePoint;
    A : double;
    LeftTurn : boolean;
    i,j : integer;
begin
  Q:=TCadVector.Create;
  V:=TCadVector.Create;
  for i:=0 to PointList.Count-1 do
  begin
    P1:=PointList.Items[i];
    Q.X:=P1.X-Point.X;
    Q.Y:=P1.Y-Point.Y;
    LeftTurn:=false;
    for j:=0 to PointList.Count-1 do
    begin
      P2:=PointList.Items[j];
      V.X:=P2.X-Point.X;
      V.Y:=P2.Y-Point.Y;
      A:=Q.CrossProduct(V);
      if A>0 then
        LeftTurn:=true;
    end;
    if not LeftTurn then
    begin
      Q.Free;
      V.Free;
      Result:=P1;
      exit;
    end;
  end;
  Q.Free;
  V.Free;
  Result:=nil;
end;

procedure TCadMesh.Triangulate(PointList: TRefList);
var
    i : integer;
    P0 : TTrianglePoint;
    P1 : TTrianglePoint;
    P2 : TTrianglePoint;
    TempList : TList;
begin

  //OutputDebugString(PWideChar(IntToStr(PointList.Count)));

  if (PointList.Count=3) then
  begin
    P0:=TTrianglePoint(PointList.Items[0]);
    P1:=TTrianglePoint(PointList.Items[1]);
    P2:=TTrianglePoint(PointList.Items[2]);
    FCadCanvas.Solid3(P0.X, P0.Y, P1.X, P1.Y, P2.X, P2.Y);
    exit;
  end;

  if (PointList.Count=4) then
  begin
    P0:=TTrianglePoint(PointList.Items[0]);
    P1:=TTrianglePoint(PointList.Items[1]);
    P2:=TTrianglePoint(PointList.Items[2]);
    FCadCanvas.Solid3(P0.X, P0.Y, P1.X, P1.Y, P2.X, P2.Y);
    P0:=TTrianglePoint(PointList.Items[3]);

    // Sometime a strange point set is generated, this
    // if statement will skip it.

    //if (P0.X<>0) and (P0.Y<>0) then
      FCadCanvas.Solid3(P0.X, P0.Y, P1.X, P1.Y, P2.X, P2.Y);
    exit;
  end;

  if (PointList.Count<=2) then
    exit;


  TempList:=TList.Create;

  for i:=1 to PointList.Count-1 do
  begin
    P0:=TTrianglePoint(PointList.Items[i]);
    TempList.Add(P0);
  end;

  GPivotPoint:=TTrianglePoint(PointList.Items[0]);
  GClosestPoint:=FindLeftMost(GPivotPoint, TempList);
  GCadMesh:=Self;

  TempList.Sort(AngleCompare);
  (*
  for i:=0 to TempList.Count-1 do
  begin
    P0:=TempList.Items[i];
    FCadCanvas.TextHeight:=0.5;
    FCadCanvas.TextOut(P0.X, P0.Y, IntToStr(i));
  end;

  FCadCanvas.TextOut(GClosestPoint.X+0.5, GClosestPoint.Y+0.5, 'cl');
  *)

  P0:=GPivotPoint;

  for i:=0 to TempList.Count-2 do
  begin
    P1:=TempList.Items[i];
    P2:=TempList.Items[i+1];
    FCadCanvas.Solid3(P0.X, P0.Y, P1.X, P1.Y, P2.X, P2.Y);
  end;


  TempList.Free;
end;

procedure TCadMesh.CreateIsoLines;
var
    i, j : integer;
    TriElement : TTriangleElement;
    P1, P2 : TTrianglePoint;

    Layer : TCadLayer;
begin
  P1:=TTrianglePoint.Create;
  P2:=TTrianglePoint.Create;

  for i:=0 to FIsoLines.Size-1 do
  begin
    Layer:=FCadCanvas.AddLayer(format('ZMISO_%d',[i]));
    Layer.Color:=FIsoLines.Colors[i];
    FIsoLayers.Add(Layer);
  end;

  for i:=0 to FTriangle.ElementCount-1 do
  begin
    TriElement:=FTriangle.Elements[i];
    for j:=0 to FIsoLines.Size-1 do
    begin
      FCadCanvas.CurrentLayer:=FIsoLayers[j];
      if TriElement.Point[1].Value=TriElement.Point[2].Value then
      begin
        if TriElement.Point[1].Value=FIsoLines.Values[j] then
        begin
          FCadCanvas.MoveTo(TriElement.Point[1].X, TriElement.Point[1].Y);
          FCadCanvas.LineTo(TriElement.Point[2].X, TriElement.Point[2].Y);
        end;
      end;
      if TriElement.Point[1].Value=TriElement.Point[3].Value then
      begin
        if TriElement.Point[1].Value=FIsoLines.Values[j] then
        begin
          FCadCanvas.MoveTo(TriElement.Point[1].X, TriElement.Point[1].Y);
          FCadCanvas.LineTo(TriElement.Point[3].X, TriElement.Point[3].Y);
        end;
      end;
      if TriElement.Point[2].Value=TriElement.Point[3].Value then
      begin
        if TriElement.Point[2].Value=FIsoLines.Values[j] then
        begin
          FCadCanvas.MoveTo(TriElement.Point[2].X, TriElement.Point[2].Y);
          FCadCanvas.LineTo(TriElement.Point[3].X, TriElement.Point[3].Y);
        end;
      end;
      if TriElement.GetIntersectionPoints(FIsoLines.Values[j], P1, P2) then
      begin
        FCadCanvas.MoveTo(P1.X, P1.Y);
        FCadCanvas.LineTo(P2.X, P2.Y);
      end;
    end;
  end;

  P1.Free;
  P2.Free;
end;

procedure TCadMesh.CreateTriangles;
var
    i : integer;
    TriElement : TTriangleElement;
begin
  FCadCanvas.CurrentLayer:=FCadCanvas.AddLayer('ZMTRIANGLES');
  FTriangleLayer:=FCadCanvas.CurrentLayer;
  for i:=0 to FTriangle.ElementCount-1 do
  begin
    TriElement:=FTriangle.Elements[i];
    FCadCanvas.BeginPolyLine;
    FCadCanvas.AddPoint(TriElement.Point[1].X, TriElement.Point[1].Y);
    FCadCanvas.AddPoint(TriElement.Point[2].X, TriElement.Point[2].Y);
    FCadCanvas.AddPoint(TriElement.Point[3].X, TriElement.Point[3].Y);
    FCadCanvas.AddPoint(TriElement.Point[1].X, TriElement.Point[1].Y);
    FCadCanvas.EndPolyLine;
  end;
end;

destructor TCadMesh.Destroy;
begin
  FIsoLayers.Free;
  FIntervalLayers.Free;
  FIsoLines.Free;
  inherited;
end;

procedure TCadMesh.Execute;
begin
  if assigned(FCadCanvas) and assigned(FTriangle) then
  begin

    Self.Clear;
    
    case FMeshType of
      mtTriangles : begin
        CreateTriangles;
      end;
      mtIsolines : begin
        CreateIsoLines;
      end;
      mtFilled : begin
        CreateFilledMesh;
      end;
    end;
  end;
end;

procedure TCadMesh.PrintLn(txt: string);
begin
  if assigned(DebugOutput) then
  begin
    DebugOutput.Lines.Add(txt);
  end;
end;


procedure TCadMesh.Clear;
var
    i : integer;
    Layer : TCadLayer;
begin
  if assigned(FCadCanvas) then
  begin

    // Delete layers used for filled mesh

    for i:=0 to FIntervalLayers.Count-1 do
    begin
      Layer:=TCadLayer(FIntervalLayers[i]);
      FCadCanvas.DeleteLayer(Layer);
    end;

    // Delete layers used for isolines

    for i:=0 to FIsoLayers.Count-1 do
    begin
      Layer:=TCadLayer(FIsoLayers[i]);
      FCadCanvas.DeleteLayer(Layer);
    end;

    FIntervalLayers.Clear;
    FIsoLayers.Clear;
  end;
end;

procedure TCadMesh.SetDryRun(const Value: boolean);
begin
  FDryRun := Value;
end;

{ TIsoLines }

procedure TIsoLines.Clear;
var
    IsoLine : TIsoLine;
    Interval : TInterval;
    i : integer;
begin
  for i:=0 to FIsoLines.Count-1 do
  begin
    IsoLine:=FIsoLines.Items[i];
    IsoLine.Free
  end;

  for i:=0 to FIntervals.Count-1 do
  begin
    Interval:=FIntervals.Items[i];
    Interval.Free
  end;

  FIsoLines.Clear;
  FIntervals.Clear;

  FSize:=0;
end;

constructor TIsoLines.Create;
begin
  inherited Create;
  FIsoLines:=TList.Create;
  FIntervals:=TList.Create;
  FMax:=1.0;
  FMin:=-1.0;
  FAutoUpdate:=true;
end;

destructor TIsoLines.Destroy;
begin
  Clear;
  FIntervals.Free;
  FIsoLines.Free;
  inherited;
end;

function TIsoLines.GetColor(idx: integer): integer;
var
    IsoLine : TIsoLine;
begin
  if (idx>=0) and (idx<FSize) then
    begin
      IsoLine:=FIsoLines.Items[idx];
      Result:=IsoLine.Color;
    end
  else
    Result:=256;
end;

function TIsoLines.GetIntervalColor(idx: integer): integer;
var
    Interval : TInterval;
begin
  if (idx>=0) and (idx<=FSize) then
    begin
      Interval:=FIntervals.Items[idx];
      Result:=Interval.Color;
    end
  else
    Result:=256;
end;

function TIsoLines.GetIntervalSize: integer;
begin
  Result:=FSize+1;
end;

function TIsoLines.GetValue(idx: integer): double;
var
    IsoLine : TIsoLine;
begin
  if (idx>=0) and (idx<FSize) then
    begin
      IsoLine:=FIsoLines.Items[idx];
      Result:=IsoLine.Value;
    end
  else
    Result:=0;
end;

function TIsoLines.InsideInterval(idx : integer; var P : TTrianglePoint) : boolean;
var
    Iso0 : TIsoLine;
    Iso1 : TIsoLine;
    value    : double;
    v0       : double;
    v1       : double;
begin

  //          0 | 1 | 2 | 3 | n | n+1
  // -INF ------o-->o-->o-->o-->o------- INF

  //value := P.Value+1e-30;
  value := P.Value;
  if (idx=0) then
    begin
      Iso1:=FIsoLines.Items[idx];
      v1:=Iso1.Value;
      if (value<=v1) then
        begin
          Result:=true;
          exit;
        end
      else
        begin
          Result:=false;
          exit;
        end;
    end
  else
    begin
      if (idx=FIsoLines.Count) then
        begin
          Iso0:=FIsoLines.Items[idx-1];
          v0:=Iso0.Value;
          //if (value>=v0) then
          if (value>=v0) then
            begin
              Result:=true;
              exit;
            end
          else
            begin
              Result:=false;
              exit;
            end;
        end
      else
        begin
          Iso0:=FIsoLines.Items[idx-1];
          Iso1:=FIsoLines.Items[idx];
          v0:=Iso0.Value;
          v1:=Iso1.Value;
          //if (value>=v0) and (value<=v1) then
          if (value>=v0) and (value<=v1) then
            begin
              Result:=true;
              exit;
            end
          else
            begin
              Result:=false;
              exit;
            end;
        end;
    end;
end;

procedure TIsoLines.SetAutoUpdate(const Value: boolean);
begin
  FAutoUpdate := Value;
end;

procedure TIsoLines.SetColor(idx: integer; const Value: integer);
var
    IsoLine : TIsoLine;
begin
  if (idx>=0) and (idx<FSize) then
  begin
    IsoLine:=FIsoLines.Items[idx];
    IsoLine.Color:=Value;
  end;
end;

procedure TIsoLines.SetIntervalColor(idx: integer; const Value: integer);
var
    Interval : TInterval;
begin
  if (idx>=0) and (idx<FSize+1) then
  begin
    Interval:=FIntervals.Items[idx];
    Interval.Color:=Value;
  end;
end;

procedure TIsoLines.SetMax(const Value: double);
begin
  FMax := Value;
  if FAutoUpdate then
    UpdateLines;
end;

procedure TIsoLines.SetMin(const Value: double);
begin
  FMin := Value;
  if FAutoUpdate then
    UpdateLines;
end;

procedure TIsoLines.SetSize(const Value: integer);
var
    IsoLine : TIsoLine;
    Interval : TInterval;
    i : integer;
begin
  Clear;

  for i:=0 to Value-1 do
  begin
    IsoLine:=TIsoLine.Create;
    IsoLine.Value:=0;
    IsoLine.Color:=i+1;
    FIsoLines.Add(IsoLine);
  end;

  for i:=0 to Value do
  begin
    Interval:=TInterval.Create;
    Interval.Color:=i+1;
    FIntervals.Add(Interval);
  end;

  FSize := Value;
end;

procedure TIsoLines.SetValue(idx: integer; Value: double);
var
    IsoLine : TIsoLine;
begin
  if (idx>=0) and (idx<FSize) then
  begin
    IsoLine:=FIsoLines.Items[idx];
    IsoLine.Value:=Value;
  end;
end;

procedure TIsoLines.UpdateLines;
var
    IsoLine : TIsoLine;
    i : integer;
    delta : double;
    value : double;
begin
  delta:=(FMax-FMin)/(FSize-1);
  value:=FMin;
  for i:=0 to FIsoLines.Count-1 do
  begin
    IsoLine:=FIsoLines.Items[i];
    IsoLine.Value:=value;
    value:=value+delta;
  end;
end;

{ TIsoLine }

constructor TIsoLine.Create;
begin
  FColor:=256;
  FValue:=0.0;
end;

{ TInterval }

constructor TInterval.Create;
begin
  inherited Create;
  FColor:=256;
end;

end.
