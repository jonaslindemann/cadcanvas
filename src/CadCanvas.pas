{
@abstract(Component for drawing CAD like elements on virtual canvas.)
This unit contains the CadCanvas component for supporting a CAD like drawing
surface. The unit also contains the classes implementing the supported CAD
geomtry primitives.
}
unit CadCanvas;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, Contnrs;

const

  CadCanvasMajorVersion = 1;
  CadCanvasMinorVersion = 3;
  CadCanvasRelease      = 3;

  ColorByLayer = 256;

type

  { Base class for all CAD geometric elements }
  TCadElement = class
  private
    FColor : integer;
    function GetColorByLayer: boolean;
  public
    constructor Create;

    { Color of element. 1-255, 256 = color by layer }
    property Color : integer read FColor write FColor;

    { Returns true if color by layer }
    property ColorByLayer : boolean read GetColorByLayer;
  end;

  { Basic vector class }
  TCadVector = class(TCadElement)
  protected
    FX : double;
    FY : double;
  private
    function GetLength: double;
  public
    { Computes the scalar product with vector v. }
    function ScalarProduct(v : TCadVector) : double;

    { Computes the cross product with vector v. }
    function CrossProduct(v : TCadVector) : double;

    { Computes the angle between two vectors. }
    function Angle(v : TCadVector) : double;

    { Returns true if vector equal to v. }
    function EqualTo(v : TCadVector) : boolean;

    { Assigns the vector from the input vector v. }
    procedure Assign(v : TCadVector);

    { Returns the length of the vector. }
    property Length : double read GetLength;

    { Vector x component. }
    property X : double read FX write FX;

    { Vector y component. }
    property Y : double read FY write FY;
  end;

  { Alias for the TCadVector }
  TCadPoint = class(TCadVector)
  protected
  public
  end;


  { Base class for line elements }
  TCadLine = class(TCadElement)
  private

  public
    constructor Create;
  end;

  { Simple line class }
  TCadSimpleLine = class(TCadLine)
  private
    FStartPoint : TCadPoint;
    FEndPoint : TCadPoint;
  public
    constructor Create;
    destructor Destroy; override;

    { Computes the intersection point with the line L. Returns an @link(TCadPoint)
      instance if succesful, otherwise nil is returned. The TCadPoint
      instance is not freed by the class. }
    function Intersection(L : TCadSimpleLine) : TCadPoint;

    { Start point of line }
    property StartPoint : TCadPoint read FStartPoint;

    { End point of line }
    property EndPoint : TCadPoint read FEndPoint;
  end;

  { Polyline class }
  TCadPolyLine = class(TCadLine)
  private
    FPoints : TObjectList;

    function GetCount : integer;
    function GetPoint(idx : integer) : TCadPoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    { Adds a point to the polyline. Point is owned by polyline. }
    procedure Add(Point : TCadPoint); overload;

    { Adds a point to the polyline. }
    procedure Add(x, y : double); overload;

    { Returns the number of point in polyline. }
    property Count : integer read GetCount;

    { Return point at index, first = 0, last = Count-1 }
    property Points[index : integer] : TCadPoint read GetPoint;
  end;

  { Polygon class (not yet implemented). }
  TCadPolygon = class(TCadElement)
  private
    FPoints : TObjectList;

    function GetCount : integer;
    function GetPoint(idx : integer) : TCadPoint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(Point : TCadPoint);

    property Count : integer read GetCount;
    property Points[index : integer] : TCadPoint read GetPoint;
  end;

  { Solid class (4 sided polygon) }
  TCadSolid = class(TCadElement)
  private
    FPoints : TObjectList;

    function GetPoint(idx : integer) : TCadPoint;
    function GetCount: integer;
    function GetIsSolid3: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Returns point at index, first = 0, last = 3. Point references
      are read-only. }
    property Points[index : integer] : TCadPoint read GetPoint;
    property Count : integer read GetCount;
    property IsSolid3 : boolean read GetIsSolid3;
  end;

  { Text class }
  TCadText = class(TCadElement)
  private
    FPosition : TCadPoint;
    FTextString : string;
    FHeight : double;
    FRotationAngle : double;
  public
    constructor Create;
    destructor Destroy; override;

    { Text to be output }
    property TextString : string read FTextString write FTextString;

    { Text height in units }
    property Height : double read FHeight write FHeight;

    { Text insertion point, lower left corner }
    property Position : TCadPoint read FPosition;

    { Text rotation counter clockwise in degrees }
    property RotationAngle : double read FRotationAngle write FRotationAngle;
  end;

  { Layer class for containing a set of drawing elements with common
    properties. Elements are added by CadCanvas. }
  TCadLayer = class
  private
    FElements : TObjectList;
    FLayerName : string;
    FColor : integer;
    FVisible : boolean;

    function GetCount: integer;
    function GetElement(idx: integer): TCadElement;
  public
    constructor Create;
    destructor Destroy; override;

    { Clears all element in the layer }
    procedure Clear;

    { Adds an element to the layer }
    procedure Add(Element : TCadElement);

    { Returns the number of elements in the layer }
    property Count : integer read GetCount;

    { Returns element at index, first = 0, last = Count-1 }
    property Elements[index : integer] : TCadElement read GetElement;

    { Layer name }
    property Name : string read FLayerName write FLayerName;

    { Layer color, 1-255 }
    property Color : integer read FColor write FColor;

    { Layer visibility. If set to false, the layer will not be rendered
      by a CadCanvas component. }
    property Visible : boolean read FVisible write FVisible;
  end;

  { Text justification constants. For use with @link(TextJustifyX) and @link(TextJustifyY) properties. }
  TCadTextJustify = (tjLeft, tjRight, tjCenter, tjTop, tjBottom);

  { Non visual component for managing a CAD drawing surface. The CadCanvas
    component connects to the output components CadDxfDevice and CadWinDevice. }
  TCadCanvas = class(TComponent)
  private
    { Private declarations }
    FLayers : TObjectList;

    FCurrentLayer : TCadLayer;
    FCurrentPolyLine : TCadPolyLine;
    FCurrentPolyGon : TCadPolyGon;

    FCurrentColor : integer;

    FStartPoint : TCadPoint;
    FEndPoint : TCadPoint;

    FModelMax : TCadPoint;
    FModelMin : TCadPoint;
    FTextHeight: double;
    FTextRotation : double;
    FTextTweakFactor : double;
    FTextJustifyX : TCadTextJustify;
    FTextJustifyY : TCadTextJustify;

    function GetCadLayer(idx: integer): TCadLayer;
    procedure UpdateMaxMin(Point : TCadPoint);
    function GetLayerCount: integer;
    procedure SetCurrentLayer(const Layer : TCadLayer);

    function IsValidLayer(const Layer : TCadLayer) : boolean;
    procedure SetTextHeight(const Value: double);
    procedure SetTextRotation(const Value: double);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    { Returns the index of the @code(Name) layer. }
    function GetLayerIndex(Name : string) : integer;

    { Returns the layer instance of a layer named @code(Name). If not found @nil is
      returned. }
    function GetLayerWithName(Name : string) : TCadLayer;

    { Resets the text justify of the @link(TextOut) method. }
    procedure ResetTextJustify;

    { Reset max and min calculation settings }
    procedure ResetMaxMin;

    { Clear all elements and layers in the CAD drawing surface }
    procedure Clear;

    { Creates and adds a new layer to the canvas }
    function AddLayer(Name : string) : TCadLayer;

    { Delete the layer at @code(idx). }
    procedure DeleteLayerIndex(idx : integer);

    { Delete the @code(Layer). }
    procedure DeleteLayer(Layer : TCadLayer);

    { Adds an @code(Element) instance to the current layer. }
    procedure AddElement(Element : TCadElement);

    { Moves the invisible cursor to given position }
    procedure MoveTo(x, y : double);

    { Creates line from the cursor position to the new position. The cursor
      is moved to the new position. }
    procedure LineTo(x, y : double);

    { Starts a polyline }
    procedure BeginPolyLine;

    { Adds a point to the current polyline }
    procedure AddPoint(x, y : double);

    { Adds points from given polyline. }
    procedure AddFromPolyline(PolyLine : TCadPolyline);

    { Adds points from given polygon. }
    procedure AddFromPolyGon(PolyGon : TCadPolyGon);

    { Creats a rectangle consisting of line elements (@link(TCadLine)). }
    procedure Rectangle(x1, y1, x2, y2 : double);

    { Finishes the current polyline }
    procedure EndPolyLine;

    { Begins a new polygon. }
    procedure BeginPolyGon;

    { Ends current polygon. }
    procedure EndPolyGon;

    { Draws a text a the specified position }
    procedure TextOut(x, y : double; Text : string);

    { Returns the width of the @code(TextString) string. }
    function TextSizeX(TextString : string) : double;

    { Returns the height of the @code(TextString) string. }
    function TextSizeY(TextString : string) : double;

    { Draws a point at the specified position }
    procedure SimplePoint(x, y : double);

    { Draws 4-sided solid }
    procedure Solid4(x1, y1, x2, y2, x3, y3, x4, y4 : double);

    { Draws a 3-sided solid }
    procedure Solid3(x1, y1, x2, y2, x3, y3 : double);

    { Returns the layer at index, first = 0, last = LayerCount-1 }
    property Layers[index : integer] : TCadLayer read GetCadLayer;

    { Returns the number of layers }
    property LayerCount : integer read GetLayerCount;

    { Sets/gets the current layer }
    property CurrentLayer : TCadLayer read FCurrentLayer write SetCurrentLayer;

    { Sets/gets the current color }
    property CurrentColor : integer read FCurrentColor write FCurrentColor;

    { Returns the max position of the current drawing }
    property ModelMax : TCadPoint read FModelMax write FModelMax;

    { Returns the min position of the current drawing }
    property ModelMin : TCadPoint read FModelMin write FModelMin;

    { Sets/gets the current text height }
    property TextHeight : double read FTextHeight write SetTextHeight;

    { Sets/gets the current text rotation }
    property TextRotation : double read FTextRotation write SetTextRotation;

    { Correction factor used to calculate the width of text strings. }
    property TextTweakFactor : double read FTextTweakFactor write FTextTweakFactor;

    { Set the X justify options for text output }
    property TextJustifyX : TCadTextJustify read FTextJustifyX write FTextJustifyX;

    { Set the Y justify options for text output. }
    property TextJustifyY : TCadTextJustify read FTextJustifyY write FTextJustifyY;

  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadCanvas]);
end;

{ TCadCanvas }

function TCadCanvas.AddLayer(Name: string) : TCadLayer;
var
    CadLayer : TCadLayer;
begin
  CadLayer:=TCadLayer.Create;
  CadLayer.Name:=Name;
  FLayers.Add(CadLayer);
  Result:=CadLayer;
end;

procedure TCadCanvas.AddPoint(x, y: double);
var
    CadPoint : TCadPoint;
begin
  if assigned(FCurrentPolyLine) then
  begin
    CadPoint:=TCadPoint.Create;
    CadPoint.X:=x;
    CadPoint.Y:=y;

    FCurrentPolyLine.Add(CadPoint);
    UpdateMaxMin(CadPoint);
  end;

  if assigned(FCurrentPolyGon) then
  begin
    CadPoint:=TCadPoint.Create;
    CadPoint.X:=x;
    CadPoint.Y:=y;

    FCurrentPolyGon.Add(CadPoint);
    UpdateMaxMin(CadPoint);
  end;
end;

procedure TCadCanvas.AddFromPolyline(PolyLine: TCadPolyline);
var
    i : integer;
    P : TCadPoint;
begin
  if assigned(FCurrentPolyline) then
  begin
    for i:=0 to PolyLine.Count-1 do
    begin
      P:=PolyLine.Points[i];
      Self.AddPoint(P.X, P.Y);
    end;
  end;

  if assigned(FCurrentPolygon) then
  begin
    for i:=0 to PolyLine.Count-1 do
    begin
      P:=PolyLine.Points[i];
      Self.AddPoint(P.X, P.Y);
    end;
  end;
end;

procedure TCadCanvas.AddFromPolyGon(PolyGon: TCadPolyGon);
var
    i : integer;
    P : TCadPoint;
begin
  if assigned(FCurrentPolyline) then
  begin
    for i:=0 to PolyGon.Count-1 do
    begin
      P:=PolyGon.Points[i];
      Self.AddPoint(P.X, P.Y);
    end;
  end;

  if assigned(FCurrentPolygon) then
  begin
    for i:=0 to PolyGon.Count-1 do
    begin
      P:=PolyGon.Points[i];
      Self.AddPoint(P.X, P.Y);
    end;
  end;
end;

procedure TCadCanvas.BeginPolyGon;
begin
  if assigned(FCurrentPolyGon) then
    EndPolyGon;

  FCurrentPolyGon:=TCadPolyGon.Create;
end;

procedure TCadCanvas.BeginPolyLine;
begin
  if assigned(FCurrentPolyLine) then
    EndPolyLine;

  FCurrentPolyLine:=TCadPolyLine.Create;
  FCurrentPolyLine.Color:=FCurrentColor;
end;

procedure TCadCanvas.EndPolyLine;
begin
  if assigned(FCurrentPolyLine) then
  begin
    FCurrentLayer.Add(FCurrentPolyLine);
    FCurrentPolyLine:=nil;
  end;
end;

procedure TCadCanvas.EndPolyGon;
begin
  if assigned(FCurrentPolyGon) then
  begin
    FCurrentLayer.Add(FCurrentPolyGon);
    FCurrentPolyGon:=nil;
  end;
end;

constructor TCadCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLayers:=TObjectList.Create(False);

  FCurrentPolyLine:=nil;
  FCurrentPolyGon:=nil;

  FStartPoint:=TCadPoint.Create;
  FStartPoint.X:=0;
  FStartPoint.Y:=0;

  FEndPoint:=TCadPoint.Create;
  FEndPoint.X:=0;
  FEndPoint.Y:=0;

  AddLayer('CADCANVAS');
  FCurrentLayer:=TCadLayer(FLayers.Items[0]);

  FModelMax:=TCadPoint.Create;
  FModelMin:=TCadPoint.Create;

  FTextHeight:=5.0;
  FTextRotation:=0.0;
  FTextTweakFactor:=0.6;
  FTextJustifyX:=tjLeft;
  FTextJustifyY:=tjTop;

  FCurrentColor:=256; // BYLAYER

  ResetMaxMin;
end;

destructor TCadCanvas.Destroy;
var
    i : integer;
begin

  FStartPoint.Free;
  FEndPoint.Free;
  FModelMax.Free;
  FModelMin.Free;

  for i:=0 to FLayers.Count-1 do
    TCadLayer(FLayers.Items[i]).Free;

  FLayers.Free;
  inherited;
end;

function TCadCanvas.GetCadLayer(idx: integer): TCadLayer;
begin
  if (idx>=0) and (idx<FLayers.Count) then
    Result:=TCadLayer(FLayers.Items[idx])
  else
    Result:=nil;
end;

procedure TCadCanvas.LineTo(x, y: double);
var
    Line : TCadSimpleLine;
begin
  Line:=TCadSimpleLine.Create;
  Line.StartPoint.X:=FStartPoint.X;
  Line.StartPoint.Y:=FStartPoint.Y;
  Line.EndPoint.X:=x;
  Line.EndPoint.Y:=y;
  Line.Color:=FCurrentColor;

  FStartPoint.X:=x;
  FStartPoint.Y:=y;

  UpdateMaxMin(Line.StartPoint);
  UpdateMaxMin(Line.EndPoint);

  FCurrentLayer.Add(Line);
end;

procedure TCadCanvas.MoveTo(x, y: double);
begin
  FStartPoint.X:=x;
  FStartPoint.Y:=y;
end;

procedure TCadCanvas.UpdateMaxMin(Point: TCadPoint);
begin
  if (Point.X>FModelMax.X) then
    FModelMax.X:=Point.X;

  if (Point.Y>FModelMax.Y) then
    FModelMax.Y:=Point.Y;

  if (Point.X<FModelMin.X) then
    FModelMin.X:=Point.X;

  if (Point.Y<FModelMin.Y) then
    FModelMin.Y:=Point.Y;
end;

procedure TCadCanvas.ResetMaxMin;
begin
  FModelMax.X := -1e300;
  FModelMax.Y := -1e300;
  FModelMin.X := 1e300;
  FModelMin.Y := 1e300;
end;

function TCadCanvas.GetLayerCount: integer;
begin
  Result:=FLayers.Count;
end;

procedure TCadCanvas.Clear;
var
    i : integer;
begin
  FStartPoint.Free;
  FEndPoint.Free;
  FModelMax.Free;
  FModelMin.Free;


  for i:=0 to FLayers.Count-1 do
    TCadLayer(FLayers.Items[i]).Free;

  FLayers.Clear;

  FCurrentPolyLine:=nil;
  FCurrentPolyGon:=nil;

  FStartPoint:=TCadPoint.Create;
  FStartPoint.X:=0;
  FStartPoint.Y:=0;

  FEndPoint:=TCadPoint.Create;
  FEndPoint.X:=0;
  FEndPoint.Y:=0;

  AddLayer('CADCANVAS');
  FCurrentLayer:=TCadLayer(FLayers.Items[0]);

  FModelMax:=TCadPoint.Create;
  FModelMin:=TCadPoint.Create;

  FTextHeight:=5.0;

  FCurrentColor:=256; // BYLAYER

  ResetMaxMin;
end;

function TCadCanvas.IsValidLayer(const Layer : TCadLayer): boolean;
var
    i : integer;
begin
  for i:=0 to FLayers.Count-1 do
  begin
    if Layer=FLayers.Items[i] then
      begin
        Result:=true;
        exit;
      end;
  end;
  Result:=false;
end;

procedure TCadCanvas.SetCurrentLayer(const Layer: TCadLayer);
begin
  if IsValidLayer(Layer) then
    FCurrentLayer:=Layer
  else
    FCurrentLayer:=Layers[0];
end;

procedure TCadCanvas.SetTextHeight(const Value: double);
begin
  FTextHeight := Value;
end;

procedure TCadCanvas.TextOut(x, y: double; Text: string);
var
    TextElement : TCadText;
    OpposingPoint : TCadPoint;
begin
  OpposingPoint:=TCadPoint.Create;
  TextElement:=TCadText.Create;

  if (TextJustifyX = tjLeft) then
    begin
      TextElement.Position.X:=x;
      OpposingPoint.X:=Self.TextSizeX(Text);
    end
  else if (TextJustifyX = tjCenter) then
    begin
      TextElement.Position.X:=x-Self.TextSizeX(Text)/2;
      OpposingPoint.X:=x+Self.TextSizeX(Text)/2;
    end
  else
    begin
      TextElement.Position.X:=x-Self.TextSizeX(Text);
      OpposingPoint.X:=x;
    end;

  if (TextJustifyY = tjTop) then
    begin
      TextElement.Position.Y:=y;
      OpposingPoint.Y:=y-Self.TextSizeY(Text);
    end
  else if (TextJustifyY = tjCenter) then
    begin
      TextElement.Position.Y:=y+self.TextSizeY(Text)/2;
      OpposingPoint.Y:=y-self.TextSizeY(Text)/2;;
    end
  else
    begin
      TextElement.Position.Y:=Y+Self.TextSizeY(Text);
      OpposingPoint.Y:=y;
    end;


  TextElement.Height:=FTextHeight;
  TextElement.RotationAngle:=FTextRotation;
  TextElement.TextString:=Text;
  TextElement.Color:=FCurrentColor;

  UpdateMaxMin(TextElement.Position);
  UpdateMaxMin(OpposingPoint);

  FCurrentLayer.Add(TextElement);
  OpposingPoint.Free;
end;

procedure TCadCanvas.Solid3(x1, y1, x2, y2, x3, y3: double);
var
    CadSolid : TCadSolid;
begin
  CadSolid:=TCadSolid.Create;
  CadSolid.Points[0].X:=x1;
  CadSolid.Points[0].Y:=y1;
  CadSolid.Points[1].X:=x2;
  CadSolid.Points[1].Y:=y2;
  CadSolid.Points[2].X:=x3;
  CadSolid.Points[2].Y:=y3;
  CadSolid.Points[3].X:=x3;
  CadSolid.Points[3].Y:=y3;
  CadSolid.Color:=FCurrentColor;

  UpdateMaxMin(CadSolid.Points[0]);
  UpdateMaxMin(CadSolid.Points[1]);
  UpdateMaxMin(CadSolid.Points[2]);

  FCurrentLayer.Add(CadSolid);
end;

procedure TCadCanvas.Solid4(x1, y1, x2, y2, x3, y3, x4, y4: double);
var
    CadSolid : TCadSolid;
begin
  CadSolid:=TCadSolid.Create;
  CadSolid.Points[0].X:=x1;
  CadSolid.Points[0].Y:=y1;
  CadSolid.Points[1].X:=x2;
  CadSolid.Points[1].Y:=y2;
  CadSolid.Points[2].X:=x3;
  CadSolid.Points[2].Y:=y3;
  CadSolid.Points[3].X:=x4;
  CadSolid.Points[3].Y:=y4;
  CadSolid.Color:=FCurrentColor;

  UpdateMaxMin(CadSolid.Points[0]);
  UpdateMaxMin(CadSolid.Points[1]);
  UpdateMaxMin(CadSolid.Points[2]);
  UpdateMaxMin(CadSolid.Points[3]);

  FCurrentLayer.Add(CadSolid);
end;

procedure TCadCanvas.SetTextRotation(const Value: double);
begin
  FTextRotation := Value;
end;

procedure TCadCanvas.SimplePoint(x, y: double);
var
    CadPoint : TCadPoint;
begin
  CadPoint:=TCadPoint.Create;
  CadPoint.X:=x;
  CadPoint.Y:=y;
  CadPoint.Color:=FCurrentColor;

  UpdateMaxMin(CadPoint);

  FCurrentLayer.Add(CadPoint);
end;

procedure TCadCanvas.AddElement(Element: TCadElement);
begin
  if assigned(Element) then
  begin
    FCurrentLayer.Add(Element);

    if Element is TCadPoint then
      UpdateMaxMin(TCadPoint(Element));

    if Element is TCadSimpleLine then
    begin
      UpdateMaxMin(TCadSimpleLine(Element).StartPoint);
      UpdateMaxMin(TCadSimpleLine(Element).EndPoint);
    end;
  end;
end;

function TCadCanvas.TextSizeX(TextString: string): double;
var
    Picture : TPicture;
    w,h : double;
    ratio : double;
begin
  Picture:=TPicture.Create;
  Picture.Bitmap.Canvas.Font.Size:=24;
  w:=Picture.Bitmap.Canvas.TextWidth(TextString);
  h:=FTextTweakFactor*Picture.Bitmap.Canvas.TextHeight(TextString);
  if (h>0) then
    ratio:=w/h
  else
    ratio:=1.0;
  Result:=FTextHeight*ratio;
  Picture.Free;
end;

function TCadCanvas.TextSizeY(TextString: string): double;
begin
  Result:=FTextHeight;
end;

function TCadCanvas.GetLayerIndex(Name: string): integer;
var
    i : integer;
    Layer : TCadLayer;
begin
  for i:=0 to FLayers.Count-1 do
  begin
    Layer:=TCadLayer(FLayers.Items[i]);
    if Layer.Name = Name then
      begin
        Result:=i;
        exit;
      end;
  end;

  Result:=-1;
end;

function TCadCanvas.GetLayerWithName(Name: string): TCadLayer;
var
    LayerIndex : integer;
begin
  LayerIndex:=GetLayerIndex(Name);

  if LayerIndex>0 then
    Result:=TCadLayer(FLayers.Items[LayerIndex])
  else
    Result:=nil;
end;

procedure TCadCanvas.ResetTextJustify;
begin
  FTextJustifyX:=tjLeft;
  FTextJustifyY:=tjTop;
end;

procedure TCadCanvas.Rectangle(x1, y1, x2, y2: double);
begin
  Self.MoveTo(x1, y1);
  Self.LineTo(x2, y1);
  Self.LineTo(x2, y2);
  Self.LineTo(x1, y2);
  Self.LineTo(x1, y1);
end;

procedure TCadCanvas.DeleteLayer(Layer: TCadLayer);
var
    i : integer;
    L : TCadLayer;
begin
  for i:=0 to FLayers.Count-1 do
  begin
    L:=FLayers[i] as TCadLayer;

    if L=Layer then
    begin
      Self.DeleteLayerIndex(i);
      break;
    end;
  end;
end;

procedure TCadCanvas.DeleteLayerIndex(idx: integer);
var
    Layer : TCadLayer;
begin
  if (idx>=0) and (idx<FLayers.Count) then
  begin
    Layer:=FLayers.Items[idx] as TCadLayer;
    Layer.Free;
    FLayers.Delete(idx);
  end;
end;

{ TCadLine }

constructor TCadLine.Create;
begin
  inherited;
end;

{ TCadSimpleLine }

constructor TCadSimpleLine.Create;
begin
  inherited;
  FStartPoint:=TCadPoint.Create;
  FEndPoint:=TCadPoint.Create;
end;

destructor TCadSimpleLine.Destroy;
begin
  FStartPoint.Free;
  FEndPoint.Free;
  inherited;
end;

function TCadSimpleLine.Intersection(L: TCadSimpleLine): TCadPoint;
var
  k1, k2 : double;
  m1, m2 : double;
  x, y : double;
  x1, x2 : double;
  ISect : TCadPoint;
  dx, dy : double;

  TmpStart1, TmpStart2, TmpEnd1, TmpEnd2 : TCadPoint;
  TmpPoint : TCadPoint;

  vert1 : boolean;
  vert2 : boolean;
begin

  k1:=0; k2:=0; m1:=0; m2:=0; x1:=0; x2:=0;

  Result:=nil;

  TmpStart1:=TCadPoint.Create;
  TmpStart2:=TCadPoint.Create;
  TmpEnd1:=TCadPoint.Create;
  TmpEnd2:=TCadPoint.Create;
  TmpPoint:=TCadPoint.Create;

  vert1:=false;
  vert2:=false;

  if (FStartPoint.X>FEndPoint.X) then
    begin
      TmpStart1.Assign(FEndPoint);
      TmpEnd1.Assign(FStartPoint);
    end
  else
    begin
      TmpStart1.Assign(FStartPoint);
      TmpEnd1.Assign(FEndPoint);
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
      TmpStart2.Assign(L.EndPoint);
      TmpEnd2.Assign(L.StartPoint);
    end
  else
    begin
      TmpStart2.Assign(L.StartPoint);
      TmpEnd2.Assign(L.EndPoint);
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
      x1:=TmpEnd2.X;
      vert2:=true;
    end;

  if (k1-k2=0) then
  begin
    TmpStart1.Free;
    TmpStart2.Free;
    TmpEnd1.Free;
    TmpEnd2.Free;
    TmpPoint.Free;
    Result:=nil;
    exit;
  end;

  if (not vert1) and (not vert2) then
  begin
    //x*k1+m1 = x*k2+m2, x*(k1-k2)=m2-m1
    x:=(m2-m1)/(k1-k2);
    y:=k1*x+m1;

    if (x>=TmpStart1.X) and (x<=TmpEnd1.X) then
      if (x>=TmpStart2.X) and (x<=TmpEnd2.X) then
        begin
          ISect:=TCadPoint.Create;
          ISect.X:=x;
          ISect.Y:=y;
          Result:=ISect;
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          exit;
        end
      else
        begin
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          Result:=nil;
          exit;
        end;
  end;

  if (vert1) and (vert2) then
  begin
    TmpStart1.Free;
    TmpStart2.Free;
    TmpEnd1.Free;
    TmpEnd2.Free;
    TmpPoint.Free;
    Result:=nil;
    exit;
  end;

  if (vert1) and (not vert2) then
  begin
    x:=x1;
    y:=k2*x + m2;

    if (TmpStart1.Y>TmpEnd1.Y) then
    begin
      TmpPoint.Assign(TmpStart1);
      TmpStart1.Assign(TmpEnd1);
      TmpEnd1.Assign(TmpStart1);
    end;

    if (TmpStart2.Y>TmpEnd2.Y) then
    begin
      TmpPoint.Assign(TmpStart2);
      TmpStart1.Assign(TmpEnd2);
      TmpEnd1.Assign(TmpStart2);
    end;

    if (y>=TmpStart1.Y) and (y<=TmpEnd1.Y) then
      if (y>=TmpStart2.Y) and (y<=TmpEnd2.Y) then
        begin
          ISect:=TCadPoint.Create;
          ISect.X:=x;
          ISect.Y:=y;
          Result:=ISect;
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          exit;
        end
      else
        begin
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          Result:=nil;
          exit;
        end;
  end;

  if (vert2) and (not vert1) then
  begin
    x:=x2;
    y:=k1*x + m1;

    if (TmpStart1.Y>TmpEnd1.Y) then
    begin
      TmpPoint.Assign(TmpStart1);
      TmpStart1.Assign(TmpEnd1);
      TmpEnd1.Assign(TmpStart1);
    end;

    if (TmpStart2.Y>TmpEnd2.Y) then
    begin
      TmpPoint.Assign(TmpStart2);
      TmpStart1.Assign(TmpEnd2);
      TmpEnd1.Assign(TmpStart2);
    end;

    if (y>=TmpStart1.Y) and (y<=TmpEnd1.Y) then
      if (y>=TmpStart2.Y) and (y<=TmpEnd2.Y) then
        begin
          ISect:=TCadPoint.Create;
          ISect.X:=x;
          ISect.Y:=y;
          Result:=ISect;
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          exit;
        end
      else
        begin
          TmpStart1.Free;
          TmpStart2.Free;
          TmpEnd1.Free;
          TmpEnd2.Free;
          TmpPoint.Free;
          Result:=nil;
          exit;
        end;
  end;
end;

{ TCadPolyLine }

procedure TCadPolyLine.Add(Point: TCadPoint);
begin
  FPoints.Add(Point);
end;

procedure TCadPolyLine.Add(x, y: double);
var
    P : TCadPoint;
begin
  P:=TCadPoint.Create;
  P.X:=x;
  P.Y:=y;
  Self.Add(P);
end;

procedure TCadPolyLine.Clear;
var
    i : integer;
begin
  for i:=0 to FPoints.Count-1 do
    TCadPoint(FPoints.Items[i]).Free;

  FPoints.Clear;
end;

constructor TCadPolyLine.Create;
begin
  inherited;
  FPoints:=TObjectList.Create(False);
end;

destructor TCadPolyLine.Destroy;
var
    i : integer;
begin
  for i:=0 to FPoints.Count-1 do
    TCadPoint(FPoints.Items[i]).Free;

  FPoints.Free;
  inherited;
end;

function TCadPolyLine.GetCount: integer;
begin
  Result:=FPoints.Count;
end;

function TCadPolyLine.GetPoint(idx: integer): TCadPoint;
begin
  if (idx>=0) and (idx<FPoints.Count) then
    Result:=TCadPoint(FPoints.Items[idx])
  else
    Result:=nil;
end;

{ TCadPolyGon }

procedure TCadPolyGon.Add(Point: TCadPoint);
begin
  FPoints.Add(Point);
end;

procedure TCadPolygon.Clear;
var
    i : integer;
begin
  for i:=0 to FPoints.Count-1 do
    TCadPoint(FPoints.Items[i]).Free;

  FPoints.Clear;
end;

constructor TCadPolyGon.Create;
begin
  inherited;
  FPoints:=TObjectList.Create(False);
end;

destructor TCadPolyGon.Destroy;
var
    i : integer;
begin
  for i:=0 to FPoints.Count-1 do
    TCadPoint(FPoints.Items[i]).Free;

  FPoints.Free;
  inherited;
end;

function TCadPolyGon.GetCount: integer;
begin
  Result:=FPoints.Count;
end;

function TCadPolyGon.GetPoint(idx: integer): TCadPoint;
begin
  if (idx>=0) and (idx<FPoints.Count) then
    Result:=TCadPoint(FPoints.Items[idx])
  else
    Result:=nil;
end;

{ TCadLayer }

procedure TCadLayer.Add(Element: TCadElement);
begin
  FElements.Add(Element);
end;

procedure TCadLayer.Clear;
var
    i : integer;
begin
  for i:=0 to FElements.Count-1 do
    TCadElement(FElements.Items[i]).Free;
  FElements.Clear;
end;

constructor TCadLayer.Create;
begin
  FElements:=TObjectList.Create(False);
  FLayerName:='CADCANVAS';
  FColor:=1;
  FVisible:=true;
end;

destructor TCadLayer.Destroy;
begin
  Self.Clear;
  FElements.Free;
  inherited;
end;

function TCadLayer.GetCount: integer;
begin
  Result:=FElements.Count;
end;

function TCadLayer.GetElement(idx: integer): TCadElement;
begin
  if (idx>=0) and (idx<FElements.Count) then
    Result:=TCadElement(FElements.Items[idx])
  else
    Result:=nil;
end;

{ TCadText }

constructor TCadText.Create;
begin
  FPosition:=TCadPoint.Create;
  FHeight:=5.0;
  FTextString:='';
  FRotationAngle:=0;
end;

destructor TCadText.Destroy;
begin
  FPosition.Free;
  inherited;
end;

{ TCadSolid }

constructor TCadSolid.Create;
begin
  inherited;
  FPoints:=TObjectList.Create(False);
  FPoints.Add(TCadPoint.Create);
  FPoints.Add(TCadPoint.Create);
  FPoints.Add(TCadPoint.Create);
  FPoints.Add(TCadPoint.Create);
end;

destructor TCadSolid.Destroy;
var
    i : integer;
begin
  for i:=0 to FPoints.Count-1 do
    TCadPoint(FPoints.Items[i]).Free;

  FPoints.Free;
  inherited;
end;

function TCadSolid.GetCount: integer;
begin
  Result:=FPoints.Count;
end;

function TCadSolid.GetIsSolid3: boolean;
begin
  Result:=(Self.Points[2].X = Self.Points[3].X) and
    (Self.Points[2].Y = Self.Points[3].Y);
end;

function TCadSolid.GetPoint(idx: integer): TCadPoint;
begin
  if (idx>=0) and (idx<4) then
    begin
      Result:=FPoints.Items[idx] as TCadPoint;
    end
  else
    Result:=nil;
end;

{ TCadElement }

constructor TCadElement.Create;
begin
  inherited;
  FColor:=256; // BYLAYER
end;

function TCadElement.GetColorByLayer: boolean;
begin
  if (FColor=256) then
    Result:=true
  else
    Result:=false;
end;

{ TCadVector }

function TCadVector.Angle(v: TCadVector): double;
var
    k : double;
begin
  if (Self.Length=0) or (v.length=0) then
  begin
    Result:=0;
    exit;
  end;

  if (Self.EqualTo(v)) then
  begin
    Result:=0;
    exit;
  end;

  k:=Self.ScalarProduct(v)/(Self.Length*v.Length);
  Result:=ArcCos(k);
end;

procedure TCadVector.Assign(v: TCadVector);
begin
  FX:=v.X;
  FY:=v.Y;
end;

function TCadVector.CrossProduct(v: TCadVector): double;
begin
  Result:=FX*v.Y-v.X*FY;
end;

function TCadVector.EqualTo(v: TCadVector): boolean;
begin
  Result:=(Self.x=v.x) and (Self.y=v.y);
end;

function TCadVector.GetLength: double;
begin
  Result:=sqrt(sqr(FX)+sqr(FY));
end;

function TCadVector.ScalarProduct(v: TCadVector): double;
begin
  Result:=Self.x*v.x+Self.y*v.y;
end;

end.
