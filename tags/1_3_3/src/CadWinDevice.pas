{
@abstract(Output device component for drawing a CadCanvas in a window)
@author(Jonas Lindemann)
This unit contains the CadWinDevice component for drawing the elements
of a CadCanvas component in a window. The CadCanvas is assigned using the
CadCanvas property of the component. This can be done directly in the design
mode of the Delphi IDE.
}
unit CadWinDevice;

{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CadCanvas, Contnrs;

type

  { Layer class for describing the properties of the layer while
    drawing it in a window }
  TCadWinLayer = class
  private
    FPen : TPen;
    FFont : TFont;
  public
    constructor Create;
    destructor Destroy; override;

    { Pen used to draw the line }
    property Pen : TPen read FPen;

    { Font used to draw text }
    property Font : TFont read FFont;
  end;

  { Color map class for representing index colors with Delphi colors }
  TCadWinColors = class
  private
    FColors : array [1..256] of TColor;
    function GetColor(index: integer): TColor;
    procedure SetColor(index: integer; Color : TColor);
  public
    constructor Create;
    destructor Destroy; override;

    { Gets/sets the index color of the color map, index = 1 - 255 }
    property Color[index : integer] : TColor read GetColor write SetColor;
  end;

  { Component for drawing the contents of CadCanvas on a window }
  TCadWinDevice = class(TGraphicControl)
  private
    { Private declarations }
    FCadCanvas : TCadCanvas;
    FWindowMin : TCadPoint;
    FWindowMax : TCadPoint;
    FWinColors : TCadWinColors;
    FScaleFactor : double;
    FAutoSize: boolean;
    FDrawBackground : boolean;
    FLayers : TObjectList;
    FWMFFileName: string;
    FSolidOutlines : boolean;
    FPointSize : integer;

    FMarginX : integer;
    FMarginY : integer;

    function CreateAngledFont(Font: HFont; Angle: Longint): HFont;
    procedure TextOutA(Canvas: TCanvas; X, Y, Angle: Integer; Text: string);

    procedure UpdateWindow;
    procedure SetScaleFactor(const Value: double);
    function GetWindowMax: TCadPoint;
    function GetWindowMin: TCadPoint;
    procedure SetAutoSizeProp(const Value: boolean);
    procedure SetDrawBackground(const Value: boolean);
    function GetCadWinLayers(idx: integer): TCadWinLayer;
    function GetColors(index: integer): TColor;

    procedure RenderToCanvas(ACanvas : TCanvas);
    procedure SetMarginX(const Value: integer);
    procedure SetMarginY(const Value: integer);
    procedure SetColor(index: integer; const Value: TColor);
    procedure SetSolidOutlines(const Value: boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    { Converts a world coordinate to a screen coordinate }
    procedure W2S(Point : TCadPoint; var sx, sy : integer);

    { Converts a world length to a screen length }
    function L2S(length : double) : integer;

    { Converts a screen coordinate to a world coordinate }
    procedure S2W(sx, sy : integer; Point : TCadPoint);

    { Sets the window to be drawn in world coordinates }
    procedure SetWindow(MinPoint, MaxPoint : TCadPoint);

    { Zooms to the extent of the drawing in the CadCanvas drawing }
    procedure ZoomExtent;

    { Renders the CadCanvas drawing in the window }
    procedure Render;

    { Renders the CadCanvas drawing to a Windows Metafile specified
      in the Filename property }
    procedure RenderWMF;

    { Add a TCadWinLayer representing a TCadLayer in the CadCanvas drawing }
    function AddLayer : TCadWinLayer;

    { Clears all defined layers }
    procedure ClearLayers;

    { Returns the minimum point of the drawn window }
    property WindowMin : TCadPoint read GetWindowMin;

    { Returns the maximum point of the drawn window }
    property WindowMax : TCadPoint read GetWindowMax;

    { Returns the index window layer }
    property Layers[index : integer] : TCadWinLayer read GetCadWinLayers;

    { Gets/Sets the color to represent the indexed color }
    property WindowColor[index : integer] : TColor read GetColors write SetColor;
  published
    { Gets/Sets the CadCanvas used when drawing }
    property CadCanvas : TCadCanvas read FCadCanvas write FCadCanvas;

    { Gets/Sets the scalefactor used when drawing }
    property ScaleFactor : double read FScaleFactor write SetScaleFactor;

    { Determines if the window is automatically zoom to the extents when redrawn }
    property AutoSize : boolean read FAutoSize write SetAutoSizeProp;

    { Determines if a background is to be drawn }
    property DrawBackground : boolean read FDrawBackground write SetDrawBackground;

    { Gets/sets the filename used when creating WMF files }
    property FileName : string read FWMFFileName write FWMFFileName;

    { Gets/sets the left right margin in pixels }
    property MarginX : integer read FMarginX write SetMarginX;

    { Gets/sets the top bottom margin in pixels }
    property MarginY : integer read FMarginY write SetMarginY;

    property SolidOutlines : boolean  read FSolidOutlines write SetSolidOutlines;

    property PointSize : integer read FPointSize write FPointSize;

    property Align;
    property Anchors;
    property Font;
    property Color;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadWinDevice]);
end;

// Taken from
//      Latium Software  http://www.latiumsoftware.com
//      Email: latium@latiumsoftware.com

function TCadWinDevice.CreateAngledFont(Font: HFont; Angle: Longint): HFont;
var
  FontInfo: TLogFont;    // Font information structure
begin

  // Get the information of the font passed as parameter

  if GetObject(Font, SizeOf(FontInfo), @FontInfo) = 0 then
  begin
    Result := 0;
    exit;
  end;

  // Set the angle

  FontInfo.lfEscapement := Angle;
  FontInfo.lfOrientation := Angle;

  // Set the quality

  FontInfo.lfQuality := PROOF_QUALITY;

  // Create a new font with the modified information
  // The new font must be released calling DeleteObject

  Result := CreateFontIndirect(FontInfo);
end;

// Taken from
//      Latium Software  http://www.latiumsoftware.com
//      Email: latium@latiumsoftware.com

procedure TCadWinDevice.TextOutA(Canvas: TCanvas; X, Y, Angle: Integer;
  Text: string);
var
  OriginalFont, AngledFont: HFont;
begin

  // Create an angled font from the current font

  AngledFont := CreateAngledFont(Canvas.Font.Handle, Angle);

  if AngledFont <> 0 then
  begin

    // Set it temporarily as the current font

    OriginalFont := SelectObject(Canvas.Handle, AngledFont);

    if OriginalFont <> 0 then
    begin

      // Write the text

      Canvas.TextOut(X, Y, Text);

      // Restore the original font

      if SelectObject(Canvas.Handle, OriginalFont) = 0 then
      begin
        Canvas.Font.Handle := AngledFont;

        // raise Exception.Create('Couldn''t restore font');

        exit;
      end;
    end;

    // Release the angled font

    DeleteObject(AngledFont)
  end;
end;

{ TCadWinDevice }

constructor TCadWinDevice.Create(AOwner: TComponent);
begin
  inherited;

  // Inherited properties initial values

  Width:=100;
  Height:=100;
  Color:=clWhite;

  // Initializing new property values

  FCadCanvas:=nil;
  FWindowMin:=TCadPoint.Create;
  FWindowMax:=TCadPoint.Create;
  FWindowMin.X:=0.0;
  FWindowMin.Y:=0.0;
  FWindowMax.X:=Width;
  FWindowMax.Y:=Height;
  FScaleFactor:=20.0;
  FDrawBackground:=false;
  FAutoSize:=true;
  FWMFFileName:='noname.wmf';

  FMarginX:=3;
  FMarginY:=3;

  FSolidOutlines:=false;
  FPointSize:=2;

  FWinColors:=TCadWinColors.Create;

  FLayers:=TObjectList.Create(False);

  AddLayer;
end;

procedure TCadWinDevice.Render;
begin
  RenderToCanvas(Canvas);
end;
procedure TCadWinDevice.Paint;
begin
  inherited;
  Render;
end;

procedure TCadWinDevice.S2W(sx, sy: integer; Point: TCadPoint);
begin

end;

procedure TCadWinDevice.W2S(Point: TCadPoint; var sx, sy: integer);
begin
  sx:=FMarginX + round((Point.X-FWindowMin.X)*FScaleFactor);
  sy:=(Height-FMarginY)-round((Point.Y-FWindowMin.Y)*FScaleFactor)-1;
end;

procedure TCadWinDevice.UpdateWindow;
var
    dx, dy : double;
    ObjectRatio : double;
    WindowRatio : double;
begin
  dx:=FWindowMax.X-FWindowMin.X;
  dy:=FWindowMax.Y-FWindowMin.Y;

  if (height=0) or (dy=0) then
    exit;

  try
  ObjectRatio:=dx/dy;
  WindowRatio:=(Width-FMarginX*2)/(Height-FMarginY*2);

  if (ObjectRatio>WindowRatio) then
    FScaleFactor:=(Width-FMarginX*2)/dx
  else
    FScaleFactor:=(Height-FMarginY*2)/dy;
  except
    on EZeroDivide do
    begin
      FScaleFactor:=0;
    end;
  end;
end;

destructor TCadWinDevice.Destroy;
var
    i : integer;
begin
  FWindowMin.Free;
  FWindowMax.Free;

  for i:=0 to FLayers.Count-1 do
    TCadWinLayer(FLayers.Items[i]).Free;

  FLayers.Free;

  FWinColors.Free;
  inherited;
end;

procedure TCadWinDevice.SetScaleFactor(const Value: double);
begin
  FScaleFactor := Value;
end;

function TCadWinDevice.GetWindowMax: TCadPoint;
begin
  Result:=FWindowMax;
  UpdateWindow;
end;

function TCadWinDevice.GetWindowMin: TCadPoint;
begin
  Result:=FWindowMin;
  UpdateWindow;
end;

procedure TCadWinDevice.SetWindow(MinPoint, MaxPoint: TCadPoint);
begin
  FWindowMin.X:=MinPoint.X;
  FWindowMin.Y:=MinPoint.Y;
  FWindowMax.X:=MaxPoint.X;
  FWindowMax.Y:=MaxPoint.Y;
  UpdateWindow;
end;

procedure TCadWinDevice.ZoomExtent;
begin
  if assigned(CadCanvas) then
    SetWindow(CadCanvas.ModelMin, CadCanvas.ModelMax);
end;

procedure TCadWinDevice.SetAutoSizeProp(const Value: boolean);
begin
  FAutoSize := Value;
  Invalidate;
end;

procedure TCadWinDevice.Resize;
begin
  inherited;
  if (FAutoSize) then
    ZoomExtent;
end;

procedure TCadWinDevice.SetDrawBackground(const Value: boolean);
begin
  FDrawBackground := Value;
  Invalidate;
end;

function TCadWinDevice.GetCadWinLayers(idx: integer): TCadWinLayer;
begin
  if (idx>=0) and (idx<FLayers.Count) then
    begin
      Result:=FLayers.Items[idx] as TCadWinLayer;
    end
  else
    Result:=nil;
end;

function TCadWinDevice.AddLayer: TCadWinLayer;
var
    Layer : TCadWinLayer;
begin
  Layer:=TCadWinLayer.Create;
  FLayers.Add(Layer);
  Result:=Layer;
end;

function TCadWinDevice.L2S(length: double) : integer;
begin
  Result:=round(Length*FScaleFactor);
end;

procedure TCadWinDevice.SetColor(index: integer; const Value: TColor);
begin
  if (index>=1) and (index<256) then
    FWinColors.Color[index]:=Value;
end;

function TCadWinDevice.GetColors(index: integer): TColor;
begin
  if (index>=1) and (index<256) then
    Result:=FWinColors.Color[index]
  else
    Result:=clBlack;
end;

procedure TCadWinDevice.RenderWMF;
var
    Metafile : TMetafile;
    MetafileCanvas : TMetafileCanvas;
begin
  Metafile:=TMetafile.Create;
  Metafile.Enhanced:=true;
  Metafile.Width:=Width;
  Metafile.Height:=Height;

  MetafileCanvas:=TMetafileCanvas.Create(Metafile,0);
  RenderToCanvas(MetafileCanvas);
  MetafileCanvas.Free;

  if (FWMFFileName<>'') then
    Metafile.SaveToFile(FWMFFileName);

  Metafile.free;
end;

procedure TCadWinDevice.RenderToCanvas(ACanvas: TCanvas);
var
    i, j, k : integer;
    Layer : TCadLayer;
    Element : TCadElement;
    SimpleLine : TCadSimpleLine;
    CadPolyLine : TCadPolyLine;
    CadText : TCadText;
    CadSolid : TCadSolid;
    CadPoint : TCadPoint;
    sx1, sy1, sx2, sy2 : integer;
    first : boolean;
    PointArray : array [1..5] of TPoint;
begin
  with ACanvas do
  begin
    if (FDrawBackground) then
    begin
      Brush.Color:=Color;
      Brush.Style:=bsSolid;
      FillRect(Rect(0,0,Width,Height));
    end;

    if assigned(CadCanvas) then
    begin
      for i:=0 to CadCanvas.LayerCount-1 do
      begin
        Layer:=CadCanvas.Layers[i];

        if (i>=0) and (i<FLayers.Count) then
          begin
            Pen.Color:=Layers[i].Pen.Color;
            Pen.Style:=Layers[i].Pen.Style;
            Pen.Width:=Layers[i].Pen.Width;
          end
        else
          begin
            Pen.Color:=clBlack;
            Pen.Style:=psSolid;
            Pen.Width:=1;
          end;

        for j:=0 to Layer.Count-1 do
        begin

          Brush.Style:=bsClear;
          Pen.Mode:=pmCopy;

          Element:=Layer.Elements[j];

          if Element is TCadPoint then
          begin
            CadPoint:=TCadPoint(Element);
            W2S(CadPoint, sx1, sy1);
            Pen.Width:=3;
            if CadPoint.ColorByLayer then
              Pen.Color:=FWinColors.Color[Layer.Color]
            else
              Pen.Color:=FWinColors.Color[CadPoint.Color];

            MoveTo(sx1,sy1-FPointSize);
            LineTo(sx1,sy1+FPointSize);
            MoveTo(sx1-FPointSize,sy1);
            LineTo(sx1+FPointSize,sy1);
            Pen.Width:=1;
          end;

          if Element is TCadSimpleLine then
          begin
            SimpleLine:=TCadSimpleLine(Element);
            W2S(SimpleLine.StartPoint, sx1, sy1);
            W2S(SimpleLine.EndPoint, sx2, sy2);
            if SimpleLine.ColorByLayer then
              Pen.Color:=FWinColors.Color[Layer.Color]
            else
              Pen.Color:=FWinColors.Color[SimpleLine.Color];
            MoveTo(sx1, sy1);
            LineTo(sx2, sy2);
          end;

          if Element is TCadPolyLine then
          begin
            CadPolyLine:=TCadPolyLine(Element);
            if CadPolyline.ColorByLayer then
              Pen.Color:=FWinColors.Color[Layer.Color]
            else
              Pen.Color:=FWinColors.Color[CadPolyLine.Color];
            first:=true;
            for k:=0 to CadPolyLine.Count-1 do
            begin
              W2S(CadPolyLine.Points[k], sx1, sy1);
              if first then
                begin
                  first:=false;
                  MoveTo(sx1, sy1);
                end
              else
                LineTo(sx1, sy1);
            end;
          end;

          if Element is TCadPolygon then
          begin

          end;

          if Element is TCadText then
          begin
            CadText:=TCadText(Element);
            Font.Name:='Arial';
            Font.Size:=L2S(CadText.Height);
            Brush.Style:=bsClear;
            TextFlags:=TextFlags or ETO_OPAQUE;
            W2S(CadText.Position, sx1, sy1);
            if CadText.ColorByLayer then
              Font.Color:=FWinColors.Color[Layer.Color]
            else
              Font.Color:=FWinColors.Color[CadText.Color];
            if (CadText.RotationAngle=0) then
              TextOut(sx1, sy1-TextHeight(CadText.TextString), CadText.TextString)
            else
              TextOutA(Canvas, sx1, sy1-TextHeight(CadText.TextString), round(CadText.RotationAngle*10), CadText.TextString);
          end;

          if Element is TCadSolid then
          begin
            CadSolid:=TCadSolid(Element);
            W2S(CadSolid.Points[0], sx1, sy1);
            PointArray[1].x:=sx1;
            PointArray[1].y:=sy1;
            W2S(CadSolid.Points[1], sx1, sy1);
            PointArray[2].x:=sx1;
            PointArray[2].y:=sy1;
            W2S(CadSolid.Points[2], sx1, sy1);
            PointArray[4].x:=sx1;
            PointArray[4].y:=sy1;
            W2S(CadSolid.Points[3], sx1, sy1);
            PointArray[3].x:=sx1;
            PointArray[3].y:=sy1;
            W2S(CadSolid.Points[0], sx1, sy1);
            PointArray[5].x:=sx1;
            PointArray[5].y:=sy1;
            if not SolidOutlines then
              begin
                if CadSolid.ColorByLayer then
                  Brush.Color:=FWinColors.Color[Layer.Color]
                else
                  Brush.Color:=FWinColors.Color[CadSolid.Color];
                Pen.Style:=psClear;
                PolyGon(Slice(PointArray, 4));
                Pen.Style:=psSolid;
              end
            else
              begin
                if CadSolid.ColorByLayer then
                  Pen.Color:=FWinColors.Color[Layer.Color]
                else
                  Pen.Color:=FWinColors.Color[CadSolid.Color];
                PolyLine(PointArray);
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCadWinDevice.SetMarginX(const Value: integer);
begin
  FMarginX := Value;
  UpdateWindow;
end;

procedure TCadWinDevice.SetMarginY(const Value: integer);
begin
  FMarginY := Value;
  UpdateWindow;
end;

procedure TCadWinDevice.ClearLayers;
var
    i : integer;
begin
  for i:=0 to FLayers.Count-1 do
    TCadWinLayer(FLayers.Items[i]).Free;

  FLayers.Clear;

  AddLayer;
end;

procedure TCadWinDevice.SetSolidOutlines(const Value: boolean);
begin
  FSolidOutlines := Value;
end;

{ TCadWinLayer }

constructor TCadWinLayer.Create;
begin
  inherited;

  FPen:=TPen.Create;
  FFont:=TFont.Create;

  FPen.Mode:=pmCopy;
  FPen.Style:=psSolid;
  FPen.Color:=clBlack;
end;

destructor TCadWinLayer.Destroy;
begin
  FPen.Free;
  FFont.Free;
  inherited;
end;

{ TCadWinColors }

constructor TCadWinColors.Create;
var
    i : integer;
begin
  inherited Create;

  FColors[1] := clBlack;
  FColors[2] := clMaroon;
  FColors[3] := clGreen;
  FColors[4] := clOlive;
  FColors[5] := clNavy;
  FColors[6] := clPurple;
  FColors[7] := clTeal;
  FColors[8] := clGray;
  FColors[9] := clSilver;
  FColors[10] := clRed;
  FColors[11] := clLime;
  FColors[12] := clYellow;
  FColors[13] := clBlue;
  FColors[14] := clFuchsia;
  FColors[15] := clAqua;
  FColors[16] := clLtGray;
  FColors[17] := clDkGray;
  FColors[18] := clWhite;

  for i:=19 to 256 do
    FColors[i]:=clBlack;

end;

destructor TCadWinColors.Destroy;
begin
  inherited;

end;

function TCadWinColors.GetColor(index: integer): TColor;
begin
  if (index>=1) and (index<=256) then
    begin
      Result:=FColors[index];
    end
  else
    Result:=clBlack;
end;

procedure TCadWinColors.SetColor(index: integer; Color: TColor);
begin
  if (index>=1) and (index<=256) then
    begin
      FColors[index]:=Color;
    end
end;

{$WARN UNSAFE_CODE ON}

end.
