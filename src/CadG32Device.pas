{
@abstract(Output device component for drawing a CadCanvas in a window)
@author(Jonas Lindemann)
This unit contains the CadG32Device component for drawing the elements
of a CadCanvas component in a window. The CadCanvas is assigned using the
CadCanvas property of the component. This can be done directly in the design
mode of the Delphi IDE.
}
unit CadG32Device;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CadCanvas, GR32, GR32_Image, GR32_Polygons;

type

  { Layer class for describing the properties of the layer while
    drawing it in a window }
  TCadG32Layer = class
  private
    FColor : TColor32;
  public
    constructor Create;
    destructor Destroy; override;

    { Pen used to draw the line }
    property Color : TColor32 read FColor write FColor;
  end;

  { Color map class for representing index colors with Delphi colors }
  TCadG32Colors = class
  private
    FColors : array [1..256] of TColor32;
    function GetColor(index: integer): TColor32;
    procedure SetColor(index: integer; Color : TColor32);
  public
    constructor Create;
    destructor Destroy; override;

    { Gets/sets the index color of the color map, index = 1 - 255 }
    property Color[index : integer] : TColor32 read GetColor write SetColor;
  end;

  { Component for drawing the contents of CadCanvas on a window }
  TCadG32Device = class(TComponent)
  private
    { Private declarations }
    FCadCanvas : TCadCanvas;
    FWindowMin : TCadPoint;
    FWindowMax : TCadPoint;
    FWinColors : TCadG32Colors;
    FScaleFactor : double;
    FAutoSize: boolean;
    FDrawBackground : boolean;
    FLayers : TList;
    FWMFFileName: string;
    FSolidOutlines : boolean;
    FPointSize : integer;
    FImageWidth : integer;
    FImageHeight : integer;

    FMarginX : integer;
    FMarginY : integer;

    FImage32 : TImage32;
    FPolygon32 : TPolygon32;
    FAutoCenter: boolean;
    FAntialias : boolean;

    //function CreateAngledFont(Font: HFont; Angle: Longint): HFont;

    procedure UpdateWindow;
    procedure SetScaleFactor(const Value: double);
    function GetWindowMax: TCadPoint;
    function GetWindowMin: TCadPoint;
    procedure SetAutoSizeProp(const Value: boolean);
    procedure SetDrawBackground(const Value: boolean);
    function GetCadG32Layers(idx: integer): TCadG32Layer;
    function GetColors(index: integer): TColor32;

    procedure RenderToImage(Image : TImage32);
    procedure SetMarginX(const Value: integer);
    procedure SetMarginY(const Value: integer);
    procedure SetColor(index: integer; const Value: TColor32);
    procedure SetSolidOutlines(const Value: boolean);
    procedure SetImage32(const Value: TImage32);

    procedure BitmapResize(Sender: TObject);
    procedure SetAutoCenter(const Value: boolean);

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

    { Add a TCadG32Layer representing a TCadLayer in the CadCanvas drawing }
    function AddLayer : TCadG32Layer;

    { Clears all defined layers }
    procedure ClearLayers;

    procedure SaveToFile(const Filename : string);

    { Returns the minimum point of the drawn window }
    property WindowMin : TCadPoint read GetWindowMin;

    { Returns the maximum point of the drawn window }
    property WindowMax : TCadPoint read GetWindowMax;

    { Returns the index window layer }
    property Layers[index : integer] : TCadG32Layer read GetCadG32Layers;

    { Gets/Sets the color to represent the indexed color }
    property WindowColor[index : integer] : TColor32 read GetColors write SetColor;
  published
    { Gets/Sets the CadCanvas used when drawing }
    property CadCanvas : TCadCanvas read FCadCanvas write FCadCanvas;

    { Gets/Sets the Image used when drawing }
    property Image : TImage32 read FImage32 write SetImage32;

    { Gets/Sets the scalefactor used when drawing }
    property ScaleFactor : double read FScaleFactor write SetScaleFactor;

    { Determines if the window is automatically zoom to the extents when redrawn }
    property AutoSize : boolean read FAutoSize write SetAutoSizeProp;

    property AutoCenter : boolean read FAutoCenter write SetAutoCenter;

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

    property Antialias : boolean read FAntialias write FAntialias;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadG32Device]);
end;

// Taken from
//      Latium Software  http://www.latiumsoftware.com
//      Email: latium@latiumsoftware.com

//function TCadG32Device.CreateAngledFont(Font: HFont; Angle: Longint): HFont;
//var
//  FontInfo: TLogFont;    // Font information structure
//begin
//
//  // Get the information of the font passed as parameter
//
//  if GetObject(Font, SizeOf(FontInfo), @FontInfo) = 0 then
//  begin
//    Result := 0;
//    exit;
//  end;
//
//  // Set the angle
//
//  FontInfo.lfEscapement := Angle;
//  FontInfo.lfOrientation := Angle;
//
//  // Set the quality
//
//  FontInfo.lfQuality := PROOF_QUALITY;
//
//  // Create a new font with the modified information
//  // The new font must be released calling DeleteObject
//
//  Result := CreateFontIndirect(FontInfo);
//end;

{ TCadG32Device }

constructor TCadG32Device.Create(AOwner: TComponent);
begin
  inherited;

  // Inherited properties initial values


  // Initializing new property values

  FImageWidth:=100;
  FImageHeight:=100;
  FCadCanvas:=nil;
  FWindowMin:=TCadPoint.Create;
  FWindowMax:=TCadPoint.Create;
  FWindowMin.X:=0.0;
  FWindowMin.Y:=0.0;
  FWindowMax.X:=FImageWidth;
  FWindowMax.Y:=FImageHeight;
  FScaleFactor:=20.0;
  FDrawBackground:=false;
  FAutoSize:=true;
  FAutoCenter:=true;
  FWMFFileName:='noname.wmf';
  FAntialias:=true;

  FMarginX:=3;
  FMarginY:=3;

  FSolidOutlines:=false;
  FPointSize:=2;

  FWinColors:=TCadG32Colors.Create;

  FLayers:=TList.Create;

  FImage32:=nil;

  FPolygon32:=TPolygon32.Create;

  AddLayer;
end;

procedure TCadG32Device.Render;
begin
  if assigned(FImage32) then
    RenderToImage(FImage32);
end;

procedure TCadG32Device.S2W(sx, sy: integer; Point: TCadPoint);
begin
  if FScaleFactor>0 then
  begin
    Point.X:=(sx - FMarginX)/FScaleFactor + FWindowMin.X;
    Point.Y:=FWindowMin.Y + (FImageHeight - FMarginY - 1 - sy)/FScaleFactor;
  end;
end;

procedure TCadG32Device.SaveToFile(const Filename: string);
begin
  if assigned(Image) then
  begin
    Image.Bitmap.SaveToFile(Filename);
  end;
end;

procedure TCadG32Device.W2S(Point: TCadPoint; var sx, sy: integer);
var
  mx, my, wx, wy : double;
begin
  mx:=0; my:=0; wx:=0; wy:=0;
  if assigned(FCadCanvas) then
  begin
    mx:=FCadCanvas.ModelMax.X - FCadCanvas.ModelMin.X;
    my:=FCadCanvas.ModelMax.Y - FCadCanvas.ModelMin.Y;
    wx:=FWindowMax.X-FWindowMin.X;
    wy:=FWindowMax.Y-FWindowMin.Y;
  end;
  if FAutoCenter then
    begin
      if mx/my>1.0 then
        begin
          sx:=FMarginX + round((Point.X-FWindowMin.X)*FScaleFactor);
          sy:=(FImageHeight-FMarginY-round((wy/2-my/2)*FScaleFactor))-round((Point.Y-FWindowMin.Y)*FScaleFactor)-1;
        end
      else
        begin
          sx:=FMarginX + round((wx/2-mx/2)*FScaleFactor) + round((Point.X-FWindowMin.X)*FScaleFactor);
          sy:=(FImageHeight-FMarginY)-round((Point.Y-FWindowMin.Y)*FScaleFactor)-1;
        end;
    end
  else
    begin
      sx:=FMarginX + round((Point.X-FWindowMin.X)*FScaleFactor);
      sy:=(FImageHeight-FMarginY)-round((Point.Y-FWindowMin.Y)*FScaleFactor)-1;
    end;
end;

procedure TCadG32Device.UpdateWindow;
var
    dx, dy : double;
    ObjectRatio : double;
    WindowRatio : double;
    P1, P2 : TCadPoint;
begin
  dx:=FWindowMax.X-FWindowMin.X;
  dy:=FWindowMax.Y-FWindowMin.Y;

  if ((FImageHeight-FMarginY*2)=0) or (dy=0) then
    exit;

  try
    ObjectRatio:=dx/dy;
    WindowRatio:=(FImageWidth-FMarginX*2)/(FImageHeight-FMarginY*2);

    if (ObjectRatio>WindowRatio) then
      FScaleFactor:=(FImageWidth-FMarginX*2)/dx
    else
      FScaleFactor:=(FImageHeight-FMarginY*2)/dy;
  except
    on EZeroDivide do
    begin
      FScaleFactor:=0;
    end;
    on EInvalidOp do
    begin
      FScaleFactor:=0;
    end;
  end;

  // Update window points

  P1:=TCadPoint.Create;
  P2:=TCadPoint.Create;

  S2W(FMarginX, FImageHeight-FMarginY, P1);
  S2W(FImageWidth-FMarginX, FMarginY, P2);

  FWindowMin.Assign(P1);
  FWindowMax.Assign(P2);

  P1.Free;
  P2.Free;

end;

destructor TCadG32Device.Destroy;
var
    i : integer;
begin
  FWindowMin.Free;
  FWindowMax.Free;

  for i:=0 to FLayers.Count-1 do
    TCadG32Layer(FLayers.Items[i]).Free;

  FLayers.Free;

  FWinColors.Free;
  FPolygon32.Free;
  inherited;
end;

procedure TCadG32Device.SetScaleFactor(const Value: double);
begin
  FScaleFactor := Value;
end;

function TCadG32Device.GetWindowMax: TCadPoint;
begin
  Result:=FWindowMax;
  UpdateWindow;
end;

function TCadG32Device.GetWindowMin: TCadPoint;
begin
  Result:=FWindowMin;
  UpdateWindow;
end;

procedure TCadG32Device.SetWindow(MinPoint, MaxPoint: TCadPoint);
begin
  FWindowMin.X:=MinPoint.X;
  FWindowMin.Y:=MinPoint.Y;
  FWindowMax.X:=MaxPoint.X;
  FWindowMax.Y:=MaxPoint.Y;
  UpdateWindow;
end;

procedure TCadG32Device.ZoomExtent;
begin
  if assigned(CadCanvas) then
    SetWindow(CadCanvas.ModelMin, CadCanvas.ModelMax);
end;

procedure TCadG32Device.SetAutoSizeProp(const Value: boolean);
begin
  FAutoSize := Value;
  //Invalidate;
end;

procedure TCadG32Device.SetAutoCenter(const Value: boolean);
begin
  FAutoCenter := Value;
end;

procedure TCadG32Device.SetDrawBackground(const Value: boolean);
begin
  FDrawBackground := Value;
  //Invalidate;
end;

function TCadG32Device.GetCadG32Layers(idx: integer): TCadG32Layer;
begin
  if (idx>=0) and (idx<FLayers.Count) then
    begin
      Result:=FLayers.Items[idx];
    end
  else
    Result:=nil;
end;

function TCadG32Device.AddLayer: TCadG32Layer;
var
    Layer : TCadG32Layer;
begin
  Layer:=TCadG32Layer.Create;
  FLayers.Add(Layer);
  Result:=Layer;
end;

function TCadG32Device.L2S(length: double) : integer;
begin
  Result:=round(Length*FScaleFactor);
end;

procedure TCadG32Device.SetColor(index: integer; const Value: TColor32);
begin
  if (index>=1) and (index<256) then
    FWinColors.Color[index]:=Value;
end;

function TCadG32Device.GetColors(index: integer): TColor32;
begin
  if (index>=1) and (index<256) then
    Result:=FWinColors.Color[index]
  else
    Result:=clBlack;
end;

procedure TCadG32Device.RenderToImage(Image: TImage32);
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
    PointArray : TArrayOfFixedPoint;
    Color32 : TColor32;
    TempPoly : TPolygon32;
begin
  FImageWidth:=FImage32.Width;
  FImageHeight:=FImage32.Height;
  UpdateWindow;


  SetLength(PointArray, 5);

  with Image do
  begin
    Bitmap.BeginUpdate;

    if (FDrawBackground) then
    begin
      Bitmap.Clear(clWhite32);
    end;

    if assigned(CadCanvas) then
    begin
      for i:=CadCanvas.LayerCount-1 downto 0 do
      begin
        Layer:=CadCanvas.Layers[i];

        if Layer.Visible then
        begin
          if (i>=0) and (i<FLayers.Count) then
            begin
              Bitmap.PenColor:=CadCanvas.Layers[i].Color;
            end
          else
            begin
              Bitmap.PenColor:=clGreen32;
            end;

          for j:=0 to Layer.Count-1 do
          begin

            Element:=Layer.Elements[j];

            if Element is TCadPoint then
            begin
              CadPoint:=TCadPoint(Element);
              W2S(CadPoint, sx1, sy1);
              if CadPoint.ColorByLayer then
                Bitmap.PenColor:=FWinColors.Color[Layer.Color]
              else
                Bitmap.PenColor:=FWinColors.Color[CadPoint.Color];

              Bitmap.MoveTo(sx1,sy1-FPointSize);
              Bitmap.LineToAS(sx1,sy1+FPointSize);
              Bitmap.MoveTo(sx1-FPointSize,sy1);
              Bitmap.LineToAS(sx1+FPointSize,sy1);
            end;

            if Element is TCadSimpleLine then
            begin
              SimpleLine:=TCadSimpleLine(Element);
              W2S(SimpleLine.StartPoint, sx1, sy1);
              W2S(SimpleLine.EndPoint, sx2, sy2);
              if SimpleLine.ColorByLayer then
                Bitmap.PenColor:=FWinColors.Color[Layer.Color]
              else
                Bitmap.PenColor:=FWinColors.Color[SimpleLine.Color];
              Bitmap.LineAS(sx1, sy1, sx2, sy2, Bitmap.PenColor, true);
            end;

            if Element is TCadPolyLine then
            begin
              CadPolyLine:=TCadPolyLine(Element);
              if CadPolyline.ColorByLayer then
                Bitmap.PenColor:=FWinColors.Color[Layer.Color]
              else
                Bitmap.PenColor:=FWinColors.Color[CadPolyLine.Color];
              first:=true;
              for k:=0 to CadPolyLine.Count-1 do
              begin
                W2S(CadPolyLine.Points[k], sx1, sy1);
                if first then
                  begin
                    first:=false;
                    Bitmap.MoveTo(sx1, sy1);
                  end
                else
                  Bitmap.LineToAS(sx1, sy1);
              end;
            end;

            if Element is TCadPolygon then
            begin

            end;

            if Element is TCadText then
            begin
              CadText:=TCadText(Element);
              W2S(CadText.Position, sx1, sy1);
              if CadText.ColorByLayer then
                Color32:=FWinColors.Color[Layer.Color]
              else
                Color32:=FWinColors.Color[CadText.Color];
              Bitmap.Font.Name:='Arial';
              Bitmap.Font.Size:=L2S(CadText.Height);
              Bitmap.RenderText(sx1, sy1, CadText.TextString, 4, Color32);
            end;

            if Element is TCadSolid then
            begin
              CadSolid:=TCadSolid(Element);
              FPolygon32.Clear;
              FPolygon32.Closed:=true;
              FPolygon32.AntialiasMode:=am8times;
              FPolygon32.Antialiased:=Self.Antialias;
              W2S(CadSolid.Points[0], sx1, sy1);
              FPolygon32.Add(FixedPoint(sx1, sy1));
              W2S(CadSolid.Points[1], sx1, sy1);
              FPolygon32.Add(FixedPoint(sx1, sy1));
              W2S(CadSolid.Points[2], sx1, sy1);
              FPolygon32.Add(FixedPoint(sx1, sy1));

              if not CadSolid.IsSolid3 then
              begin
                W2S(CadSolid.Points[3], sx1, sy1);
                FPolygon32.Add(FixedPoint(sx1, sy1));
              end;

              GR32.SetGamma(0.2);
              if not SolidOutlines then
                begin
                  if CadSolid.ColorByLayer then
                    Color32:=FWinColors.Color[Layer.Color]
                  else
                    Color32:=FWinColors.Color[CadSolid.Color];

                  FPolygon32.Draw(Bitmap, Color32, Color32);
                  //FPolygon32.DrawFill(Bitmap, Color32);
                  //FPolygon32.DrawEdge(Bitmap, Color32);
                end
              else
                begin
                  if CadSolid.ColorByLayer then
                    Color32:=FWinColors.Color[Layer.Color]
                  else
                    Color32:=FWinColors.Color[CadSolid.Color];

                  FPolygon32.DrawEdge(Bitmap, Color32);
                end;

              GR32.SetGamma(0.7);
            end;
          end;
        end;
      end;
    end;
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
  Image.Repaint;
end;

procedure TCadG32Device.SetMarginX(const Value: integer);
begin
  FMarginX := Value;
  UpdateWindow;
end;

procedure TCadG32Device.SetMarginY(const Value: integer);
begin
  FMarginY := Value;
  UpdateWindow;
end;

procedure TCadG32Device.ClearLayers;
var
    i : integer;
begin
  for i:=0 to FLayers.Count-1 do
    TCadG32Layer(FLayers.Items[i]).Free;

  FLayers.Clear;

  AddLayer;
end;

procedure TCadG32Device.SetSolidOutlines(const Value: boolean);
begin
  FSolidOutlines := Value;
end;

procedure TCadG32Device.SetImage32(const Value: TImage32);
begin
  FImage32 := Value;
  FImage32.OnBitmapResize:=Self.BitmapResize;
  UpdateWindow;
end;

procedure TCadG32Device.BitmapResize(Sender: TObject);
begin
  FImageWidth:=FImage32.Width;
  FImageHeight:=FImage32.Height;
  UpdateWindow;
end;


{ TCadG32Layer }

constructor TCadG32Layer.Create;
begin
  inherited;
  FColor:=clRed32;
end;

destructor TCadG32Layer.Destroy;
begin
  inherited;
end;

{ TCadG32Colors }

constructor TCadG32Colors.Create;
var
    i : integer;
begin
  inherited Create;


  FColors[1] := clBlack32;
  FColors[2] := clMaroon32;
  FColors[3] := clGreen32;
  FColors[4] := clOlive32;
  FColors[5] := clNavy32;
  FColors[6] := clPurple32;
  FColors[7] := clTeal32;
  FColors[8] := clGray32;
  FColors[9] := clBlack32;
//FColors[9] := clSilver32;
  FColors[10] := clRed32;
  FColors[11] := clLime32;
  FColors[12] := clYellow32;
  FColors[13] := clBlue32;
  FColors[14] := clFuchsia32;
  FColors[15] := clAqua32;
  FColors[16] := clBlack32;
//  FColors[16] := clLtGray32;
  FColors[17] := clBlack32;
//  FColors[17] := clDkGray32;
  FColors[18] := clWhite32;

  for i:=1 to 10 do
  begin
    FColors[i]:=GR32.Color32(i*25,i*25,i*25);
  end;

  for i:=19 to 256 do
    FColors[i]:=GR32.Color32(0,0,0);
end;

destructor TCadG32Colors.Destroy;
begin
  inherited;

end;

function TCadG32Colors.GetColor(index: integer): TColor32;
begin
  if (index>=1) and (index<=256) then
    begin
      Result:=FColors[index];
    end
  else
    Result:=clBlack32;
end;

procedure TCadG32Colors.SetColor(index: integer; Color: TColor32);
begin
  if (index>=1) and (index<=256) then
    begin
      FColors[index]:=Color;
    end;
end;

end.
