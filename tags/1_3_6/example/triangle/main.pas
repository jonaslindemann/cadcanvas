unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Triangle, StdCtrls, CadCanvas, CadDxfDevice, ExtCtrls, CadWinDevice,
  CadMesh, Math, ComCtrls, CadG32Device, GR32_Image, DateUtils, Menus, GR32,
  CadSurfaceDiagram2D, ToolWin, Grids, ValEdit;

type
  TfrmMain = class(TForm)
    CadCanvas: TCadCanvas;
    Image: TImage32;
    CadG32Device: TCadG32Device;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    dlgOpen: TOpenDialog;
    CadSurfaceDiagram2D: TCadSurfaceDiagram2D;
    CadDxfDevice: TCadDxfDevice;
    dlgSave: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lblXLimits: TLabel;
    lblMin: TLabel;
    lblMax: TLabel;
    lblYLimits: TLabel;
    edtXMin: TEdit;
    edtXMax: TEdit;
    chkAutoX: TCheckBox;
    chkAutoY: TCheckBox;
    edtYMax: TEdit;
    edtYMin: TEdit;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    chkShowTriangles: TCheckBox;
    AntialiasCheck: TCheckBox;
    TabSheet4: TTabSheet;
    btnTest1: TButton;
    Button2: TButton;
    btnTest3: TButton;
    CoolBar1: TCoolBar;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    AxesProperties: TValueListEditor;
    DiagramProperties: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure chkAutoXClick(Sender: TObject);
    procedure chkAutoYClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnAddXClick(Sender: TObject);
    procedure btnSubtractXClick(Sender: TObject);
    procedure btnAddYClick(Sender: TObject);
    procedure btnSubtractYClick(Sender: TObject);
    procedure chkShowTrianglesClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnTest1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure AntialiasCheckClick(Sender: TObject);
    procedure btnTest3Click(Sender: TObject);
    procedure AxesPropertiesStringsChange(Sender: TObject);
  private
    { Private declarations }
    procedure FillValueList;
    procedure RetrieveFromValueList;
    procedure UpdateDiagram;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;
  DecimalSeparator := '.';
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  Image.SetupBitmap;
  CadG32Device.ZoomExtent;
  CadG32Device.Render;
end;

procedure TfrmMain.mnuFileOpenClick(Sender: TObject);
var
  InputFile: TextFile;
  ContourFile : TextFile;
  x, y, value, offset: double;
  i: integer;
  MainFilename : string;
  ContourFilename : string;
begin

  if not dlgOpen.Execute then
    exit;

  // Clear diagram

  CadSurfaceDiagram2D.Clear;

  // Read values from file

  MainFilename:=dlgOpen.FileName;
  ContourFilename:=ExtractFilePath(MainFilename)+StringReplace(ExtractFilename(MainFilename),ExtractFileExt(MainFilename),'.lvl',[rfReplaceAll, rfIgnoreCase]);

  AssignFile(InputFile, dlgOpen.FileName);
  Reset(InputFile);

  readln(InputFile);

  CadSurfaceDiagram2D.OffsetContour.Clear;

  while not eof(InputFile) do
  begin
    readln(InputFile, x, y, value);
    CadSurfaceDiagram2D.AddPoint(x, -y, value);
  end;

  CloseFile(InputFile);

  if FileExists(ContourFilename) then
  begin

    CadSurfaceDiagram2D.OffsetContour.Clear;

    AssignFile(ContourFile, ContourFilename);
    Reset(ContourFile);

    while not eof(ContourFile) do
    begin
      readln(ContourFile, x, offset);
      CadSurfaceDiagram2D.OffsetContour.Add(x, offset);
    end;

    CloseFile(ContourFile);
  end;

  // Set surface diagram options

  CadSurfaceDiagram2D.Axes.TickDistanceX := 50;
  CadSurfaceDiagram2D.Axes.TickDistanceY := 20;
  CadSurfaceDiagram2D.Axes.TickLabelFormat := '%.0f';
  CadSurfaceDiagram2D.Diagram.LegendFormat := '%.0f';
  CadSurfaceDiagram2D.Diagram.LegendSize := 1.0;

  // Set the desited delaunay triangle implementation

  //CadSurfaceDiagram2D.Mesh.TriangleExecutable :=
  //  '..\..\depends\triangle\bin\triangle.exe';
  //CadSurfaceDiagram2D.Mesh.TriangleImplementation := tiExternal;

  CadSurfaceDiagram2D.Mesh.TriangleImplementation:=tiInternal;

  // Define iso line setup

  CadSurfaceDiagram2D.IsoLines.Size := 13;

  // No automatic updating of isolines when assigning max or min properties

  CadSurfaceDiagram2D.IsoLines.AutoUpdate := true;

  // Change iso line values (zero-based index)

  value := 2000;

  for i := 0 to CadSurfaceDiagram2D.IsoLines.Size - 1 do
  begin
    CadSurfaceDiagram2D.IsoLines.Values[i] := value;
    value := value + 250;
  end;

  // Set Meshing parameters. No autocalculation of max/min

  CadSurfaceDiagram2D.Mesh.AutoLimits := true;

  // Add surface contour for clipping

  CadSurfaceDiagram2D.Mesh.ClipSurface := false;
  CadSurfaceDiagram2D.Mesh.ClipSurfaceType := scRemoveCrossing;

  // CadSurfaceDiagram2D.SurfaceContour.Add(0, -100);
  // CadSurfaceDiagram2D.SurfaceContour.Add(500, -80);
  // CadSurfaceDiagram2D.SurfaceContour.Add(1100, -20);

  // Define device colors

  // Isoline    1   2   3   4   5   6   7   8   9    10   11   12   13
  // Color    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14

  CadG32Device.WindowColor[1] := GR32.Color32(0, 0, 128);
  CadG32Device.WindowColor[2] := GR32.Color32(0, 32, 192);
  CadG32Device.WindowColor[3] := GR32.Color32(0, 100, 112);
  CadG32Device.WindowColor[4] := GR32.Color32(0, 128, 76);
  CadG32Device.WindowColor[5] := GR32.Color32(0, 152, 52);
  CadG32Device.WindowColor[6] := GR32.Color32(172, 152, 0);
  CadG32Device.WindowColor[7] := GR32.Color32(255, 176, 0);
  CadG32Device.WindowColor[8] := GR32.Color32(255, 128, 0);
  CadG32Device.WindowColor[9] := GR32.Color32(255, 76, 0);
  CadG32Device.WindowColor[10] := GR32.Color32(240, 0, 0);
  CadG32Device.WindowColor[11] := GR32.Color32(192, 0, 0);
  CadG32Device.WindowColor[12] := GR32.Color32(168, 0, 0);
  CadG32Device.WindowColor[13] := GR32.Color32(168 - 24, 0, 0);
  CadG32Device.WindowColor[14] := GR32.Color32(128, 0, 0);

  // Create Diagram

  CadSurfaceDiagram2D.Execute;

  // Draw diagram

  CadG32Device.ZoomExtent;
  CadG32Device.Render;

  edtXMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxX);
  edtYMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxY);
  edtXMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinX);
  edtYMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinY);

  FillValueList;
end;

procedure TfrmMain.FillValueList;
begin
  AxesProperties.Values['LabelDistance']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelDistance);
  AxesProperties.Values['LabelDistanceX']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelDistanceX);
  AxesProperties.Values['LabelDistanceY']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelDistanceY);
  AxesProperties.Values['LabelSize']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelSize);
  AxesProperties.Values['LabelUnitSize']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelUnitSize);
  AxesProperties.Values['LabelUnitSpacing']:=FloatToStr(CadSurfaceDiagram2D.Axes.LabelUnitSpacing);
  AxesProperties.Values['LabelUnitX']:=CadSurfaceDiagram2D.Axes.LabelUnitX;
  AxesProperties.Values['LabelUnitY']:=CadSurfaceDiagram2D.Axes.LabelUnitY;
  AxesProperties.Values['LabelX']:=CadSurfaceDiagram2D.Axes.LabelX;
  AxesProperties.Values['LabelY']:=CadSurfaceDiagram2D.Axes.LabelY;
  AxesProperties.Values['TickDistanceX']:=FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceX);
  AxesProperties.Values['TickDistanceY']:=FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceY);
  AxesProperties.Values['TickLabelDistance']:=FloatToStr(CadSurfaceDiagram2D.Axes.TickLabelDistance);
  AxesProperties.Values['TickLabelFormat']:=CadSurfaceDiagram2D.Axes.TickLabelFormat;
  AxesProperties.Values['TickLabelSize']:=FloatToStr(CadSurfaceDiagram2D.Axes.TickLabelSize);
  AxesProperties.Values['TickSize']:=FloatToStr(CadSurfaceDiagram2D.Axes.TickSize);
  AxesProperties.Values['TicksTop']:=BoolToStr(CadSurfaceDiagram2D.Axes.TicksTop);
  AxesProperties.Values['TicksBottom']:=BoolToStr(CadSurfaceDiagram2D.Axes.TicksBottom);
  AxesProperties.Values['TicksLeft']:=BoolToStr(CadSurfaceDiagram2D.Axes.TicksLeft);
  AxesProperties.Values['TicksRight']:=BoolToStr(CadSurfaceDiagram2D.Axes.TicksRight);

  DiagramProperties.Values['Title']:=CadSurfaceDiagram2D.Diagram.Title;
  DiagramProperties.Values['TitleSize']:=FloatToStr(CadSurfaceDiagram2D.Diagram.TitleSize);
  DiagramProperties.Values['SubTitle']:=CadSurfaceDiagram2D.Diagram.SubTitle;
  DiagramProperties.Values['SubTitleSize']:=FloatToStr(CadSurfaceDiagram2D.Diagram.SubTitleSize);
  DiagramProperties.Values['TitleSpacing']:=FloatToStr(CadSurfaceDiagram2D.Diagram.TitleSpacing);
  DiagramProperties.Values['TitleDistance']:=FloatToStr(CadSurfaceDiagram2D.Diagram.TitleDistance);
  DiagramProperties.Values['ShowLegend']:=BoolToStr(CadSurfaceDiagram2D.Diagram.ShowLegend);
  DiagramProperties.Values['LegendDistance']:=FloatToStr(CadSurfaceDiagram2D.Diagram.LegendDistance);
  DiagramProperties.Values['LegendHeight']:=FloatToStr(CadSurfaceDiagram2D.Diagram.LegendHeight);
  DiagramProperties.Values['LegendTextHeight']:=FloatToStr(CadSurfaceDiagram2D.Diagram.LegendTextHeight);
  DiagramProperties.Values['LegendTextDistance']:=FloatToStr(CadSurfaceDiagram2D.Diagram.LegendTextDistance);
  DiagramProperties.Values['LegendFormat']:=CadSurfaceDiagram2D.Diagram.LegendFormat;
  DiagramProperties.Values['VerticalExaggeration']:=FloatToStr(CadSurfaceDiagram2D.Diagram.VerticalExaggeration);
  DiagramProperties.Values['LegendSize']:=FloatToStr(CadSurfaceDiagram2D.Diagram.LegendSize);
end;

procedure TfrmMain.RetrieveFromValueList;
begin
  CadSurfaceDiagram2D.Axes.LabelDistance:=StrToFloat(AxesProperties.Values['LabelDistance']);
  CadSurfaceDiagram2D.Axes.LabelDistanceX:=StrToFloat(AxesProperties.Values['LabelDistanceX']);
  CadSurfaceDiagram2D.Axes.LabelDistanceY:=StrToFloat(AxesProperties.Values['LabelDistanceY']);
  CadSurfaceDiagram2D.Axes.LabelSize:=StrToFloat(AxesProperties.Values['LabelSize']);
  CadSurfaceDiagram2D.Axes.LabelUnitSize:=StrToFloat(AxesProperties.Values['LabelUnitSize']);
  CadSurfaceDiagram2D.Axes.LabelUnitSpacing:=StrToFloat(AxesProperties.Values['LabelUnitSpacing']);
  CadSurfaceDiagram2D.Axes.LabelUnitX:=AxesProperties.Values['LabelUnitX'];
  CadSurfaceDiagram2D.Axes.LabelUnitY:=AxesProperties.Values['LabelUnitY'];
  CadSurfaceDiagram2D.Axes.LabelX:=AxesProperties.Values['LabelX'];
  CadSurfaceDiagram2D.Axes.LabelY:=AxesProperties.Values['LabelY'];
  CadSurfaceDiagram2D.Axes.TickDistanceX:=StrToFloat(AxesProperties.Values['TickDistanceX']);
  CadSurfaceDiagram2D.Axes.TickDistanceY:=StrToFloat(AxesProperties.Values['TickDistanceY']);
  CadSurfaceDiagram2D.Axes.TickLabelDistance:=StrToFloat(AxesProperties.Values['TickLabelDistance']);
  CadSurfaceDiagram2D.Axes.TickLabelFormat:=AxesProperties.Values['TickLabelFormat'];
  CadSurfaceDiagram2D.Axes.TickLabelSize:=StrToFloat(AxesProperties.Values['TickLabelSize']);
  CadSurfaceDiagram2D.Axes.TickSize:=StrToFloat(AxesProperties.Values['TickSize']);
  CadSurfaceDiagram2D.Axes.TicksTop:=StrToBool(AxesProperties.Values['TicksTop']);
  CadSurfaceDiagram2D.Axes.TicksBottom:=StrToBool(AxesProperties.Values['TicksBottom']);
  CadSurfaceDiagram2D.Axes.TicksLeft:=StrToBool(AxesProperties.Values['TicksLeft']);
  CadSurfaceDiagram2D.Axes.TicksRight:=StrToBool(AxesProperties.Values['TicksRight']);

  CadSurfaceDiagram2D.Diagram.Title:=DiagramProperties.Values['Title'];
  CadSurfaceDiagram2D.Diagram.TitleSize:=StrToFloat(DiagramProperties.Values['TitleSize']);
  CadSurfaceDiagram2D.Diagram.SubTitle:=DiagramProperties.Values['SubTitle'];
  CadSurfaceDiagram2D.Diagram.SubTitleSize:=StrToFloat(DiagramProperties.Values['SubTitleSize']);
  CadSurfaceDiagram2D.Diagram.TitleSpacing:=StrToFloat(DiagramProperties.Values['TitleSpacing']);
  CadSurfaceDiagram2D.Diagram.TitleDistance:=StrToFloat(DiagramProperties.Values['TitleDistance']);
  CadSurfaceDiagram2D.Diagram.ShowLegend:=StrToBool(DiagramProperties.Values['ShowLegend']);
  CadSurfaceDiagram2D.Diagram.LegendDistance:=StrToFloat(DiagramProperties.Values['LegendDistance']);
  CadSurfaceDiagram2D.Diagram.LegendHeight:=StrToFloat(DiagramProperties.Values['LegendHeight']);
  CadSurfaceDiagram2D.Diagram.LegendTextHeight:=StrToFloat(DiagramProperties.Values['LegendTextHeight']);
  CadSurfaceDiagram2D.Diagram.LegendTextDistance:=StrToFloat(DiagramProperties.Values['LegendTextDistance']);
  CadSurfaceDiagram2D.Diagram.LegendFormat:=DiagramProperties.Values['LegendFormat'];
  CadSurfaceDiagram2D.Diagram.VerticalExaggeration:=StrToFloat(DiagramProperties.Values['VerticalExaggeration']);
  CadSurfaceDiagram2D.Diagram.LegendSize:=StrToFloat(DiagramProperties.Values['LegendSize']);
end;

procedure TfrmMain.UpdateDiagram;
begin

  RetrieveFromValueList;

  CadSurfaceDiagram2D.Axes.MinX := StrToFloat(edtXMin.Text);
  CadSurfaceDiagram2D.Axes.MinY := StrToFloat(edtYMin.Text);
  CadSurfaceDiagram2D.Axes.MaxX := StrToFloat(edtXMax.Text);
  CadSurfaceDiagram2D.Axes.MaxY := StrToFloat(edtYMax.Text);

  CadSurfaceDiagram2D.Execute;

  CadG32Device.ZoomExtent;
  CadG32Device.Render;
end;

procedure TfrmMain.chkAutoXClick(Sender: TObject);
begin
  Self.CadSurfaceDiagram2D.Axes.AutoX := chkAutoX.Checked;
  if chkAutoX.Checked then
  begin
    edtXMax.Enabled := false;
    edtXMin.Enabled := false;
    edtXMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxX);
    edtXMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinX);
  end
  else
  begin
    edtXMax.Enabled := true;
    edtXMin.Enabled := true;
    edtXMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxX);
    edtXMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinX);
  end;
end;

procedure TfrmMain.chkAutoYClick(Sender: TObject);
begin
  CadSurfaceDiagram2D.Axes.AutoY := chkAutoY.Checked;
  if chkAutoY.Checked then
  begin
    edtYMax.Enabled := false;
    edtYMin.Enabled := false;
    edtYMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxY);
    edtYMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinY);
  end
  else
  begin
    edtYMax.Enabled := true;
    edtYMin.Enabled := true;
    edtYMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxY);
    edtYMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinY);
  end;
end;

procedure TfrmMain.btnUpdateClick(Sender: TObject);
begin
  UpdateDiagram;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if assigned(Image) then
  begin
    if dlgSave.Execute() then
    begin
      Self.CadG32Device.SaveToFile(dlgSave.FileName);
    end;
  end;
end;

procedure TfrmMain.AntialiasCheckClick(Sender: TObject);
begin
  CadG32Device.Antialias:=AntialiasCheck.Checked;
  CadG32Device.ZoomExtent;
  CadG32Device.Render;
end;

procedure TfrmMain.AxesPropertiesStringsChange(Sender: TObject);
begin
  //UpdateDiagram;
end;

procedure TfrmMain.btnAddXClick(Sender: TObject);
var
  value: double;
begin
  value := StrToFloat(edtXMin.Text);
  value := value + 1;
  edtXMin.Text := FloatToStr(value);

  value := StrToFloat(edtXMax.Text);
  value := value + 1;
  edtXMax.Text := FloatToStr(value);

  btnUpdateClick(Self);
end;

procedure TfrmMain.btnSubtractXClick(Sender: TObject);
var
  value: double;
begin
  value := StrToFloat(edtXMin.Text);
  value := value - 1;
  edtXMin.Text := FloatToStr(value);

  value := StrToFloat(edtXMax.Text);
  value := value - 1;
  edtXMax.Text := FloatToStr(value);

  btnUpdateClick(Self);
end;

procedure TfrmMain.btnAddYClick(Sender: TObject);
var
  value: double;
begin
  value := StrToFloat(edtYMin.Text);
  value := value + 1;
  edtYMin.Text := FloatToStr(value);

  value := StrToFloat(edtYMax.Text);
  value := value + 1;
  edtYMax.Text := FloatToStr(value);

  btnUpdateClick(Self);
end;

procedure TfrmMain.btnSubtractYClick(Sender: TObject);
var
  value: double;
begin
  value := StrToFloat(edtYMin.Text);
  value := value - 1;
  edtYMin.Text := FloatToStr(value);

  value := StrToFloat(edtYMax.Text);
  value := value - 1;
  edtYMax.Text := FloatToStr(value);

  btnUpdateClick(Self);
end;

procedure TfrmMain.chkShowTrianglesClick(Sender: TObject);
begin
  CadSurfaceDiagram2D.Diagram.ShowTriangles := chkShowTriangles.Checked;
  CadG32Device.ZoomExtent;
  CadG32Device.Render;
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
begin
  CadDxfDevice.Render;
end;

procedure TfrmMain.btnTest1Click(Sender: TObject);
var
  x, y, dx, dy: double;
  rows, cols: double;
begin
  // Clear diagram

  CadSurfaceDiagram2D.Clear;

  // Read values from file

  rows := 20;
  cols := 20;

  x := -2 * pi;
  y := -2 * pi;
  dx := 4 * pi / cols;
  dy := 4 * pi / rows;

  while y <= 2 * pi do
  begin
    while x <= 2 * pi do
    begin
      CadSurfaceDiagram2D.AddPoint(x, y, sin(sqrt(x*x+y*y)));
      x := x + dx;
    end;
    x := -2 * pi;
    y := y + dy;
  end;

  // Create a surface contour

  CadSurfaceDiagram2D.OffsetContour.Clear;
  CadSurfaceDiagram2D.OffsetContour.Add(-2*pi, 0);
  CadSurfaceDiagram2D.OffsetContour.Add(0, 0.5);
  CadSurfaceDiagram2D.OffsetContour.Add(2*pi, 0);

  // Define iso line setup

  CadSurfaceDiagram2D.IsoLines.Size := 13;

  // No automatic updating of isolines when assigning max or min properties

  CadSurfaceDiagram2D.IsoLines.AutoUpdate := true;
  CadSurfaceDiagram2D.Mesh.AutoLimits := true;
  CadSurfaceDiagram2D.Mesh.AlignWithSurface := true;

  // Define device colors

  // Isoline    1   2   3   4   5   6   7   8   9    10   11   12   13
  // Color    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14

  CadG32Device.WindowColor[1] := GR32.Color32(255, 0, 0);
  CadG32Device.WindowColor[2] := GR32.Color32(0, 32, 192);
  CadG32Device.WindowColor[3] := GR32.Color32(0, 100, 112);
  CadG32Device.WindowColor[4] := GR32.Color32(0, 128, 76);
  CadG32Device.WindowColor[5] := GR32.Color32(0, 152, 52);
  CadG32Device.WindowColor[6] := GR32.Color32(172, 152, 0);
  CadG32Device.WindowColor[7] := GR32.Color32(255, 176, 0);
  CadG32Device.WindowColor[8] := GR32.Color32(255, 128, 0);
  CadG32Device.WindowColor[9] := GR32.Color32(255, 76, 0);
  CadG32Device.WindowColor[10] := GR32.Color32(240, 0, 0);
  CadG32Device.WindowColor[11] := GR32.Color32(192, 0, 0);
  CadG32Device.WindowColor[12] := GR32.Color32(168, 0, 0);
  CadG32Device.WindowColor[13] := GR32.Color32(168 - 24, 0, 0);
  CadG32Device.WindowColor[14] := GR32.Color32(0, 255, 0);

  // Create Diagram

  CadSurfaceDiagram2D.Execute;

  // Draw diagram

  CadG32Device.ZoomExtent;
  CadG32Device.Render;

  edtXMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxX);
  edtYMax.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MaxY);
  edtXMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinX);
  edtYMin.Text := FloatToStr(CadSurfaceDiagram2D.Axes.MinY);

end;

procedure TfrmMain.btnTest3Click(Sender: TObject);
var
    T : TTriangleElement;
    TP1 : TTrianglePoint;
    TP2 : TTrianglePoint;
    TP3 : TTrianglePoint;

    TL : TTriangleLine;

    RL : TRefList;
    i : integer;

    DT : TTriangleElement;
    CE : TCadSolid;
begin
  CadCanvas.Clear;

  T:=TTriangleElement.Create;
  TP1:=TTrianglePoint.Create;
  TP2:=TTrianglePoint.Create;
  TP3:=TTrianglePoint.Create;
  TL:=TTriangleLine.Create;

  T.Point[1]:=TP3;
  T.Point[2]:=TP1;
  T.Point[3]:=TP2;

  T.Point[1].X:=0.0;
  T.Point[1].Y:=0.5;
  T.Point[2].X:=1.0;
  T.Point[2].Y:=0.0;
  T.Point[3].X:=1.0;
  T.Point[3].Y:=1.0;

  TL.StartPoint.X:=0.1;
  TL.StartPoint.Y:=1e300;
  TL.EndPoint.X:=0.1;
  TL.EndPoint.Y:=-1e300;

  RL:=T.Split(TL);

  if assigned(RL) then
  begin
    for i := 0 to RL.Count - 1 do
    begin
      DT:=TTriangleElement(RL.Items[i]);
      CadCanvas.MoveTo(DT.Point[1].X, DT.Point[1].Y);
      CadCanvas.LineTo(DT.Point[2].X, DT.Point[2].Y);
      CadCanvas.LineTo(DT.Point[3].X, DT.Point[3].Y);
      CadCanvas.LineTo(DT.Point[1].X, DT.Point[1].Y);
    end;
  end;

  TL.Free;
  RL.Free;
  T.Free;

  CadG32Device.ZoomExtent;
  CadG32Device.Render;

end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

end.
