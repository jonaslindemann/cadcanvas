unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Triangle, StdCtrls, CadCanvas, CadDxfDevice, ExtCtrls, CadWinDevice,
  CadMesh, Math, ComCtrls, CadG32Device, GR32_Image, DateUtils, Menus, GR32,
  CadSurfaceDiagram2D;

type
  TfrmMain = class(TForm)
    CadCanvas: TCadCanvas;
    Image: TImage32;
    CadG32Device: TCadG32Device;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    dlgOpen: TOpenDialog;
    Panel1: TPanel;
    lblXLimits: TLabel;
    lblYLimits: TLabel;
    lblMin: TLabel;
    lblMax: TLabel;
    edtXMin: TEdit;
    edtXMax: TEdit;
    edtYMin: TEdit;
    edtYMax: TEdit;
    chkAutoX: TCheckBox;
    chkAutoY: TCheckBox;
    btnUpdate: TButton;
    edtTickX: TEdit;
    lblTickX: TLabel;
    edtTickY: TEdit;
    lblTickY: TLabel;
    btnAddX: TButton;
    btnSubtractX: TButton;
    btnAddY: TButton;
    btnSubtractY: TButton;
    chkShowTriangles: TCheckBox;
    CadSurfaceDiagram2D: TCadSurfaceDiagram2D;
    edtVerticalExaggeration: TEdit;
    edtTickLabelDistance: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtTickLabelSize: TEdit;
    Label3: TLabel;
    CadDxfDevice: TCadDxfDevice;
    btnExport: TButton;
    btnTest1: TButton;
    Button1: TButton;
    dlgSave: TSaveDialog;
    Button2: TButton;
    AntialiasCheck: TCheckBox;
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
    procedure Button2Click(Sender: TObject);
    procedure AntialiasCheckClick(Sender: TObject);
  private
    { Private declarations }
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

  CadSurfaceDiagram2D.Mesh.TriangleExecutable :=
    '..\..\depends\triangle\bin\triangle.exe';
  CadSurfaceDiagram2D.Mesh.TriangleImplementation := tiExternal;

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
  edtTickX.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceX);
  edtTickY.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceY);
  edtTickLabelDistance.Text := FloatToStr
    (CadSurfaceDiagram2D.Axes.TickLabelDistance);
  edtTickLabelSize.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickLabelSize);
  edtVerticalExaggeration.Text := FloatToStr
    (CadSurfaceDiagram2D.Diagram.VerticalExaggeration);
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
  CadSurfaceDiagram2D.Axes.MinX := StrToFloat(edtXMin.Text);
  CadSurfaceDiagram2D.Axes.MinY := StrToFloat(edtYMin.Text);
  CadSurfaceDiagram2D.Axes.MaxX := StrToFloat(edtXMax.Text);
  CadSurfaceDiagram2D.Axes.MaxY := StrToFloat(edtYMax.Text);
  CadSurfaceDiagram2D.Axes.TickDistanceX := StrToFloat(edtTickX.Text);
  CadSurfaceDiagram2D.Axes.TickDistanceY := StrToFloat(edtTickY.Text);
  CadSurfaceDiagram2D.Axes.TickLabelDistance := StrToFloat
    (edtTickLabelDistance.Text);
  CadSurfaceDiagram2D.Axes.TickLabelSize := StrToFloat(edtTickLabelSize.Text);
  CadSurfaceDiagram2D.Diagram.VerticalExaggeration := StrToFloat
    (edtVerticalExaggeration.Text);

  CadSurfaceDiagram2D.Execute;

  CadG32Device.ZoomExtent;
  CadG32Device.Render;
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

procedure TfrmMain.Button2Click(Sender: TObject);
var
  x, y, dx, dy: double;
  rows, cols: double;
begin
  // Clear diagram

  CadSurfaceDiagram2D.Clear;

  // Create a surface contour

  CadSurfaceDiagram2D.SurfaceContour.Clear;
  CadSurfaceDiagram2D.SurfaceContour.Add(0.4, 0.5);
  CadSurfaceDiagram2D.SurfaceContour.Add(0.6, 0.5);

  // Create test case

  CadSurfaceDiagram2D.AddPoint(0.0, 0.0, 0.0);
  CadSurfaceDiagram2D.AddPoint(1.0, 0.0, 0.0);
  CadSurfaceDiagram2D.AddPoint(0.5, 1.0 * sqrt(3) * 0.5, 0.0);

  // Define iso line setup

  CadSurfaceDiagram2D.IsoLines.Size := 13;

  // No automatic updating of isolines when assigning max or min properties

  CadSurfaceDiagram2D.IsoLines.AutoUpdate := true;
  CadSurfaceDiagram2D.Mesh.AutoLimits := true;

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
  edtTickX.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceX);
  edtTickY.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceY);
  edtTickLabelDistance.Text := FloatToStr
    (CadSurfaceDiagram2D.Axes.TickLabelDistance);
  edtTickLabelSize.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickLabelSize);
  edtVerticalExaggeration.Text := FloatToStr
    (CadSurfaceDiagram2D.Diagram.VerticalExaggeration);

end;

procedure TfrmMain.AntialiasCheckClick(Sender: TObject);
begin
  CadG32Device.Antialias:=AntialiasCheck.Checked;
  CadG32Device.ZoomExtent;
  CadG32Device.Render;
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
  edtTickX.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceX);
  edtTickY.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickDistanceY);
  edtTickLabelDistance.Text := FloatToStr
    (CadSurfaceDiagram2D.Axes.TickLabelDistance);
  edtTickLabelSize.Text := FloatToStr(CadSurfaceDiagram2D.Axes.TickLabelSize);
  edtVerticalExaggeration.Text := FloatToStr
    (CadSurfaceDiagram2D.Diagram.VerticalExaggeration);

end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

end.
