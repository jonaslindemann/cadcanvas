unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CadCanvas, CadWinDevice, StdCtrls, CadDxfDevice, ToolWin, ComCtrls;

type
  TfrmMain = class(TForm)
    CadWinDevice: TCadWinDevice;
    CadCanvas: TCadCanvas;
    CadDxfDevice: TCadDxfDevice;
    ToolBar1: TToolBar;
    btnCreate: TButton;
    btnDraw: TButton;
    btnExportDxf: TButton;
    btnExportWMF: TButton;
    btnTest1: TButton;
    btnTest2: TButton;
    procedure btnCreateClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure btnExportDxfClick(Sender: TObject);
    procedure btnExportWMFClick(Sender: TObject);
    procedure btnTest1Click(Sender: TObject);
    procedure btnTest2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.btnCreateClick(Sender: TObject);
var
    x, y : double;
    FunctionLayer : TCadLayer;
    FunctionWinLayer : TCadWinLayer;
    AxisLayer : TCadLayer;
    AxisWinLayer : TCadWinLayer;
    TextLayer : TCadLayer;
    SolidLayer : TCadLayer;
    w,h : double;
begin

  // Setup CadWinDevice layers (maps to CadCanvas layers)
  // (Optional)

  CadWinDevice.ClearLayers;
  CadWinDevice.WindowColor[2]:=clRed;

  FunctionWinLayer:=CadWinDevice.AddLayer;
  FunctionWinLayer.Pen.Width:=2;

  AxisWinLayer:=CadWinDevice.AddLayer;
  AxisWinLayer.Pen.Style:=psDash;
  AxisWinLayer.Pen.Width:=1;

  // Setup up CadCanvas

  CadCanvas.Clear;

  // Setup layers

  FunctionLayer:=CadCanvas.AddLayer('FUNCTION');
  AxisLayer:=CadCanvas.AddLayer('AXIS');
  AxisLayer.Color:=2;
  TextLayer:=CadCanvas.AddLayer('TEXT');
  SolidLayer:=CadCanvas.AddLayer('SOLID');

  // Draw function

  CadCanvas.CurrentLayer:=FunctionLayer;

  x:=0;

  CadCanvas.BeginPolyLine;
  while (x<4*pi) do
  begin
    y:=sin(x+sin(x));
    CadCanvas.AddPoint(x, y);
    x:=x+0.05;
  end;
  CadCanvas.EndPolyLine;

  // Draw axis

  CadCanvas.CurrentLayer:=AxisLayer;

  CadCanvas.MoveTo(0,1.0);
  CadCanvas.LineTo(0,-1.0);
  CadCanvas.MoveTo(0,0);
  CadCanvas.LineTo(4*pi,0);

  // Draw a title

  CadCanvas.CurrentLayer:=TextLayer;

  CadCanvas.TextHeight:=0.3;
  CadCanvas.TextRotation:=45.0;
  CadCanvas.TextOut(4*pi/2, 0.0, 'Diagram');
  CadCanvas.TextRotation:=0.0;

  CadCanvas.MoveTo(2*pi,1.0);
  CadCanvas.LineTo(2*pi,-1.0);
  CadCanvas.TextTweakFactor:=0.6; // WMF
  //CadCanvas.TextTweakFactor:=0.4; // DXF
  W:=CadCanvas.TextSizeX('Centered text');
  H:=CadCanvas.TextSizeY('Centered text');
  CadCanvas.TextOut(2*pi-W/2.0,1.0-H/2,'Centered text');

  CadCanvas.CurrentLayer:=SolidLayer;

  CadCanvas.CurrentColor:=15;
  CadCanvas.Solid4(0.5, 0.5, 0.75, 0.5, 0.5, 0.75, 0.75, 0.75);

end;

procedure TfrmMain.btnDrawClick(Sender: TObject);
begin

  // Draw on device

  CadWinDevice.ZoomExtent;
  CadWinDevice.Render;
end;

procedure TfrmMain.btnExportDxfClick(Sender: TObject);
begin
  CadDxfDevice.FileName:='CadDxfDevice.dxf';
  CadDxfDevice.Render;
end;

procedure TfrmMain.btnExportWMFClick(Sender: TObject);
begin
  CadWinDevice.FileName:='CadWinDevice.emf';
  CadWinDevice.RenderWMF;
end;

procedure TfrmMain.btnTest1Click(Sender: TObject);
var
    x, y : double;
    TestLayer : TCadLayer;
    TestWinLayer : TCadWinLayer;
begin

  // Setup CadWinDevice layers (maps to CadCanvas layers)
  // (Optional)

  CadWinDevice.ClearLayers;

  TestWinLayer:=CadWinDevice.AddLayer;
  TestWinLayer.Pen.Width:=3;

  // Setup CadCanvas

  CadCanvas.Clear;

  // Setup layers

  TestLayer:=CadCanvas.AddLayer('TEST');
  TestLayer.Color:=4;

  // Draw function

  CadCanvas.CurrentLayer:=TestLayer;

  CadCanvas.MoveTo(0.0,0.0);
  CadCanvas.LineTo(1.0,0.0);
  CadCanvas.LineTo(1.0,4.0);
  CadCanvas.LineTo(0.0,4.0);
  CadCanvas.LineTo(0.0,0.0);

end;

procedure TfrmMain.btnTest2Click(Sender: TObject);
var
    x, y : double;
    TestLayer : TCadLayer;
    TestWinLayer : TCadWinLayer;
begin

  // Setup CadWinDevice layers (maps to CadCanvas layers)
  // (Optional)

  CadWinDevice.ClearLayers;

  TestWinLayer:=CadWinDevice.AddLayer;
  TestWinLayer.Pen.Width:=3;

  // Setup CadCanvas

  CadCanvas.Clear;

  // Setup layers

  TestLayer:=CadCanvas.AddLayer('TEST');
  TestLayer.Color:=2;

  // Draw function

  CadCanvas.CurrentLayer:=TestLayer;

  CadCanvas.MoveTo(0.0,0.0);
  CadCanvas.LineTo(4.0,0.0);
  CadCanvas.LineTo(4.0,1.0);
  CadCanvas.LineTo(0.0,1.0);
  CadCanvas.LineTo(0.0,0.0);

end;


end.
