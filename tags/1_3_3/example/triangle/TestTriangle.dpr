program TestTriangle;

uses
  Forms,
  main in 'main.pas' {frmMain},
  CadMesh in '..\..\src\CadMesh.pas',
  CadG32Device in '..\..\src\CadG32Device.pas',
  Triangle in '..\..\src\Triangle.pas',
  FastGEO in '..\..\depends\fastgeo\FastGEO.pas',
  CadSurfaceDiagram2D in '..\..\src\CadSurfaceDiagram2D.pas',
  CadCanvas in '..\..\src\CadCanvas.pas',
  EriViz in '..\..\src\EriViz.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
