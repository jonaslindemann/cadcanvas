program CanvasTest;

uses
  Forms,
  main in 'main.pas' {frmMain},
  CadCanvas in '..\src\CadCanvas.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
