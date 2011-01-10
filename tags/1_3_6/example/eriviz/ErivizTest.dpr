program ErivizTest;

uses
  Forms,
  main in 'main.pas' {Form1},
  EriViz in '..\..\src\EriViz.pas',
  FenceForm in 'FenceForm.pas' {FenceEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFenceEditor, FenceEditor);
  Application.Run;
end.
