unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, EriViz, StdCtrls;

type
  TForm1 = class(TForm)
    EriViz: TEriViz;
    ReadButton: TButton;
    WriteButton: TButton;
    procedure ReadButtonClick(Sender: TObject);
    procedure WriteButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ReadButtonClick(Sender: TObject);
var
  EriVizFile : TEriVizFile;
begin
  EriVizFile:=TEriVizFile.Create;
  EriVizFile.Filename:='..\..\examples\salix01.fnc';
  EriVizFile.Load;
  EriVizFile.Free;
end;

procedure TForm1.WriteButtonClick(Sender: TObject);
var
  EriVizFile : TEriVizFile;
begin
  EriVizFile:=TEriVizFile.Create;
  EriVizFile.Filename:='..\..\examples\salix01-new.fnc';
  EriVizFile.Save;
  EriVizFile.Free;
end;

end.
