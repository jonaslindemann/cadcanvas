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
    EditorButton: TButton;
    procedure ReadButtonClick(Sender: TObject);
    procedure WriteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditorButtonClick(Sender: TObject);
  private
    { Private declarations }
    FEriVizFile : TEriVizFile;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses FenceForm;

{$R *.dfm}

procedure TForm1.EditorButtonClick(Sender: TObject);
begin
  EriViz.Execute;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FEriVizFile.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEriVizFile:=TEriVizFile.Create;
end;

procedure TForm1.ReadButtonClick(Sender: TObject);
begin
  FEriVizFile.Filename:='salix01.fnc';
  FEriVizFile.Load;
end;

procedure TForm1.WriteButtonClick(Sender: TObject);
begin
  FEriVizFile.Filename:='salix01-new.fnc';
  FEriVizFile.Save;
end;

end.
