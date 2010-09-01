unit CadDxfDevice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CadCanvas;

type
  TCadDxfDevice = class(TComponent)
  private
    { Private declarations }
    FCadCanvas : TCadCanvas;
    FFileName : string;
    FDxfFile : TextFile;
    FFileOpen : boolean;
    FPartial : boolean;

    FSolidOutlines : boolean;

    //FLayers : TList;
  protected
    { Protected declarations }
    procedure DxfTag(number : integer; value : string);
    procedure DxfInt(number : integer; value : integer);
    procedure DxfFloat(number : integer; value : double);

    procedure DxfBeginSection(value : string);
    procedure DxfEndSection;

    procedure DxfBeginTables;
    procedure DxfEndTables;

    procedure DxfBeginTable(value : string);
    procedure DxfEndTable;

    procedure DxfBeginLayer;
    procedure DxfEndLayer;

    procedure DxfBeginEntitites;
    procedure DxfEndEntitites;

    procedure DxfColor(number : integer);

    procedure DxfLineType(name : string);

    procedure DxfLine;
    procedure DxfLayer(value : string);
    procedure DxfFirstPoint(x, y : double);
    procedure DxfSecondPoint(x, y : double);
    procedure DxfThirdPoint(x, y : double);
    procedure DxfFourthPoint(x, y : double);

    procedure DxfPolyline;
    procedure DxfPolylineIntro;
    procedure DxfPolylineFlag(flag : integer);
    procedure DxfVertex;
    procedure DxfEndSeq;

    procedure DxfText;
    procedure DxfTextHeight(value : double);
    procedure DxfTextAlign(value : integer);
    procedure DxfTextString(value : string);
    procedure DxfTextRotation(value : double);

    procedure DxfSolid;

    procedure DxfEndOfFile;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Render;
  published
    { Published declarations }
    property CadCanvas : TCadCanvas read FCadCanvas write FCadCanvas;
    property FileName : string read FFileName write FFileName;
    property Partial : boolean read FPartial write FPartial;
    property SolidOutlines : boolean read FSolidOutlines write FSolidOutlines;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadDxfDevice]);
end;

{ TCadDxfDevice }

constructor TCadDxfDevice.Create(AOwner: TComponent);
begin
  inherited;

  // Initializing new property values

  FCadCanvas:=nil;
  FFileName:='noname.dxf';
  FFileOpen:=false;
  FPartial:=true;
  FSolidOutlines:=false;
end;

destructor TCadDxfDevice.Destroy;
begin
  inherited;
end;

procedure TCadDxfDevice.Render;
var
    i, j, k : integer;
    Layer : TCadLayer;
    Element : TCadElement;
    SimpleLine : TCadSimpleLine;
    CadPolyLine : TCadPolyLine;
    CadText : TCadText;
    CadSolid : TCadSolid;
    OldSeparator : char;
begin
  if assigned(CadCanvas) then
  begin

    OldSeparator:=DecimalSeparator;
    DecimalSeparator:='.';

    AssignFile(FDxfFile, FFileName);
    rewrite(FDxfFile);

    FFileOpen:=true;

    if not FPartial then
    begin

      DxfBeginTables;
      DxfBeginLayer;

      for i:=0 to CadCanvas.LayerCount-1 do
      begin
        DxfTag(0, 'LAYER');
        DxfTag(2,CadCanvas.Layers[i].Name);
        DxfInt(70, 64);
        DxfLineType('CONTINUOUS');
        DxfColor(CadCanvas.Layers[i].Color);
      end;

      DxfEndLayer;
      DxfEndTables;

    end;

    DxfBeginEntitites;

    for i:=0 to CadCanvas.LayerCount-1 do
    begin
      Layer:=CadCanvas.Layers[i];

      for j:=0 to Layer.Count-1 do
      begin
        Element:=Layer.Elements[j];

        if Element is TCadSimpleLine then
        begin
          SimpleLine:=TCadSimpleLine(Element);

          DxfLine;
          DxfLayer(Layer.Name);
          if (not Element.ColorByLayer) then
            DxfColor(Element.Color);
          DxfFirstPoint(
            SimpleLine.StartPoint.X,
            SimpleLine.StartPoint.Y
          );
          DxfSecondPoint(
            SimpleLine.EndPoint.X,
            SimpleLine.EndPoint.Y
          );
        end;

        if Element is TCadPolyLine then
        begin
          CadPolyLine:=TCadPolyLine(Element);

          DxfPolyline;
          if (not Element.ColorByLayer) then
            DxfColor(Element.Color);
          DxfLayer(Layer.Name);
          DxfPolylineIntro;

          for k:=0 to CadPolyLine.Count-1 do
          begin
            DxfVertex;
            DxfLayer(Layer.Name);
            DxfFirstPoint(
              CadPolyLine.Points[k].X,
              CadPolyLine.Points[k].Y
            );
            DxfFloat(30,0);
            DxfPolylineFlag(32);
          end;
          DxfEndSeq;
          DxfLayer(Layer.Name);
        end;

        if Element is TCadPolygon then
        begin

        end;

        if Element is TCadText then
        begin
          CadText:=TCadText(Element);

          DxfText;
          DxfLayer(Layer.Name);
          if (not Element.ColorByLayer) then
            DxfColor(Element.Color);
          DxfTextString(CadText.TextString);
          DxfFirstPoint(
            CadText.Position.X,
            CadText.Position.Y
          );
          DxfTextHeight(CadText.Height);
          DxfTextRotation(CadText.RotationAngle);
        end;

        if Element is TCadSolid then
        begin
          CadSolid:=TCadSolid(Element);

          if not SolidOutlines then
            begin
              DxfSolid;
              DxfLayer(Layer.Name);
              if (not Element.ColorByLayer) then
                DxfColor(Element.Color);
              DxfFirstPoint(CadSolid.Points[0].X, CadSolid.Points[0].Y);
              DxfSecondPoint(CadSolid.Points[1].X, CadSolid.Points[1].Y);
              DxfThirdPoint(CadSolid.Points[2].X, CadSolid.Points[2].Y);
              DxfFourthPoint(CadSolid.Points[3].X, CadSolid.Points[3].Y);
            end
          else
            begin
              DxfPolyline;
              if (not Element.ColorByLayer) then
                DxfColor(Element.Color);
              DxfLayer(Layer.Name);
              DxfPolylineIntro;

              for k:=0 to 3 do
              begin
                DxfVertex;
                DxfLayer(Layer.Name);
                DxfFirstPoint(
                  CadSolid.Points[k].X,
                  CadSolid.Points[k].Y
                );
                DxfFloat(30,0);
                DxfPolylineFlag(32);
              end;
              DxfVertex;
              DxfLayer(Layer.Name);
              DxfFirstPoint(
                CadSolid.Points[0].X,
                CadSolid.Points[0].Y
              );
              DxfFloat(30,0);
              DxfPolylineFlag(32);
              DxfEndSeq;
              DxfLayer(Layer.Name);
            end;
        end;
      end;
    end;

    DxfEndEntitites;
    DxfEndOfFile;

    DecimalSeparator:=OldSeparator;

    CloseFile(FDxfFile);
    FFileOpen:=false;
  end;
end;

procedure TCadDxfDevice.DxfTag(number: integer; value: string);
begin
  if FFileOpen then
  begin
    writeln(FDxfFile, '  ',number);
    writeln(FDxfFile, value);
  end;
end;

procedure TCadDxfDevice.DxfBeginSection(value : string);
begin
  DxfTag(0, 'SECTION');
  DxfTag(2, value);
end;

procedure TCadDxfDevice.DxfEndSection;
begin
  DxfTag(0, 'ENDSEC');
end;

procedure TCadDxfDevice.DxfEndOfFile;
begin
  DxfTag(0, 'EOF');
end;

procedure TCadDxfDevice.DxfBeginEntitites;
begin
  DxfBeginSection('ENTITIES');
end;

procedure TCadDxfDevice.DxfLine;
begin
  DxfTag(0, 'LINE');
end;

procedure TCadDxfDevice.DxfFirstPoint(x, y: double);
begin
  DxfFloat(10, x);
  DxfFloat(20, y);
end;

procedure TCadDxfDevice.DxfLayer(value: string);
begin
  DxfTag(8, value);
end;

procedure TCadDxfDevice.DxfSecondPoint(x, y: double);
begin
  DxfFloat(11, x);
  DxfFloat(21, y);
end;

procedure TCadDxfDevice.DxfThirdPoint(x, y: double);
begin
  DxfFloat(12, x);
  DxfFloat(22, y);
end;

procedure TCadDxfDevice.DxfFourthPoint(x, y: double);
begin
  DxfFloat(13, x);
  DxfFloat(23, y);
end;

procedure TCadDxfDevice.DxfEndEntitites;
begin
  DxfEndSection;
end;

procedure TCadDxfDevice.DxfPolyline;
begin
  DxfTag(0, 'POLYLINE');
end;

procedure TCadDxfDevice.DxfVertex;
begin
  DxfTag(0, 'VERTEX');
end;

procedure TCadDxfDevice.DxfEndSeq;
begin
  DxfTag(0, 'SEQEND');
end;

procedure TCadDxfDevice.DxfPolylineFlag(flag: integer);
begin
  DxfInt(70, flag);
end;

procedure TCadDxfDevice.DxfPolylineIntro;
begin
  DxfInt(66, 1);
  DxfFloat(10, 0.0);
  DxfFloat(20, 0.0);
  DxfFloat(30, 0.0);
  DxfPolylineFlag(8);
end;

procedure TCadDxfDevice.DxfFloat(number: integer; value: double);
begin
  if FFileOpen then
  begin
    writeln(FDxfFile, '  ',number);
    writeln(FDxfFile, format('%g',[value]));
  end;
end;

procedure TCadDxfDevice.DxfInt(number, value: integer);
begin
  if FFileOpen then
  begin
    writeln(FDxfFile, '  ',number);
    //writeln(FDxfFile, '     ',value);
    writeln(FDxfFile, value);
  end;
end;

procedure TCadDxfDevice.DxfText;
begin
  DxfTag(0, 'TEXT');
end;

procedure TCadDxfDevice.DxfTextAlign(value: integer);
begin
  DxfInt(72, value);
end;

procedure TCadDxfDevice.DxfTextHeight(value: double);
begin
  DxfFloat(40, value);
end;

procedure TCadDxfDevice.DxfTextString(value: string);
begin
  DxfTag(1, value);
end;

procedure TCadDxfDevice.DxfTextRotation(value: double);
begin
  DxfFloat(50, value);
end;

procedure TCadDxfDevice.DxfSolid;
begin
  DxfTag(0, 'SOLID');
end;


procedure TCadDxfDevice.DxfColor(number: integer);
begin
  DxfInt(62, number);
end;

procedure TCadDxfDevice.DxfBeginTables;
begin
  DxfBeginSection('TABLES');
end;

procedure TCadDxfDevice.DxfEndTables;
begin
  DxfEndSection;
end;

procedure TCadDxfDevice.DxfBeginTable(value: string);
begin
  DxfTag(0, 'TABLE');
  DxfTag(2, value);
end;

procedure TCadDxfDevice.DxfEndTable;
begin
  DxfTag(0, 'ENDTAB');
end;

procedure TCadDxfDevice.DxfBeginLayer;
begin
  DxfBeginTable('LAYER');
  //DxfInt(70, CadCanvas.LayerCount);
end;

procedure TCadDxfDevice.DxfEndLayer;
begin
  DxfEndTable;
end;

procedure TCadDxfDevice.DxfLineType(name: string);
begin
  DxfTag(6, name);
end;


end.
