unit CadSurfaceDiagram2D;

interface

uses
  SysUtils, Classes, CadCanvas, Triangle, CadMesh, CadG32Device, GR32, Dialogs,
  Contnrs, IniFiles;

const

  StandardScaleCount = 15;

  StandardScales: array [1 .. 15] of double =
    (1, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000,
    100000);

type

  TScaling = class
  private
    FScaleX: double;
    FUnitMultiplier: double;
    FVerticalExaggeration: double;
    procedure SetScaleX(const Value: double);
    procedure SetUnitMultiplier(const Value: double);
    procedure SetVerticalExaggeration(const Value: double);

  public
    constructor Create;

    function L2G(d: double): double;
    function G2L(d: double): double;

    property ScaleX: double read FScaleX write SetScaleX;
    property UnitMultiplier
      : double read FUnitMultiplier write SetUnitMultiplier;
    property VerticalExaggeration: double read FVerticalExaggeration write
      SetVerticalExaggeration;
  end;

  TCadAxes2D = class
  private
    FCadCanvas: TCadCanvas;
    FMinPoint: TCadPoint;
    FMaxPoint: TCadPoint;
    FTickDistanceX: double;
    FTickDistanceY: double;
    FTickSize: double;
    FTickLabelSize: double;
    FTickLabelDistance: double;
    FTicksTop: boolean;
    FTicksBottom: boolean;
    FTicksLeft: boolean;
    FTicksRight: boolean;

    FScaling: TScaling;

    FLabelUnitY: string;
    FLabelX: string;
    FLabelUnitX: string;
    FLabelY: string;
    FTickLabelFormat: string;

    FLabelSize: double;
    FLabelDistanceX: double;
    FLabelDistanceY: double;
    FLabelUnitSpacing: double;
    FLabelUnitSize: double;
    FLabelDistance: double;

    FLabelXVisible: boolean;
    FLabelYVisible: boolean;
    FLabelXJustify: TCadTextJustify;
    FLabelYJustify: TCadTextJustify;
    FLabelXPosition: integer;
    FLabelYPosition: integer;
    FLabelUnitXVisible: boolean;
    FLabelUnitYVisible: boolean;

    procedure SetCadCanvas(const Value: TCadCanvas);
    procedure SetTickDistanceX(const Value: double);
    procedure SetTickDistanceY(const Value: double);
    procedure SetTickSize(const Value: double);
    procedure SetTickLabelDistance(const Value: double);
    procedure SetTickLabelSize(const Value: double);
    procedure SetLabelDistance(const Value: double);
    procedure SetLabelSize(const Value: double);
    procedure SetLabelUnitSpacing(const Value: double);
    procedure SetLabelUnitX(const Value: string);
    procedure SetLabelUnitY(const Value: string);
    procedure SetLabelX(const Value: string);
    procedure SetLabelY(const Value: string);
    procedure SetLabelUnitSize(const Value: double);
    procedure SetTickLabelFormat(const Value: string);
    procedure SetTicksBottom(const Value: boolean);
    procedure SetTicksLeft(const Value: boolean);
    procedure SetTicksRight(const Value: boolean);
    procedure SetTicksTop(const Value: boolean);
    procedure SetLabelDistanceX(const Value: double);
    procedure SetLabelDistanceY(const Value: double);
    procedure SetLabelXVisible(const Value: boolean);
    procedure SetLabelYVisible(const Value: boolean);
    procedure SetLabelXJustify(const Value: TCadTextJustify);
    procedure SetLabelYJustify(const Value: TCadTextJustify);
    procedure SetLabelXPosition(const Value: integer);
    procedure SetLabelYPosition(const Value: integer);
    procedure SetLabelUnitXVisible(const Value: boolean);
    procedure SetLabelUnitYVisible(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;

    property CadCanvas: TCadCanvas read FCadCanvas write SetCadCanvas;

    property MinPoint: TCadPoint read FMinPoint;
    property MaxPoint: TCadPoint read FMaxPoint;
    property TickDistanceX: double read FTickDistanceX write SetTickDistanceX;
    property TickDistanceY: double read FTickDistanceY write SetTickDistanceY;
    property TickSize: double read FTickSize write SetTickSize;
    property TickLabelSize: double read FTickLabelSize write SetTickLabelSize;
    property TickLabelDistance: double read FTickLabelDistance write
      SetTickLabelDistance;
    property TickLabelFormat
      : string read FTickLabelFormat write SetTickLabelFormat;
    property TicksTop: boolean read FTicksTop write SetTicksTop;
    property TicksBottom: boolean read FTicksBottom write SetTicksBottom;
    property TicksLeft: boolean read FTicksLeft write SetTicksLeft;
    property TicksRight: boolean read FTicksRight write SetTicksRight;
    property LabelX: string read FLabelX write SetLabelX;
    property LabelY: string read FLabelY write SetLabelY;
    property LabelUnitX: string read FLabelUnitX write SetLabelUnitX;
    property LabelUnitY: string read FLabelUnitY write SetLabelUnitY;
    property LabelSize: double read FLabelSize write SetLabelSize;
    property LabelUnitSize: double read FLabelUnitSize write SetLabelUnitSize;
    property LabelDistance: double read FLabelDistance write SetLabelDistance;
    property LabelDistanceX
      : double read FLabelDistanceX write SetLabelDistanceX;
    property LabelDistanceY
      : double read FLabelDistanceY write SetLabelDistanceY;
    property LabelUnitSpacing: double read FLabelUnitSpacing write
      SetLabelUnitSpacing;
    property LabelXVisible: boolean read FLabelXVisible write SetLabelXVisible;
    property LabelYVisible: boolean read FLabelYVisible write SetLabelYVisible;
    property Scaling: TScaling read FScaling write FScaling;

    property LabelYJustify: TCadTextJustify read FLabelYJustify write
      SetLabelYJustify;
    property LabelXJustify: TCadTextJustify read FLabelXJustify write
      SetLabelXJustify;
    property LabelXPosition
      : integer read FLabelXPosition write SetLabelXPosition;
    property LabelYPosition
      : integer read FLabelYPosition write SetLabelYPosition;
    property LabelUnitXVisible: boolean read FLabelUnitXVisible write
      SetLabelUnitXVisible;
    property LabelUnitYVisible: boolean read FLabelUnitYVisible write
      SetLabelUnitYVisible;
  end;

  { Property group defining properties for the diagram axes. Properties given in
    drawing units are scaled to represent a certain size on a "virtual drawing
    canvas" given in a specified scale 1:xxx. (Need to find a better definition
    ...) }
  TCadAxesRecord = class(TPersistent)
  private
    FAutoX: boolean;
    FAutoY: boolean;
    FMinX: double;
    FMaxY: double;
    FMaxX: double;
    FMinY: double;

    FNotify: boolean;
    FOnChangeAutoY: TNotifyEvent;
    FOnChangeAutoX: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FTickSize: double;
    FTickDistanceX: double;
    FTickDistanceY: double;
    FTickLabelDistance: double;
    FTickLabelSize: double;
    FTicksTop: boolean;
    FTicksBottom: boolean;
    FTicksLeft: boolean;
    FTicksRight: boolean;
    FLabelSize: double;
    FLabelDistance: double;
    FLabelDistanceX: double;
    FLabelDistanceY: double;
    FLabelUnitSpacing: double;
    FLabelUnitSize: double;
    FLabelUnitX: string;
    FLabelX: string;
    FLabelY: string;
    FLabelUnitY: string;
    FTickLabelFormat: string;
    FLabelXVisible: boolean;
    FLabelYVisible: boolean;
    FLabelUnitXVisible: boolean;
    FLabelXJustify: TCadTextJustify;
    FLabelUnitYVisible: boolean;
    FLabelYJustify: TCadTextJustify;
    FLabelXPosition: integer;
    FLabelYPosition: integer;

    procedure SetAutoX(const Value: boolean);
    procedure SetAutoY(const Value: boolean);
    procedure SetMaxX(const Value: double);
    procedure SetMaxY(const Value: double);
    procedure SetMinX(const Value: double);
    procedure SetMinY(const Value: double);

    procedure SetOnChangeAutoX(const Value: TNotifyEvent);
    procedure SetOnChangeAutoY(const Value: TNotifyEvent);
    procedure SetOnChangeValue(const Value: TNotifyEvent);

    procedure ChangeAutoX;
    procedure ChangeAutoY;
    procedure ChangeValue;
    procedure SetTickDistanceX(const Value: double);
    procedure SetTickDistanceY(const Value: double);
    procedure SetTickSize(const Value: double);
    procedure SetTickLabelDistance(const Value: double);
    procedure SetTickLabelSize(const Value: double);
    procedure SetLabelDistance(const Value: double);
    procedure SetLabelSize(const Value: double);
    procedure SetLabelUnitSize(const Value: double);
    procedure SetLabelUnitSpacing(const Value: double);
    procedure SetLabelUnitX(const Value: string);
    procedure SetLabelUnitY(const Value: string);
    procedure SetLabelX(const Value: string);
    procedure SetLabelY(const Value: string);
    procedure SetTickLabelFormat(const Value: string);
    procedure SetTicksBottom(const Value: boolean);
    procedure SetTicksLeft(const Value: boolean);
    procedure SetTicksRight(const Value: boolean);
    procedure SetTicksTop(const Value: boolean);
    procedure SetLabelDistanceX(const Value: double);
    procedure SetLabelDistanceY(const Value: double);
    procedure SetLabelXVisible(const Value: boolean);
    procedure SetLabelYVisible(const Value: boolean);
    procedure SetLabelUnitXVisible(const Value: boolean);
    procedure SetLabelUnitYVisible(const Value: boolean);
    procedure SetLabelXJustify(const Value: TCadTextJustify);
    procedure SetLabelXPosition(const Value: integer);
    procedure SetLabelYJustify(const Value: TCadTextJustify);
    procedure SetLabelYPosition(const Value: integer);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LockNotify;
    procedure UnlockNotify;

    procedure ReadFromIni(Source: TIniFile);
    procedure SaveToIni(Dest: TIniFile);

    property OnChangeValue: TNotifyEvent read FOnChangeValue write
      SetOnChangeValue;
    property OnChangeAutoX: TNotifyEvent read FOnChangeAutoX write
      SetOnChangeAutoX;
    property OnChangeAutoY: TNotifyEvent read FOnChangeAutoY write
      SetOnChangeAutoY;
  published
    { Automatic calculation of axes X extents. }
    property AutoX: boolean read FAutoX write SetAutoX;

    { Automatic calculation of axes Y extents. }
    property AutoY: boolean read FAutoY write SetAutoY;

    { Max X coordinate of diagram axes. When AutoX i set to @true this property
      is automatically calculated. }
    property MaxX: double read FMaxX write SetMaxX;

    { Min X coordinate of diagram axes. When AutoX i set to @true this property
      is automatically calculated. }
    property MinX: double read FMinX write SetMinX;

    { Max Y coordinate of diagram axes. When AutoY i set to @true this property
      is automatically calculated. }
    property MaxY: double read FMaxY write SetMaxY;

    { Min Y coordinate of diagram axes. When AutoY i set to @true this property
      is automatically calculated. }
    property MinY: double read FMinY write SetMinY;

    { Distance between axes X tick labels. }
    property TickDistanceX: double read FTickDistanceX write SetTickDistanceX;

    { Distance between axes Y tick labels. }
    property TickDistanceY: double read FTickDistanceY write SetTickDistanceY;

    { Size of the tick mark (drawing units). }
    property TickSize: double read FTickSize write SetTickSize;

    { Distance to tick label. (drawing units). }
    property TickLabelDistance: double read FTickLabelDistance write
      SetTickLabelDistance;

    { Tick label size. (drawing units). }
    property TickLabelSize: double read FTickLabelSize write SetTickLabelSize;

    { Format string used to represent the tick label value. See the Delphi
      format routine. }
    property TickLabelFormat
      : string read FTickLabelFormat write SetTickLabelFormat;

    property TicksTop: boolean read FTicksTop write SetTicksTop;
    property TicksBottom: boolean read FTicksBottom write SetTicksBottom;
    property TicksLeft: boolean read FTicksLeft write SetTicksLeft;
    property TicksRight: boolean read FTicksRight write SetTicksRight;

    { X-axis label string. }
    property LabelX: string read FLabelX write SetLabelX;

    { X-axis label string. }
    property LabelY: string read FLabelY write SetLabelY;

    { X-axis label unit string. }
    property LabelUnitX: string read FLabelUnitX write SetLabelUnitX;

    { X-axis label unit string. }
    property LabelUnitY: string read FLabelUnitY write SetLabelUnitY;

    { Axis label size. (drawing units) }
    property LabelSize: double read FLabelSize write SetLabelSize;

    { Axis label unit size. (drawing units) }
    property LabelUnitSize: double read FLabelUnitSize write SetLabelUnitSize;

    { Distance from diagram axes to Axis label. (drawing units) }
    property LabelDistance: double read FLabelDistance write SetLabelDistance;

    property LabelDistanceX
      : double read FLabelDistanceX write SetLabelDistanceX;
    property LabelDistanceY
      : double read FLabelDistanceY write SetLabelDistanceY;

    { Spacing between axis label and axis unit label. (drawing units) }
    property LabelUnitSpacing: double read FLabelUnitSpacing write
      SetLabelUnitSpacing;
    property LabelXVisible: boolean read FLabelXVisible write SetLabelXVisible;
    property LabelYVisible: boolean read FLabelYVisible write SetLabelYVisible;

    property LabelYJustify: TCadTextJustify read FLabelYJustify write
      SetLabelYJustify;
    property LabelXJustify: TCadTextJustify read FLabelXJustify write
      SetLabelXJustify;
    property LabelXPosition
      : integer read FLabelXPosition write SetLabelXPosition;
    property LabelYPosition
      : integer read FLabelYPosition write SetLabelYPosition;
    property LabelUnitXVisible: boolean read FLabelUnitXVisible write
      SetLabelUnitXVisible;
    property LabelUnitYVisible: boolean read FLabelUnitYVisible write
      SetLabelUnitYVisible;

  end;

  // Title < -- titlespacing -- > Subtitle < -- Titledistance -- >

  { Property group defining global diagram related properties. }
  TCadDiagramRecord = class(TPersistent)
  private
    FShowTriangles: boolean;
    FOnChangeShowTriangles: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FNotify: boolean;
    FTitleSize: double;
    FSubTitleSize: double;
    FTitleSpacing: double;
    FTitleDistance: double;
    FSubTitle: string;
    FTitle: string;
    FShowLegend: boolean;
    FLegendDistance: double;
    FLegendHeight: double;
    FLegendTextDistance: double;
    FLegendTextHeight: double;
    FLegendFormat: string;
    FScaleX: double;
    FVerticalExaggeration: double;
    FAutoScale: boolean;
    FLegendSize: double;
    FLegendLabel: string;
    FLegendLabelDistance: double;
    FLegendLabelPos: integer;
    FLegendLabelSize: double;

    procedure SetShowTriangles(const Value: boolean);
    procedure SetOnChangeShowTriangles(const Value: TNotifyEvent);

    procedure ChangeShowTriangles;
    procedure ChangeValue;
    procedure SetSubTitle(const Value: string);
    procedure SetSubTitleSize(const Value: double);
    procedure SetTitle(const Value: string);
    procedure SetTitleDistance(const Value: double);
    procedure SetTitleSize(const Value: double);
    procedure SetTitleSpacing(const Value: double);
    procedure SetOnChangeValue(const Value: TNotifyEvent);
    procedure SetLegendDistance(const Value: double);
    procedure SetShowLegend(const Value: boolean);
    procedure SetLegendHeight(const Value: double);
    procedure SetLegendTextDistance(const Value: double);
    procedure SetLegendTextHeight(const Value: double);
    procedure SetLegendFormat(const Value: string);
    procedure SetScaleX(const Value: double);
    procedure SetVerticalExaggeration(const Value: double);
    procedure SetAutoScale(const Value: boolean);
    procedure SetLegendSize(const Value: double);
    procedure SetLegendLabel(const Value: string);
    procedure SetLegendLabelDistance(const Value: double);
    procedure SetLegendLabelPos(const Value: integer);
    procedure SetLegendLabelSize(const Value: double);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LockNotify;
    procedure UnlockNotify;

    procedure ReadFromIni(Source: TIniFile);
    procedure SaveToIni(Dest: TIniFile);

    property OnChangeShowTriangles
      : TNotifyEvent read FOnChangeShowTriangles write SetOnChangeShowTriangles;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write
      SetOnChangeValue;
  published
    { Show triangle mesh used for meshing. }
    property ShowTriangles: boolean read FShowTriangles write SetShowTriangles;

    { Diagram title. }
    property Title: string read FTitle write SetTitle;

    { Diagram title size in drawing units. }
    property TitleSize: double read FTitleSize write SetTitleSize;

    { Diagram sub title. }
    property SubTitle: string read FSubTitle write SetSubTitle;

    { Diagram sub title size in drawing units. }
    property SubTitleSize: double read FSubTitleSize write SetSubTitleSize;

    { Spacing between title and sub title in drawing units. }
    property TitleSpacing: double read FTitleSpacing write SetTitleSpacing;

    { Distance between title and diagram axes in drawing units. }
    property TitleDistance: double read FTitleDistance write SetTitleDistance;

    { Legend visibility flag. }
    property ShowLegend: boolean read FShowLegend write SetShowLegend;

    { Distance from diagram axes to legend in drawing units. }
    property LegendDistance
      : double read FLegendDistance write SetLegendDistance;

    { Legend height in drawing units. }
    property LegendHeight: double read FLegendHeight write SetLegendHeight;

    { Legend text height in drawing units. }
    property LegendTextHeight: double read FLegendTextHeight write
      SetLegendTextHeight;

    { Distance between legend text labels and legend in drawing units. }
    property LegendTextDistance: double read FLegendTextDistance write
      SetLegendTextDistance;

    { Format used to represent the legend values. See the Delphi format routine. }
    property LegendFormat: string read FLegendFormat write SetLegendFormat;

    { Diagram X scale. 1:ScaleX. When @code(AutoScale) is set to @true this
      property is automatically calculated. }
    property ScaleX: double read FScaleX write SetScaleX;

    { Sets the diagram vertical exaggeration. The diagram height is scaled with
      this amount in the y direction. }
    property VerticalExaggeration: double read FVerticalExaggeration write
      SetVerticalExaggeration;

    { Flag for automatic scale calculation based on diagram input values. }
    property AutoScale: boolean read FAutoScale write SetAutoScale;

    { Relative legend size with respect to diagram axes. (1.0 default) }
    property LegendSize: double read FLegendSize write SetLegendSize;

    property LegendLabel: string read FLegendLabel write SetLegendLabel;
    property LegendLabelDistance: double read FLegendLabelDistance write
      SetLegendLabelDistance;
    property LegendLabelPos
      : integer read FLegendLabelPos write SetLegendLabelPos;
    property LegendLabelSize : double read FLegendLabelSize write SetLegendLabelSize;
  end;

  { @value scClip Remove triangles above surface, clip crossing triangles. (Unstable)
    @value scRemove Remove triangles above surface. No clipping.
    @value scRemoveCrossing Remove triangles above and crossing surface. No clipping.
    @value scRemoveReverse Remove triangles below surface. No clipping.
    @value scRemoveCrossingReverse Remove triangles below and crossing surface. No clipping. }
  TCadMeshClipSurfaceType = (scClip, scRemove, scRemoveCrossing,
    scRemoveReverse, scRemoveCrossingReverse);
  TCadMeshDuplicateProcessing = (tdIgnore, tdReplace, tdMean);

  { Property group describing properties controlling diagram mesh generation. }
  TCadMeshRecord = class(TPersistent)
  private
    FOnChangeValue: TNotifyEvent;
    FNotify: boolean;
    FAutoLimits: boolean;
    FMinValue: double;
    FMaxValue: double;
    FMeshType: TMeshType;
    FClipSurface: boolean;
    FClipSurfaceType: TCadMeshClipSurfaceType;
    FAlignWithSurface: boolean;
    FTriangleExecutable: string;
    FTriangleImplementation: TTriangleImplementation;
    FDuplicateProcessing: TCadMeshDuplicateProcessing;
    procedure ChangeValue;
    procedure SetOnChangeValue(const Value: TNotifyEvent);
    procedure SetAutoLimits(const Value: boolean);
    procedure SetMaxValue(const Value: double);
    procedure SetMeshType(const Value: TMeshType);
    procedure SetMinValue(const Value: double);
    procedure SetClipSurface(const Value: boolean);
    procedure SetClipSurfaceType(const Value: TCadMeshClipSurfaceType);
    procedure SetTriangleExecutable(const Value: string);
    procedure SetTriangleImplementation(const Value: TTriangleImplementation);
    procedure SetAlignWithSurface(const Value: boolean);
    procedure SetDuplicateProcessing(const Value: TCadMeshDuplicateProcessing);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LockNotify;
    procedure UnlockNotify;

    procedure ReadFromIni(Source: TIniFile);
    procedure SaveToIni(Dest: TIniFile);

    property OnChangeValue: TNotifyEvent read FOnChangeValue write
      SetOnChangeValue;
  published
    { Automatically calculate diagram limits. (Default @true). }
    property AutoLimits: boolean read FAutoLimits write SetAutoLimits;

    { Return max diagram value. }
    property MaxValue: double read FMaxValue write SetMaxValue;

    { Return min diagram value. }
    property MinValue: double read FMinValue write SetMinValue;

    { Set the desired mesh type. mtTriangles, mtIsolines, mtSolid. (Default mtSolid). }
    property MeshType: TMeshType read FMeshType write SetMeshType;

    { Clip to surface. (Default @false). }
    property ClipSurface: boolean read FClipSurface write SetClipSurface;

    { Determines how the triangles are clipped to a defined surface contour. }
    property ClipSurfaceType
      : TCadMeshClipSurfaceType read FClipSurfaceType write SetClipSurfaceType;

    property AlignWithSurface: boolean read FAlignWithSurface write
      SetAlignWithSurface;

    { Search path to Triangle executable. }
    property TriangleExecutable: string read FTriangleExecutable write
      SetTriangleExecutable;

    { Delaunay algorithm used to create triangulation used to create the diagram mesh. }
    property TriangleImplementation
      : TTriangleImplementation read FTriangleImplementation write
      SetTriangleImplementation;

    property DuplicateProcessing
      : TCadMeshDuplicateProcessing read FDuplicateProcessing write
      SetDuplicateProcessing;
  end;

  TCadScalingOptions = (soAbsolute, soRelativeHeight, soRelativeWidth);

  TTrianglePointList = class
  private
    FPoints: TObjectList;
    function GetPoint(idx: integer): TTrianglePoint;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPoint(x, y, Value: double);
    procedure Clear;

    property Point[idx: integer]: TTrianglePoint read GetPoint;
    property Count: integer read GetCount;
  end;

  TCadSurfaceContour = class(TCadPolyLine)
  end;

  TCadOffsetContour = class(TCadPolyLine)
  end;

  { Two-dimensional Surface diagram component. }
  TCadSurfaceDiagram2D = class(TComponent)
  private
    { Private declarations }
    FCadCanvas: TCadCanvas;
    FCadMesh: TCadMesh;
    FTriangle: TTriangle;
    FAxes2D: TCadAxes2D;
    FNotationLayer: TCadLayer;
    FTitleLayer: TCadLayer;
    FAxes: TCadAxesRecord;
    FDiagram: TCadDiagramRecord;
    FDirty: boolean;
    FMesh: TCadMeshRecord;
    FScaling: TScaling;
    FOffsetContour: TCadOffsetContour;
    FTempPointList: TTrianglePointList;

    procedure SetCadCanvas(const Value: TCadCanvas);
    procedure SetAxes(const Value: TCadAxesRecord);

    procedure CreateAxes;
    procedure CreateLayers;
    procedure CreateTitles;
    procedure CreateLegend;
    procedure CreateTriangulation;
    procedure CreateMesh;
    procedure UpdateMesh;
    procedure UpdateAxes;
    procedure UpdateAxesProperties;
    procedure UpdateTitles;
    procedure CalcScaling;

    procedure AxesChangeValue(Sender: TObject);
    procedure AxesChangeAutoX(Sender: TObject);
    procedure AxesChangeAutoY(Sender: TObject);
    procedure DiagramChangeValue(Sender: TObject);
    procedure DiagramChangeShowTriangles(Sender: TObject);
    procedure MeshChangeValue(Sender: TObject);
    procedure SetDiagram(const Value: TCadDiagramRecord);
    procedure SetIsoLines(const Value: TIsoLines);
    function GetIsoLines: TIsoLines;
    procedure SetMesh(const Value: TCadMeshRecord);
    procedure SetOffsetContour(const Value: TCadOffsetContour);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Add a point/value pair to the surface diagram. }
    procedure AddPoint(x, y, Value: double);

    { Clear all added point/value pairs. }
    procedure Clear;

    { Create surface diagram. The generated diagram is sent to the CadCanvas
      specified in the @code(CadCanvas) property. }
    procedure Execute;

    procedure SaveSettings(IniFile: TIniFile);
    procedure LoadSettings(IniFile: TIniFile);

    property Axes2D: TCadAxes2D read FAxes2D;

    { Defines the isolines used to generated both a solid surface diagram
      and a isoline based diagram, see @link(TIsoLines). }
    property IsoLines: TIsoLines read GetIsoLines write SetIsoLines;

    { Controls the diagram scaling, see @link(TScaling). }
    property Scaling: TScaling read FScaling;

    property OffsetContour: TCadOffsetContour read FOffsetContour write
      SetOffsetContour;

  published
    { Published declarations }
    { Target CadCanvas to send generated diagram. }
    property CadCanvas: TCadCanvas read FCadCanvas write SetCadCanvas;

    { Axes property group. }
    property Axes: TCadAxesRecord read FAxes write SetAxes;

    { Diagram property group. }
    property Diagram: TCadDiagramRecord read FDiagram write SetDiagram;

    { Mesh property group. }
    property Mesh: TCadMeshRecord read FMesh write SetMesh;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZoomMedia', [TCadSurfaceDiagram2D]);
end;

{ TCadSurfaceDiagram2D }

constructor TCadSurfaceDiagram2D.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialize variables

  FCadCanvas := nil;
  FNotationLayer := nil;
  FTitleLayer := nil;

  // Initialize internal utility objects
  // for triangulation and meshing

  FTriangle := TTriangle.Create(Self);
  FTriangle.TriangleImplementation := tiInternal;

  FCadMesh := TCadMesh.Create(Self);
  FCadMesh.Triangle := FTriangle;
  FCadMesh.IsoLines.Size := 12;

  FScaling := TScaling.Create;

  // Axes object

  FAxes2D := TCadAxes2D.Create;
  FAxes2D.Scaling := FScaling;

  // Initialize axes sub property

  FAxes := TCadAxesRecord.Create(AOwner);
  FAxes.OnChangeValue := Self.AxesChangeValue;
  FAxes.OnChangeAutoX := Self.AxesChangeAutoX;
  FAxes.OnChangeAutoY := Self.AxesChangeAutoY;

  FDiagram := TCadDiagramRecord.Create(AOwner);
  FDiagram.OnChangeShowTriangles := Self.DiagramChangeShowTriangles;
  FDiagram.OnChangeValue := Self.DiagramChangeValue;

  FMesh := TCadMeshRecord.Create(AOwner);
  FMesh.OnChangeValue := Self.MeshChangeValue;

  FOffsetContour := TCadOffsetContour.Create;

  FTempPointList := TTrianglePointList.Create;

  FDirty := true;

end;

destructor TCadSurfaceDiagram2D.Destroy;
begin

  // Clean up

  FCadMesh.Free;
  FTriangle.Free;
  FAxes2D.Free;
  FAxes.Free;
  FDiagram.Free;
  FMesh.Free;
  FScaling.Free;
  FOffsetContour.Free;
  FTempPointList.Free;

  inherited;
end;

procedure TCadSurfaceDiagram2D.AddPoint(x, y, Value: double);
begin

  // Add unscaled points to a temporary point list, so that
  // we can reapply vertical exaggeration later on, without the need
  // to add the points again.

  FTempPointList.AddPoint(x, y, Value);
  FDirty := true;
end;

procedure TCadSurfaceDiagram2D.Clear;
begin
  FTempPointList.Clear;

  if assigned(CadCanvas) then
    CadCanvas.Clear;

  FTriangle.Clear;
  FCadMesh.Clear;

  CreateLayers;

  FDirty := true;
end;

procedure TCadSurfaceDiagram2D.CreateLayers;
begin
  if assigned(CadCanvas) then
  begin
    FNotationLayer := CadCanvas.AddLayer('NOTATION');
    FTitleLayer := CadCanvas.AddLayer('TITLES');
  end;
end;

procedure TCadSurfaceDiagram2D.CreateTriangulation;
var
  i: integer;
  TP: TTrianglePoint;
begin

  // Clear old triangulation

  if (FMesh.TriangleImplementation = tiExternal) then
  begin
    FTriangle.TriangleExecutable := FMesh.TriangleExecutable;
    FTriangle.TriangleImplementation := tiExternal;
  end
  else
    FTriangle.TriangleImplementation := tiInternal;

  if FMesh.DuplicateProcessing = tdIgnore then
    FTriangle.DuplicateProcessing := TTriangleDuplicateProcessing.tdIgnore
  else if FMesh.DuplicateProcessing = tdReplace then
    FTriangle.DuplicateProcessing := TTriangleDuplicateProcessing.tdReplace
  else
    FTriangle.DuplicateProcessing := TTriangleDuplicateProcessing.tdMean;

  FTriangle.Clear;

  // Add temporary points

  for i := 0 to FTempPointList.Count - 1 do
  begin
    TP := FTempPointList.Point[i];
    FTriangle.AddPoint(TP.x, TP.y * FScaling.VerticalExaggeration, TP.Value);
  end;

  // Set triangle options

  FCadMesh.Triangle.ClipPolyline := FMesh.ClipSurface;

  if FMesh.ClipSurfaceType = scClip then
    FTriangle.TriangleClipProcessing := tpClip
  else if FMesh.ClipSurfaceType = scRemove then
    FTriangle.TriangleClipProcessing := tpRemove
  else if FMesh.ClipSurfaceType = scRemoveReverse then
    FTriangle.TriangleClipProcessing := tpRemoveReverse
  else if FMesh.ClipSurfaceType = scRemoveCrossing then
    FTriangle.TriangleClipProcessing := tpRemoveCrossing
  else if FMesh.ClipSurfaceType = scRemoveCrossingReverse then
    FTriangle.TriangleClipProcessing := tpRemoveCrossingReverse;

  // Triangulate

  FTriangle.Execute;

  // Do deformation

  if (FOffsetContour.Count > 0) then
  begin

    FTriangle.OffsetPolyline.Clear;

    for i := 0 to FOffsetContour.Count - 1 do
      FTriangle.OffsetPolyline.AddPoint(FOffsetContour.Points[i].x,
        FOffsetContour.Points[i].y * FScaling.VerticalExaggeration);

    FTriangle.Offset;

  end;

  // Clip triangle elements to axes

  UpdateAxesProperties;

  FTriangle.TriangleClipRect.Left := Axes.MinX;
  FTriangle.TriangleClipRect.Right := Axes.MaxX;
  FTriangle.TriangleClipRect.Top := Axes.MaxY * FScaling.VerticalExaggeration;
  FTriangle.TriangleClipRect.Bottom :=
    Axes.MinY * FScaling.VerticalExaggeration;

  // Do the clipping...

  FTriangle.Clip;

end;

procedure TCadSurfaceDiagram2D.CreateMesh;
begin

  // This is the important stuff...

  // FCadMesh.MeshType:=mtTriangles;
  // FCadMesh.Execute;

  // FCadMesh.TriangleLayer.Visible:=FDiagram.ShowTriangles;
  // FCadMesh.TriangleLayer.Color:=0;

  FCadMesh.MeshType := FMesh.MeshType;
  if FMesh.FAutoLimits then
  begin
    FCadMesh.IsoLines.Min := FTriangle.MinValue;
    FCadMesh.IsoLines.Max := FTriangle.MaxValue;
  end
  else
  begin
    FCadMesh.IsoLines.Min := FMesh.MinValue;
    FCadMesh.IsoLines.Max := FMesh.MaxValue;
  end;

  //
  // 1   2   3   n         = .IsoLines.Size := n
  // 1 <--|-2-|-3-|-n-|--> n+1

  FCadMesh.Execute;

end;

procedure TCadSurfaceDiagram2D.CreateAxes;
begin
  if assigned(CadCanvas) then
  begin

    // Create extra graphics

    CadCanvas.CurrentLayer := FNotationLayer;

    UpdateAxesProperties;

    Axes2D.Execute;

  end;
end;

procedure TCadSurfaceDiagram2D.Execute;
begin

  // Make sure we have everything needed.

  if not assigned(CadCanvas) then
    exit;

  // Clear CadCanvas

  CadCanvas.Clear;

  // Create layers

  CreateLayers;

  // Triangulate

  CreateTriangulation;

  // Create a solid 2D diagram using the CadMesh component

  CreateMesh;

  // Compute an appropriate scale

  if Self.Diagram.AutoScale then
    CalcScaling;

  // Create axes

  CreateAxes;

  // Create title

  CreateTitles;

  // Create legend

  if Self.Diagram.ShowLegend then
    CreateLegend;

  // Create surface contour

  // CreateSurfaceContour;

end;

procedure TCadSurfaceDiagram2D.UpdateMesh;
begin
  if assigned(CadCanvas) then
  begin
    FCadMesh.Clear;
    FTriangle.Clear;
    CreateTriangulation;
    CreateMesh;
  end;
end;

procedure TCadSurfaceDiagram2D.UpdateAxes;
begin
  if assigned(CadCanvas) then
  begin
    if assigned(FNotationLayer) then
    begin
      FNotationLayer.Clear;
      CadCanvas.CurrentLayer := FNotationLayer;
      Self.CreateAxes;
    end;
  end;
end;

procedure TCadSurfaceDiagram2D.UpdateTitles;
begin
  if assigned(CadCanvas) then
  begin
    if assigned(FTitleLayer) then
    begin
      FTitleLayer.Clear;
      CadCanvas.CurrentLayer := FTitleLayer;
      Self.CreateTitles;
    end;
  end;
end;

procedure TCadSurfaceDiagram2D.UpdateAxesProperties;
begin

  // Make sure we do not call any "OnChange" methods

  FAxes.LockNotify;

  Axes2D.TickDistanceX := FAxes.TickDistanceX;
  Axes2D.TickDistanceY := FAxes.TickDistanceY;
  Axes2D.TickSize := FAxes.TickSize;
  Axes2D.TickLabelSize := FAxes.TickLabelSize;
  Axes2D.TickLabelDistance := FAxes.TickLabelDistance;
  Axes2D.TickLabelFormat := FAxes.TickLabelFormat;
  Axes2D.TicksTop := FAxes.TicksTop;
  Axes2D.TicksBottom := FAxes.TicksBottom;
  Axes2D.TicksLeft := FAxes.TicksLeft;
  Axes2D.TicksRight := FAxes.TicksRight;
  Axes2D.LabelX := FAxes.LabelX;
  Axes2D.LabelY := FAxes.LabelY;
  Axes2D.LabelUnitX := FAxes.LabelUnitX;
  Axes2D.LabelUnitY := FAxes.LabelUnitY;
  Axes2D.LabelSize := FAxes.LabelSize;
  Axes2D.LabelUnitSize := FAxes.LabelUnitSize;
  Axes2D.LabelDistance := FAxes.LabelDistance;
  Axes2D.LabelDistanceX := FAxes.LabelDistanceX;
  Axes2D.LabelDistanceY := FAxes.LabelDistanceY;
  Axes2D.LabelUnitSpacing := FAxes.LabelUnitSpacing;
  Axes2D.LabelXVisible := FAxes.LabelXVisible;
  Axes2D.LabelYVisible := FAxes.LabelYVisible;

  Axes2D.LabelUnitXVisible := FAxes.LabelUnitXVisible;
  Axes2D.LabelXJustify := FAxes.LabelXJustify;
  Axes2D.LabelUnitYVisible := FAxes.LabelUnitYVisible;
  Axes2D.LabelYJustify := FAxes.LabelYJustify;
  Axes2D.LabelXPosition := FAxes.LabelXPosition;
  Axes2D.LabelYPosition := FAxes.LabelYPosition;

  if FAxes.AutoX then
  begin
    Axes2D.MinPoint.x := FTriangle.MinPoint.x;
    Axes2D.MaxPoint.x := FTriangle.MaxPoint.x;
    FAxes.MinX := Axes2D.MinPoint.x;
    FAxes.MaxX := Axes2D.MaxPoint.x;
  end
  else
  begin
    Axes2D.MinPoint.x := FAxes.MinX;
    Axes2D.MaxPoint.x := FAxes.MaxX;
  end;

  if FAxes.AutoY then
  begin
    Axes2D.MinPoint.y := FTriangle.MinPoint.y;
    Axes2D.MaxPoint.y := FTriangle.MaxPoint.y;
    FAxes.MinY := Axes2D.MinPoint.y / FDiagram.FVerticalExaggeration;
    FAxes.MaxY := Axes2D.MaxPoint.y / FDiagram.FVerticalExaggeration;
  end
  else
  begin
    Axes2D.MinPoint.y := Self.FAxes.MinY * FDiagram.FVerticalExaggeration;
    Axes2D.MaxPoint.y := Self.FAxes.MaxY * FDiagram.FVerticalExaggeration;
  end;

  FAxes.UnlockNotify;
end;

procedure TCadSurfaceDiagram2D.CalcScaling;
var
  i: integer;
  DiagramWidth, DiagramHeight: double;
  DiagramSize: double;
  ScaleFactor: double;
  TextHeight: double;
  Diff: double;
  MinDiff: double;
  MinDiffPos: integer;
begin

  // Calculate the size of the current diagram

  DiagramWidth := FAxes.MaxX - FAxes.MinX;
  DiagramHeight := FAxes.MaxY - FAxes.MinY;

  if DiagramWidth > DiagramHeight then
    DiagramSize := DiagramWidth
  else
    DiagramSize := DiagramHeight;

  // TextHeight = 3% of DiagramSize

  TextHeight := 3 * DiagramSize / 100;

  ScaleFactor := TextHeight / 5E-3;

  // Loop through the standard scalings to find an
  // appropriate scale

  MinDiff := 1E300;
  MinDiffPos := 0;

  for i := 1 to StandardScaleCount do
  begin
    Diff := abs(ScaleFactor - StandardScales[i]);
    if Diff < MinDiff then
    begin
      MinDiff := Diff;
      MinDiffPos := i;
    end;
  end;

  FDiagram.ScaleX := StandardScales[MinDiffPos];

end;

procedure TCadSurfaceDiagram2D.AxesChangeAutoX(Sender: TObject);
begin
  if FAxes.FAutoX then
  begin
    FAxes.MaxX := FTriangle.MaxPoint.x;
    FAxes.MinX := FTriangle.MinPoint.x;
    Axes2D.MaxPoint.x := FTriangle.MaxPoint.x;
  end;
  Self.UpdateAxes;
end;

procedure TCadSurfaceDiagram2D.AxesChangeAutoY(Sender: TObject);
begin
  if FAxes.AutoY then
  begin
    FAxes.MaxY := FTriangle.MaxPoint.y / FScaling.VerticalExaggeration;
    FAxes.MinY := FTriangle.MinPoint.y / FScaling.VerticalExaggeration;
    Axes2D.MaxPoint.y := FTriangle.MaxPoint.y;
  end;
  Self.UpdateAxes;
end;

procedure TCadSurfaceDiagram2D.AxesChangeValue(Sender: TObject);
begin
  Self.UpdateAxes;
end;

procedure TCadSurfaceDiagram2D.DiagramChangeValue(Sender: TObject);
begin
  FScaling.ScaleX := FDiagram.ScaleX;

  if FDiagram.VerticalExaggeration <> FScaling.VerticalExaggeration then
  begin
    FScaling.VerticalExaggeration := FDiagram.VerticalExaggeration;
    Self.UpdateMesh;
    Self.UpdateAxes;
    Self.UpdateTitles;
  end;
end;

procedure TCadSurfaceDiagram2D.MeshChangeValue(Sender: TObject);
begin

end;

procedure TCadSurfaceDiagram2D.SetCadCanvas(const Value: TCadCanvas);
begin
  FCadCanvas := Value;
  FCadMesh.CadCavnas := Value;
  FAxes2D.CadCanvas := Value;

  CreateLayers;
end;

procedure TCadSurfaceDiagram2D.SaveSettings(IniFile: TIniFile);
begin
  Self.Diagram.SaveToIni(IniFile);
  Self.Axes.SaveToIni(IniFile);
  Self.Mesh.SaveToIni(IniFile);
end;

procedure TCadSurfaceDiagram2D.LoadSettings(IniFile: TIniFile);
begin
  Self.Diagram.ReadFromIni(IniFile);
  Self.Axes.ReadFromIni(IniFile);
  Self.Mesh.ReadFromIni(IniFile);
end;

procedure TCadSurfaceDiagram2D.SetAxes(const Value: TCadAxesRecord);
begin
  FAxes.Assign(Value);
end;

procedure TCadSurfaceDiagram2D.SetDiagram(const Value: TCadDiagramRecord);
begin
  FDiagram.Assign(Value);
end;

procedure TCadSurfaceDiagram2D.SetMesh(const Value: TCadMeshRecord);
begin
  FMesh.Assign(Value);
end;

procedure TCadSurfaceDiagram2D.SetOffsetContour(const Value: TCadOffsetContour);
begin
  FOffsetContour := Value;
end;

procedure TCadSurfaceDiagram2D.DiagramChangeShowTriangles(Sender: TObject);
begin
  if FDiagram.ShowTriangles then
  begin
    if assigned(FCadMesh) then
    begin
      if assigned(FCadMesh.TriangleLayer) then
        FCadMesh.TriangleLayer.Visible := true;
    end;
  end
  else
  begin
    if assigned(FCadMesh) then
    begin
      if assigned(FCadMesh.TriangleLayer) then
        FCadMesh.TriangleLayer.Visible := false;
    end;
  end;
end;

procedure TCadSurfaceDiagram2D.CreateTitles;
var
  DiagramWidth: double;
  // DiagramHeight : double;
begin
  if assigned(CadCanvas) then
  begin

    // Create extra graphics

    CadCanvas.CurrentLayer := FNotationLayer;

    DiagramWidth := Self.Axes2D.MaxPoint.x - Self.Axes2D.MinPoint.x;
    // DiagramHeight:=Self.Axes2D.MaxPoint.Y - Self.Axes2D.MinPoint.Y;

    CadCanvas.TextJustifyX := tjCenter;
    CadCanvas.TextJustifyY := tjBottom;

    if (FDiagram.SubTitle = '') then
    begin
      CadCanvas.TextHeight := Scaling.L2G(FDiagram.TitleSize);
      CadCanvas.TextOut(Self.Axes2D.MinPoint.x + DiagramWidth / 2,
        Self.Axes2D.MaxPoint.y + Scaling.L2G(FDiagram.TitleDistance),
        FDiagram.Title);
    end
    else
    begin
      CadCanvas.TextHeight := Scaling.L2G(FDiagram.SubTitleSize);
      CadCanvas.TextOut(Self.Axes2D.MinPoint.x + DiagramWidth / 2,
        Self.Axes2D.MaxPoint.y + Scaling.L2G(FDiagram.TitleDistance),
        FDiagram.SubTitle);

      CadCanvas.TextHeight := Scaling.L2G(FDiagram.TitleSize);
      CadCanvas.TextOut(Self.Axes2D.MinPoint.x + DiagramWidth / 2,
        Self.Axes2D.MaxPoint.y + Scaling.L2G(FDiagram.TitleDistance)
          + Scaling.L2G(FDiagram.SubTitleSize) + Scaling.L2G
          (FDiagram.TitleSpacing), FDiagram.Title);
    end;

    CadCanvas.ResetTextJustify;

  end;
end;

procedure TCadSurfaceDiagram2D.CreateLegend;
var
  DiagramWidth: double;
  // DiagramHeight : double;
  StartX, StartY, MidX: double;
  ColorWidth: double;
  i: integer;
begin
  if assigned(CadCanvas) then
  begin
    CadCanvas.CurrentLayer := FNotationLayer;
    DiagramWidth := Self.Axes2D.MaxPoint.x - Self.Axes2D.MinPoint.x;
    // DiagramHeight:=Self.Axes2D.MaxPoint.Y - Self.Axes2D.MinPoint.Y;

    MidX := (Self.Axes2D.MinPoint.x + Self.Axes2D.MaxPoint.x) * 0.5;

    StartX := MidX - DiagramWidth * FDiagram.LegendSize * 0.5;
    StartY := Self.Axes2D.MinPoint.y - Scaling.L2G(Diagram.LegendDistance);

    ColorWidth := DiagramWidth * FDiagram.LegendSize /
      (FCadMesh.IsoLines.Size + 1);

    for i := 0 to FCadMesh.IsoLines.IntervalSize - 1 do
    begin
      CadCanvas.CurrentLayer := FCadMesh.IntervalLayers.Items[i] as TCadLayer;
      CadCanvas.CurrentColor := 256;
      CadCanvas.Solid4(StartX + i * ColorWidth, StartY, StartX + (i + 1)
          * ColorWidth, StartY, StartX + (i + 1) * ColorWidth,
        StartY - Scaling.L2G(Diagram.LegendHeight), StartX + i * ColorWidth,
        StartY - Scaling.L2G(Diagram.LegendHeight));
      CadCanvas.CurrentLayer := FNotationLayer;
      CadCanvas.CurrentColor := 255;
      CadCanvas.MoveTo(StartX + i * ColorWidth, StartY);
      CadCanvas.LineTo(StartX + i * ColorWidth, StartY - Scaling.L2G
          (Diagram.LegendHeight));
    end;

    CadCanvas.CurrentLayer := FNotationLayer;
    CadCanvas.CurrentColor := 255;

    for i := 1 to FCadMesh.IsoLines.Size do
    begin
      CadCanvas.MoveTo(StartX + i * ColorWidth, StartY);
      CadCanvas.LineTo(StartX + i * ColorWidth, StartY - Scaling.L2G
          (Diagram.LegendHeight));
      CadCanvas.TextJustifyY := tjTop;
      CadCanvas.TextJustifyX := tjCenter;
      CadCanvas.TextHeight := Scaling.L2G(Diagram.LegendTextHeight);
      CadCanvas.TextOut(StartX + i * ColorWidth, StartY - Scaling.L2G
          (Diagram.LegendHeight) - Scaling.L2G(Diagram.LegendTextDistance),
        format(Diagram.LegendFormat, [FCadMesh.IsoLines.Values[i - 1]]));
      CadCanvas.ResetTextJustify;
    end;

    CadCanvas.MoveTo(StartX, StartY);
    CadCanvas.LineTo(StartX + DiagramWidth * FDiagram.LegendSize, StartY);
    CadCanvas.LineTo(StartX + DiagramWidth * FDiagram.LegendSize,
      StartY - Scaling.L2G(Diagram.LegendHeight));
    CadCanvas.LineTo(StartX, StartY - Scaling.L2G(Diagram.LegendHeight));
    CadCanvas.LineTo(StartX, StartY);

    if Diagram.LegendLabelPos = 0 then
    begin
      CadCanvas.TextJustifyX := tjLeft;
      CadCanvas.TextJustifyY := tjTop;
      CadCanvas.TextHeight := Scaling.L2G(Diagram.LegendLabelSize);
      CadCanvas.TextOut(
          StartX,
          StartY - Scaling.L2G(Diagram.LegendHeight)
          - Scaling.L2G(Diagram.LegendTextDistance)
          - Scaling.L2G(Diagram.LegendTextHeight)
          - Scaling.L2G(Diagram.LegendLabelDistance), Diagram.LegendLabel);
    end;

    if Diagram.LegendLabelPos = 1 then
    begin
      CadCanvas.TextJustifyX := tjRight;
      CadCanvas.TextJustifyY := tjTop;
      CadCanvas.TextHeight := Scaling.L2G(Diagram.LegendLabelSize);
      CadCanvas.TextOut(StartX + DiagramWidth * FDiagram.LegendSize,
          StartY - Scaling.L2G(Diagram.LegendHeight)
          - Scaling.L2G(Diagram.LegendTextDistance)
          - Scaling.L2G(Diagram.LegendTextHeight)
          - Scaling.L2G(Diagram.LegendLabelDistance), Diagram.LegendLabel);
    end;
  end;
end;

procedure TCadSurfaceDiagram2D.SetIsoLines(const Value: TIsoLines);
begin
end;

function TCadSurfaceDiagram2D.GetIsoLines: TIsoLines;
begin
  if assigned(FCadMesh) then
    Result := FCadMesh.IsoLines
  else
    Result := nil;
end;

{ TCadAxes2D }

constructor TCadAxes2D.Create;
begin
  inherited Create;

  // Initialize canvas variable

  FCadCanvas := nil;

  // Min and max points define the diagram axes.

  FMinPoint := TCadPoint.Create;
  FMaxPoint := TCadPoint.Create;

  // Initialize it to something sane.

  MinPoint.x := 0;
  MinPoint.y := 0;
  MaxPoint.x := 1;
  MaxPoint.y := 1;

  FTickDistanceX := 0.1;
  FTickDistanceY := 0.1;
  FTickSize := 1;
  FTickLabelSize := 2.5;
  FTickLabelDistance := 0.5;

  FLabelX := 'x';
  FLabelY := 'y';
  FLabelUnitX := '(m)';
  FLabelUnitY := '(m)';

  FLabelSize := 2.5;
  FLabelUnitSize := 2.5;
  FLabelDistance := 2;
  FLabelDistanceX := 2;
  FLabelDistanceY := 2;
  FLabelUnitSpacing := 2;

  FLabelXVisible := true;
  FLabelYVisible := true;

  FLabelXJustify := tjRight;
  FLabelYJustify := tjLeft;

  FLabelXPosition := 0;
  FLabelYPosition := 1;

  FLabelUnitXVisible := false;
  FLabelUnitYVisible := false;

  FScaling := nil;
end;

destructor TCadAxes2D.Destroy;
begin

  // Clean up

  FMinPoint.Free;
  FMaxPoint.Free;
  inherited;
end;

procedure TCadAxes2D.Execute;
var
  x: double;
  y: double;

  LastTickX: double;
  LastTickY: double;
begin
  if assigned(CadCanvas) and assigned(Scaling) then
  begin

    // Draw border

    CadCanvas.CurrentColor := 255;

    CadCanvas.MoveTo(MinPoint.x, MinPoint.y);
    CadCanvas.LineTo(MaxPoint.x, MinPoint.y);
    CadCanvas.LineTo(MaxPoint.x, MaxPoint.y);
    CadCanvas.LineTo(MinPoint.x, MaxPoint.y);
    CadCanvas.LineTo(MinPoint.x, MinPoint.y);

    // Draw labels

    with CadCanvas do
    begin
      if Self.LabelXVisible then
      begin
        case FLabelXPosition of
          0:
            begin
              // Default position top of x
              TextJustifyX := FLabelXJustify;
              TextJustifyY := tjBottom;
              if FLabelUnitYVisible then
              begin
                TextHeight := Scaling.L2G(FLabelUnitSize);
                TextOut(MaxPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceX),
                  FLabelUnitX);
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MaxPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceX)
                    + Scaling.L2G(FLabelSize) + Scaling.L2G(FLabelUnitSpacing),
                  FLabelX)
              end
              else
              begin
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MaxPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceX)
                    + Scaling.L2G(FLabelSize), FLabelX)
              end;
            end;
          1:
            begin
              // Default position bottom of x
              TextJustifyX := FLabelXJustify;
              TextJustifyY := tjTop;
              if FLabelUnitYVisible then
              begin
                TextHeight := Scaling.L2G(FLabelUnitSize);
                TextOut(MaxPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceX),
                  FLabelUnitX);
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MaxPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceX)
                    - Scaling.L2G(FLabelSize) - Scaling.L2G(FLabelUnitSpacing),
                  FLabelX)
              end
              else
              begin
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MaxPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceY)
                    - Scaling.L2G(FLabelSize), FLabelX)
              end;
            end;
          2:
            begin
              TextJustifyX := tjRight;
              TextJustifyY := tjCenter;
              TextHeight := Scaling.L2G(FLabelSize);
              TextOut(MaxPoint.x + Scaling.L2G(FLabelDistanceX), MinPoint.y,
                FLabelX);
              TextHeight := Scaling.L2G(FLabelUnitSize);
              TextOut(MaxPoint.x + Scaling.L2G(FLabelDistanceX) + TextSizeX
                  (FLabelUnitX) + Scaling.L2G(FLabelUnitSpacing), MinPoint.y,
                FLabelUnitX);
            end;
        end;
      end;

      if Self.LabelYVisible then
      begin
        case FLabelYPosition of
          0:
            begin
              // Default position top of y
              TextJustifyX := FLabelYJustify;
              TextJustifyY := tjBottom;
              if FLabelUnitYVisible then
              begin
                TextHeight := Scaling.L2G(FLabelUnitSize);
                TextOut(MinPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceY),
                  FLabelUnitY);
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MinPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceY)
                    + Scaling.L2G(FLabelSize) + Scaling.L2G(FLabelUnitSpacing),
                  FLabelY)
              end
              else
              begin
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MinPoint.x, MaxPoint.y + Scaling.L2G(FLabelDistanceY)
                    + Scaling.L2G(FLabelSize), FLabelY)
              end;
            end;
          1:
            begin
              // Default position bottom of y
              TextJustifyX := FLabelYJustify;
              TextJustifyY := tjTop;
              if FLabelUnitYVisible then
              begin
                TextHeight := Scaling.L2G(FLabelUnitSize);
                TextOut(MinPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceY),
                  FLabelUnitY);
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MinPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceY)
                    - Scaling.L2G(FLabelSize) - Scaling.L2G(FLabelUnitSpacing),
                  FLabelY)
              end
              else
              begin
                TextHeight := Scaling.L2G(FLabelSize);
                TextOut(MinPoint.x, MinPoint.y - Scaling.L2G(FLabelDistanceY)
                    - Scaling.L2G(FLabelSize), FLabelY)
              end;
            end;
        end;
      end;
    end;

    // Draw tick marks

    x := MinPoint.x;

    while x < MaxPoint.x do
    begin
      with CadCanvas do
      begin
        if Self.TicksBottom then
        begin
          MoveTo(x, MinPoint.y);
          LineTo(x, MinPoint.y - Scaling.L2G(FTickSize));
          TextJustifyX := tjCenter;
          TextJustifyY := tjTop;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(x, MinPoint.y - Scaling.L2G(FTickSize) - Scaling.L2G
              (FTickLabelDistance), format(FTickLabelFormat, [x]));
          ResetTextJustify;
        end;

        if Self.TicksTop then
        begin
          MoveTo(x, MaxPoint.y);
          LineTo(x, MaxPoint.y + Scaling.L2G(FTickSize));
          TextJustifyX := tjCenter;
          TextJustifyY := tjBottom;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(x, MaxPoint.y + Scaling.L2G(FTickSize) + Scaling.L2G
              (FTickLabelDistance), format(FTickLabelFormat, [x]));
          ResetTextJustify;
        end;
        LastTickX := x;
        x := x + FTickDistanceX;
      end;
    end;

    // Draw last tick... X

    x := MaxPoint.x;

    if (x - LastTickX) >= Scaling.L2G(FTickDistanceX) then
    begin
      with CadCanvas do
      begin
        if Self.TicksBottom then
        begin
          MoveTo(x, MinPoint.y);
          LineTo(x, MinPoint.y - Scaling.L2G(FTickSize));
          TextJustifyX := tjCenter;
          TextJustifyY := tjTop;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(x, MinPoint.y - Scaling.L2G(FTickSize) - Scaling.L2G
              (FTickLabelDistance), format(FTickLabelFormat, [x]));
          ResetTextJustify;
        end;

        if Self.TicksTop then
        begin
          MoveTo(x, MaxPoint.y);
          LineTo(x, MaxPoint.y + Scaling.L2G(FTickSize));
          TextJustifyX := tjCenter;
          TextJustifyY := tjBottom;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(x, MaxPoint.y + Scaling.L2G(FTickSize) + Scaling.L2G
              (FTickLabelDistance), format(FTickLabelFormat, [x]));
          ResetTextJustify;
        end;
      end;
    end;

    y := MinPoint.y;

    while y < MaxPoint.y do
    begin
      with CadCanvas do
      begin
        if Self.TicksLeft then
        begin
          MoveTo(MinPoint.x, y);
          LineTo(MinPoint.x - Scaling.L2G(FTickSize), y);
          TextJustifyY := tjCenter;
          TextJustifyX := tjRight;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(MinPoint.x - Scaling.L2G(FTickSize) - Scaling.L2G
              (FTickLabelDistance), y, format(FTickLabelFormat,
              [y / Scaling.VerticalExaggeration]));
          ResetTextJustify;
        end;

        if Self.TicksRight then
        begin
          MoveTo(MaxPoint.x, y);
          LineTo(MaxPoint.x + Scaling.L2G(FTickSize), y);
          TextJustifyY := tjCenter;
          TextJustifyX := tjLeft;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(MaxPoint.x + Scaling.L2G(FTickSize) + Scaling.L2G
              (FTickLabelDistance), y, format(FTickLabelFormat,
              [y / Scaling.VerticalExaggeration]));
          ResetTextJustify;
        end;
        LastTickY := y;
        y := y + FTickDistanceY * Scaling.VerticalExaggeration;
      end;
    end;

    // Draw last tick...

    y := MaxPoint.y;

    if (y - LastTickY) >= Scaling.L2G(FTickDistanceY) then
    begin
      with CadCanvas do
      begin
        if Self.TicksLeft then
        begin
          MoveTo(MinPoint.x, y);
          LineTo(MinPoint.x - Scaling.L2G(FTickSize), y);
          TextJustifyY := tjCenter;
          TextJustifyX := tjRight;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(MinPoint.x - Scaling.L2G(FTickSize) - Scaling.L2G
              (FTickLabelDistance), y, format(FTickLabelFormat,
              [y / Scaling.VerticalExaggeration]));
          ResetTextJustify;
        end;

        if Self.TicksRight then
        begin
          MoveTo(MaxPoint.x, y);
          LineTo(MaxPoint.x + Scaling.L2G(FTickSize), y);
          TextJustifyY := tjCenter;
          TextJustifyX := tjLeft;
          TextHeight := Scaling.L2G(FTickLabelSize);
          TextOut(MaxPoint.x + Scaling.L2G(FTickSize) + Scaling.L2G
              (FTickLabelDistance), y, format(FTickLabelFormat,
              [y / Scaling.VerticalExaggeration]));
          ResetTextJustify;
        end;
      end;
    end;
  end;
end;

procedure TCadAxes2D.SetCadCanvas(const Value: TCadCanvas);
begin
  FCadCanvas := Value;
end;

procedure TCadAxes2D.SetLabelDistance(const Value: double);
begin
  FLabelDistance := Value;
end;

procedure TCadAxes2D.SetLabelDistanceX(const Value: double);
begin
  FLabelDistanceX := Value;
end;

procedure TCadAxes2D.SetLabelDistanceY(const Value: double);
begin
  FLabelDistanceY := Value;
end;

procedure TCadAxes2D.SetLabelSize(const Value: double);
begin
  FLabelSize := Value;
end;

procedure TCadAxes2D.SetLabelUnitSize(const Value: double);
begin
  FLabelUnitSize := Value;
end;

procedure TCadAxes2D.SetLabelUnitSpacing(const Value: double);
begin
  FLabelUnitSpacing := Value;
end;

procedure TCadAxes2D.SetLabelUnitX(const Value: string);
begin
  FLabelUnitX := Value;
end;

procedure TCadAxes2D.SetLabelUnitXVisible(const Value: boolean);
begin
  FLabelUnitXVisible := Value;
end;

procedure TCadAxes2D.SetLabelUnitY(const Value: string);
begin
  FLabelUnitY := Value;
end;

procedure TCadAxes2D.SetLabelUnitYVisible(const Value: boolean);
begin
  FLabelUnitYVisible := Value;
end;

procedure TCadAxes2D.SetLabelX(const Value: string);
begin
  FLabelX := Value;
end;

procedure TCadAxes2D.SetLabelXJustify(const Value: TCadTextJustify);
begin
  FLabelXJustify := Value;
end;

procedure TCadAxes2D.SetLabelXPosition(const Value: integer);
begin
  FLabelXPosition := Value;
end;

procedure TCadAxes2D.SetLabelXVisible(const Value: boolean);
begin
  FLabelXVisible := Value;
end;

procedure TCadAxes2D.SetLabelY(const Value: string);
begin
  FLabelY := Value;
end;

procedure TCadAxes2D.SetLabelYJustify(const Value: TCadTextJustify);
begin
  FLabelYJustify := Value;
end;

procedure TCadAxes2D.SetLabelYPosition(const Value: integer);
begin
  FLabelYPosition := Value;
end;

procedure TCadAxes2D.SetLabelYVisible(const Value: boolean);
begin
  FLabelYVisible := Value;
end;

procedure TCadAxes2D.SetTickDistanceX(const Value: double);
begin
  FTickDistanceX := Value;
end;

procedure TCadAxes2D.SetTickDistanceY(const Value: double);
begin
  FTickDistanceY := Value;
end;

procedure TCadAxes2D.SetTickLabelDistance(const Value: double);
begin
  FTickLabelDistance := Value;
end;

procedure TCadAxes2D.SetTickLabelFormat(const Value: string);
begin
  FTickLabelFormat := Value;
end;

procedure TCadAxes2D.SetTickLabelSize(const Value: double);
begin
  FTickLabelSize := Value;
end;

procedure TCadAxes2D.SetTicksBottom(const Value: boolean);
begin
  FTicksBottom := Value;
end;

procedure TCadAxes2D.SetTickSize(const Value: double);
begin
  FTickSize := Value;
end;

procedure TCadAxes2D.SetTicksLeft(const Value: boolean);
begin
  FTicksLeft := Value;
end;

procedure TCadAxes2D.SetTicksRight(const Value: boolean);
begin
  FTicksRight := Value;
end;

procedure TCadAxes2D.SetTicksTop(const Value: boolean);
begin
  FTicksTop := Value;
end;

{ TCadDiagramRecord }

constructor TCadDiagramRecord.Create(AOwner: TComponent);
begin
  inherited Create;
  FNotify := true;
  FOnChangeShowTriangles := nil;
  FOnChangeValue := nil;
  Self.FTitleSize := 5;
  Self.FSubTitleSize := 3.5;
  Self.FTitleSpacing := 2;
  Self.FTitleDistance := 20;
  Self.FSubTitle := 'Subtitle';
  Self.FTitle := 'Title';
  Self.FShowLegend := true;
  Self.FLegendDistance := 20;
  Self.FLegendHeight := 5;
  Self.FLegendTextHeight := 2.5;
  Self.FLegendTextDistance := 2;
  Self.FLegendFormat := '%g';
  Self.FLegendSize := 1.0;
  Self.FScaleX := 500;
  Self.FVerticalExaggeration := 1;
  Self.FAutoScale := true;

  Self.FLegendLabel := 'LegendLabel';
  Self.FLegendLabelDistance := 2;
  Self.FLegendLabelPos := 1;
  Self.FLegendLabelSize := 2.5;
end;

destructor TCadDiagramRecord.Destroy;
begin

  inherited;
end;

procedure TCadDiagramRecord.Assign(Source: TPersistent);
begin
  if Source is TCadDiagramRecord then
    with TCadDiagramRecord(Source) do
    begin
      Self.FShowTriangles := FShowTriangles;
      Self.FTitleSize := FTitleSize;
      Self.FSubTitleSize := FSubTitleSize;
      Self.FTitleSpacing := FTitleSpacing;
      Self.FTitleDistance := FTitleDistance;
      Self.FSubTitle := FSubTitle;
      Self.FTitle := FTitle;
      Self.FShowLegend := FShowLegend;
      Self.FLegendDistance := FLegendDistance;
      Self.FLegendHeight := FLegendHeight;
      Self.FLegendTextDistance := FLegendTextDistance;
      Self.FLegendTextHeight := FLegendTextHeight;
      Self.FLegendFormat := FLegendFormat;
      Self.FScaleX := FScaleX;
      Self.FVerticalExaggeration := FVerticalExaggeration;
      Self.FAutoScale := FAutoScale;
      Self.FLegendSize := FLegendSize;
      Self.FLegendLabel := FLegendLabel;
      Self.FLegendLabelDistance := FLegendLabelDistance;
      Self.FLegendLabelPos := FLegendLabelPos;
      Self.FLegendLabelSize := FLegendLabelSize;
    end
    else
      inherited; // raises an exception
end;

procedure TCadDiagramRecord.SetShowTriangles(const Value: boolean);
begin
  FShowTriangles := Value;
  ChangeShowTriangles;
end;

procedure TCadDiagramRecord.SetOnChangeShowTriangles(const Value: TNotifyEvent);
begin
  FOnChangeShowTriangles := Value;
end;

procedure TCadDiagramRecord.ChangeShowTriangles;
begin
  if assigned(OnChangeShowTriangles) and FNotify then
    OnChangeShowTriangles(Self);
end;

procedure TCadDiagramRecord.ChangeValue;
begin
  if assigned(OnChangeValue) and FNotify then
    OnChangeValue(Self);
end;

procedure TCadDiagramRecord.LockNotify;
begin
  FNotify := false;
end;

procedure TCadDiagramRecord.SaveToIni(Dest: TIniFile);
begin
  with Dest do
  begin
    WriteBool('Diagram', 'ShowTriangles', FShowTriangles);
    WriteFloat('Diagram', 'TitleSize', FTitleSize);
    WriteFloat('Diagram', 'SubTitleSize', FSubTitleSize);
    WriteFloat('Diagram', 'TitleSpacing', FTitleSpacing);
    WriteFloat('Diagram', 'TitleDistance', FTitleDistance);
    WriteString('Diagram', 'SubTitle', FSubTitle);
    WriteString('Diagram', 'Title', FTitle);
    WriteBool('Diagram', 'ShowLegend', FShowLegend);
    WriteFloat('Diagram', 'LegendDistance', FLegendDistance);
    WriteFloat('Diagram', 'LegendHeight', FLegendHeight);
    WriteFloat('Diagram', 'LegendTextHeight', FLegendTextHeight);
    WriteFloat('Diagram', 'LegendTextDistance', FLegendTextDistance);
    WriteString('Diagram', 'LegendFormat', FLegendFormat);
    WriteFloat('Diagram', 'ScaleX', FScaleX);
    WriteFloat('Diagram', 'VerticalExaggeration', FVerticalExaggeration);
    WriteBool('Diagram', 'AutoScale', FAutoScale);
    WriteFloat('Diagram', 'LegendSize', FLegendSize);

    WriteString('Diagram', 'LegendLabel', FLegendLabel);
    WriteFloat('Diagram', 'LegendLabelDistance', FLegendLabelDistance);
    WriteInteger('Diagram', 'LegendLabelPos', FLegendLabelPos);
    WriteFloat('Diagram', 'LegendLabelSize', FLegendLabelSize);
  end;
end;

procedure TCadDiagramRecord.ReadFromIni(Source: TIniFile);
begin
  with Source do
  begin
    FShowTriangles := ReadBool('Diagram', 'ShowTriangles', FShowTriangles);
    FTitleSize := ReadFloat('Diagram', 'TitleSize', FTitleSize);
    FSubTitleSize := ReadFloat('Diagram', 'SubTitleSize', FSubTitleSize);
    FTitleSpacing := ReadFloat('Diagram', 'TitleSpacing', FTitleSpacing);
    FTitleDistance := ReadFloat('Diagram', 'TitleDistance', FTitleDistance);
    FSubTitle := ReadString('Diagram', 'SubTitle', FSubTitle);
    FTitle := ReadString('Diagram', 'Title', FTitle);
    FShowLegend := ReadBool('Diagram', 'ShowLegend', FShowLegend);
    FLegendDistance := ReadFloat('Diagram', 'LegendDistance', FLegendDistance);
    FLegendHeight := ReadFloat('Diagram', 'LegendHeight', FLegendHeight);
    FLegendTextHeight := ReadFloat('Diagram', 'LegendTextHeight',
      FLegendTextHeight);
    FLegendTextDistance := ReadFloat('Diagram', 'LegendTextDistance',
      FLegendTextDistance);
    FLegendFormat := ReadString('Diagram', 'LegendFormat', FLegendFormat);
    FScaleX := ReadFloat('Diagram', 'ScaleX', FScaleX);
    FVerticalExaggeration := ReadFloat('Diagram', 'VerticalExaggeration',
      FVerticalExaggeration);
    FAutoScale := ReadBool('Diagram', 'AutoScale', FAutoScale);
    FLegendSize := ReadFloat('Diagram', 'LegendSize', FLegendSize);

    FLegendLabel := ReadString('Diagram', 'LegendLabel', FLegendLabel);
    FLegendLabelDistance := ReadFloat('Diagram', 'LegendLabelDistance',
      FLegendLabelDistance);
    FLegendLabelPos := ReadInteger('Diagram', 'LegendLabelPos',
      FLegendLabelPos);
    FLegendLabelSize := ReadFloat('Diagram', 'LegendLabelSize', FLegendLabelSize);
  end;
end;

procedure TCadDiagramRecord.UnlockNotify;
begin
  FNotify := true;
end;

procedure TCadDiagramRecord.SetSubTitle(const Value: string);
begin
  FSubTitle := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetSubTitleSize(const Value: double);
begin
  FSubTitleSize := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetTitle(const Value: string);
begin
  FTitle := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetTitleDistance(const Value: double);
begin
  FTitleDistance := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetTitleSize(const Value: double);
begin
  FTitleSize := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetTitleSpacing(const Value: double);
begin
  FTitleSpacing := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetOnChangeValue(const Value: TNotifyEvent);
begin
  FOnChangeValue := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendDistance(const Value: double);
begin
  FLegendDistance := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetShowLegend(const Value: boolean);
begin
  FShowLegend := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendHeight(const Value: double);
begin
  FLegendHeight := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendLabel(const Value: string);
begin
  FLegendLabel := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendLabelDistance(const Value: double);
begin
  FLegendLabelDistance := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendLabelPos(const Value: integer);
begin
  FLegendLabelPos := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendLabelSize(const Value: double);
begin
  FLegendLabelSize := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendTextDistance(const Value: double);
begin
  FLegendTextDistance := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendTextHeight(const Value: double);
begin
  FLegendTextHeight := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendFormat(const Value: string);
begin
  FLegendFormat := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetScaleX(const Value: double);
begin
  FScaleX := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetVerticalExaggeration(const Value: double);
begin
  FVerticalExaggeration := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetAutoScale(const Value: boolean);
begin
  FAutoScale := Value;
  ChangeValue;
end;

procedure TCadDiagramRecord.SetLegendSize(const Value: double);
begin
  FLegendSize := Value;
  ChangeValue;
end;

{ TCadAxesRecord }

constructor TCadAxesRecord.Create(AOwner: TComponent);
begin
  inherited Create;
  FOnChangeValue := nil;
  FOnChangeAutoX := nil;
  FOnChangeAutoY := nil;
  FAutoX := true;
  FAutoY := true;
  FMinX := 0;
  FMinY := 0;
  FMaxX := 1;
  FMaxY := 1;
  FTickDistanceX := 0.1;
  FTickDistanceY := 0.1;
  FTickSize := 0.01;
  FTickLabelSize := 0.02;
  FTickLabelDistance := 0.02;
  FTickLabelFormat := '%g';
  FTicksTop := true;
  FTicksBottom := false;
  FTicksLeft := true;
  FTicksRight := true;
  FLabelX := 'x';
  FLabelY := 'y';
  FLabelUnitX := '(m)';
  FLabelUnitY := '(m)';
  FLabelSize := 10;
  FLabelUnitSize := 10;
  FLabelDistance := 2;
  FLabelDistanceX := 2;
  FLabelDistanceY := 2;
  FLabelUnitSpacing := 2;
  FNotify := true;
  FLabelXVisible := true;
  FLabelYVisible := true;
end;

destructor TCadAxesRecord.Destroy;
begin

  inherited;
end;

procedure TCadAxesRecord.Assign(Source: TPersistent);
begin
  if Source is TCadAxesRecord then
    with TCadAxesRecord(Source) do
    begin
      Self.FAutoX := FAutoX;
      Self.FAutoY := FAutoY;
      Self.FMinX := FMinX;
      Self.FMaxX := FMaxX;
      Self.FMinY := FMinY;
      Self.FMaxY := FMaxY;
      Self.FTickDistanceX := FTickDistanceX;
      Self.FTickDistanceY := FTickDistanceY;
      Self.FTickSize := FTickSize;
      Self.FTickLabelDistance := FTickLabelDistance;
      Self.FTickLabelSize := FTickLabelSize;
      Self.FTickLabelFormat := FTickLabelFormat;
      Self.FTicksTop := FTicksTop;
      Self.FTicksBottom := FTicksBottom;
      Self.FTicksLeft := FTicksLeft;
      Self.FTicksRight := FTicksRight;
      Self.FLabelX := FLabelX;
      Self.FLabelY := FLabelY;
      Self.FLabelUnitX := FLabelUnitX;
      Self.FLabelUnitY := FLabelUnitY;
      Self.FLabelSize := FLabelSize;
      Self.FLabelUnitSize := FLabelUnitSize;
      Self.FLabelDistance := FLabelDistance;
      Self.FLabelDistanceX := FLabelDistanceX;
      Self.FLabelDistanceY := FLabelDistanceY;
      Self.FLabelUnitSpacing := FLabelUnitSpacing;
      Self.FLabelXVisible := FLabelXVisible;
      Self.FLabelYVisible := FLabelYVisible;

      Self.FLabelXVisible := FLabelXVisible;
      Self.FLabelYVisible := FLabelYVisible;
      Self.FLabelXJustify := FLabelXJustify;
      Self.FLabelYJustify := FLabelYJustify;
      Self.FLabelXPosition := FLabelXPosition;
      Self.FLabelYPosition := FLabelYPosition;
      Self.FLabelUnitXVisible := FLabelUnitXVisible;
      Self.FLabelUnitYVisible := FLabelUnitYVisible;

    end
    else
      inherited; // raises an exception
end;

procedure TCadAxesRecord.SaveToIni(Dest: TIniFile);
begin
  with Dest do
  begin
    WriteBool('Axes', 'AutoX', FAutoX);
    WriteBool('Axes', 'AutoY', FAutoY);
    WriteFloat('Axes', 'MinX', FMinX);
    WriteFloat('Axes', 'MaxX', FMaxX);
    WriteFloat('Axes', 'MinY', FMinY);
    WriteFloat('Axes', 'MaxY', FMaxY);
    WriteFloat('Axes', 'TickDistanceX', FTickDistanceX);
    WriteFloat('Axes', 'TickDistanceY', FTickDistanceY);
    WriteFloat('Axes', 'TickSize', FTickSize);
    WriteFloat('Axes', 'TickLabelDistance', FTickLabelDistance);
    WriteFloat('Axes', 'TickLabelSize', FTickLabelSize);
    WriteString('Axes', 'TickLabelFormat', FTickLabelFormat);
    WriteBool('Axes', 'TicksTop', FTicksTop);
    WriteBool('Axes', 'TicksBottom', FTicksBottom);
    WriteBool('Axes', 'TicksLeft', FTicksLeft);
    WriteBool('Axes', 'TicksRight', FTicksRight);
    WriteString('Axes', 'LabelX', FLabelX);
    WriteString('Axes', 'LabelY', FLabelY);
    WriteString('Axes', 'LabelUnitX', FLabelUnitX);
    WriteString('Axes', 'LabelUnitY', FLabelUnitY);
    WriteFloat('Axes', 'LabelSize', FLabelSize);
    WriteFloat('Axes', 'LabelUnitSize', FLabelUnitSize);
    WriteFloat('Axes', 'LabelDistance', FLabelDistance);
    WriteFloat('Axes', 'LabelDistanceX', FLabelDistanceX);
    WriteFloat('Axes', 'LabelDistanceY', FLabelDistanceY);
    WriteFloat('Axes', 'LabelUnitSpacing', FLabelUnitSpacing);
    WriteBool('Axes', 'LabelXVisible', FLabelXVisible);
    WriteBool('Axes', 'LabelYVisible', FLabelYVisible);

    if FLabelXJustify = tjLeft then
      WriteInteger('Axes', 'LabelXJustify', 0);
    if FLabelXJustify = tjCenter then
      WriteInteger('Axes', 'LabelXJustify', 1);
    if FLabelXJustify = tjRight then
      WriteInteger('Axes', 'LabelXJustify', 2);

    if FLabelYJustify = tjLeft then
      WriteInteger('Axes', 'LabelYJustify', 0);
    if FLabelYJustify = tjCenter then
      WriteInteger('Axes', 'LabelYJustify', 1);
    if FLabelYJustify = tjRight then
      WriteInteger('Axes', 'LabelYJustify', 2);

    WriteInteger('Axes', 'LabelXPosition', FLabelXPosition);
    WriteInteger('Axes', 'LabelYPosition', FLabelYPosition);
    WriteBool('Axes', 'LabelUnitXVisible', FLabelUnitXVisible);
    WriteBool('Axes', 'LabelUnitYVisible', FLabelUnitYVisible);
  end;
end;

procedure TCadAxesRecord.ReadFromIni(Source: TIniFile);
var
  TempVal: integer;
begin
  with Source do
  begin
    FAutoX := ReadBool('Axes', 'AutoX', FAutoX);
    FAutoY := ReadBool('Axes', 'AutoY', FAutoY);
    FMinX := ReadFloat('Axes', 'MinX', FMinX);
    FMaxX := ReadFloat('Axes', 'MaxX', FMaxX);
    FMinY := ReadFloat('Axes', 'MinY', FMinY);
    FMaxY := ReadFloat('Axes', 'MaxY', FMaxY);
    FTickDistanceX := ReadFloat('Axes', 'TickDistanceX', FTickDistanceX);
    FTickDistanceY := ReadFloat('Axes', 'TickDistanceY', FTickDistanceY);
    FTickSize := ReadFloat('Axes', 'TickSize', FTickSize);
    FTickLabelDistance := ReadFloat('Axes', 'TickLabelDistance',
      FTickLabelDistance);
    FTickLabelSize := ReadFloat('Axes', 'TickLabelSize', FTickLabelSize);
    FTickLabelFormat := ReadString('Axes', 'TickLabelFormat', FTickLabelFormat);
    FTicksTop := ReadBool('Axes', 'TicksTop', FTicksTop);
    FTicksBottom := ReadBool('Axes', 'TicksBottom', FTicksBottom);
    FTicksLeft := ReadBool('Axes', 'TicksLeft', FTicksLeft);
    FTicksRight := ReadBool('Axes', 'TicksRight', FTicksRight);
    FLabelX := ReadString('Axes', 'LabelX', FLabelX);
    FLabelY := ReadString('Axes', 'LabelY', FLabelY);
    FLabelUnitX := ReadString('Axes', 'LabelUnitX', FLabelUnitX);
    FLabelUnitY := ReadString('Axes', 'LabelUnitY', FLabelUnitY);
    FLabelSize := ReadFloat('Axes', 'LabelSize', FLabelSize);
    FLabelUnitSize := ReadFloat('Axes', 'LabelUnitSize', FLabelUnitSize);
    FLabelDistance := ReadFloat('Axes', 'LabelDistance', FLabelDistance);
    FLabelDistanceX := ReadFloat('Axes', 'LabelDistanceX', FLabelDistanceX);
    FLabelDistanceY := ReadFloat('Axes', 'LabelDistanceY', FLabelDistanceY);
    FLabelUnitSpacing := ReadFloat('Axes', 'LabelUnitSpacing',
      FLabelUnitSpacing);
    FLabelXVisible := ReadBool('Axes', 'LabelXVisible', FLabelXVisible);
    FLabelYVisible := ReadBool('Axes', 'LabelYVisible', FLabelYVisible);

    TempVal := ReadInteger('Axes', 'LabelXJustify', 0);

    if TempVal = 0 then
      FLabelXJustify := tjLeft;
    if TempVal = 1 then
      FLabelXJustify := tjCenter;
    if TempVal = 2 then
      FLabelXJustify := tjRight;

    TempVal := ReadInteger('Axes', 'LabelYJustify', 0);

    if TempVal = 0 then
      FLabelYJustify := tjLeft;
    if TempVal = 1 then
      FLabelYJustify := tjCenter;
    if TempVal = 2 then
      FLabelYJustify := tjRight;

    FLabelXPosition := ReadInteger('Axes', 'LabelXPosition', FLabelXPosition);
    FLabelYPosition := ReadInteger('Axes', 'LabelYPosition', FLabelYPosition);
    FLabelUnitXVisible := ReadBool('Axes', 'LabelUnitXVisible',
      FLabelUnitXVisible);
    FLabelUnitYVisible := ReadBool('Axes', 'LabelUnitYVisible',
      FLabelUnitYVisible);

  end;
end;

procedure TCadAxesRecord.ChangeAutoX;
begin
  if assigned(OnChangeAutoX) and FNotify then
    OnChangeAutoX(Self);
end;

procedure TCadAxesRecord.ChangeAutoY;
begin
  if assigned(OnChangeAutoY) and FNotify then
    OnChangeAutoY(Self);
end;

procedure TCadAxesRecord.ChangeValue;
begin
  if assigned(OnChangeValue) and FNotify then
    OnChangeValue(Self);
end;

procedure TCadAxesRecord.SetAutoX(const Value: boolean);
begin
  FAutoX := Value;
  ChangeAutoX;
end;

procedure TCadAxesRecord.SetAutoY(const Value: boolean);
begin
  FAutoY := Value;
  ChangeAutoY;
end;

procedure TCadAxesRecord.SetMaxX(const Value: double);
begin
  FMaxX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetMaxY(const Value: double);
begin
  FMaxY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetMinX(const Value: double);
begin
  FMinX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetMinY(const Value: double);
begin
  FMinY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTickDistanceX(const Value: double);
begin
  FTickDistanceX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTickDistanceY(const Value: double);
begin
  FTickDistanceY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTickSize(const Value: double);
begin
  FTickSize := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTicksLeft(const Value: boolean);
begin
  FTicksLeft := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTicksRight(const Value: boolean);
begin
  FTicksRight := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTicksTop(const Value: boolean);
begin
  FTicksTop := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetOnChangeAutoX(const Value: TNotifyEvent);
begin
  FOnChangeAutoX := Value;
end;

procedure TCadAxesRecord.SetOnChangeAutoY(const Value: TNotifyEvent);
begin
  FOnChangeAutoY := Value;
end;

procedure TCadAxesRecord.SetOnChangeValue(const Value: TNotifyEvent);
begin
  FOnChangeValue := Value;
end;

procedure TCadAxesRecord.LockNotify;
begin
  FNotify := false;
end;

procedure TCadAxesRecord.UnlockNotify;
begin
  FNotify := true;
end;

procedure TCadAxesRecord.SetTickLabelDistance(const Value: double);
begin
  FTickLabelDistance := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTickLabelSize(const Value: double);
begin
  FTickLabelSize := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTicksBottom(const Value: boolean);
begin
  FTicksBottom := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelDistance(const Value: double);
begin
  FLabelDistance := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelDistanceX(const Value: double);
begin
  FLabelDistanceX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelDistanceY(const Value: double);
begin
  FLabelDistanceY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelSize(const Value: double);
begin
  FLabelSize := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitSize(const Value: double);
begin
  FLabelUnitSize := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitSpacing(const Value: double);
begin
  FLabelUnitSpacing := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitX(const Value: string);
begin
  FLabelUnitX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitXVisible(const Value: boolean);
begin
  FLabelUnitXVisible := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitY(const Value: string);
begin
  FLabelUnitY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelUnitYVisible(const Value: boolean);
begin
  FLabelUnitYVisible := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelX(const Value: string);
begin
  FLabelX := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelXJustify(const Value: TCadTextJustify);
begin
  FLabelXJustify := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelXPosition(const Value: integer);
begin
  FLabelXPosition := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelXVisible(const Value: boolean);
begin
  FLabelXVisible := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelY(const Value: string);
begin
  FLabelY := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelYJustify(const Value: TCadTextJustify);
begin
  FLabelYJustify := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelYPosition(const Value: integer);
begin
  FLabelYPosition := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetLabelYVisible(const Value: boolean);
begin
  FLabelYVisible := Value;
  ChangeValue;
end;

procedure TCadAxesRecord.SetTickLabelFormat(const Value: string);
begin
  FTickLabelFormat := Value;
  ChangeValue;
end;

{ TCadMeshRecord }

constructor TCadMeshRecord.Create(AOwner: TComponent);
begin
  inherited Create;
  FNotify := true;

  FAutoLimits := true;
  FMaxValue := 1;
  FMinValue := -1;
  FMeshType := mtFilled;
  FTriangleImplementation := tiInternal;
  FTriangleExecutable := '';
  FDuplicateProcessing := tdMean;
end;

procedure TCadMeshRecord.Assign(Source: TPersistent);
begin
  if Source is TCadMeshRecord then
    with TCadMeshRecord(Source) do
    begin
      Self.FAutoLimits := FAutoLimits;
      Self.FMaxValue := FMaxValue;
      Self.FMinValue := FMinValue;
      Self.FMeshType := FMeshType;
      Self.FClipSurface := FClipSurface;
      Self.FTriangleExecutable := FTriangleExecutable;
      Self.FTriangleImplementation := FTriangleImplementation;
      Self.FDuplicateProcessing := FDuplicateProcessing;
    end
    else
      inherited; // raises an exception
end;

procedure TCadMeshRecord.SaveToIni(Dest: TIniFile);
begin
  with Dest do
  begin
    WriteBool('Mesh', 'AutoLimits', FAutoLimits);
    WriteFloat('Mesh', 'MaxValue', FMaxValue);
    WriteFloat('Mesh', 'MinValue', FMinValue);
  end;
end;

procedure TCadMeshRecord.ReadFromIni(Source: TIniFile);
begin
  with Source do
  begin
    FAutoLimits := ReadBool('Mesh', 'AutoLimits', FAutoLimits);
    FMaxValue := ReadFloat('Mesh', 'MaxValue', FMaxValue);
    FMinValue := ReadFloat('Mesh', 'MinValue', FMinValue);
  end;
end;

destructor TCadMeshRecord.Destroy;
begin

  inherited;
end;

procedure TCadMeshRecord.LockNotify;
begin
  FNotify := false;
end;

procedure TCadMeshRecord.UnlockNotify;
begin
  FNotify := true;
end;

procedure TCadMeshRecord.ChangeValue;
begin
  if assigned(OnChangeValue) and FNotify then
    OnChangeValue(Self);
end;

procedure TCadMeshRecord.SetAlignWithSurface(const Value: boolean);
begin
  FAlignWithSurface := Value;
end;

procedure TCadMeshRecord.SetAutoLimits(const Value: boolean);
begin
  FAutoLimits := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetMaxValue(const Value: double);
begin
  FMaxValue := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetMeshType(const Value: TMeshType);
begin
  FMeshType := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetMinValue(const Value: double);
begin
  FMinValue := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetOnChangeValue(const Value: TNotifyEvent);
begin
  FOnChangeValue := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetClipSurface(const Value: boolean);
begin
  FClipSurface := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetClipSurfaceType
  (const Value: TCadMeshClipSurfaceType);
begin
  FClipSurfaceType := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetDuplicateProcessing
  (const Value: TCadMeshDuplicateProcessing);
begin
  FDuplicateProcessing := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetTriangleExecutable(const Value: string);
begin
  FTriangleExecutable := Value;
  ChangeValue;
end;

procedure TCadMeshRecord.SetTriangleImplementation
  (const Value: TTriangleImplementation);
begin
  FTriangleImplementation := Value;
  ChangeValue;
end;

{ TScaling }

constructor TScaling.Create;
begin
  FScaleX := 500;
  FUnitMultiplier := 1E3;
  FVerticalExaggeration := 1.0;
end;

function TScaling.G2L(d: double): double;
begin
  Result := FUnitMultiplier * d / FScaleX;
end;

function TScaling.L2G(d: double): double;
begin
  Result := d * FScaleX / FUnitMultiplier;
end;

procedure TScaling.SetScaleX(const Value: double);
begin
  FScaleX := Value;
end;

procedure TScaling.SetUnitMultiplier(const Value: double);
begin
  FUnitMultiplier := Value;
end;

procedure TScaling.SetVerticalExaggeration(const Value: double);
begin
  FVerticalExaggeration := Value;
end;

{ TCadPointList }

constructor TTrianglePointList.Create;
begin
  inherited;
  FPoints := TObjectList.Create(false);
end;

destructor TTrianglePointList.Destroy;
begin
  Self.Clear;
  FPoints.Free;
  inherited;
end;

procedure TTrianglePointList.AddPoint(x, y, Value: double);
var
  TP: TTrianglePoint;
begin
  TP := TTrianglePoint.Create;
  TP.x := x;
  TP.y := y;
  TP.Value := Value;
  FPoints.Add(TP)
end;

procedure TTrianglePointList.Clear;
var
  i: integer;
  TP: TTrianglePoint;
begin
  for i := 0 to FPoints.Count - 1 do
  begin
    TP := TTrianglePoint(FPoints[i]);
    TP.Free;
  end;
  FPoints.Clear;
end;

function TTrianglePointList.GetPoint(idx: integer): TTrianglePoint;
begin
  if (idx >= 0) and (idx < FPoints.Count) then
    Result := TTrianglePoint(FPoints[idx])
  else
    Result := nil;
end;

function TTrianglePointList.GetCount: integer;
begin
  Result := FPoints.Count;
end;

end.
