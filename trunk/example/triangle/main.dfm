object frmMain: TfrmMain
  Left = 310
  Top = 402
  Caption = 'Auto'
  ClientHeight = 562
  ClientWidth = 949
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage32
    Left = 254
    Top = 25
    Width = 695
    Height = 537
    Align = alClient
    AutoSize = True
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    ExplicitLeft = 556
    ExplicitTop = 260
    ExplicitWidth = 393
    ExplicitHeight = 302
  end
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 5
    Top = 30
    Width = 244
    Height = 527
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 343
    object TabSheet1: TTabSheet
      Caption = 'Axes'
      ExplicitWidth = 931
      ExplicitHeight = 111
      object lblXLimits: TLabel
        Left = 6
        Top = 30
        Width = 32
        Height = 13
        Caption = 'X limits'
      end
      object lblMin: TLabel
        Left = 44
        Top = 11
        Width = 17
        Height = 13
        Caption = 'Min'
      end
      object lblMax: TLabel
        Left = 112
        Top = 11
        Width = 20
        Height = 13
        Caption = 'Max'
      end
      object lblYLimits: TLabel
        Left = 6
        Top = 54
        Width = 32
        Height = 13
        Caption = 'Y limits'
      end
      object edtXMin: TEdit
        Left = 44
        Top = 27
        Width = 61
        Height = 21
        Enabled = False
        TabOrder = 0
      end
      object edtXMax: TEdit
        Left = 112
        Top = 27
        Width = 61
        Height = 21
        Enabled = False
        TabOrder = 1
      end
      object chkAutoX: TCheckBox
        Left = 180
        Top = 27
        Width = 97
        Height = 17
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = chkAutoXClick
      end
      object chkAutoY: TCheckBox
        Left = 180
        Top = 50
        Width = 42
        Height = 17
        Caption = 'Auto'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkAutoYClick
      end
      object edtYMax: TEdit
        Left = 112
        Top = 51
        Width = 61
        Height = 21
        Enabled = False
        TabOrder = 4
      end
      object edtYMin: TEdit
        Left = 44
        Top = 51
        Width = 61
        Height = 21
        Enabled = False
        TabOrder = 5
      end
      object AxesProperties: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 94
        Width = 230
        Height = 402
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Strings.Strings = (
          'LabelDistance=10'
          'LabelSize=3'
          'LabelUnitSize=3'
          'LabelUnitSpacing=0.5'
          'LabelUnitX=(m)'
          'LabelUnitY=(m)'
          'LabelX=x'
          'LabelY=y'
          'TickDistanceX=10'
          'TickDistanceY=10'
          'TickLabelDistance=2'
          'TickLabelFormat=%.0f'
          'TickLabelSize=3'
          'TickSize=1'
          'TicksTops=1'
          'TicksBottom=1'
          'TicksLeft=1'
          'TicksRight=1')
        TabOrder = 6
        TitleCaptions.Strings = (
          'Property'
          'Value')
        OnStringsChange = AxesPropertiesStringsChange
        ColWidths = (
          93
          131)
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Diagram'
      ImageIndex = 1
      ExplicitWidth = 931
      ExplicitHeight = 111
      object DiagramProperties: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 230
        Height = 493
        Align = alClient
        Strings.Strings = (
          'Title=Title'
          'TitleSize=5'
          'SubTitle=Subtitle'
          'SubTitleSize=3.5'
          'TitleSpacing=2'
          'TitleDistance=20'
          'ShowLegend=1'
          'LegendDistance=20'
          'LegendHeight=5'
          'LegendTextHeight=2.5'
          'LegendTextDistance=2'
          'LegendFormat=%g'
          'VerticalExaggeration=1'
          'LegendSize=1')
        TabOrder = 0
        TitleCaptions.Strings = (
          'Property'
          'Value')
        OnStringsChange = AxesPropertiesStringsChange
        ColWidths = (
          93
          131)
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Mesh'
      ImageIndex = 2
      ExplicitWidth = 931
      ExplicitHeight = 111
      object chkShowTriangles: TCheckBox
        Left = 12
        Top = 9
        Width = 97
        Height = 17
        Caption = 'Show triangles'
        TabOrder = 0
        OnClick = chkShowTrianglesClick
      end
      object AntialiasCheck: TCheckBox
        Left = 12
        Top = 32
        Width = 97
        Height = 17
        Caption = 'Antialias'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = AntialiasCheckClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Tests'
      ImageIndex = 3
      ExplicitWidth = 931
      ExplicitHeight = 111
      object btnTest1: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Test1'
        TabOrder = 0
        OnClick = btnTest1Click
      end
      object Button2: TButton
        Left = 84
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Test2'
        TabOrder = 1
      end
      object btnTest3: TButton
        Left = 166
        Top = 3
        Width = 50
        Height = 25
        Caption = 'Test3'
        TabOrder = 2
        OnClick = btnTest3Click
      end
    end
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 949
    Height = 25
    AutoSize = True
    BandBorderStyle = bsNone
    Bands = <
      item
        Control = ToolBar2
        ImageIndex = -1
        Width = 947
      end>
    EdgeBorders = []
    object ToolBar2: TToolBar
      AlignWithMargins = True
      Left = 11
      Top = 0
      Width = 938
      Height = 25
      Margins.Top = 20
      ButtonHeight = 19
      ButtonWidth = 82
      Caption = 'ToolBar2'
      List = True
      AllowTextButtons = True
      TabOrder = 0
      object ToolButton4: TToolButton
        Left = 0
        Top = 0
        Caption = 'Update'
        ImageIndex = 0
        Style = tbsTextButton
        OnClick = btnUpdateClick
      end
      object ToolButton5: TToolButton
        Left = 51
        Top = 0
        Caption = 'Export...'
        ImageIndex = 1
        Style = tbsTextButton
        OnClick = btnExportClick
      end
      object ToolButton6: TToolButton
        Left = 106
        Top = 0
        Caption = 'Export image...'
        ImageIndex = 2
        Style = tbsTextButton
        OnClick = Button1Click
      end
    end
  end
  object CadCanvas: TCadCanvas
    Left = 552
    Top = 260
  end
  object CadG32Device: TCadG32Device
    CadCanvas = CadCanvas
    Image = Image
    ScaleFactor = 0.382978723404255300
    AutoSize = True
    AutoCenter = True
    DrawBackground = True
    FileName = 'noname.wmf'
    MarginX = 20
    MarginY = 20
    SolidOutlines = False
    PointSize = 2
    Antialias = True
    Left = 652
    Top = 196
  end
  object mnuMain: TMainMenu
    Left = 424
    Top = 185
    object mnuFile: TMenuItem
      Caption = 'File'
      object mnuFileOpen: TMenuItem
        Caption = 'Open...'
        OnClick = mnuFileOpenClick
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'txt'
    Filter = '*.txt'
    Title = 'Open data file'
    Left = 484
    Top = 184
  end
  object CadSurfaceDiagram2D: TCadSurfaceDiagram2D
    CadCanvas = CadCanvas
    Axes.AutoX = True
    Axes.AutoY = True
    Axes.MaxX = -1E300
    Axes.MinX = 1E300
    Axes.MaxY = -1E300
    Axes.MinY = 1E300
    Axes.TickDistanceX = 10.000000000000000000
    Axes.TickDistanceY = 10.000000000000000000
    Axes.TickSize = 1.000000000000000000
    Axes.TickLabelDistance = 2.000000000000000000
    Axes.TickLabelSize = 3.000000000000000000
    Axes.TickLabelFormat = '%g'
    Axes.TicksTop = True
    Axes.TicksBottom = True
    Axes.TicksLeft = True
    Axes.TicksRight = True
    Axes.LabelX = 'x'
    Axes.LabelY = 'y'
    Axes.LabelUnitX = '(m)'
    Axes.LabelUnitY = '(m)'
    Axes.LabelSize = 3.000000000000000000
    Axes.LabelUnitSize = 3.000000000000000000
    Axes.LabelDistance = 10.000000000000000000
    Axes.LabelUnitSpacing = 0.500000000000000000
    Diagram.ShowTriangles = False
    Diagram.Title = 'Title'
    Diagram.TitleSize = 5.000000000000000000
    Diagram.SubTitle = 'Subtitle'
    Diagram.SubTitleSize = 3.500000000000000000
    Diagram.TitleSpacing = 2.000000000000000000
    Diagram.TitleDistance = 30.000000000000000000
    Diagram.ShowLegend = False
    Diagram.LegendDistance = 30.000000000000000000
    Diagram.LegendHeight = 5.000000000000000000
    Diagram.LegendTextHeight = 3.000000000000000000
    Diagram.LegendTextDistance = 1.000000000000000000
    Diagram.LegendFormat = '%g'
    Diagram.ScaleX = 2000.000000000000000000
    Diagram.VerticalExaggeration = 1.000000000000000000
    Diagram.AutoScale = True
    Diagram.LegendSize = 2.000000000000000000
    Mesh.AutoLimits = True
    Mesh.MaxValue = 1.000000000000000000
    Mesh.MinValue = -1.000000000000000000
    Mesh.MeshType = mtFilled
    Mesh.ClipSurface = False
    Mesh.ClipSurfaceType = scRemoveCrossing
    Mesh.AlignWithSurface = False
    Mesh.TriangleImplementation = tiInternal
    Left = 556
    Top = 212
  end
  object CadDxfDevice: TCadDxfDevice
    CadCanvas = CadCanvas
    FileName = 'test.dxf'
    Partial = False
    SolidOutlines = False
    Left = 628
    Top = 252
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'bmp'
    Filter = '*.bmp'
    Left = 452
    Top = 244
  end
end
