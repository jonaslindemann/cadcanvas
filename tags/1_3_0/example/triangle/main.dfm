object frmMain: TfrmMain
  Left = 310
  Top = 402
  Caption = 'Auto'
  ClientHeight = 553
  ClientWidth = 671
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
    Left = 0
    Top = 145
    Width = 671
    Height = 408
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 671
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblXLimits: TLabel
      Left = 12
      Top = 24
      Width = 32
      Height = 13
      Caption = 'X limits'
    end
    object lblYLimits: TLabel
      Left = 12
      Top = 52
      Width = 32
      Height = 13
      Caption = 'Y limits'
    end
    object lblMin: TLabel
      Left = 56
      Top = 8
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object lblMax: TLabel
      Left = 124
      Top = 8
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object lblTickX: TLabel
      Left = 432
      Top = 20
      Width = 31
      Height = 13
      Caption = 'Tick X'
    end
    object lblTickY: TLabel
      Left = 432
      Top = 48
      Width = 31
      Height = 13
      Caption = 'Tick Y'
    end
    object Label1: TLabel
      Left = 400
      Top = 68
      Width = 69
      Height = 13
      Caption = 'Label distance'
    end
    object Label2: TLabel
      Left = 256
      Top = 96
      Width = 62
      Height = 13
      Caption = 'Exaggeration'
    end
    object Label3: TLabel
      Left = 404
      Top = 92
      Width = 67
      Height = 13
      Caption = 'Tick label size'
    end
    object edtXMin: TEdit
      Left = 56
      Top = 24
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 0
    end
    object edtXMax: TEdit
      Left = 124
      Top = 24
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 1
    end
    object edtYMin: TEdit
      Left = 56
      Top = 48
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 2
    end
    object edtYMax: TEdit
      Left = 124
      Top = 48
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 3
    end
    object chkAutoX: TCheckBox
      Left = 192
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Auto'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkAutoXClick
    end
    object chkAutoY: TCheckBox
      Left = 192
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Auto'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkAutoYClick
    end
    object btnUpdate: TButton
      Left = 548
      Top = 20
      Width = 75
      Height = 25
      Caption = 'Update'
      TabOrder = 6
      OnClick = btnUpdateClick
    end
    object edtTickX: TEdit
      Left = 476
      Top = 20
      Width = 61
      Height = 21
      TabOrder = 7
    end
    object edtTickY: TEdit
      Left = 476
      Top = 44
      Width = 61
      Height = 21
      TabOrder = 8
    end
    object btnAddX: TButton
      Left = 240
      Top = 24
      Width = 33
      Height = 21
      Caption = '+'
      TabOrder = 9
      OnClick = btnAddXClick
    end
    object btnSubtractX: TButton
      Left = 276
      Top = 24
      Width = 33
      Height = 21
      Caption = '-'
      TabOrder = 10
      OnClick = btnSubtractXClick
    end
    object btnAddY: TButton
      Left = 240
      Top = 48
      Width = 33
      Height = 21
      Caption = '+'
      TabOrder = 11
      OnClick = btnAddYClick
    end
    object btnSubtractY: TButton
      Left = 276
      Top = 48
      Width = 33
      Height = 21
      Caption = '-'
      TabOrder = 12
      OnClick = btnSubtractYClick
    end
    object chkShowTriangles: TCheckBox
      Left = 324
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Show triangles'
      TabOrder = 13
      OnClick = chkShowTrianglesClick
    end
    object edtVerticalExaggeration: TEdit
      Left = 332
      Top = 92
      Width = 61
      Height = 21
      TabOrder = 14
    end
    object edtTickLabelDistance: TEdit
      Left = 476
      Top = 68
      Width = 61
      Height = 21
      TabOrder = 15
    end
    object edtTickLabelSize: TEdit
      Left = 476
      Top = 92
      Width = 61
      Height = 21
      TabOrder = 16
    end
    object btnExport: TButton
      Left = 548
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Export'
      TabOrder = 17
      OnClick = btnExportClick
    end
    object btnTest1: TButton
      Left = 12
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Test1'
      TabOrder = 18
      OnClick = btnTest1Click
    end
    object Button1: TButton
      Left = 548
      Top = 83
      Width = 75
      Height = 25
      Caption = 'Export Image'
      TabOrder = 19
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 93
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Test2'
      TabOrder = 20
    end
    object AntialiasCheck: TCheckBox
      Left = 12
      Top = 122
      Width = 97
      Height = 17
      Caption = 'Antialias'
      Checked = True
      State = cbChecked
      TabOrder = 21
      OnClick = AntialiasCheckClick
    end
    object btnTest3: TButton
      Left = 175
      Top = 80
      Width = 50
      Height = 25
      Caption = 'Test3'
      TabOrder = 22
      OnClick = btnTest3Click
    end
  end
  object CadCanvas: TCadCanvas
    Left = 112
    Top = 156
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
    Left = 184
    Top = 156
  end
  object mnuMain: TMainMenu
    Left = 336
    Top = 165
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
    Left = 408
    Top = 164
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
    Left = 16
    Top = 156
  end
  object CadDxfDevice: TCadDxfDevice
    CadCanvas = CadCanvas
    FileName = 'test.dxf'
    Partial = False
    SolidOutlines = False
    Left = 112
    Top = 208
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'bmp'
    Filter = '*.bmp'
    Left = 408
    Top = 216
  end
end
