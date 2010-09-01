object frmMain: TfrmMain
  Left = 263
  Top = 667
  Width = 403
  Height = 338
  HorzScrollBar.Visible = False
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CadWinDevice: TCadWinDevice
    Left = 0
    Top = 29
    Width = 395
    Height = 277
    CadCanvas = CadCanvas
    ScaleFactor = 2.47
    AutoSize = True
    DrawBackground = False
    FileName = 'noname.wmf'
    MarginX = 15
    MarginY = 15
    SolidOutlines = False
    PointSize = 2
    Align = alClient
    Color = clWhite
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 395
    Height = 29
    ButtonHeight = 25
    Caption = 'ToolBar1'
    TabOrder = 0
    object btnCreate: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Create'
      TabOrder = 0
      OnClick = btnCreateClick
    end
    object btnTest1: TButton
      Left = 75
      Top = 2
      Width = 50
      Height = 25
      Caption = 'Test 1'
      TabOrder = 4
      OnClick = btnTest1Click
    end
    object btnTest2: TButton
      Left = 125
      Top = 2
      Width = 52
      Height = 25
      Caption = 'Test 2'
      TabOrder = 5
      OnClick = btnTest2Click
    end
    object btnDraw: TButton
      Left = 177
      Top = 2
      Width = 44
      Height = 25
      Caption = 'Draw'
      TabOrder = 1
      OnClick = btnDrawClick
    end
    object btnExportDxf: TButton
      Left = 221
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Export DXF'
      TabOrder = 2
      OnClick = btnExportDxfClick
    end
    object btnExportWMF: TButton
      Left = 296
      Top = 2
      Width = 75
      Height = 25
      Caption = 'Export WMF'
      TabOrder = 3
      OnClick = btnExportWMFClick
    end
  end
  object CadCanvas: TCadCanvas
    Left = 4
    Top = 68
  end
  object CadDxfDevice: TCadDxfDevice
    CadCanvas = CadCanvas
    FileName = 'noname.dxf'
    Partial = True
    SolidOutlines = False
    Left = 36
    Top = 68
  end
end
