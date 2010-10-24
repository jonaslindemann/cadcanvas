object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 427
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ReadButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 0
    OnClick = ReadButtonClick
  end
  object WriteButton: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Write'
    TabOrder = 1
    OnClick = WriteButtonClick
  end
  object EriViz: TEriViz
    AutoDetect = False
    Left = 172
    Top = 148
  end
end
