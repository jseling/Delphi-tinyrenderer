object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Rasterizer'
  ClientHeight = 221
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 200
    Height = 200
    Stretch = True
    OnMouseMove = ImageMouseMove
  end
  object lblDuracao: TLabel
    Left = 214
    Top = 8
    Width = 50
    Height = 13
    Caption = 'lblDuracao'
  end
  object lblX: TLabel
    Left = 232
    Top = 40
    Width = 16
    Height = 13
    Caption = 'lblX'
  end
  object lblY: TLabel
    Left = 232
    Top = 59
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object lblZ: TLabel
    Left = 233
    Top = 78
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object lblPos: TLabel
    Left = 232
    Top = 112
    Width = 27
    Height = 13
    Caption = 'lblPos'
  end
end
