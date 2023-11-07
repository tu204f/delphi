object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 481
  ClientWidth = 795
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 387
    Top = 356
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 468
    Top = 356
    Width = 75
    Height = 25
    Caption = 'Learn'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 540
    Top = 356
    Width = 75
    Height = 25
    Caption = 'Acuracy'
    TabOrder = 2
  end
end
