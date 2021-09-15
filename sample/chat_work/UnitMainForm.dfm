object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 278
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 109
    Height = 25
    Caption = #1057#1090#1072#1088#1090' '#1089#1077#1088#1074#1077#1088
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 47
    Width = 269
    Height = 166
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 283
    Top = 47
    Width = 269
    Height = 166
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 283
    Top = 16
    Width = 106
    Height = 25
    Caption = #1057#1090#1072#1088#1090' '#1082#1083#1080#1077#1085#1090
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 283
    Top = 219
    Width = 269
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
  end
  object Button4: TButton
    Left = 283
    Top = 246
    Width = 75
    Height = 25
    Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100
    TabOrder = 5
  end
  object Button5: TButton
    Left = 168
    Top = 16
    Width = 109
    Height = 25
    Caption = #1057#1090#1086#1087' '#1089#1077#1088#1074#1077#1088
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 446
    Top = 16
    Width = 106
    Height = 25
    Caption = #1057#1090#1086#1087' '#1082#1083#1080#1077#1085#1090
    TabOrder = 7
    OnClick = Button6Click
  end
  object IdTCPClient1: TIdTCPClient
    OnDisconnected = IdTCPClient1Disconnected
    OnWork = IdTCPClient1Work
    OnConnected = IdTCPClient1Connected
    ConnectTimeout = 0
    Host = '127.0.0.1'
    Port = 5000
    ReadTimeout = -1
    Left = 396
    Top = 84
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    OnConnect = IdTCPServer1Connect
    OnDisconnect = IdTCPServer1Disconnect
    OnException = IdTCPServer1Exception
    OnExecute = IdTCPServer1Execute
    Left = 128
    Top = 84
  end
end
