object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 411
  ClientWidth = 801
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 401
    Height = 395
    Lines.Strings = (
      '{'
      '    "retCode": 0,'
      '    "retMsg": "OK",'
      '    "result": {'
      '        "symbol": "BTCUSD",'
      '        "category": "inverse",'
      '        "list": ['
      '            ['
      '                "1670608800000",'
      '                "17071",'
      '                "17073",'
      '                "17027",'
      '                "17055.5",'
      '                "268611",'
      '                "15.74462667"'
      '            ],'
      '            ['
      '                "1670605200000",'
      '                "17071.5",'
      '                "17071.5",'
      '                "17061",'
      '                "17071",'
      '                "4177",'
      '                "0.24469757"'
      '            ],'
      '            ['
      '                "1670601600000",'
      '                "17086.5",'
      '                "17088",'
      '                "16978",'
      '                "17071.5",'
      '                "6356",'
      '                "0.37288112"'
      '            ]'
      '        ]'
      '    },'
      '    "retExtInfo": {},'
      '    "time": 1672025956592'
      '}')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 415
    Top = 8
    Width = 378
    Height = 395
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
