object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 592
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 45
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo: TMemo
    Left = 8
    Top = 330
    Width = 715
    Height = 254
    Lines.Strings = (
      'Memo')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 135
    Top = 8
    Width = 121
    Height = 45
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object StrGridN1: TStringGrid
    Left = 8
    Top = 67
    Width = 77
    Height = 222
    ColCount = 1
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
    ScrollBars = ssNone
    TabOrder = 3
  end
  object StrGridW1: TStringGrid
    Left = 91
    Top = 67
    Width = 230
    Height = 222
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
    TabOrder = 4
  end
  object StrGridN2: TStringGrid
    Left = 327
    Top = 67
    Width = 77
    Height = 222
    ColCount = 1
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
    ScrollBars = ssNone
    TabOrder = 5
  end
  object StrGridW2: TStringGrid
    Left = 410
    Top = 67
    Width = 230
    Height = 222
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
    TabOrder = 6
  end
  object StrGridN3: TStringGrid
    Left = 646
    Top = 67
    Width = 77
    Height = 222
    ColCount = 1
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
    ScrollBars = ssNone
    TabOrder = 7
  end
  object Button3: TButton
    Left = 10
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 91
    Top = 295
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 296
    Top = 128
  end
end
