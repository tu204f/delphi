object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 305
  ClientWidth = 901
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    901
    305)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 89
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 36
    Width = 885
    Height = 261
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 13
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
    TabOrder = 1
  end
  object ProgressBar1: TProgressBar
    Left = 316
    Top = 8
    Width = 445
    Height = 17
    TabOrder = 2
  end
  object Timer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = TimerTimer
    Left = 220
    Top = 148
  end
end
