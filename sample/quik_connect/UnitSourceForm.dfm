object SourceForm: TSourceForm
  Left = 0
  Top = 0
  Caption = 'SourceForm'
  ClientHeight = 466
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    573
    466)
  PixelsPerInch = 96
  TextHeight = 13
  object StrGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 557
    Height = 450
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultRowHeight = 18
    DoubleBuffered = True
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 280
    Top = 264
  end
end
