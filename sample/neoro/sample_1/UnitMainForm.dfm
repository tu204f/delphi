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
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 381
    Height = 341
    Proportional = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 8
    Top = 347
    Width = 33
    Height = 45
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -37
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 398
    Width = 33
    Height = 45
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -37
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 387
    Top = 399
    Width = 208
    Height = 45
    Caption = 'Acuracy: 0%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -37
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 10
    Top = 454
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
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
  object Chart1: TChart
    Left = 387
    Top = 0
    Width = 400
    Height = 341
    Legend.Visible = False
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    View3D = False
    TabOrder = 2
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TLineSeries
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Button3: TButton
    Left = 540
    Top = 356
    Width = 75
    Height = 25
    Caption = 'Acuracy'
    TabOrder = 3
    OnClick = Button3Click
  end
  object ProgressBar1: TProgressBar
    Left = 387
    Top = 450
    Width = 400
    Height = 17
    Min = 1
    Max = 600000
    Position = 1
    TabOrder = 4
  end
end
