object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'MainForm'
  ClientHeight = 322
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    359
    322)
  PixelsPerInch = 96
  TextHeight = 18
  object ButtonTolls: TButton
    Left = 1
    Top = 2
    Width = 123
    Height = 25
    Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099
    TabOrder = 0
    OnClick = ButtonTollsClick
  end
  object EditSecurity: TEdit
    Left = 159
    Top = 2
    Width = 56
    Height = 36
    Alignment = taRightJustify
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = '000'
  end
  object ButtonQuikTable: TButton
    Left = 322
    Top = 2
    Width = 47
    Height = 25
    Caption = 'QUIK'
    TabOrder = 2
    OnClick = ButtonQuikTableClick
  end
  object GridPanel: TGridPanel
    Left = 1
    Top = 28
    Width = 356
    Height = 293
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = PanelLeft
        Row = 0
      end
      item
        Column = 1
        Control = PanelRight
        Row = 0
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 3
    object PanelLeft: TPanel
      Left = 1
      Top = 1
      Width = 177
      Height = 291
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 0
      inline UserOrderBuy: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 169
        Height = 283
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 169
        ExplicitHeight = 283
        inherited ledValueRSI: TLabeledEdit
          Top = 42
          Height = 33
          EditLabel.Width = 127
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -102
          EditLabel.ExplicitTop = 46
          EditLabel.ExplicitWidth = 127
          EditLabel.ExplicitHeight = 25
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited UpDownValue1: TUpDown
          Top = 42
          Height = 33
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited UpDownQuantity: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledQuantity: TLabeledEdit
          Height = 33
          EditLabel.Width = 23
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 2
          EditLabel.ExplicitTop = 9
          EditLabel.ExplicitWidth = 23
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited CheckBoxAuto1: TCheckBox
          Top = 50
          ExplicitTop = 50
        end
        inherited UpDownValueActive1: TUpDown
          Left = 132
          Top = 42
          Height = 33
          ExplicitLeft = 132
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited Edit1: TEdit
          Left = 96
          Top = 42
          Height = 33
          ExplicitLeft = 96
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited LabeledEdit1: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 88
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue2: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive2: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit2: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit2: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 125
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue3: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive3: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit3: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit3: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 171
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue4: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive4: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit4: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit4: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 211
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue5: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive5: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit5: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit5: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 251
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue6: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive6: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit6: TEdit
          Height = 33
          ExplicitHeight = 33
        end
      end
    end
    object PanelRight: TPanel
      Left = 178
      Top = 1
      Width = 177
      Height = 291
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 1
      inline UserOrderSell: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 169
        Height = 283
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitWidth = 169
        ExplicitHeight = 283
        inherited ledValueRSI: TLabeledEdit
          Top = 41
          Height = 33
          EditLabel.Width = 127
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -102
          EditLabel.ExplicitTop = 45
          EditLabel.ExplicitWidth = 127
          EditLabel.ExplicitHeight = 25
          ExplicitTop = 41
          ExplicitHeight = 33
        end
        inherited UpDownValue1: TUpDown
          Top = 41
          Height = 33
          ExplicitTop = 41
          ExplicitHeight = 33
        end
        inherited UpDownQuantity: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledQuantity: TLabeledEdit
          Height = 33
          EditLabel.Width = 23
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 2
          EditLabel.ExplicitTop = 9
          EditLabel.ExplicitWidth = 23
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited CheckBoxAuto1: TCheckBox
          Top = 51
          ExplicitTop = 51
        end
        inherited UpDownValueActive1: TUpDown
          Top = 42
          Height = 33
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited Edit1: TEdit
          Top = 42
          Height = 33
          ExplicitTop = 42
          ExplicitHeight = 33
        end
        inherited LabeledEdit1: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 88
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue2: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive2: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit2: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit2: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 125
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue3: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive3: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit3: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit3: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 171
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue4: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive4: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit4: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit4: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 211
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue5: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive5: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit5: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited LabeledEdit5: TLabeledEdit
          Height = 33
          EditLabel.Width = 135
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = -110
          EditLabel.ExplicitTop = 251
          EditLabel.ExplicitWidth = 135
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValue6: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownValueActive6: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited Edit6: TEdit
          Height = 33
          ExplicitHeight = 33
        end
        inherited CheckBoxQty: TCheckBox
          Left = 133
          Top = 12
          ExplicitLeft = 133
          ExplicitTop = 12
        end
      end
    end
  end
  object QtyEdit: TEdit
    Left = 247
    Top = 1
    Width = 54
    Height = 26
    TabOrder = 4
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 288
    Top = 8
  end
end
