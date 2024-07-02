object UserOrderFrame: TUserOrderFrame
  Left = 0
  Top = 0
  Width = 171
  Height = 290
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -23
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  ParentFont = False
  TabOrder = 0
  object ledValueRSI: TLabeledEdit
    Left = 28
    Top = 46
    Width = 40
    Height = 36
    EditLabel.Width = 144
    EditLabel.Height = 28
    EditLabel.Caption = 'ledValueRSI'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '50'
  end
  object UpDownValue1: TUpDown
    Left = 68
    Top = 46
    Width = 28
    Height = 36
    Associate = ledValueRSI
    Position = 50
    TabOrder = 1
  end
  object UpDownQuantity: TUpDown
    Left = 92
    Top = 4
    Width = 26
    Height = 36
    Associate = ledQuantity
    Min = 1
    Position = 1
    TabOrder = 2
  end
  object ledQuantity: TLabeledEdit
    Left = 28
    Top = 4
    Width = 64
    Height = 36
    EditLabel.Width = 24
    EditLabel.Height = 28
    EditLabel.Caption = #1050':'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = '1'
  end
  object CheckBoxActive1: TCheckBox
    Left = 9
    Top = 55
    Width = 16
    Height = 17
    Caption = '1'
    TabOrder = 4
  end
  object CheckBoxAuto1: TCheckBox
    Left = 153
    Top = 52
    Width = 20
    Height = 17
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object Button1: TButton
    Left = 9
    Top = 381
    Width = 122
    Height = 37
    Caption = 'Button1'
    TabOrder = 6
    OnClick = Button1Click
  end
  object UpDownValueActive1: TUpDown
    Left = 131
    Top = 46
    Width = 16
    Height = 36
    Associate = Edit1
    Min = -100
    Position = 5
    TabOrder = 7
  end
  object Edit1: TEdit
    Left = 95
    Top = 46
    Width = 36
    Height = 36
    TabOrder = 8
    Text = '5'
  end
  object LabeledEdit1: TLabeledEdit
    Left = 28
    Top = 83
    Width = 40
    Height = 36
    EditLabel.Width = 151
    EditLabel.Height = 28
    EditLabel.Caption = 'LabeledEdit1'
    LabelPosition = lpLeft
    TabOrder = 9
    Text = '50'
  end
  object UpDownValue2: TUpDown
    Left = 68
    Top = 83
    Width = 28
    Height = 36
    Associate = LabeledEdit1
    Position = 50
    TabOrder = 10
  end
  object UpDownValueActive2: TUpDown
    Left = 132
    Top = 83
    Width = 16
    Height = 36
    Associate = Edit2
    Min = -100
    Position = 5
    TabOrder = 11
  end
  object Edit2: TEdit
    Left = 96
    Top = 83
    Width = 36
    Height = 36
    TabOrder = 12
    Text = '5'
  end
  object LabeledEdit2: TLabeledEdit
    Left = 28
    Top = 120
    Width = 40
    Height = 36
    EditLabel.Width = 151
    EditLabel.Height = 28
    EditLabel.Caption = 'LabeledEdit2'
    LabelPosition = lpLeft
    TabOrder = 13
    Text = '50'
  end
  object UpDownValue3: TUpDown
    Left = 68
    Top = 120
    Width = 24
    Height = 36
    Associate = LabeledEdit2
    Position = 50
    TabOrder = 14
  end
  object UpDownValueActive3: TUpDown
    Left = 131
    Top = 120
    Width = 16
    Height = 36
    Associate = Edit3
    Min = -100
    Position = 5
    TabOrder = 15
  end
  object Edit3: TEdit
    Left = 95
    Top = 120
    Width = 36
    Height = 36
    TabOrder = 16
    Text = '5'
  end
  object CheckBoxAuto2: TCheckBox
    Left = 153
    Top = 90
    Width = 20
    Height = 17
    Checked = True
    State = cbChecked
    TabOrder = 17
  end
  object CheckBoxActive2: TCheckBox
    Left = 6
    Top = 88
    Width = 16
    Height = 17
    Caption = '1'
    TabOrder = 18
  end
  object CheckBoxAuto3: TCheckBox
    Left = 153
    Top = 126
    Width = 20
    Height = 17
    Checked = True
    State = cbChecked
    TabOrder = 19
  end
  object CheckBoxActive3: TCheckBox
    Left = 6
    Top = 129
    Width = 16
    Height = 17
    Caption = '1'
    TabOrder = 20
  end
  object LabeledEdit3: TLabeledEdit
    Left = 28
    Top = 166
    Width = 40
    Height = 36
    EditLabel.Width = 151
    EditLabel.Height = 28
    EditLabel.Caption = 'LabeledEdit2'
    LabelPosition = lpLeft
    TabOrder = 21
    Text = '50'
  end
  object UpDownValue4: TUpDown
    Left = 68
    Top = 166
    Width = 26
    Height = 36
    Associate = LabeledEdit3
    Position = 50
    TabOrder = 22
  end
  object UpDownValueActive4: TUpDown
    Left = 131
    Top = 166
    Width = 16
    Height = 36
    Associate = Edit4
    Min = -100
    Position = 6
    TabOrder = 23
  end
  object Edit4: TEdit
    Left = 95
    Top = 166
    Width = 36
    Height = 36
    TabOrder = 24
    Text = '6'
  end
  object CheckBoxAuto4: TCheckBox
    Left = 153
    Top = 172
    Width = 20
    Height = 18
    Checked = True
    State = cbChecked
    TabOrder = 25
  end
  object CheckBoxActive4: TCheckBox
    Left = 6
    Top = 175
    Width = 16
    Height = 18
    Caption = '1'
    TabOrder = 26
  end
  object LabeledEdit4: TLabeledEdit
    Left = 28
    Top = 206
    Width = 40
    Height = 36
    EditLabel.Width = 151
    EditLabel.Height = 28
    EditLabel.Caption = 'LabeledEdit2'
    LabelPosition = lpLeft
    TabOrder = 27
    Text = '50'
  end
  object UpDownValue5: TUpDown
    Left = 68
    Top = 206
    Width = 26
    Height = 36
    Associate = LabeledEdit4
    Position = 50
    TabOrder = 28
  end
  object UpDownValueActive5: TUpDown
    Left = 131
    Top = 206
    Width = 16
    Height = 36
    Associate = Edit5
    Min = -100
    Position = 5
    TabOrder = 29
  end
  object Edit5: TEdit
    Left = 95
    Top = 206
    Width = 36
    Height = 36
    TabOrder = 30
    Text = '5'
  end
  object CheckBoxAuto5: TCheckBox
    Left = 153
    Top = 212
    Width = 20
    Height = 18
    Checked = True
    State = cbChecked
    TabOrder = 31
  end
  object CheckBoxActive5: TCheckBox
    Left = 6
    Top = 215
    Width = 16
    Height = 18
    Caption = '1'
    TabOrder = 32
  end
  object LabeledEdit5: TLabeledEdit
    Left = 28
    Top = 246
    Width = 40
    Height = 36
    EditLabel.Width = 151
    EditLabel.Height = 28
    EditLabel.Caption = 'LabeledEdit2'
    LabelPosition = lpLeft
    TabOrder = 33
    Text = '50'
  end
  object UpDownValue6: TUpDown
    Left = 68
    Top = 246
    Width = 26
    Height = 36
    Associate = LabeledEdit5
    Position = 50
    TabOrder = 34
  end
  object UpDownValueActive6: TUpDown
    Left = 131
    Top = 246
    Width = 16
    Height = 36
    Associate = Edit6
    Min = -100
    Position = 5
    TabOrder = 35
  end
  object Edit6: TEdit
    Left = 95
    Top = 246
    Width = 36
    Height = 36
    TabOrder = 36
    Text = '5'
  end
  object CheckBoxAuto6: TCheckBox
    Left = 153
    Top = 248
    Width = 20
    Height = 18
    Checked = True
    State = cbChecked
    TabOrder = 37
  end
  object CheckBoxActive6: TCheckBox
    Left = 6
    Top = 255
    Width = 16
    Height = 18
    Caption = '1'
    TabOrder = 38
  end
  object CheckBoxQty: TCheckBox
    Left = 131
    Top = 13
    Width = 33
    Height = 17
    Checked = True
    State = cbChecked
    TabOrder = 39
  end
end
