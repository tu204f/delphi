object DataModuleDB: TDataModuleDB
  OldCreateOrder = False
  Height = 379
  Width = 370
  object FDCommand: TFDCommand
    Connection = FDConnection
    Transaction = FDTransaction
    Left = 32
    Top = 92
  end
  object FDPhysDB2DriverLink: TFDPhysDB2DriverLink
    Left = 152
    Top = 24
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 152
    Top = 88
  end
  object FDTransaction: TFDTransaction
    Connection = FDConnection
    Left = 32
    Top = 148
  end
  object FDQuery: TFDQuery
    Transaction = FDTransaction
    UpdateTransaction = FDTransaction
    Left = 36
    Top = 220
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 272
    Top = 24
  end
  object FDConnection: TFDConnection
    Transaction = FDTransaction
    UpdateTransaction = FDTransaction
    Left = 40
    Top = 20
  end
  object FDUpdateSQL: TFDUpdateSQL
    Connection = FDConnection
    Left = 156
    Top = 284
  end
  object FDPhysPgDriverLink: TFDPhysPgDriverLink
    Left = 156
    Top = 156
  end
  object FDPhysFBDriverLink: TFDPhysFBDriverLink
    Left = 156
    Top = 216
  end
end
