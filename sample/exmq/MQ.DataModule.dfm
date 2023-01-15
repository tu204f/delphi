object DM: TDM
  OldCreateOrder = False
  Height = 206
  Width = 235
  object FDConnection: TFDConnection
    Transaction = FDTransaction
    UpdateTransaction = FDTransaction
    Left = 56
    Top = 20
  end
  object FDTransaction: TFDTransaction
    Connection = FDConnection
    Left = 56
    Top = 76
  end
end
