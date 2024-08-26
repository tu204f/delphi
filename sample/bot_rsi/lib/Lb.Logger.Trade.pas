unit Lb.Logger.Trade;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils;

type
  TLoggerTrade = record
    Side: TQBTypeSide;
    Price: Double;
    Qty: Double;
    Line: TTypeLine;
    InfoMsg: String;
  end;


implementation

end.
