unit Lb.Bybit.Save;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.Bybit.SysUtils;

type
  TTrade = record
    ExecID: String;
    Symbol: String;
    Price: Double;
    Size: Double;
    Side: TTypeSide;
    Time: Int64;
  end;
  TTradeList = TList<TTrade>;

  TBufferTrade = class(TObject)

  end;

implementation

end.
