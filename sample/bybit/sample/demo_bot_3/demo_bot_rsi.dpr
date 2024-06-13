program demo_bot_rsi;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Indicator in '..\..\lb\indicator\Lb.Indicator.pas',
  UnitOrderFrame in 'UnitOrderFrame.pas' {OrderFrame: TFrame},
  Lb.Bybit.RealTime in '..\..\lb\trade\Lb.Bybit.RealTime.pas',
  Lb.Bybit.Trade in '..\..\lb\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.ServerTime in '..\..\lb\market\Lb.Bybit.ServerTime.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.Setting in '..\..\..\..\library\Lb.Setting.pas',
  Lb.Bybit.OrderBook in '..\..\lb\market\Lb.Bybit.OrderBook.pas',
  UnitOrderBookFrame in 'order_book\UnitOrderBookFrame.pas' {OrderBookFrame: TFrame},
  UnitOrderBookRowFrame in 'order_book\UnitOrderBookRowFrame.pas' {OrderBookRowFrame: TFrame},
  Lb.HistoryIndicator in 'lb\Lb.HistoryIndicator.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.TradeBot in 'lb\Lb.TradeBot.pas',
  Lb.Params in 'lb\Lb.Params.pas',
  Lb.OperationTrade in 'lb\Lb.OperationTrade.pas',
  Lb.Instruments in 'lb\Lb.Instruments.pas',
  UnitSecurityFrame in 'lb\security\UnitSecurityFrame.pas' {SecurityFrame: TFrame},
  Lb.TableSCV in 'lb\security\Lb.TableSCV.pas',
  Lb.VirtualTrade in 'lb\agent\Lb.VirtualTrade.pas',
  UnitEditFrame in 'lb\edit\UnitEditFrame.pas' {ValueFrame: TFrame},
  UnitGridOrdersFrame in 'lb\UnitGridOrdersFrame.pas' {GridOrdersFrame: TFrame},
  UnitLineEditFrame in 'lb\edit\UnitLineEditFrame.pas' {LineEditFrame: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}
  TLogger.ClearLog;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
