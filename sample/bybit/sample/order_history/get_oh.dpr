program get_oh;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  Lb.Bybit.Encryption in '..\..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.RecentTrade in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.ServerTime in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.RealTime in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.RealTime.pas',
  Lb.Bybit.Trade in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.OrderHistory in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.OrderHistory.pas',
  Lb.Bybit.TradeHistory in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.TradeHistory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
