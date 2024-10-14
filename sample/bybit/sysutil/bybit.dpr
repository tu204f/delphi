program bybit;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Bybit.RealTime in '..\..\..\library\trade\bybit\trade\Lb.Bybit.RealTime.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bot.Tiket in '..\..\..\library\trade\bybit\Lb.Bot.Tiket.pas',
  Lb.Bybit.Announcement in '..\..\..\library\trade\bybit\Lb.Bybit.Announcement.pas',
  Lb.Bybit.Candels in '..\..\..\library\trade\bybit\Lb.Bybit.Candels.pas',
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Test in '..\..\..\library\trade\bybit\Lb.Bybit.Test.pas',
  Lb.SysUtils in '..\..\..\library\trade\bybit\Lb.SysUtils.pas',
  Lb.TradeMan in '..\..\..\library\trade\bybit\Lb.TradeMan.pas',
  Lb.Bybit.Position in '..\..\..\library\trade\bybit\position\Lb.Bybit.Position.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\..\library\trade\bybit\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.RecentTrade in '..\..\..\library\trade\bybit\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  UnitCandelFrame in '..\..\..\library\trade\bybit\chart\UnitCandelFrame.pas' {CandelFrame: TFrame},
  UnitChartFrame in '..\..\..\library\trade\bybit\chart\UnitChartFrame.pas' {ChartFrame: TFrame},
  Lb.Logger in '..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
