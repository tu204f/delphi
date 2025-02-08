program bq_ein_bot;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Platform.Bybit in '..\bq_rsi_2\lb\Lb.Platform.Bybit.pas',
  Lb.Platform in '..\bq_rsi_2\lb\Lb.Platform.pas',
  Lb.SysUtils in '..\bq_rsi_2\lb\Lb.SysUtils.pas',
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Indiñator in '..\bq_rsi_2\lb\Lb.Indiñator.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Journal.Trading.v2 in '..\bq_rsi_2\lb\Lb.Journal.Trading.v2.pas';

{$R *.res}

begin
  TLogger.ClearLog;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
