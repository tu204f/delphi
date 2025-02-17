program bq_ein_bot;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Indiñator in '..\lb\Lb.Indiñator.pas',
  Lb.Journal.Trading.v2 in '..\lb\Lb.Journal.Trading.v2.pas',
  Lb.Platform.Bybit in '..\lb\Lb.Platform.Bybit.pas',
  Lb.Platform in '..\lb\Lb.Platform.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas';

{$R *.res}

begin
  TLogger.ClearLog;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
