program bq_pair_2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Tickers in '..\..\..\library\trade\bybit\market\Lb.Bybit.Tickers.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas';

{$R *.res}

begin
  TLogger.ClearLog;
  BybitHostDemo := True;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
