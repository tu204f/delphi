program recent_trade;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.RecentTrade in '..\..\lb\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\lb\market\Lb.Bybit.InstrumentsInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
