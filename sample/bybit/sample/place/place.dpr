program place;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.ServerTime in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.Trade in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.RealTime in '..\..\..\..\library\trade\bybit\trade\Lb.Bybit.RealTime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
