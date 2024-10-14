program instrument;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.InstrumentsInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
