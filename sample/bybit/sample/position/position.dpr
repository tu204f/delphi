program position;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Position in '..\..\..\..\library\trade\bybit\position\Lb.Bybit.Position.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  BybitHostDemo := True;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
