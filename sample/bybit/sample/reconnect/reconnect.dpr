program reconnect;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFormMain in 'UnitFormMain.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit in '..\..\lb\Lb.Bybit.pas',
  Lb.Bybit.ServerTime in '..\..\lb\market\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Test in '..\..\lb\Lb.Bybit.Test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
