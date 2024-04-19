program indicator;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Indicator in '..\..\lb\indicator\Lb.Indicator.pas',
  Lb.SaveHistory in 'Lb.SaveHistory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
