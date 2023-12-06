program instrument;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.InstrumentsInfo in '..\..\lb\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\lb\Lb.Bybit.Kline.pas',
  Lb.Bybit.ServerTime in '..\..\lb\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
