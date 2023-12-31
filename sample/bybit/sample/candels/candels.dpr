program candels;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Candels in '..\..\lb\Lb.Bybit.Candels.pas',
  Lb.SysUtils.Candel in '..\..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  UnitCandelFrame in '..\..\lb\chart\UnitCandelFrame.pas' {CandelFrame: TFrame},
  UnitChartFrame in '..\..\lb\chart\UnitChartFrame.pas' {ChartFrame: TFrame},
  Lb.SysUtils in '..\..\lb\Lb.SysUtils.pas',
  Lb.TradeMan in '..\..\lb\Lb.TradeMan.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.Setting in '..\..\..\..\library\setting\Lb.Setting.pas',
  Lb.ConfigIniFile in '..\..\..\..\library\setting\Lb.ConfigIniFile.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.ServerTime in '..\..\lb\market\Lb.Bybit.ServerTime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
