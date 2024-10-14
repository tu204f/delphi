program candels;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils.Candel in '..\..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.Setting in '..\..\..\..\library\setting\Lb.Setting.pas',
  Lb.ConfigIniFile in '..\..\..\..\library\setting\Lb.ConfigIniFile.pas',
  Lb.Bot.Tiket in '..\..\..\..\library\trade\bybit\Lb.Bot.Tiket.pas',
  Lb.Bybit.SysUtils in '..\..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Encryption in '..\..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.Kline in '..\..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.Candels in '..\..\..\..\library\trade\bybit\Lb.Bybit.Candels.pas',
  UnitCandelFrame in '..\..\..\..\library\trade\bybit\chart\UnitCandelFrame.pas' {CandelFrame: TFrame},
  UnitChartFrame in '..\..\..\..\library\trade\bybit\chart\UnitChartFrame.pas' {ChartFrame: TFrame},
  Lb.SysUtils in '..\..\..\..\library\trade\bybit\Lb.SysUtils.pas',
  Lb.TradeMan in '..\..\..\..\library\trade\bybit\Lb.TradeMan.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
