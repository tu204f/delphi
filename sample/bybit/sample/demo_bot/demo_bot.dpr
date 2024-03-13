program demo_bot;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Setting in '..\..\..\..\library\Lb.Setting.pas',
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Test in '..\..\lb\Lb.Bybit.Test.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.RecentTrade in '..\..\lb\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.ServerTime in '..\..\lb\market\Lb.Bybit.ServerTime.pas',
  UnitCandelFrame in '..\..\lb\chart\UnitCandelFrame.pas' {CandelFrame: TFrame},
  UnitChartFrame in '..\..\lb\chart\UnitChartFrame.pas' {ChartFrame: TFrame},
  Lb.SysUtils.Candel in '..\..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Bybit.Candels in '..\..\lb\Lb.Bybit.Candels.pas',
  Lb.Bot.Tiket in 'Lb.Bot.Tiket.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
