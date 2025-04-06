program bq_candel_4;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Indicator in '..\lb\Lb.Indicator.pas',
  Lb.Journal.Trading in '..\lb\Lb.Journal.Trading.pas',
  Lb.Platform.Bybit in '..\lb\Lb.Platform.Bybit.pas',
  Lb.Platform in '..\lb\Lb.Platform.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas',
  UnitWorkBotPanelFrame in 'frame\UnitWorkBotPanelFrame.pas' {WorkBotPanelFrame: TFrame},
  Lb.Breakdown in '..\lb\Lb.Breakdown.pas',
  Lb.Bot.Candel in '..\lb\Lb.Bot.Candel.pas',
  Lb.Bybit.OrderHistory in '..\..\..\library\trade\bybit\trade\Lb.Bybit.OrderHistory.pas',
  UnitPositionGridFrame in 'frame\UnitPositionGridFrame.pas' {PositionGridFrame: TFrame},
  UnitWorkBotGrid in 'frame\UnitWorkBotGrid.pas' {WorkBotGridFrame: TFrame},
  Lb.WorkBot.Grid in 'frame\Lb.WorkBot.Grid.pas',
  Lb.CustomWorkBot in '..\lb\Lb.CustomWorkBot.pas';

{$R *.res}

begin
  BybitHostDemo := True;

  TLogger.ClearLog;

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
