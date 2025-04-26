program bq_training;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.Tickers in '..\..\..\library\trade\bybit\market\Lb.Bybit.Tickers.pas',
  Lb.CustomWorkBot in '..\lb\Lb.CustomWorkBot.pas',
  Lb.Journal.Trading in '..\lb\Lb.Journal.Trading.pas',
  Lb.Platform.Bybit in '..\lb\Lb.Platform.Bybit.pas',
  Lb.Platform in '..\lb\Lb.Platform.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas',
  UnitPositionGridFrame in 'frame\UnitPositionGridFrame.pas' {PositionGridFrame: TFrame},
  Lb.WorkBot.Grid in 'frame\Lb.WorkBot.Grid.pas',
  Lb.Indicator in '..\lb\Lb.Indicator.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Securoty in '..\lb\Lb.Securoty.pas',
  Lb.Bot.GridBot in '..\lb\Lb.Bot.GridBot.pas',
  UnitHistoryFrame in 'frame\UnitHistoryFrame.pas' {HistoryFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
