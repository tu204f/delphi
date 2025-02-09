program rsi_bq_2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bot in 'lb\Lb.Bot.pas',
  Lb.SysUtils in 'lb\Lb.SysUtils.pas',
  Lb.Platform in 'lb\Lb.Platform.pas',
  BTMemoryModule in '..\..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\..\library\trade\libquik\QuikTransOrder.pas',
  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Platform.Bybit in 'lb\Lb.Platform.Bybit.pas',
  Lb.ManagerRisk in 'lb\Lb.ManagerRisk.pas',
  Lb.Bybit.Encryption in '..\..\..\library\trade\bybit\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\..\library\trade\bybit\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\..\library\trade\bybit\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\..\library\trade\bybit\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.ServerTime in '..\..\..\library\trade\bybit\market\Lb.Bybit.ServerTime.pas',
  Lb.Level in 'lb\Lb.Level.pas',
  Lb.TradeBox in 'lb\Lb.TradeBox.pas',
  Lb.Journal.Trading_Old in 'lb\Lb.Journal.Trading_Old.pas',
  Lb.ParamClose in 'lb\Lb.ParamClose.pas',
  Lb.Bybit.Trade in '..\..\..\library\trade\bybit\trade\Lb.Bybit.Trade.pas',
  UnitJournalPositionFrame in 'UnitJournalPositionFrame.pas' {JournalPositionFrame: TFrame},
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Journal.DB in 'lb\Lb.Journal.DB.pas',
  Lb.Indiñator in 'lb\Lb.Indiñator.pas',
  Lb.Journal.Trading.v2 in 'lb\Lb.Journal.Trading.v2.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
  TLogger.Log('Ñòàðò ïðîãðàììû');
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
