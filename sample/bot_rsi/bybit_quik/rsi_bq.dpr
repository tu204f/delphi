program rsi_bq;

{$i platform.inc}
{$R 'db.res' '..\lib\history\db.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitOrderCategoryFrame in 'UnitOrderCategoryFrame.pas' {OrderCategoryFrame: TFrame},
  UnitMainClientFrame in 'UnitMainClientFrame.pas' {MainClientFrame: TFrame},
  UnitCategoryFrame in 'UnitCategoryFrame.pas' {CategoryFrame: TFrame},
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  UnitPlatformSettingFrame in 'setting\UnitPlatformSettingFrame.pas' {PlatformSettingFrame: TFrame},
  UnitStatusFrame in 'UnitStatusFrame.pas' {StatusFrame: TFrame},
  Lb.ApplicationVersion in '..\..\..\library\Lb.ApplicationVersion.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.History.DB in '..\lib\Lb.History.DB.pas',
  Lb.SysUtils in '..\lib\Lb.SysUtils.pas',
  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas',
  UnitSettingBybitFrame in 'setting\UnitSettingBybitFrame.pas' {SettingBybitFrame: TFrame},
  UnitSettingQuikFrame in 'setting\UnitSettingQuikFrame.pas' {SettingQuikFrame: TFrame},
  UnitTableFrame in 'table\UnitTableFrame.pas' {TableFrame: TFrame},
  UnitTableVirtualTrade in 'table\UnitTableVirtualTrade.pas' {TableVirtualTradeFrame: TFrame},
  UnitQuikExportFrame in 'table\UnitQuikExportFrame.pas' {QuikExportFrame: TFrame},
  BTMemoryModule in '..\..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\..\library\trade\libquik\QuikTransOrder.pas',
  Lb.Level in '..\lib\Lb.Level.pas',
  Lb.Bybit.Encryption in '..\..\bybit\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\bybit\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.RealTime in '..\..\bybit\lb\trade\Lb.Bybit.RealTime.pas',
  Lb.Bybit.Trade in '..\..\bybit\lb\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.Position in '..\..\bybit\lb\position\Lb.Bybit.Position.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\bybit\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\bybit\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\bybit\lb\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.RecentTrade in '..\..\bybit\lb\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.ServerTime in '..\..\bybit\lb\market\Lb.Bybit.ServerTime.pas',
  Lb.Indicator in '..\..\bybit\lb\indicator\Lb.Indicator.pas',
  Lb.Status.Quik in 'status\Lb.Status.Quik.pas',
  Lb.Status.Bybit in 'status\Lb.Status.Bybit.pas',
  Lb.Status in 'status\Lb.Status.pas',
  Lb.HistoryIndicator in '..\lib\Lb.HistoryIndicator.pas',
  UnitSettingFrame in 'setting\UnitSettingFrame.pas' {SettingFrame: TFrame},
  UnitSettingLevelFrame in 'setting\UnitSettingLevelFrame.pas' {SettingLevelFrame: TFrame},
  UnitLogForm in 'log\UnitLogForm.pas' {LogForm},
  UnitBybitExportFrame in 'table\UnitBybitExportFrame.pas' {BybitExportFrame: TFrame},
  UnitSettingTacticsFrame in 'setting\UnitSettingTacticsFrame.pas' {SettingTacticsFrame: TFrame},
  Lb.VirtualTrade.V2 in '..\lib\Lb.VirtualTrade.V2.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
