program rsi_bybit;

{$i debug.inc}



{$R 'db.res' '..\lib\history\db.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitOrderCategoryFrame in 'UnitOrderCategoryFrame.pas' {OrderCategoryFrame: TFrame},
  UnitMainClientFrame in 'UnitMainClientFrame.pas' {MainClientFrame: TFrame},
  UnitCategoryFrame in 'UnitCategoryFrame.pas' {CategoryFrame: TFrame},
  Lb.Bybit.SysUtils in '..\..\bybit\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Encryption in '..\..\bybit\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.RealTime in '..\..\bybit\lb\trade\Lb.Bybit.RealTime.pas',
  Lb.Bybit.Trade in '..\..\bybit\lb\trade\Lb.Bybit.Trade.pas',
  Lb.Bybit.InstrumentsInfo in '..\..\bybit\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.Kline in '..\..\bybit\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.OrderBook in '..\..\bybit\lb\market\Lb.Bybit.OrderBook.pas',
  Lb.Bybit.RecentTrade in '..\..\bybit\lb\market\Lb.Bybit.RecentTrade.pas',
  Lb.Bybit.ServerTime in '..\..\bybit\lb\market\Lb.Bybit.ServerTime.pas',
  Lb.Indicator in '..\..\bybit\lb\indicator\Lb.Indicator.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  UnitSettingFrame in 'UnitSettingFrame.pas' {SettingFrame: TFrame},
  UnitStatusFrame in 'UnitStatusFrame.pas' {StatusFrame: TFrame},
  Lb.ApplicationVersion in '..\..\..\library\Lb.ApplicationVersion.pas',
  Lb.Bybit.Position in '..\..\bybit\lb\position\Lb.Bybit.Position.pas',
  UnitWorkTableFrame in 'UnitWorkTableFrame.pas' {WorkTableFrame: TFrame},
  UnitTableTradeFrame in 'UnitTableTradeFrame.pas' {TableTradeFrame: TFrame},
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.History.DB in '..\lib\Lb.History.DB.pas',
  Lb.HistoryIndicator.Bybit in '..\lib\Lb.HistoryIndicator.Bybit.pas',
  Lb.SysUtils in '..\lib\Lb.SysUtils.pas',
  Lb.Trader in '..\lib\Lb.Trader.pas',
  Lb.VirtualTrade in '..\lib\Lb.VirtualTrade.pas',
  Lb.Level in '..\lib\Lb.Level.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
