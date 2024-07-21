program rsi_bq;

{$I bybit_quik.inc}

(******************************************************************************)
(* Совеместная работа - Quik и Bybit                                          *)
(******************************************************************************)

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
  Lb.Bybit.Position in '..\..\bybit\lb\position\Lb.Bybit.Position.pas',

  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas',

  Lb.Indicator in '..\..\bybit\lb\indicator\Lb.Indicator.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.HistoryIndicator in 'lib\Lb.HistoryIndicator.pas',
  UnitSettingFrame in 'UnitSettingFrame.pas' {SettingFrame: TFrame},
  Lb.SysUtils in 'lib\Lb.SysUtils.pas',
  UnitStatusFrame in 'UnitStatusFrame.pas' {StatusFrame: TFrame},
  Lb.ApplicationVersion in '..\..\..\library\Lb.ApplicationVersion.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
