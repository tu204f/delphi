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
  Lb.Category in 'lb\Lb.Category.pas',
  Lb.Platform.Trading in 'lb\Lb.Platform.Trading.pas',
  UnitCategoryFrame in 'frame\UnitCategoryFrame.pas' {CategoryFrame: TFrame},
  UniCategoryListFrame in 'frame\UniCategoryListFrame.pas' {CategoryListFrame: TFrame},
  Lb.ParamClose in 'lb\Lb.ParamClose.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
  TLogger.Log('Старт программы');
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
