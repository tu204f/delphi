program rsi_quik.v2;

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
  UnitSettingFrame in 'setting\UnitSettingFrame.pas' {SettingFrame: TFrame},
  UnitStatusFrame in 'UnitStatusFrame.pas' {StatusFrame: TFrame},
  Lb.ApplicationVersion in '..\..\..\library\Lb.ApplicationVersion.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Resource.Script in '..\..\..\library\Lb.Resource.Script.pas',
  Lb.History.DB in '..\lib\Lb.History.DB.pas',
  Lb.SysUtils in '..\lib\Lb.SysUtils.pas',
  Lb.VirtualTrade in '..\lib\Lb.VirtualTrade.pas',
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
