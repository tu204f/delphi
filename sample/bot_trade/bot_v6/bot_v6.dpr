program bot_v6;

uses
  System.StartUpCopy,
  FMX.Forms,
  Lb.Setting in '..\..\..\library\Lb.Setting.pas',
  UnitDefaultFrame in 'module\default\UnitDefaultFrame.pas' {DefaultFrame: TFrame},
  Lb.Module.SysUtils in 'module\Lb.Module.SysUtils.pas',
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitTiketsMainFrame in 'module\tiket\UnitTiketsMainFrame.pas' {TiketsMainFrame: TFrame},
  UnitOpTradeFrame in 'module\op_trade\UnitOpTradeFrame.pas' {OpTradeFrame: TFrame},
  Lb.OpTrade in 'module\op_trade\Lb.OpTrade.pas',
  UnitConverCandel.V1 in 'module\conver_candel_v1\UnitConverCandel.V1.pas' {ConverCandelV1Frame: TFrame},
  Lb.SysUtils.Structure in 'module\conver_candel_v1\lib\Lb.SysUtils.Structure.pas',
  Lb.SysUtils.Structure.DB in 'module\conver_candel_v1\lib\Lb.SysUtils.Structure.DB.pas',
  Lb.SysUtils.Structure.Life in 'module\conver_candel_v1\lib\Lb.SysUtils.Structure.Life.pas',
  UnitChartsMainFrame in 'module\charts\UnitChartsMainFrame.pas' {ChartsMainFrame: TFrame},
  UnitChartCandelsFrame in 'module\charts\lib\UnitChartCandelsFrame.pas' {ChartCandelsFrame: TFrame},
  UnitTiketV2MainFrame in 'module\tiket_v2\UnitTiketV2MainFrame.pas' {TiketV2MainFrame: TFrame},
  Lb.Tiket.v2.Life in 'module\tiket_v2\lib\Lb.Tiket.v2.Life.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Tiket.v2.DataBase in 'module\tiket_v2\lib\Lb.Tiket.v2.DataBase.pas',
  UnitTitket.v2.Frame in 'module\tiket_v2\lib\UnitTitket.v2.Frame.pas' {TitketLifeFrame: TFrame},
  Lb.Tiket.v2.Life.v2 in 'module\tiket_v2\lib\Lb.Tiket.v2.Life.v2.pas',
  UnitTiketsMainFrameVer3 in 'module\tiket_v3\UnitTiketsMainFrameVer3.pas' {TiketsMainFrameVer3: TFrame},
  Lb.Trader in 'module\charts\lib\Lb.Trader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
