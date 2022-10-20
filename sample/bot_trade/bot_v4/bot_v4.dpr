program bot_v4;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils.Candel in '..\src\Lb.SysUtils.Candel.pas',
  UnitHistoryCandels in 'lib\UnitHistoryCandels.pas' {HistoryFrame: TFrame},
  UnitCandelFrame in '..\src\charts.v2\UnitCandelFrame.pas' {CandelFrame: TFrame},
  UnitCandelsFrame in '..\src\charts.v2\UnitCandelsFrame.pas' {CandelsFrame: TFrame},
  Lb.HistoryCandels in 'lib\Lb.HistoryCandels.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Candel.StructuresFile in '..\src\Lb.Candel.StructuresFile.pas',
  Lb.Candel.Source in '..\src\Lb.Candel.Source.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
