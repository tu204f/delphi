program bot_v5;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitBarsFrame in '..\src\charts.v3\UnitBarsFrame.pas' {BarsFrame: TFrame},
  UnitBarFrame in '..\src\charts.v3\UnitBarFrame.pas' {BarFrame: TFrame},
  UnitDoubleBarsFrame in '..\src\charts.v3\UnitDoubleBarsFrame.pas' {DoubleBarsFrame: TFrame},
  UnitGridFrame in '..\src\grid\UnitGridFrame.pas' {GirdFrame: TFrame},
  UnitDoubleGridFrame in '..\src\grid\UnitDoubleGridFrame.pas' {DoubleGridFrame: TFrame},
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.SysUtils.SearhPatern in '..\..\..\library\trade\patern\Lb.SysUtils.SearhPatern.pas',
  Lb.SysUtils.StructureFile in '..\..\..\library\trade\patern\Lb.SysUtils.StructureFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
