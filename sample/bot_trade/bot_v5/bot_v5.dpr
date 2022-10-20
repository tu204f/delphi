program bot_v5;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils.Candel in '..\src\Lb.SysUtils.Candel.pas',
  UnitBarsFrame in '..\src\charts.v3\UnitBarsFrame.pas' {BarsFrame: TFrame},
  UnitBarFrame in '..\src\charts.v3\UnitBarFrame.pas' {BarFrame: TFrame},
  UnitDoubleBarsFrame in '..\src\charts.v3\UnitDoubleBarsFrame.pas' {DoubleBarsFrame: TFrame},
  UnitGridFrame in '..\src\grid\UnitGridFrame.pas' {GirdFrame: TFrame},
  UnitDoubleGridFrame in '..\src\grid\UnitDoubleGridFrame.pas' {DoubleGridFrame: TFrame},
  Lb.SysUtils.StructureFile in '..\src\Lb.SysUtils.StructureFile.pas',
  Lb.SysUtils.SearhPatern in '..\src\Lb.SysUtils.SearhPatern.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
