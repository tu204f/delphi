program bot_v5;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitGridFrame in '..\src\grid\UnitGridFrame.pas' {GirdFrame: TFrame},
  UnitDoubleGridFrame in '..\src\grid\UnitDoubleGridFrame.pas' {DoubleGridFrame: TFrame},
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.SysUtils.SearhPatern in '..\..\..\library\trade\patern\Lb.SysUtils.SearhPatern.pas',
  Lb.SysUtils.StructureFile in '..\..\..\library\trade\patern\Lb.SysUtils.StructureFile.pas',
  UnitBarFrame in '..\..\..\library\trade\patern\chart\UnitBarFrame.pas' {BarFrame: TFrame},
  UnitBarsFrame in '..\..\..\library\trade\patern\chart\UnitBarsFrame.pas' {BarsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
