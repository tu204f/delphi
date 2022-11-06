program one_sec;









uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitQuikTableGridForm in 'UnitQuikTableGridForm.pas' {QuikTableGridForm},
  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  UnitBarFrame in '..\..\..\library\trade\patern\chart\UnitBarFrame.pas' {BarFrame: TFrame},
  UnitBarsFrame in '..\..\..\library\trade\patern\chart\UnitBarsFrame.pas' {BarsFrame: TFrame},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.SysUtils.SearhPatern in '..\..\..\library\trade\patern\Lb.SysUtils.SearhPatern.pas',
  Lb.SysUtils.StructureFile in '..\..\..\library\trade\patern\Lb.SysUtils.StructureFile.pas',
  UnitSearchStructureForm in 'UnitSearchStructureForm.pas' {SearchStructureForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchStructureForm, SearchStructureForm);
  Application.Run;
end.
