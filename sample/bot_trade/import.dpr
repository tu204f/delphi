program import;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitImportMainForm in 'UnitImportMainForm.pas' {MainForm},
  Lb.Candel.Source in 'src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in 'src\Lb.Candel.SysUtils.pas',
  Lb.DataModuleDB in '..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.SysUtils.ISO860 in '..\..\library\Lb.SysUtils.ISO860.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
