program import_file.v2;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.Source in '..\src\Lb.Candel.Source.pas',
  Lb.SysUtils.Candel in '..\src\Lb.SysUtils.Candel.pas',
  Lb.DataModuleDB in '..\..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
